#![forbid(unsafe_code)]
#![deny(rust_2018_idioms)]
#![deny(unused_must_use)]

//! Update subcommand for language self-updating via GitHub API
//!
//! This module handles updating the Veil language itself by checking GitHub releases
//! and downloading the appropriate binary for the current platform.

use anyhow::{Context, Result, anyhow};
use serde::Deserialize;
use std::env;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

/// GitHub API response for releases
#[derive(Debug, Deserialize)]
struct GitHubRelease {
    tag_name: String,
    #[allow(dead_code)]
    name: String,
    prerelease: bool,
    draft: bool,
    assets: Vec<GitHubAsset>,
    #[allow(dead_code)]
    published_at: String,
}

/// GitHub API response for release assets
#[derive(Debug, Deserialize)]
struct GitHubAsset {
    name: String,
    browser_download_url: String,
    size: u64,
}

/// Update channel configuration
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpdateChannel {
    Stable,  // Regular releases only
    Nightly, // Pre-releases (nightly builds)
}

impl Default for UpdateChannel {
    fn default() -> Self {
        UpdateChannel::Stable
    }
}

impl std::fmt::Display for UpdateChannel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UpdateChannel::Stable => write!(f, "stable"),
            UpdateChannel::Nightly => write!(f, "nightly"),
        }
    }
}

impl std::str::FromStr for UpdateChannel {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        match s.to_lowercase().as_str() {
            "stable" => Ok(UpdateChannel::Stable),
            "nightly" => Ok(UpdateChannel::Nightly),
            _ => Err(anyhow!("Invalid channel: {}. Use 'stable' or 'nightly'", s)),
        }
    }
}

/// Current version information
#[derive(Debug)]
struct VersionInfo {
    current: String,
    latest: String,
    download_url: String,
    asset_name: String,
    size: u64,
}

/// Execute the update flow for the Veil language itself.
///
/// This function checks GitHub releases API for updates and downloads the appropriate
/// binary for the current platform if an update is available.
///
/// Arguments:
/// - `verbose`: Print extra information during the update process.
/// - `force`: Force update even if already up to date.
pub fn run_update(verbose: bool, force: bool, channel: UpdateChannel) -> Result<()> {
    if verbose {
        println!(
            "🔍 Checking for ve toolchain updates on {} channel...",
            channel
        );
    }

    // Get current version
    let current_version = get_current_version()?;
    if verbose {
        println!("📦 Current version: {}", current_version);
    }

    // Check for updates
    let version_info = match check_for_updates(verbose, channel) {
        Ok(Some(info)) => info,
        Ok(None) => {
            if force {
                println!("🔄 No updates found, but forcing reinstall...");
                // Get the latest version anyway for forced reinstall
                get_latest_version_info(verbose, channel)?
            } else {
                println!("✅ You're already running the latest version!");
                return Ok(());
            }
        }
        Err(e) => return Err(e),
    };

    if !force && version_info.current == version_info.latest {
        println!("✅ You're already running the latest version!");
        return Ok(());
    }

    // Confirm update
    if !force {
        println!(
            "🆕 Update available: {} -> {}",
            version_info.current, version_info.latest
        );
        println!(
            "📁 Asset: {} ({:.1} MB)",
            version_info.asset_name,
            version_info.size as f64 / 1024.0 / 1024.0
        );
        print!("❓ Do you want to proceed? [y/N]: ");
        std::io::stdout().flush()?;

        let mut input = String::new();
        std::io::stdin().read_line(&mut input)?;
        if !input.trim().to_lowercase().starts_with('y') {
            println!("❌ Update cancelled.");
            return Ok(());
        }
    }

    // Download and install update
    download_and_install(version_info, verbose)?;

    println!("✅ ve toolchain updated successfully!");
    println!("🔄 Please restart your terminal to use the updated version.");

    Ok(())
}

/// Check if updates are available by comparing with GitHub releases.
fn check_for_updates(verbose: bool, channel: UpdateChannel) -> Result<Option<VersionInfo>> {
    let current_version = get_current_version()?;

    if verbose {
        println!("🔍 Fetching latest release information from GitHub...");
    }

    let latest_release = get_latest_release(channel)?;

    if verbose {
        println!(
            "🏷️  Latest release: {} ({})",
            latest_release.tag_name,
            if latest_release.prerelease {
                "pre-release"
            } else {
                "stable"
            }
        );
    }

    let target_triple = get_target_triple();
    let asset = find_matching_asset(&latest_release.assets, &target_triple)?;

    let version_info = VersionInfo {
        current: current_version.clone(),
        latest: latest_release.tag_name.clone(),
        download_url: asset.browser_download_url.clone(),
        asset_name: asset.name.clone(),
        size: asset.size,
    };

    // Normalize versions for comparison (remove 'v' prefix if present)
    let current_normalized = current_version
        .strip_prefix('v')
        .unwrap_or(&current_version);
    let latest_normalized = latest_release
        .tag_name
        .strip_prefix('v')
        .unwrap_or(&latest_release.tag_name);

    if current_normalized != latest_normalized {
        Ok(Some(version_info))
    } else {
        Ok(None)
    }
}

/// Get the latest version info for forced reinstalls.
fn get_latest_version_info(_verbose: bool, channel: UpdateChannel) -> Result<VersionInfo> {
    let current_version = get_current_version()?;
    let latest_release = get_latest_release(channel)?;
    let target_triple = get_target_triple();
    let asset = find_matching_asset(&latest_release.assets, &target_triple)?;

    Ok(VersionInfo {
        current: current_version,
        latest: latest_release.tag_name.clone(),
        download_url: asset.browser_download_url.clone(),
        asset_name: asset.name.clone(),
        size: asset.size,
    })
}

/// Fetch the latest release from GitHub API.
fn get_latest_release(channel: UpdateChannel) -> Result<GitHubRelease> {
    let url = match channel {
        UpdateChannel::Stable => "https://api.github.com/repos/veil-lang/veil/releases/latest",
        UpdateChannel::Nightly => "https://api.github.com/repos/veil-lang/veil/releases",
    };

    let response = ureq::get(url)
        .set("User-Agent", "veil-updater")
        .call()
        .with_context(|| "Failed to fetch release information from GitHub")?;

    match channel {
        UpdateChannel::Stable => {
            let release: GitHubRelease = response
                .into_json()
                .with_context(|| "Failed to parse GitHub API response")?;

            if release.draft {
                return Err(anyhow!("Latest release is a draft"));
            }

            Ok(release)
        }
        UpdateChannel::Nightly => {
            let releases: Vec<GitHubRelease> = response
                .into_json()
                .with_context(|| "Failed to parse GitHub API response")?;

            // Find the latest pre-release
            let latest_prerelease = releases
                .into_iter()
                .filter(|r| r.prerelease && !r.draft)
                .next()
                .ok_or_else(|| anyhow!("No nightly releases found"))?;

            Ok(latest_prerelease)
        }
    }
}

/// Find the matching asset for the current platform.
fn find_matching_asset<'a>(
    assets: &'a [GitHubAsset],
    target_triple: &str,
) -> Result<&'a GitHubAsset> {
    // Look for exact target triple match first
    if let Some(asset) = assets.iter().find(|a| a.name.contains(target_triple)) {
        return Ok(asset);
    }

    // Fallback to platform-specific matching
    let os = env::consts::OS;
    let arch = env::consts::ARCH;

    let platform_patterns = match os {
        "windows" => vec!["windows", "win", "pc-windows"],
        "macos" => vec!["darwin", "apple", "macos"],
        "linux" => vec!["linux", "unknown-linux"],
        _ => vec![os],
    };

    let arch_patterns = match arch {
        "x86_64" => vec!["x86_64", "amd64"],
        "aarch64" => vec!["aarch64", "arm64"],
        _ => vec![arch],
    };

    for asset in assets {
        let name_lower = asset.name.to_lowercase();
        let has_platform = platform_patterns.iter().any(|p| name_lower.contains(p));
        let has_arch = arch_patterns.iter().any(|a| name_lower.contains(a));

        if has_platform && has_arch {
            return Ok(asset);
        }
    }

    Err(anyhow!(
        "No compatible binary found for {} ({}). Available assets: {}",
        target_triple,
        format!("{}-{}", os, arch),
        assets
            .iter()
            .map(|a| a.name.as_str())
            .collect::<Vec<_>>()
            .join(", ")
    ))
}

/// Download and install the update using cargo install.
fn download_and_install(version_info: VersionInfo, verbose: bool) -> Result<()> {
    if verbose {
        println!("⬇️  Downloading {} from GitHub...", version_info.asset_name);
    }

    // Create temporary directory
    let temp_dir = std::env::temp_dir().join("veil-update");
    fs::create_dir_all(&temp_dir)?;

    // Download the asset
    let asset_path = temp_dir.join(&version_info.asset_name);
    let response = ureq::get(&version_info.download_url)
        .set("User-Agent", "veil-updater")
        .call()
        .with_context(|| "Failed to download update")?;

    let mut file = fs::File::create(&asset_path)?;
    std::io::copy(&mut response.into_reader(), &mut file)?;

    if verbose {
        println!("📦 Downloaded to: {}", asset_path.display());
    }

    // Extract and install using cargo
    install_with_cargo(&asset_path, verbose)?;

    // Cleanup
    fs::remove_dir_all(&temp_dir)?;

    Ok(())
}

/// Install using cargo install --path after extracting source.
fn install_with_cargo(archive_path: &PathBuf, verbose: bool) -> Result<()> {
    if verbose {
        println!("🔧 Installing using cargo install...");
    }

    // Check if cargo is available
    if Command::new("cargo").arg("--version").output().is_err() {
        return Err(anyhow!(
            "Installation requires 'cargo' command but it's not available.\n\
             Please install Rust/Cargo or extract {} manually",
            archive_path.display()
        ));
    }

    // Create temporary extraction directory
    let temp_extract = std::env::temp_dir().join("veil-extract");
    if temp_extract.exists() {
        fs::remove_dir_all(&temp_extract)?;
    }
    fs::create_dir_all(&temp_extract)?;

    // Extract the archive
    extract_archive(archive_path, &temp_extract, verbose)?;

    // Find the source directory (should contain Cargo.toml)
    let source_dir = find_source_directory(&temp_extract)?;

    if verbose {
        println!("🔍 Found source at: {}", source_dir.display());
    }

    // Create backup of current binary if it exists
    let current_exe = env::current_exe()?;
    let backup_path = current_exe.with_extension("backup");

    if current_exe.exists() {
        if verbose {
            println!("💾 Creating backup of current ve binary...");
        }
        fs::copy(&current_exe, &backup_path)?;
    }

    // Use cargo install --path to build and install the CLI package specifically
    let mut cmd = Command::new("cargo");
    cmd.args(&["install", "--path", "crates/cli", "--force"]);

    // Set the working directory to the extracted source
    cmd.current_dir(&source_dir);

    if !verbose {
        cmd.arg("--quiet");
    }

    if verbose {
        println!("🔨 Running: cargo install --path crates/cli --force");
    }

    let output = cmd
        .output()
        .with_context(|| "Failed to execute cargo install")?;

    if !output.status.success() {
        // Restore backup if installation fails
        if backup_path.exists() {
            if verbose {
                println!("❌ Installation failed, restoring backup...");
            }
            fs::rename(&backup_path, &current_exe)?;
        }

        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        return Err(anyhow!(
            "Cargo install failed:\nStdout: {}\nStderr: {}",
            stdout,
            stderr
        ));
    }

    // Verify the new binary works
    let verify_output = Command::new("ve").arg("--version").output();

    match verify_output {
        Ok(output) if output.status.success() => {
            if verbose {
                println!("✅ New binary verified successfully");
                println!(
                    "   Version: {}",
                    String::from_utf8_lossy(&output.stdout).trim()
                );
            }
            // Remove backup on successful verification
            if backup_path.exists() {
                fs::remove_file(&backup_path)?;
            }
        }
        _ => {
            // Restore backup if verification fails
            if backup_path.exists() {
                if verbose {
                    println!("❌ Verification failed, restoring backup...");
                }
                fs::rename(&backup_path, &current_exe)?;
                return Err(anyhow!(
                    "New binary verification failed, restored previous version"
                ));
            } else {
                return Err(anyhow!(
                    "New binary verification failed and no backup available"
                ));
            }
        }
    }

    // Cleanup
    fs::remove_dir_all(&temp_extract)?;

    if verbose {
        println!("🎉 Installation completed successfully!");
    }

    Ok(())
}

/// Extract archive to target directory.
fn extract_archive(archive_path: &PathBuf, extract_dir: &Path, verbose: bool) -> Result<()> {
    if verbose {
        println!("📦 Extracting archive...");
    }

    let archive_str = archive_path
        .to_str()
        .ok_or_else(|| anyhow!("Invalid archive path: {}", archive_path.display()))?;

    let archive_name = archive_path
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("");

    let output = if archive_name.ends_with(".tar.gz") || archive_name.ends_with(".tgz") {
        // Handle tar.gz files
        if Command::new("tar").arg("--version").output().is_err() {
            return Err(anyhow!(
                "Archive extraction requires 'tar' command but it's not available.\n\
                 Please install tar"
            ));
        }

        Command::new("tar")
            .args(&["-xzf", archive_str])
            .arg("-C")
            .arg(extract_dir)
            .output()
            .with_context(|| "Failed to execute tar command")?
    } else if archive_name.ends_with(".zip") {
        // Handle zip files
        if Command::new("unzip").arg("-v").output().is_ok() {
            Command::new("unzip")
                .args(&["-q", archive_str])
                .arg("-d")
                .arg(extract_dir)
                .output()
                .with_context(|| "Failed to execute unzip command")?
        } else {
            return Err(anyhow!(
                "Archive extraction requires 'unzip' command for .zip files"
            ));
        }
    } else {
        return Err(anyhow!(
            "Unsupported archive format: {}\n\
             Supported formats: .tar.gz, .tgz, .zip",
            archive_name
        ));
    };

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(anyhow!(
            "Failed to extract archive: {}\nArchive: {}",
            stderr,
            archive_path.display()
        ));
    }

    Ok(())
}

/// Find the source directory containing Cargo.toml.
fn find_source_directory(extract_dir: &Path) -> Result<PathBuf> {
    // Look for Cargo.toml in the extracted directory
    let direct_cargo = extract_dir.join("Cargo.toml");
    if direct_cargo.exists() {
        return Ok(extract_dir.to_path_buf());
    }

    // Search in subdirectories
    for entry in fs::read_dir(extract_dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_dir() {
            let cargo_toml = path.join("Cargo.toml");
            if cargo_toml.exists() {
                return Ok(path);
            }
        }
    }

    Err(anyhow!(
        "Could not find Cargo.toml in extracted archive.\n\
         This doesn't appear to be a valid Rust source archive."
    ))
}

/// Get the current version of ve.
fn get_current_version() -> Result<String> {
    // Use the package version from Cargo.toml
    // This should match the workspace version
    let version = env!("CARGO_PKG_VERSION");
    Ok(format!("v{}", version))
}

/// Get the target triple for the current platform.
fn get_target_triple() -> String {
    let arch = env::consts::ARCH;
    let os = env::consts::OS;

    match os {
        "windows" => format!("{}-pc-windows-msvc", arch),
        "macos" => format!("{}-apple-darwin", arch),
        "linux" => format!("{}-unknown-linux-gnu", arch),
        _ => format!("{}-unknown-{}", arch, os),
    }
}

/// Public function to check for updates without installing.
pub fn check_updates_only(verbose: bool, channel: UpdateChannel) -> Result<bool> {
    match check_for_updates(verbose, channel)? {
        Some(version_info) => {
            println!(
                "🆕 Update available: {} -> {}",
                version_info.current, version_info.latest
            );
            Ok(true)
        }
        None => {
            if verbose {
                println!("✅ No updates available");
            }
            Ok(false)
        }
    }
}
