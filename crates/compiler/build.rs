use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    // Re-run if this script changes or if common git metadata changes.
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=.git/HEAD");
    println!("cargo:rerun-if-changed=.git/refs/heads");
    println!("cargo:rerun-if-changed=.git/refs/tags");
    println!("cargo:rerun-if-env-changed=SOURCE_DATE_EPOCH");

    // Collect metadata
    let git_sha = git_sha_short().unwrap_or_else(|| "unknown".to_string());
    let git_dirty = git_dirty().unwrap_or(false);
    let git_tag = git_describe().unwrap_or_else(|| "unknown".to_string());
    let rustc_version = rustc_version().unwrap_or_else(|| "unknown".to_string());
    let target_triple = env::var("TARGET").unwrap_or_else(|_| "unknown".to_string());
    let profile = env::var("PROFILE").unwrap_or_else(|_| "unknown".to_string());
    let pkg_name = env::var("CARGO_PKG_NAME").unwrap_or_else(|_| "unknown".to_string());
    let pkg_version = env::var("CARGO_PKG_VERSION").unwrap_or_else(|_| "0.0.0".to_string());
    let build_ts = build_timestamp_unix();

    // Compose a fingerprint string (stable, order-sensitive) suitable for cache keys
    // Format: name@version|rustc|target|profile|git(tag=..,sha=..[+dirty])|ts=UNIX
    let fingerprint = format!(
        "{}@{}|{}|{}|{}|{}(sha={}{}{})|ts={}",
        pkg_name,
        pkg_version,
        rustc_version,
        target_triple,
        profile,
        if git_tag == "unknown" { "tag=-" } else { "tag" },
        git_sha,
        if git_dirty { "+dirty" } else { "" },
        if git_tag == "unknown" {
            String::new()
        } else {
            format!(",tag={}", git_tag)
        },
        build_ts
    );

    // Export as compile-time environment variables for the crate to consume:
    println!(
        "cargo:rustc-env=VEIL_COMPILER_GIT_SHA={}",
        with_dirty(git_sha, git_dirty)
    );
    println!("cargo:rustc-env=VEIL_COMPILER_GIT_TAG={}", git_tag);
    println!(
        "cargo:rustc-env=VEIL_COMPILER_RUSTC_VERSION={}",
        rustc_version
    );
    println!("cargo:rustc-env=VEIL_COMPILER_TARGET={}", target_triple);
    println!("cargo:rustc-env=VEIL_COMPILER_PROFILE={}", profile);
    println!("cargo:rustc-env=VEIL_COMPILER_PKG_NAME={}", pkg_name);
    println!("cargo:rustc-env=VEIL_COMPILER_PKG_VERSION={}", pkg_version);
    println!("cargo:rustc-env=VEIL_COMPILER_BUILD_UNIX_TS={}", build_ts);
    println!(
        "cargo:rustc-env=VEIL_COMPILER_BUILD_FINGERPRINT={}",
        fingerprint
    );

    // Optional: emit a cargo warning on dirty tree to make it visible in logs
    if git_dirty {
        println!(
            "cargo:warning=Veil compiler crate built from a dirty git tree; cache keys will include +dirty"
        );
    }
}

fn with_dirty(mut sha: String, dirty: bool) -> String {
    if dirty {
        sha.push_str("+dirty");
    }
    sha
}

fn run_cmd(cmd: &str, args: &[&str]) -> Option<String> {
    let output = Command::new(cmd).args(args).output().ok()?;
    if !output.status.success() {
        return None;
    }
    let s = String::from_utf8(output.stdout).ok()?;
    let trimmed = s.trim();
    if trimmed.is_empty() {
        None
    } else {
        Some(trimmed.to_string())
    }
}

fn git_sha_short() -> Option<String> {
    // Prefer a 12-char short SHA for readability
    run_cmd("git", &["rev-parse", "--short=12", "HEAD"])
}

fn git_describe() -> Option<String> {
    // Try to get nearest tag; fallback is handled by caller
    run_cmd("git", &["describe", "--tags", "--always"])
}

fn git_dirty() -> Option<bool> {
    // Fast path: if no .git directory, not a git checkout
    if !Path::new(".git").exists() {
        return Some(false);
    }
    // Use 'git status --porcelain' to detect uncommitted changes
    let out = run_cmd("git", &["status", "--porcelain"])?;
    Some(!out.trim().is_empty())
}

fn rustc_version() -> Option<String> {
    // Use the RUSTC from the environment if available
    let rustc = env::var("RUSTC").unwrap_or_else(|_| "rustc".to_string());
    run_cmd(&rustc, &["--version", "--verbose"]).and_then(|full| {
        // Take the first line to keep it compact
        full.lines().next().map(|s| s.to_string())
    })
}

fn build_timestamp_unix() -> u64 {
    if let Ok(src_epoch) = env::var("SOURCE_DATE_EPOCH") {
        if let Ok(v) = src_epoch.trim().parse::<u64>() {
            return v;
        }
    }
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

// On some CI providers, git metadata may not exist in shallow checkouts.
// This helper checks a path existence robustly without failing the build.
#[allow(dead_code)]
fn exists(path: &str) -> bool {
    fs::metadata(path).is_ok()
}
