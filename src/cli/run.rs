use crate::cli::process_build;
use std::path::PathBuf;

fn default_target_triple() -> String {
    let arch = std::env::consts::ARCH;
    let os = std::env::consts::OS;
    match os {
        "windows" => "x86_64-pc-windows-msvc".to_string(),
        "macos" => match arch {
            "aarch64" => "aarch64-apple-darwin".to_string(),
            _ => "x86_64-apple-darwin".to_string(),
        },
        "linux" => match arch {
            "aarch64" => "aarch64-unknown-linux-gnu".to_string(),
            _ => "x86_64-unknown-linux-gnu".to_string(),
        },
        _ => "x86_64-unknown-linux-gnu".to_string(),
    }
}

fn default_output_path() -> PathBuf {
    let is_windows = std::env::consts::OS == "windows";
    let filename = if is_windows { "program.exe" } else { "program" };
    PathBuf::from("build").join(filename)
}

pub fn run_project(input: std::path::PathBuf, verbose: bool) -> anyhow::Result<()> {
    let output = default_output_path();
    let target_triple = default_target_triple();
    process_build(input, output, false, target_triple, verbose, false, false)?;
    Ok(())
}
