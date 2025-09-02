use anyhow::Result;
use std::path::{Path, PathBuf};

/// Run subcommand: compiles the given input and executes the resulting binary.
/// Uses the extracted end-to-end pipeline from this crate.
pub fn run_project(input: PathBuf, verbose: bool) -> Result<()> {
    // Default output name; process_build will place it under <input_dir>/build/
    let output = PathBuf::from("program.exe");

    // Build and run (process_build executes the binary when is_test = false)
    let _built = crate::process_build(
        input,
        output,
        /* optimize */ false,
        /* target_triple */ default_target_triple(),
        /* verbose */ verbose,
        /* dump_norm_hir */ false,
        /* is_test */ false,
        /* skip_cc */ false,
    )?;

    Ok(())
}

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
