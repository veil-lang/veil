use anyhow::{Result, anyhow};
use codespan::Files;
use std::fs;
use std::path::PathBuf;

/// Test runner entry point for the CLI.
///
/// Behavior:
/// - Lists tests when `list` is true (does not compile).
/// - Otherwise, compiles the program and reports discovered tests. Execution of
///   individual test bodies is not currently supported by the generated binary,
///   but this flow validates that all code (including test blocks) type-checks
///   and compiles successfully.
///
/// Notes:
/// - Test declarations are parsed from the entry file. Each `test name { ... }`
///   found is treated as a test case.
/// - A future enhancement can synthesize a temporary main() per test and
///   execute it; for now we keep compilation-only semantics to maintain
///   consistent behavior with the rest of the pipeline.
pub fn run_test(
    input: PathBuf,
    test_name: Option<String>,
    verbose: bool,
    list: bool,
) -> Result<()> {
    let mut files = Files::<String>::new();

    // Read and parse the entry file to discover tests
    let content = fs::read_to_string(&input)
        .map_err(|e| anyhow!("Failed to read input file {}: {e}", input.display()))?;
    let file_id = files.add(input.to_string_lossy().to_string(), content);

    let program = match veil_syntax::parse_ast(&files, file_id) {
        Ok(p) => p,
        Err(diags) => {
            if verbose {
                eprintln!("Parse error(s): found {}", diags.len());
            }
            return Err(anyhow!("Parse failed for {}", input.display()));
        }
    };

    let tests: Vec<String> = program.tests.iter().map(|t| t.name.clone()).collect();

    if list {
        if tests.is_empty() {
            println!("No tests found in {}", input.display());
        } else {
            println!("Discovered {} test(s):", tests.len());
            for t in &tests {
                println!("  - {}", t);
            }
        }
        return Ok(());
    }

    // If a specific test was requested, verify it exists
    if let Some(ref name) = test_name {
        if !tests.iter().any(|t| t == name) {
            return Err(anyhow!(
                "Requested test '{}' not found in {}",
                name,
                input.display()
            ));
        }
    }

    // Compile the program as-is. This validates that test blocks type-check and
    // compile. We do not execute test bodies yet.
    let output = PathBuf::from("tests.exe");
    let built = crate::process_build(
        input.clone(),
        output,
        /* optimize */ false,
        /* target_triple */ default_target_triple(),
        /* verbose */ verbose,
        /* dump_norm_hir */ false,
        /* pass_timings */ false,
        /* cache_stats */ false,
        /* is_test */ true, // Do not run the compiled binary here
        /* skip_cc */ false, // Produce the binary to validate link-time issues
    )?;

    if verbose {
        println!("Build artifact: {}", built.display());
    }

    if tests.is_empty() {
        println!("No tests to run.");
        return Ok(());
    }

    match test_name {
        Some(name) => {
            // Placeholder: compilation succeeded; report as "collected"
            println!("Collected 1 test: {}", name);
            println!("Note: test execution is not yet implemented; compilation passed.");
        }
        None => {
            println!("Collected {} test(s).", tests.len());
            for t in &tests {
                println!("  - {}", t);
            }
            println!("Note: test execution is not yet implemented; compilation passed.");
        }
    }

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
