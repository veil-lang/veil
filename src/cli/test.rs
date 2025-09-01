use crate::cli::process_build;
use anyhow::{Context, anyhow};
use colored::*;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Instant;

pub fn run_test(
    input: PathBuf,
    test_name: Option<String>,
    verbose: bool,
    list: bool,
    recursive: bool,
) -> anyhow::Result<()> {
    if input.is_file() {
        if list {
            return list_tests_file(input);
        }

        let content = fs::read_to_string(&input)
            .with_context(|| format!("Failed to read test file: {}", input.display()))?;

        let available_tests = parse_test_names(&content);

        if available_tests.is_empty() {
            println!("{}", "No tests found in the file.".yellow());
            return Ok(());
        }

        let tests_to_run = if let Some(ref specific_test) = test_name {
            if available_tests.contains(specific_test) {
                vec![specific_test.clone()]
            } else {
                return Err(anyhow!(
                    "Test '{}' not found. Available tests: {}",
                    specific_test,
                    available_tests.join(", ")
                ));
            }
        } else {
            available_tests
        };

        let executable_path = process_build(
            input.clone(),
            "build/program.exe".into(),
            false,
            default_target_triple(),
            verbose,
            true,
        )?;

        let (_passed, _failed) =
            run_tests_with_formatting(&executable_path, &tests_to_run, verbose, Some(&input))?;
        return Ok(());
    }

    if input.is_dir() {
        let veil_files = collect_veil_files(&input, recursive)?;
        if veil_files.is_empty() {
            println!(
                "{} {}",
                "No .veil files found in directory".yellow(),
                input.display()
            );
            return Ok(());
        }

        if list {
            println!("{}", "Available tests:".bold().blue());
            let mut total = 0usize;
            for file in &veil_files {
                let content = fs::read_to_string(file)
                    .with_context(|| format!("Failed to read test file: {}", file.display()))?;
                let available_tests = parse_test_names(&content);
                if available_tests.is_empty() {
                    continue;
                }
                println!("\n{}:", file.display().to_string().cyan());
                for (i, test_name) in available_tests.iter().enumerate() {
                    println!("  {}. {}", i + 1, test_name.green());
                }
                total += available_tests.len();
            }
            println!(
                "\nTotal: {} test{}",
                total,
                if total == 1 { "" } else { "s" }
            );
            return Ok(());
        }

        let mut grand_passed = 0usize;
        let mut grand_failed = 0usize;

        for file in veil_files {
            if verbose {
                println!(
                    "{}",
                    format!("Building tests for: {}", file.display()).yellow()
                );
            }

            let content = fs::read_to_string(&file)
                .with_context(|| format!("Failed to read test file: {}", file.display()))?;
            let available_tests = parse_test_names(&content);

            if available_tests.is_empty() {
                if verbose {
                    println!(
                        "{}",
                        format!("No tests found in: {}", file.display()).yellow()
                    );
                }
                continue;
            }

            let tests_to_run = if let Some(ref specific_test) = test_name {
                let mut matches = Vec::new();
                for t in &available_tests {
                    if t == specific_test {
                        matches.push(t.clone());
                    }
                }
                if matches.is_empty() {
                    continue;
                }
                matches
            } else {
                available_tests.clone()
            };

            let executable_path = process_build(
                file.clone(),
                "build/program.exe".into(),
                false,
                default_target_triple(),
                verbose,
                true,
            )?;

            let (passed, failed) =
                run_tests_with_formatting(&executable_path, &tests_to_run, verbose, Some(&file))?;

            grand_passed += passed;
            grand_failed += failed;
        }

        println!();
        if grand_failed == 0 {
            println!(
                "{} {} test{} passed",
                "✓".green().bold(),
                grand_passed.to_string().bold(),
                if grand_passed == 1 { "" } else { "s" }
            );
        } else {
            println!(
                "{} {} passed, {} failed",
                if grand_passed > 0 { "⚠" } else { "✗" }.yellow().bold(),
                grand_passed.to_string().green().bold(),
                grand_failed.to_string().red().bold()
            );
        }

        return Ok(());
    }

    Err(anyhow!(
        "Input path is neither a file nor a directory: {}",
        input.display()
    ))
}

fn parse_test_names(content: &str) -> Vec<String> {
    let mut tests = Vec::new();

    for line in content.lines() {
        let trimmed = line.trim();
        if let Some(after_test) = trimmed.strip_prefix("test ")
            && let Some(name_end) = after_test.find(' ').or_else(|| after_test.find('{'))
        {
            let test_name = after_test[..name_end].trim().to_string();
            if !test_name.is_empty() {
                tests.push(test_name);
            }
        }
    }

    tests
}

fn run_tests_with_formatting(
    executable_path: &PathBuf,
    tests: &[String],
    verbose: bool,
    context_file: Option<&Path>,
) -> anyhow::Result<(usize, usize)> {
    let total_tests = tests.len();
    let mut passed = 0usize;
    let mut failed = 0usize;

    if let Some(file) = context_file {
        println!(
            "{} {}",
            format!("🧪 Running Tests for:").bold().blue(),
            file.display()
        );
    } else {
        println!("{}", "🧪 Running Tests".bold().blue());
    }
    println!();

    let overall_start = Instant::now();

    for (index, test_name) in tests.iter().enumerate() {
        let test_number = index + 1;

        print!("{} ", format!("[{}/{}]", test_number, total_tests).dimmed());
        print!("{} ", "RUNNING".cyan());
        print!("{}", test_name.bold());
        io::stdout().flush().unwrap();

        let test_start = Instant::now();
        let result = Command::new(executable_path)
            .arg(test_name)
            .output()
            .with_context(|| format!("Failed to run test: {}", test_name))?;

        let test_duration = test_start.elapsed();

        if result.status.success() {
            print!(
                "\r{} ",
                format!("[{}/{}]", test_number, total_tests).dimmed()
            );
            print!("{} ", "✓ PASS".green().bold());
            print!("{}", test_name.bold());
            println!(" {}", format!("({:.2?})", test_duration).dimmed());
            passed += 1;
        } else {
            print!(
                "\r{} ",
                format!("[{}/{}]", test_number, total_tests).dimmed()
            );
            print!("{} ", "✗ FAIL".red().bold());
            print!("{}", test_name.bold());
            println!(" {}", format!("({:.2?})", test_duration).dimmed());
            failed += 1;

            if verbose || !result.stderr.is_empty() {
                let stderr = String::from_utf8_lossy(&result.stderr);
                if !stderr.trim().is_empty() {
                    println!("      {}", "Error output:".red());
                    for line in stderr.lines() {
                        println!("      {}", line.dimmed());
                    }
                }
            }

            let stdout = String::from_utf8_lossy(&result.stdout);
            if !stdout.trim().is_empty() && verbose {
                println!("      {}", "Standard output:".blue());
                for line in stdout.lines() {
                    println!("      {}", line.dimmed());
                }
            }
        }
    }

    let overall_duration = overall_start.elapsed();

    println!();
    if failed == 0 {
        println!(
            "{} {} test{} passed {}",
            "✓".green().bold(),
            passed.to_string().bold(),
            if passed == 1 { "" } else { "s" },
            format!("({:.2?})", overall_duration).dimmed()
        );
    } else {
        println!(
            "{} {} passed, {} failed {}",
            if passed > 0 { "⚠" } else { "✗" }.yellow().bold(),
            passed.to_string().green().bold(),
            failed.to_string().red().bold(),
            format!("({:.2?})", overall_duration).dimmed()
        );
    }

    Ok((passed, failed))
}

fn list_tests_file(input: PathBuf) -> anyhow::Result<()> {
    let content = fs::read_to_string(&input)
        .with_context(|| format!("Failed to read test file: {}", input.display()))?;

    let available_tests = parse_test_names(&content);

    if available_tests.is_empty() {
        println!("{}", "No tests found in the file.".yellow());
        return Ok(());
    }

    println!("{}", "Available tests:".bold().blue());
    for (i, test_name) in available_tests.iter().enumerate() {
        println!("  {}. {}", i + 1, test_name.green());
    }
    println!(
        "\nTotal: {} test{}",
        available_tests.len(),
        if available_tests.len() == 1 { "" } else { "s" }
    );

    Ok(())
}

fn collect_veil_files(path: &Path, recursive: bool) -> anyhow::Result<Vec<PathBuf>> {
    let mut results = Vec::new();

    if path.is_file() {
        if path.extension().and_then(|s| s.to_str()) == Some("veil") {
            results.push(path.to_path_buf());
        }
        return Ok(results);
    }

    if !path.is_dir() {
        return Ok(results);
    }

    if recursive {
        let mut stack = vec![path.to_path_buf()];
        while let Some(curr) = stack.pop() {
            for entry in fs::read_dir(&curr).with_context(|| {
                format!(
                    "Failed to read directory while scanning: {}",
                    curr.display()
                )
            })? {
                let entry = entry?;
                let p = entry.path();
                if p.is_dir() {
                    stack.push(p);
                } else if p.is_file() && p.extension().and_then(|s| s.to_str()) == Some("veil") {
                    results.push(p);
                }
            }
        }
    } else {
        for entry in fs::read_dir(path).with_context(|| {
            format!(
                "Failed to read directory while scanning: {}",
                path.display()
            )
        })? {
            let entry = entry?;
            let p = entry.path();
            if p.is_file() && p.extension().and_then(|s| s.to_str()) == Some("veil") {
                results.push(p);
            }
        }
    }

    results.sort_by(|a, b| a.display().to_string().cmp(&b.display().to_string()));
    Ok(results)
}

fn default_target_triple() -> String {
    if cfg!(target_os = "windows") {
        "x86_64-pc-windows-msvc".to_string()
    } else if cfg!(target_os = "macos") {
        if std::env::consts::ARCH == "aarch64" {
            "aarch64-apple-darwin".to_string()
        } else {
            "x86_64-apple-darwin".to_string()
        }
    } else {
        if std::env::consts::ARCH == "aarch64" {
            "aarch64-unknown-linux-gnu".to_string()
        } else {
            "x86_64-unknown-linux-gnu".to_string()
        }
    }
}
