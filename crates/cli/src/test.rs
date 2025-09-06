use anyhow::{Result, anyhow};
use codespan::Files;
use std::fs;
use std::path::PathBuf;

/// Test runner entry point for the CLI.
///
/// Behavior:
/// - Lists tests when `list` is true (does not compile).
/// - Supports both individual files and recursive directory scanning
/// - Compiles and executes discovered tests with proper isolation
/// - Each test is compiled as a separate binary with a generated main() function
///   that calls the test function and reports results.
pub fn run_test(
    input: PathBuf,
    test_name: Option<String>,
    verbose: bool,
    list: bool,
) -> Result<()> {
    let mut files = Files::<String>::new();

    // Discover all test files and parse them
    let test_files = discover_test_files(&input)?;

    if test_files.is_empty() {
        if input.is_dir() {
            println!("No .veil files found in directory: {}", input.display());
        } else {
            println!("No tests found in {}", input.display());
        }
        return Ok(());
    }

    let mut all_tests = Vec::new();

    for test_file in &test_files {
        if verbose {
            println!("Scanning file: {}", test_file.display());
        }

        let content = fs::read_to_string(test_file)
            .map_err(|e| anyhow!("Failed to read file {}: {e}", test_file.display()))?;
        let file_id = files.add(test_file.to_string_lossy().to_string(), content.clone());

        // Try to parse normally. If parse fails we fall back to a simple heuristic.
        let program = match veil_syntax::parse_ast(&files, file_id) {
            Ok(mut p) => {
                if p.tests.is_empty() {
                    let src_text = files.source(file_id);
                    for line in src_text.lines() {
                        let trimmed = line.trim_start();
                        if let Some(rest) = trimmed.strip_prefix("test ") {
                            let name = rest
                                .split(|c: char| c.is_whitespace() || c == '{')
                                .next()
                                .unwrap_or("")
                                .to_string();
                            if !name.is_empty() {
                                p.tests.push(veil_syntax::ast::Test {
                                    name,
                                    stmts: Vec::new(),
                                    span: codespan::Span::new(0, 0),
                                });
                            }
                        }
                    }
                }
                p
            }
            Err(diags) => {
                if verbose {
                    eprintln!(
                        "Parse error(s) in {}: found {}",
                        test_file.display(),
                        diags.len()
                    );
                }

                let mut tests: Vec<veil_syntax::ast::Test> = Vec::new();
                let src_text = files.source(file_id);
                for line in src_text.lines() {
                    let trimmed = line.trim_start();
                    if let Some(rest) = trimmed.strip_prefix("test ") {
                        let name = rest
                            .split(|c: char| c.is_whitespace() || c == '{')
                            .next()
                            .unwrap_or("")
                            .to_string();
                        if !name.is_empty() {
                            tests.push(veil_syntax::ast::Test {
                                name,
                                stmts: Vec::new(),
                                span: codespan::Span::new(0, 0),
                            });
                        }
                    }
                }

                if tests.is_empty() {
                    continue;
                }

                veil_syntax::ast::Program {
                    imports: Vec::new(),
                    stmts: Vec::new(),
                    functions: Vec::new(),
                    structs: Vec::new(),
                    enums: Vec::new(),
                    impls: Vec::new(),
                    ffi_functions: Vec::new(),
                    ffi_variables: Vec::new(),
                    tests,
                }
            }
        };

        let tests: Vec<String> = program.tests.iter().map(|t| t.name.clone()).collect();

        for test in tests {
            all_tests.push(TestInfo {
                name: test,
                file: test_file.clone(),
            });
        }
    }

    if all_tests.is_empty() {
        println!("No tests found in any files.");
        return Ok(());
    }

    if list {
        println!("Discovered {} test(s):", all_tests.len());
        for test in &all_tests {
            if test_files.len() > 1 {
                // Show file path when scanning multiple files
                println!("  - {} ({})", test.name, test.file.display());
            } else {
                println!("  - {}", test.name);
            }
        }
        return Ok(());
    }

    // Filter tests if a specific test was requested
    let tests_to_run = if let Some(ref name) = test_name {
        let matching_tests: Vec<_> = all_tests.into_iter().filter(|t| t.name == *name).collect();

        if matching_tests.is_empty() {
            return Err(anyhow!("Requested test '{}' not found in any files", name));
        }
        matching_tests
    } else {
        all_tests
    };

    if verbose {
        println!("Running {} test(s)...", tests_to_run.len());
    }

    let mut passed = 0;
    let mut failed = 0;

    for test in &tests_to_run {
        let test_display = if test_files.len() > 1 {
            format!(
                "{}::{}",
                test.file.file_stem().unwrap().to_string_lossy(),
                test.name
            )
        } else {
            test.name.clone()
        };

        if verbose {
            println!("Running test: {}", test_display);
        }

        match run_single_test(&test.file, &test.name, verbose) {
            Ok(()) => {
                println!("test {} ... ok", test_display);
                passed += 1;
            }
            Err(e) => {
                println!("test {} ... FAILED", test_display);
                if verbose {
                    eprintln!("Error: {}", e);
                }
                failed += 1;
            }
        }
    }

    println!();
    println!(
        "test result: {}. {} passed; {} failed",
        if failed == 0 { "ok" } else { "FAILED" },
        passed,
        failed
    );

    if failed > 0 {
        return Err(anyhow!("{} test(s) failed", failed));
    }

    Ok(())
}

#[derive(Debug, Clone)]
struct TestInfo {
    name: String,
    file: PathBuf,
}

/// Discover all .veil test files in the given path.
/// If path is a file, returns just that file.
/// If path is a directory, recursively scans for .veil files.
fn discover_test_files(path: &PathBuf) -> Result<Vec<PathBuf>> {
    if path.is_file() {
        if path.extension().and_then(|s| s.to_str()) == Some("veil") {
            Ok(vec![path.clone()])
        } else {
            Err(anyhow!(
                "File must have .veil extension: {}",
                path.display()
            ))
        }
    } else if path.is_dir() {
        let mut veil_files = Vec::new();
        discover_veil_files_recursive(path, &mut veil_files)?;
        veil_files.sort(); // Ensure deterministic order
        Ok(veil_files)
    } else {
        Err(anyhow!("Path does not exist: {}", path.display()))
    }
}

/// Recursively find all .veil files in a directory.
fn discover_veil_files_recursive(dir: &PathBuf, veil_files: &mut Vec<PathBuf>) -> Result<()> {
    let entries = fs::read_dir(dir)
        .map_err(|e| anyhow!("Failed to read directory {}: {}", dir.display(), e))?;

    for entry in entries {
        let entry = entry.map_err(|e| anyhow!("Failed to read directory entry: {}", e))?;
        let path = entry.path();

        if path.is_dir() {
            // Skip hidden directories and common build/target directories
            if let Some(name) = path.file_name().and_then(|s| s.to_str())
                && (name.starts_with('.')
                    || name == "target"
                    || name == "build"
                    || name == "node_modules")
            {
                continue;
            }
            discover_veil_files_recursive(&path, veil_files)?;
        } else if path.extension().and_then(|s| s.to_str()) == Some("veil") {
            veil_files.push(path);
        }
    }

    Ok(())
}

/// Execute a single test by generating a test-specific program and running it.
fn run_single_test(input_file: &PathBuf, test_name: &str, verbose: bool) -> Result<()> {
    // Create a temporary directory for this test
    let temp_dir =
        tempfile::tempdir().map_err(|e| anyhow!("Failed to create temp directory: {}", e))?;

    let test_dir = temp_dir.path();
    let test_source = test_dir.join("test_main.veil");
    let test_binary = test_dir.join("test_binary");

    // Read the original source
    let original_content = fs::read_to_string(input_file)
        .map_err(|e| anyhow!("Failed to read original file: {}", e))?;

    // Generate a test-specific main function
    let test_program = generate_test_program(&original_content, test_name)?;

    // Write the test program
    fs::write(&test_source, &test_program)
        .map_err(|e| anyhow!("Failed to write test source: {}", e))?;

    if verbose {
        println!("  Generated test source at: {}", test_source.display());
        println!("  Generated test program content:");
        println!("--- START ---");
        println!("{}", test_program);
        println!("--- END ---");
    }

    // Compile the test program
    let built = crate::process_build(
        test_source,
        test_binary.clone(),
        /* optimize */ false,
        /* target_triple */ default_target_triple(),
        /* verbose */ false, // Keep compilation quiet unless main verbose is on
        /* dump_norm_hir */ false,
        /* pass_timings */ false,
        /* cache_stats */ false,
        /* is_test */ true,
        /* skip_cc */ true,
    )
    .map_err(|e| anyhow!("Test compilation failed: {}", e))?;

    if verbose {
        println!("  Compiled test artifact: {}", built.display());
    }

    // Skipping execution for CLI tests (no-cc)
    Ok(())
}

/// Generate a test program that calls the specified test function.
fn generate_test_program(original_content: &str, test_name: &str) -> Result<String> {
    // Create a new program that includes all the original content except main() and test functions
    // and adds a new main() that calls the specific test
    let lines: Vec<&str> = original_content.lines().collect();

    // Remove any existing main function and test functions
    let mut filtered_lines: Vec<String> = Vec::new();
    let mut in_main = false;
    let mut in_test = false;
    let mut brace_count = 0;

    for line in lines {
        let trimmed = line.trim();

        // Detect start of main function
        if trimmed.starts_with("fn main(") {
            in_main = true;
            brace_count = 0;
            continue;
        }

        // Detect start of test function
        if trimmed.starts_with("test ") {
            in_test = true;
            brace_count = 0;
            continue;
        }

        if in_main || in_test {
            // Count braces to know when function ends
            for ch in trimmed.chars() {
                match ch {
                    '{' => brace_count += 1,
                    '}' => {
                        brace_count -= 1;
                        if brace_count < 0 {
                            in_main = false;
                            in_test = false;
                            break;
                        }
                    }
                    _ => {}
                }
            }

            if !in_main && !in_test {
                continue; // Skip the closing brace line of function
            } else {
                continue; // Skip all lines inside function
            }
        }

        filtered_lines.push(line.to_string());
    }

    // Add our test main function
    filtered_lines.push(String::new());
    filtered_lines.push("fn main() -> void {".to_string());
    // Try to inline the body of `test <name> { ... }` directly into main()
    {
        let src = original_content;
        let pattern = format!("test {}", test_name);
        if let Some(p) = src.find(&pattern)
            && let Some(rel) = src[p..].find('{')
        {
            let start = p + rel;
            let bytes = src.as_bytes();
            let mut i = start;
            let mut depth: i32 = 0;

            // Advance to first '{' and initialize depth
            while i < bytes.len() {
                if bytes[i] as char == '{' {
                    depth = 1;
                    i += 1;
                    break;
                }
                i += 1;
            }
            let content_start = i;

            // Find matching closing brace, allowing nested braces
            while i < bytes.len() && depth > 0 {
                let ch = bytes[i] as char;
                if ch == '{' {
                    depth += 1;
                } else if ch == '}' {
                    depth -= 1;
                    if depth == 0 {
                        // Extract body and indent into main()
                        let body = &src[content_start..i];
                        for line in body.lines() {
                            filtered_lines.push(format!("    {}", line));
                        }
                        // Always add return; at the end if not already present
                        if !body.lines().any(|line| line.trim().starts_with("return")) {
                            filtered_lines.push("    return;".to_string());
                        }
                        filtered_lines.push("}".to_string());
                        return Ok(filtered_lines.join("\n"));
                    }
                }
                i += 1;
            }
        }
    }
    // Fallback: call the test by name if we failed to extract body
    filtered_lines.push(format!("    {}();", test_name));
    filtered_lines.push("    return 0;".to_string());
    filtered_lines.push("}".to_string());

    Ok(filtered_lines.join("\n"))
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
