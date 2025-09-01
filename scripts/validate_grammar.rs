//! Grammar validation script using pest_meta
//!
//! This script validates the Veil grammar file using pest_meta to catch
//! grammar issues early in CI before they cause runtime parsing failures.

use std::fs;
use std::path::Path;
use std::process;

fn main() {
    let grammar_path = "../crates/syntax/src/veil.pest";

    if !Path::new(grammar_path).exists() {
        eprintln!("Error: Grammar file not found at {}", grammar_path);
        process::exit(1);
    }

    let grammar_content = match fs::read_to_string(grammar_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading grammar file {}: {}", grammar_path, e);
            process::exit(1);
        }
    };

    // Validate the grammar using pest_meta
    match pest_meta::parser::parse(pest_meta::parser::Rule::grammar_rules, &grammar_content) {
        Ok(_) => {
            println!("✓ Grammar validation passed for {}", grammar_path);
        }
        Err(e) => {
            eprintln!("✗ Grammar validation failed for {}:", grammar_path);
            eprintln!("{}", e);
            process::exit(1);
        }
    }

    // Additional checks can be added here, such as:
    // - Checking for unreachable rules
    // - Verifying rule naming conventions
    // - Detecting potential left recursion issues
    // - Ensuring all referenced rules are defined

    println!("Grammar validation completed successfully");
}
