//! Tests for automatic prelude injection
//!
//! This module tests that the parser automatically injects `import std/prelude;`
//! for all files except the prelude itself.

use veil_ast::{ImportDeclaration, ModuleType};
use veil_diagnostics::Files;
use veil_syntax::parse_ast;

#[test]
fn test_prelude_injection_normal_file() {
    let source = r#"
fn main() -> void {
    let x = 42;
}
"#;

    let mut files = Files::new();
    let file_id = files.add("main.veil".to_string(), source.to_string());

    let program = parse_ast(&files, file_id).expect("Parse should succeed");

    // Should have auto-injected prelude import
    assert!(
        !program.imports.is_empty(),
        "Should have auto-injected prelude import"
    );

    let prelude_import = &program.imports[0];
    match prelude_import {
        ImportDeclaration::ImportAll {
            module_path,
            module_type,
            alias,
        } => {
            assert_eq!(module_path, "std/prelude");
            assert!(matches!(module_type, ModuleType::Standard));
            assert!(alias.is_none());
        }
        _ => panic!("Expected ImportAll for prelude"),
    }
}

#[test]
fn test_prelude_injection_with_existing_imports() {
    let source = r#"
import std/io;
import my_module;

fn main() -> void {
    print("Hello");
}
"#;

    let mut files = Files::new();
    let file_id = files.add("main.veil".to_string(), source.to_string());

    let program = parse_ast(&files, file_id).expect("Parse should succeed");

    // Should have 3 imports: auto-injected prelude + 2 explicit imports
    assert_eq!(
        program.imports.len(),
        3,
        "Should have prelude + 2 explicit imports"
    );

    // First import should be auto-injected prelude
    let prelude_import = &program.imports[0];
    match prelude_import {
        ImportDeclaration::ImportAll {
            module_path,
            module_type,
            alias,
        } => {
            assert_eq!(module_path, "std/prelude");
            assert!(matches!(module_type, ModuleType::Standard));
            assert!(alias.is_none());
        }
        _ => panic!("Expected ImportAll for prelude"),
    }

    // Second import should be std::io (normalized from std/io)
    let io_import = &program.imports[1];
    match io_import {
        ImportDeclaration::ImportAll { module_path, .. } => {
            assert_eq!(module_path, "std::io");
        }
        _ => panic!("Expected ImportAll for std::io"),
    }

    // Third import should be my_module
    let module_import = &program.imports[2];
    match module_import {
        ImportDeclaration::ImportAll { module_path, .. } => {
            assert_eq!(module_path, "my_module");
        }
        _ => panic!("Expected ImportAll for my_module"),
    }
}

#[test]
fn test_no_prelude_injection_for_prelude_file() {
    let source = r#"
export fn panic(message: str) -> void {
    print(message);
}

export fn assert(condition: bool, message: str) -> void {
    if !condition {
        panic(message);
    }
}

"#;

    let mut files = Files::new();
    let file_id = files.add("prelude.veil".to_string(), source.to_string());

    let program = parse_ast(&files, file_id).expect("Parse should succeed");

    // Should NOT have auto-injected prelude import
    assert!(
        program.imports.is_empty(),
        "Prelude file should not have auto-injected import"
    );
}

#[test]
fn test_no_prelude_injection_for_std_prelude_path() {
    let source = r#"
export fn max(a: i32, b: i32) -> i32 {
    if a > b {
        return a;
    }
    return b;
}
"#;

    let mut files = Files::new();
    let file_id = files.add("std/prelude.veil".to_string(), source.to_string());

    let program = parse_ast(&files, file_id).expect("Parse should succeed");

    // Should NOT have auto-injected prelude import
    assert!(
        program.imports.is_empty(),
        "std/prelude file should not have auto-injected import"
    );
}

#[test]
fn test_no_prelude_injection_for_windows_std_prelude_path() {
    let source = r#"
export fn min(a: i32, b: i32) -> i32 {
    if a < b {
        return a;
    }
    return b;
}
"#;

    let mut files = Files::new();
    let file_id = files.add("std\\prelude.veil".to_string(), source.to_string());

    let program = parse_ast(&files, file_id).expect("Parse should succeed");

    // Should NOT have auto-injected prelude import
    assert!(
        program.imports.is_empty(),
        "std\\prelude file should not have auto-injected import"
    );
}

#[test]
fn test_prelude_injection_empty_file() {
    let source = "";

    let mut files = Files::new();
    let file_id = files.add("empty.veil".to_string(), source.to_string());

    let program = parse_ast(&files, file_id).expect("Parse should succeed");

    // Should have auto-injected prelude import even for empty file
    assert!(
        !program.imports.is_empty(),
        "Empty file should still have auto-injected prelude import"
    );

    let prelude_import = &program.imports[0];
    match prelude_import {
        ImportDeclaration::ImportAll {
            module_path,
            module_type,
            alias,
        } => {
            assert_eq!(module_path, "std/prelude");
            assert!(matches!(module_type, ModuleType::Standard));
            assert!(alias.is_none());
        }
        _ => panic!("Expected ImportAll for prelude"),
    }
}

#[test]
fn test_prelude_injection_only_comments() {
    let source = r#"
/# This is a comment #/
/# Another comment #/
"#;

    let mut files = Files::new();
    let file_id = files.add("comments.veil".to_string(), source.to_string());

    let program = parse_ast(&files, file_id).expect("Parse should succeed");

    // Should have auto-injected prelude import even for comments-only file
    assert!(
        !program.imports.is_empty(),
        "Comments-only file should still have auto-injected prelude import"
    );

    let prelude_import = &program.imports[0];
    match prelude_import {
        ImportDeclaration::ImportAll {
            module_path,
            module_type,
            alias,
        } => {
            assert_eq!(module_path, "std/prelude");
            assert!(matches!(module_type, ModuleType::Standard));
            assert!(alias.is_none());
        }
        _ => panic!("Expected ImportAll for prelude"),
    }
}

#[test]
fn test_prelude_injection_with_manual_prelude_import() {
    let source = r#"
import std/prelude;

fn main() -> void {
    panic("Test");
}
"#;

    let mut files = Files::new();
    let file_id = files.add("manual_prelude.veil".to_string(), source.to_string());

    let program = parse_ast(&files, file_id).expect("Parse should succeed");

    // Should have 2 imports: auto-injected prelude + manual prelude
    // This might be redundant but ensures the auto-injection doesn't break existing code
    assert!(
        program.imports.len() >= 1,
        "Should have at least the manual prelude import"
    );

    // Both should be prelude imports
    // Check that at least one prelude import exists
    let has_prelude = program.imports.iter().any(|import| match import {
        ImportDeclaration::ImportAll { module_path, .. } => {
            module_path == "std/prelude" || module_path == "std::prelude"
        }
        _ => false,
    });
    assert!(has_prelude, "Should have at least one prelude import");
}

#[test]
fn test_prelude_injection_nested_std_file() {
    let source = r#"
export fn some_io_function() -> void {
    /# IO implementation #/
}
"#;

    let mut files = Files::new();
    let file_id = files.add("std/io.veil".to_string(), source.to_string());

    let program = parse_ast(&files, file_id).expect("Parse should succeed");

    // std/io.veil should have auto-injected prelude import (it's not the prelude itself)
    assert!(
        !program.imports.is_empty(),
        "std/io.veil should have auto-injected prelude import"
    );

    let prelude_import = &program.imports[0];
    match prelude_import {
        ImportDeclaration::ImportAll {
            module_path,
            module_type,
            alias,
        } => {
            assert_eq!(module_path, "std/prelude");
            assert!(matches!(module_type, ModuleType::Standard));
            assert!(alias.is_none());
        }
        _ => panic!("Expected ImportAll for prelude"),
    }
}
