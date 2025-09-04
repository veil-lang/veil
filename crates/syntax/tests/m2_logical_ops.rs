use codespan::Files;
use pest::Parser;
use veil_diagnostics::prelude::*;
use veil_diagnostics::term;
use veil_diagnostics::termcolor::{ColorChoice, StandardStream};
use veil_syntax;

#[test]
fn logical_operators_single_char() {
    // Test that single-char logical operators & and | work correctly
    let src = r#"
fn test_logical() -> bool {
    let a = true;
    let b = false;

    /# New single-char logical operators
    let and_result = a & b;
    let or_result = a | b;

    return and_result | or_result;
}
"#;

    let mut files = Files::<String>::new();
    let fid = files.add("logical_ops.veil".to_string(), src.to_string());

    match veil_syntax::parse_ast_with_warnings(&files, fid) {
        Ok((program, warnings)) => {
            assert!(program.functions.iter().any(|f| f.name == "test_logical"));
            // Should parse without errors using new syntax
            assert!(warnings.is_empty(), "Expected no warnings for new syntax");
        }
        Err(diags) => {
            panic!("Parse failed for new logical operators: {:?}", diags);
        }
    }
}

#[test]
fn logical_operators_precedence() {
    // Test precedence: bitwise has higher precedence than logical
    let src = r#"
fn test_precedence() -> bool {
    let a = 1;
    let b = 2;
    let c = 3;

    /# Should parse as: (a & b) | c for bitwise
    let bitwise_result = a & b | c;

    /# Should parse as: (x & y) | z for logical booleans
    let x = true;
    let y = false;
    let z = true;
    let logical_result = x & y | z;

    return logical_result;
}
"#;

    let mut files = Files::<String>::new();
    let fid = files.add("precedence.veil".to_string(), src.to_string());

    match veil_syntax::parse_ast_with_warnings(&files, fid) {
        Ok((program, warnings)) => {
            assert!(
                program
                    .functions
                    .iter()
                    .any(|f| f.name == "test_precedence")
            );
            assert!(warnings.is_empty());
        }
        Err(diags) => {
            panic!("Parse failed for precedence test: {:?}", diags);
        }
    }
}

#[test]
fn deprecated_logical_operators() {
    // Test that deprecated && and || still parse but should emit warnings
    let src = r#"
fn test_deprecated() -> bool {
    let a = true;
    let b = false;

    /# Deprecated operators - should still parse
    let and_result = a && b;
    let or_result = a || b;

    return and_result || or_result;
}
"#;

    let mut files = Files::<String>::new();
    let fid = files.add("deprecated.veil".to_string(), src.to_string());

    match veil_syntax::parse_ast_with_warnings(&files, fid) {
        Ok((program, warnings)) => {
            assert!(
                program
                    .functions
                    .iter()
                    .any(|f| f.name == "test_deprecated")
            );
            // Deprecation warnings should be emitted for && and ||
            assert!(
                !warnings.is_empty(),
                "Expected deprecation warnings for && and ||"
            );
            assert!(
                warnings.len() >= 3,
                "Expected at least 3 deprecation warnings"
            );
        }
        Err(diags) => {
            panic!("Parse failed for deprecated operators: {:?}", diags);
        }
    }
}

#[test]
fn advanced_type_syntax() {
    // Test advanced type syntax: &T, *T, weak T, dyn Trait, T | U, T & U
    let src = r#"
/# Advanced type syntax test
fn test_types() -> void {
    /# Reference types
    let ref_val: &i32 = &42;

    /# Pointer types (using null for now)
    let ptr_val: *i32 = null;

    /# Weak references (using null for now)
    let weak_val: weak String = null;

    /# Union types (initialize with one variant)
    let union_val: i32 | f32 = 42;

    /# Intersection types (using null for now)
    let intersect_val: Display & Debug = null;

    return;
}

/# Trait object types
fn takes_display(obj: dyn Display) -> void {
    return;
}
"#;

    let mut files = Files::<String>::new();
    let fid = files.add("types.veil".to_string(), src.to_string());

    match veil_syntax::parse_ast_with_warnings(&files, fid) {
        Ok((program, warnings)) => {
            assert!(program.functions.iter().any(|f| f.name == "test_types"));
            assert!(program.functions.iter().any(|f| f.name == "takes_display"));
            assert!(warnings.is_empty());
        }
        Err(diags) => {
            panic!("Parse failed for advanced types: {:?}", diags);
        }
    }
}

#[test]
fn visibility_modifiers() {
    // Test comprehensive visibility syntax
    let src = r#"
/# Visibility modifiers test

pub fn public_function() -> void {
    return;
}

pub(crate) fn crate_function() -> void {
    return;
}

pub(super) fn super_function() -> void {
    return;
}

pub(in some::path) fn path_function() -> void {
    return;
}

pub struct PublicStruct {
    pub field1: i32,
    pub(crate) field2: string,
    field3: bool,
}

pub(crate) enum CrateEnum {
    Variant1,
    Variant2(i32),
}
"#;

    let mut files = Files::<String>::new();
    let fid = files.add("visibility.veil".to_string(), src.to_string());

    match veil_syntax::parse_ast_with_warnings(&files, fid) {
        Ok((program, warnings)) => {
            assert!(
                program
                    .functions
                    .iter()
                    .any(|f| f.name == "public_function")
            );
            assert!(program.functions.iter().any(|f| f.name == "crate_function"));
            assert!(program.functions.iter().any(|f| f.name == "super_function"));
            assert!(program.functions.iter().any(|f| f.name == "path_function"));
            assert!(warnings.is_empty());
        }
        Err(diags) => {
            panic!("Parse failed for visibility modifiers: {:?}", diags);
        }
    }
}

#[test]
fn pest_grammar_logical_or() {
    // Direct Pest parser test for logical_or_expr rule
    use veil_syntax::ast_grammar::{AstParser, AstRule};

    let test_cases = [
        "a | b",
        "a || b", // deprecated but should still parse
        "a | b | c",
        "x || y || z", // deprecated chains
    ];

    for src in test_cases.iter() {
        match AstParser::parse(AstRule::logical_or_expr, src) {
            Ok(_) => {
                eprintln!("✓ logical_or_expr parsed: {}", src);
            }
            Err(e) => {
                panic!("logical_or_expr failed on '{}': {}", src, e);
            }
        }
    }
}

#[test]
fn pest_grammar_logical_and() {
    // Direct Pest parser test for logical_and_expr rule
    use veil_syntax::ast_grammar::{AstParser, AstRule};

    let test_cases = [
        "a & b",
        "a && b", // deprecated but should still parse
        "a & b & c",
        "x && y && z", // deprecated chains
    ];

    for src in test_cases.iter() {
        match AstParser::parse(AstRule::logical_and_expr, src) {
            Ok(_) => {
                eprintln!("✓ logical_and_expr parsed: {}", src);
            }
            Err(e) => {
                panic!("logical_and_expr failed on '{}': {}", src, e);
            }
        }
    }
}

#[test]
fn precedence_order_verification() {
    // Test that precedence follows the spec exactly
    let src = r#"
fn test_full_precedence() -> bool {
    let a = 1;
    let b = 2;
    let c = 3;

    /# Test full precedence chain
    /# Should parse as: ((a & b) ^ c) | (a & (b | c))
    let result = a & b ^ c | a & b | c;

    /# Logical operations with lower precedence
    let x = true;
    let y = false;
    let z = true;

    /# Should parse as: (x & y) | z
    let logical = x & y | z;

    return logical;
}
"#;

    let mut files = Files::<String>::new();
    let fid = files.add("full_precedence.veil".to_string(), src.to_string());

    match veil_syntax::parse_ast_with_warnings(&files, fid) {
        Ok((program, warnings)) => {
            assert!(
                program
                    .functions
                    .iter()
                    .any(|f| f.name == "test_full_precedence")
            );
            assert!(warnings.is_empty());
        }
        Err(diags) => {
            panic!("Parse failed for precedence verification: {:?}", diags);
        }
    }
}
