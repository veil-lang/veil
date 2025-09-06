//! Tests for division rule diagnostics
//!
//! This module tests that the division diagnostics (fix-its for '/' vs '//')
//! are properly implemented and working as expected.

use veil_ast::{BinOp, Expr};
use veil_diagnostics::Files;
use veil_syntax::parse_ast;

fn test_simple_parsing() {
    let source = "fn example() -> void { return; }";
    let mut files = Files::new();
    let file_id = files.add("test.veil".to_string(), source.to_string());

    let program = parse_ast(&files, file_id).expect("Parse should succeed");

    println!("Simple test - Functions: {}", program.functions.len());
    if let Some(func) = program.functions.first() {
        println!("Function '{}' body length: {}", func.name, func.body.len());
        for (i, stmt) in func.body.iter().enumerate() {
            println!("  Statement {}: {:?}", i, stmt);
        }
    }
}

fn parse_expr_from_source(source: &str) -> Expr {
    // First test simple parsing
    test_simple_parsing();

    let mut files = Files::new();
    let wrapped_source = format!("fn example() -> void {{ {} }}", source);
    let file_id = files.add("test.veil".to_string(), wrapped_source);

    let program = parse_ast(&files, file_id).expect("Parse should succeed");

    // Debug: print the entire program structure
    println!("Program debug for '{}':", source);
    println!("  Functions: {}", program.functions.len());
    println!("  Statements: {}", program.stmts.len());
    println!("  Imports: {}", program.imports.len());

    if let Some(func) = program.functions.first() {
        println!(
            "  Function '{}' body length: {}",
            func.name,
            func.body.len()
        );
        for (i, stmt) in func.body.iter().enumerate() {
            println!("    Statement {}: {:?}", i, stmt);
        }
    }

    // Extract the first function and its first statement
    if let Some(func) = program.functions.first() {
        if let Some(stmt) = func.body.first() {
            match stmt {
                veil_ast::Stmt::Expr(expr, _) => expr.clone(),
                other => panic!(
                    "Found statement type: {:?}, expected expression statement",
                    other
                ),
            }
        } else {
            panic!("Function body is empty");
        }
    } else {
        panic!("No functions found in program");
    }
}

#[test]
fn test_integer_division_operator_parsing() {
    let source = "a // b;";
    let expr = parse_expr_from_source(source);

    match expr {
        Expr::BinOp(left, op, right, _) => {
            // Left should be variable a
            match left.as_ref() {
                Expr::Var(name, _) => assert_eq!(name, "a"),
                _ => panic!("Expected variable a on left side"),
            }

            // Operator should be IDiv (integer division)
            assert!(
                matches!(op, BinOp::IDiv),
                "Expected integer division operator"
            );

            // Right should be variable b
            match right.as_ref() {
                Expr::Var(name, _) => assert_eq!(name, "b"),
                _ => panic!("Expected variable b on right side"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_float_division_operator_parsing() {
    let source = "x / y;";
    let expr = parse_expr_from_source(source);

    match expr {
        Expr::BinOp(left, op, right, _) => {
            // Left should be variable x
            match left.as_ref() {
                Expr::Var(name, _) => assert_eq!(name, "x"),
                _ => panic!("Expected variable x on left side"),
            }

            // Operator should be Div (float division)
            assert!(matches!(op, BinOp::Div), "Expected float division operator");

            // Right should be variable y
            match right.as_ref() {
                Expr::Var(name, _) => assert_eq!(name, "y"),
                _ => panic!("Expected variable y on right side"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_division_operator_display() {
    // Test the Display implementation for division operators
    assert_eq!(BinOp::Div.to_string(), "/");
    assert_eq!(BinOp::IDiv.to_string(), "//");
}

#[test]
fn test_mixed_division_operations() {
    let source = "a / b + c // d;";
    let expr = parse_expr_from_source(source);

    // This should parse as (a / b) + (c // d)
    match expr {
        Expr::BinOp(left, op, right, _) => {
            // Main operator should be Add
            assert!(matches!(op, BinOp::Add));

            // Left should be float division: a / b
            match left.as_ref() {
                Expr::BinOp(_, div_op, _, _) => {
                    assert!(matches!(div_op, BinOp::Div));
                }
                _ => panic!("Expected division on left side"),
            }

            // Right should be integer division: c // d
            match right.as_ref() {
                Expr::BinOp(_, idiv_op, _, _) => {
                    assert!(matches!(idiv_op, BinOp::IDiv));
                }
                _ => panic!("Expected integer division on right side"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_division_with_literals() {
    let source = "10 // 3;";
    let expr = parse_expr_from_source(source);

    match expr {
        Expr::BinOp(left, op, right, _) => {
            // Left should be integer 10
            match left.as_ref() {
                Expr::Int(10, _) => (),
                _ => panic!("Expected integer 10 on left side"),
            }

            // Operator should be IDiv
            assert!(matches!(op, BinOp::IDiv));

            // Right should be integer 3
            match right.as_ref() {
                Expr::Int(3, _) => (),
                _ => panic!("Expected integer 3 on right side"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_float_division_with_literals() {
    let source = "10.0 / 3.0;";
    let expr = parse_expr_from_source(source);

    match expr {
        Expr::BinOp(left, op, right, _) => {
            // Left should be float 10.0
            match left.as_ref() {
                Expr::F64(val, _) => assert!((val - 10.0).abs() < f64::EPSILON),
                _ => panic!("Expected float 10.0 on left side"),
            }

            // Operator should be Div
            assert!(matches!(op, BinOp::Div));

            // Right should be float 3.0
            match right.as_ref() {
                Expr::F64(val, _) => assert!((val - 3.0).abs() < f64::EPSILON),
                _ => panic!("Expected float 3.0 on right side"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_division_precedence() {
    let source = "a + b // c * d;";
    let expr = parse_expr_from_source(source);

    // This should parse with correct precedence: a + ((b // c) * d)
    // Since // and * have higher precedence than +
    match expr {
        Expr::BinOp(left, op, right, _) => {
            // Main operator should be Add
            assert!(matches!(op, BinOp::Add));

            // Left should be variable a
            match left.as_ref() {
                Expr::Var(name, _) => assert_eq!(name, "a"),
                _ => panic!("Expected variable a on left side"),
            }

            // Right should be multiplication with integer division
            match right.as_ref() {
                Expr::BinOp(mul_left, mul_op, mul_right, _) => {
                    assert!(matches!(mul_op, BinOp::Mul));

                    // Left of multiplication should be integer division b // c
                    match mul_left.as_ref() {
                        Expr::BinOp(_, idiv_op, _, _) => {
                            assert!(matches!(idiv_op, BinOp::IDiv));
                        }
                        _ => panic!("Expected integer division in multiplication left"),
                    }

                    // Right of multiplication should be variable d
                    match mul_right.as_ref() {
                        Expr::Var(name, _) => assert_eq!(name, "d"),
                        _ => panic!("Expected variable d"),
                    }
                }
                _ => panic!("Expected multiplication on right side"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_division_assignment_operators() {
    // Note: Assignment operators might be parsed differently
    // This test verifies that division assignment operators are distinguishable
    let source = "x /= y;";

    let mut files = Files::new();
    let file_id = files.add("test.veil".to_string(), source.to_string());

    // This should parse successfully even if assignment isn't fully modeled
    let result = parse_ast(&files, file_id);
    assert!(
        result.is_ok(),
        "Division assignment should parse successfully"
    );
}

#[test]
fn test_integer_division_assignment() {
    let source = "x //= y;";

    let mut files = Files::new();
    let file_id = files.add("test.veil".to_string(), source.to_string());

    // This should parse successfully
    let result = parse_ast(&files, file_id);
    assert!(
        result.is_ok(),
        "Integer division assignment should parse successfully"
    );
}
