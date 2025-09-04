//! Tests for increment and decrement operators (++/--)
//!
//! This module tests both prefix and postfix variants of increment and decrement
//! operators to ensure they are parsed correctly into the AST.

use veil_ast::{Expr, UnOp};
use veil_diagnostics::Files;
use veil_syntax::parse_ast;

fn parse_expr_from_source(source: &str) -> Expr {
    let mut files = Files::new();
    let wrapped_source = format!("fn example() -> void {{ {} }}", source);
    let file_id = files.add("test.veil".to_string(), wrapped_source);

    let program = parse_ast(&files, file_id).expect("Parse should succeed");

    // Extract the first function and its first statement
    if let Some(func) = program.functions.first() {
        if let Some(stmt) = func.body.first() {
            if let veil_ast::Stmt::Expr(expr, _) = stmt {
                return expr.clone();
            }
        }
    }

    panic!("Expected expression statement in function body");
}

#[test]
fn test_prefix_increment() {
    let source = "++x;";
    let expr = parse_expr_from_source(source);

    match expr {
        Expr::UnaryOp(UnOp::PreInc, inner, _) => match inner.as_ref() {
            Expr::Var(name, _) => assert_eq!(name, "x"),
            _ => panic!("Expected variable expression"),
        },
        _ => panic!("Expected prefix increment expression"),
    }
}

#[test]
fn test_prefix_decrement() {
    let source = "--y;";
    let expr = parse_expr_from_source(source);

    match expr {
        Expr::UnaryOp(UnOp::PreDec, inner, _) => match inner.as_ref() {
            Expr::Var(name, _) => assert_eq!(name, "y"),
            _ => panic!("Expected variable expression"),
        },
        _ => panic!("Expected prefix decrement expression"),
    }
}

#[test]
fn test_postfix_increment() {
    let source = "x++;";
    let expr = parse_expr_from_source(source);

    match expr {
        Expr::UnaryOp(UnOp::PostInc, inner, _) => match inner.as_ref() {
            Expr::Var(name, _) => assert_eq!(name, "x"),
            _ => panic!("Expected variable expression"),
        },
        _ => panic!("Expected postfix increment expression"),
    }
}

#[test]
fn test_postfix_decrement() {
    let source = "y--;";
    let expr = parse_expr_from_source(source);

    match expr {
        Expr::UnaryOp(UnOp::PostDec, inner, _) => match inner.as_ref() {
            Expr::Var(name, _) => assert_eq!(name, "y"),
            _ => panic!("Expected variable expression"),
        },
        _ => panic!("Expected postfix decrement expression"),
    }
}

#[test]
fn test_increment_in_expression() {
    let source = "x + ++y;";
    let expr = parse_expr_from_source(source);

    match expr {
        Expr::BinOp(left, op, right, _) => {
            // Left should be variable x
            match left.as_ref() {
                Expr::Var(name, _) => assert_eq!(name, "x"),
                _ => panic!("Expected variable x on left side"),
            }

            // Operator should be Add
            assert!(matches!(op, veil_ast::BinOp::Add));

            // Right should be prefix increment of y
            match right.as_ref() {
                Expr::UnaryOp(UnOp::PreInc, inner, _) => match inner.as_ref() {
                    Expr::Var(name, _) => assert_eq!(name, "y"),
                    _ => panic!("Expected variable y in increment"),
                },
                _ => panic!("Expected prefix increment on right side"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_postfix_increment_in_assignment() {
    let source = "z = x++;";
    let expr = parse_expr_from_source(source);

    match expr {
        Expr::BinOp(left, op, right, _) => {
            // Left should be variable z
            match left.as_ref() {
                Expr::Var(name, _) => assert_eq!(name, "z"),
                _ => panic!("Expected variable z on left side"),
            }

            // Operator should be assignment (represented as Eq in this parser)
            assert!(matches!(op, veil_ast::BinOp::Eq));

            // Right should be postfix increment of x
            match right.as_ref() {
                Expr::UnaryOp(UnOp::PostInc, inner, _) => match inner.as_ref() {
                    Expr::Var(name, _) => assert_eq!(name, "x"),
                    _ => panic!("Expected variable x in increment"),
                },
                _ => panic!("Expected postfix increment on right side"),
            }
        }
        _ => panic!("Expected binary operation"),
    }
}

#[test]
fn test_double_increment_prefix() {
    let source = "+++x;";
    let expr = parse_expr_from_source(source);

    // This should parse as ++(+x) or similar nested structure
    match expr {
        Expr::UnaryOp(op1, inner1, _) => {
            assert!(matches!(op1, UnOp::PreInc));
            match inner1.as_ref() {
                Expr::UnaryOp(op2, inner2, _) => {
                    assert!(matches!(op2, UnOp::Plus));
                    match inner2.as_ref() {
                        Expr::Var(name, _) => assert_eq!(name, "x"),
                        _ => panic!("Expected variable x"),
                    }
                }
                _ => panic!("Expected unary operation"),
            }
        }
        _ => panic!("Expected unary operation"),
    }
}

#[test]
fn test_mixed_increment_decrement() {
    let source = "++x--;";
    let expr = parse_expr_from_source(source);

    // This should parse as ++(x--)
    match expr {
        Expr::UnaryOp(UnOp::PreInc, inner, _) => match inner.as_ref() {
            Expr::UnaryOp(UnOp::PostDec, inner2, _) => match inner2.as_ref() {
                Expr::Var(name, _) => assert_eq!(name, "x"),
                _ => panic!("Expected variable x"),
            },
            _ => panic!("Expected postfix decrement"),
        },
        _ => panic!("Expected prefix increment"),
    }
}

#[test]
fn test_increment_operator_display() {
    // Test the Display implementation for UnOp
    assert_eq!(UnOp::PreInc.to_string(), "++");
    assert_eq!(UnOp::PostInc.to_string(), "++");
    assert_eq!(UnOp::PreDec.to_string(), "--");
    assert_eq!(UnOp::PostDec.to_string(), "--");
}

#[test]
fn test_increment_field_access() {
    let source = "obj.field++;";
    let expr = parse_expr_from_source(source);

    match expr {
        Expr::UnaryOp(UnOp::PostInc, inner, _) => match inner.as_ref() {
            Expr::FieldAccess(obj, field, _) => {
                match obj.as_ref() {
                    Expr::Var(name, _) => assert_eq!(name, "obj"),
                    _ => panic!("Expected variable obj"),
                }
                assert_eq!(field, "field");
            }
            _ => panic!("Expected field access"),
        },
        _ => panic!("Expected postfix increment"),
    }
}

#[test]
fn test_increment_array_access() {
    let source = "arr[0]++;";
    let expr = parse_expr_from_source(source);

    match expr {
        Expr::UnaryOp(UnOp::PostInc, inner, _) => match inner.as_ref() {
            Expr::ArrayAccess(arr, index, _) => {
                match arr.as_ref() {
                    Expr::Var(name, _) => assert_eq!(name, "arr"),
                    _ => panic!("Expected variable arr"),
                }
                match index.as_ref() {
                    Expr::Int(0, _) => (),
                    _ => panic!("Expected integer 0"),
                }
            }
            _ => panic!("Expected array access"),
        },
        _ => panic!("Expected postfix increment"),
    }
}
