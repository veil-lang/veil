#![allow(clippy::single_match, clippy::match_single_binding)]
//! Parser tests for function calls and `new` expression arguments to prevent regressions.

use veil_ast::{BinOp, Expr};
use veil_diagnostics::Files;
use veil_syntax::parse_ast;

fn parse_expr_from_source(source: &str) -> Expr {
    let mut files = Files::new();
    // Wrap the snippet into a minimal function so we can parse an expression statement
    let wrapped_source = format!("fn example() -> void {{ {} }}", source);
    let file_id = files.add("test.veil".to_string(), wrapped_source);

    let program = parse_ast(&files, file_id).expect("Parse should succeed");

    // Extract the first function and its first statement as an expression
    if let Some(func) = program.functions.first()
        && let Some(stmt) = func.body.first()
            && let veil_ast::Stmt::Expr(expr, _) = stmt {
                return expr.clone();
            }

    panic!("Expected expression statement in function body");
}

#[test]
fn test_simple_call_no_args() {
    let expr = parse_expr_from_source("f();");

    match expr {
        Expr::Call(name, args, _) => {
            assert_eq!(name, "f");
            assert!(args.is_empty(), "Expected no arguments");
        }
        _ => panic!("Expected function call"),
    }
}

#[test]
fn test_call_one_arg() {
    let expr = parse_expr_from_source("f(x);");

    match expr {
        Expr::Call(name, args, _) => {
            assert_eq!(name, "f");
            assert_eq!(args.len(), 1);
            match &args[0] {
                Expr::Var(n, _) => assert_eq!(n, "x"),
                _ => panic!("Expected variable x"),
            }
        }
        _ => panic!("Expected function call"),
    }
}

#[test]
fn test_call_multiple_args_mixed() {
    let expr = parse_expr_from_source("f(a, b + c, 42);");

    match expr {
        Expr::Call(name, args, _) => {
            assert_eq!(name, "f");
            assert_eq!(args.len(), 3);

            // a
            match &args[0] {
                Expr::Var(n, _) => assert_eq!(n, "a"),
                _ => panic!("Expected variable a"),
            }

            // b + c
            match &args[1] {
                Expr::BinOp(l, op, r, _) => {
                    assert!(matches!(op, BinOp::Add));
                    match l.as_ref() {
                        Expr::Var(n, _) => assert_eq!(n, "b"),
                        _ => panic!("Expected variable b"),
                    }
                    match r.as_ref() {
                        Expr::Var(n, _) => assert_eq!(n, "c"),
                        _ => panic!("Expected variable c"),
                    }
                }
                _ => panic!("Expected binary addition b + c"),
            }

            // 42
            match &args[2] {
                Expr::Int(42, _) => {}
                _ => panic!("Expected integer literal 42"),
            }
        }
        _ => panic!("Expected function call"),
    }
}

#[test]
fn test_nested_calls_as_arguments() {
    let expr = parse_expr_from_source("f(g(x), h(1 + 2));");

    match expr {
        Expr::Call(name, args, _) => {
            assert_eq!(name, "f");
            assert_eq!(args.len(), 2);

            // g(x)
            match &args[0] {
                Expr::Call(n, a, _) => {
                    assert_eq!(n, "g");
                    assert_eq!(a.len(), 1);
                    match &a[0] {
                        Expr::Var(x, _) => assert_eq!(x, "x"),
                        _ => panic!("Expected variable x"),
                    }
                }
                _ => panic!("Expected nested call g(x)"),
            }

            // h(1 + 2)
            match &args[1] {
                Expr::Call(n, a, _) => {
                    assert_eq!(n, "h");
                    assert_eq!(a.len(), 1);
                    match &a[0] {
                        Expr::BinOp(l, op, r, _) => {
                            assert!(matches!(op, BinOp::Add));
                            match l.as_ref() {
                                Expr::Int(1, _) => {}
                                _ => panic!("Expected integer 1"),
                            }
                            match r.as_ref() {
                                Expr::Int(2, _) => {}
                                _ => panic!("Expected integer 2"),
                            }
                        }
                        _ => panic!("Expected addition 1 + 2"),
                    }
                }
                _ => panic!("Expected nested call h(1 + 2)"),
            }
        }
        _ => panic!("Expected function call"),
    }
}

#[test]
fn test_field_access_call_is_lowered_to_synthetic_method_call() {
    // obj.method(1, 2) becomes Call("<method>.call", [FieldAccess(obj, "method"), 1, 2])
    let expr = parse_expr_from_source("obj.method(1, 2);");

    match expr {
        Expr::Call(name, args, _) => {
            assert_eq!(name, "<method>.call");
            assert_eq!(args.len(), 3);

            match &args[0] {
                Expr::FieldAccess(obj, field, _) => {
                    match obj.as_ref() {
                        Expr::Var(n, _) => assert_eq!(n, "obj"),
                        _ => panic!("Expected variable obj"),
                    }
                    assert_eq!(field, "method");
                }
                _ => panic!("Expected field access as first synthetic receiver arg"),
            }

            match &args[1] {
                Expr::Int(1, _) => {}
                _ => panic!("Expected integer 1"),
            }
            match &args[2] {
                Expr::Int(2, _) => {}
                _ => panic!("Expected integer 2"),
            }
        }
        _ => panic!("Expected Call(\"<method>.call\", ...)"),
    }
}

#[test]
fn test_array_access_call_is_lowered_to_synthetic_method_call() {
    // arr[0](x) becomes Call("<method>.call", [ArrayAccess(arr, 0), x])
    let expr = parse_expr_from_source("arr[0](x);");

    match expr {
        Expr::Call(name, args, _) => {
            assert_eq!(name, "<method>.call");
            assert_eq!(args.len(), 2);

            match &args[0] {
                Expr::ArrayAccess(arr, index, _) => {
                    match arr.as_ref() {
                        Expr::Var(n, _) => assert_eq!(n, "arr"),
                        _ => panic!("Expected variable arr"),
                    }
                    match index.as_ref() {
                        Expr::Int(0, _) => {}
                        _ => panic!("Expected integer index 0"),
                    }
                }
                _ => panic!("Expected array access as first synthetic receiver arg"),
            }

            match &args[1] {
                Expr::Var(x, _) => assert_eq!(x, "x"),
                _ => panic!("Expected variable x"),
            }
        }
        _ => panic!("Expected Call(\"<method>.call\", ...)"),
    }
}

#[test]
fn test_new_expr_no_args() {
    let expr = parse_expr_from_source("new Foo();");

    match expr {
        Expr::New(name, args, _) => {
            assert_eq!(name, "Foo");
            assert!(args.is_empty(), "Expected no arguments");
        }
        _ => panic!("Expected new-expression"),
    }
}

#[test]
fn test_new_expr_with_args_and_nested_call() {
    let expr = parse_expr_from_source("new Foo(a, 1 + 2, bar());");

    match expr {
        Expr::New(name, args, _) => {
            assert_eq!(name, "Foo");
            assert_eq!(args.len(), 3);

            // a
            match &args[0] {
                Expr::Var(n, _) => assert_eq!(n, "a"),
                _ => panic!("Expected variable a"),
            }

            // 1 + 2
            match &args[1] {
                Expr::BinOp(l, op, r, _) => {
                    assert!(matches!(op, BinOp::Add));
                    match l.as_ref() {
                        Expr::Int(1, _) => {}
                        _ => panic!("Expected integer 1"),
                    }
                    match r.as_ref() {
                        Expr::Int(2, _) => {}
                        _ => panic!("Expected integer 2"),
                    }
                }
                _ => panic!("Expected addition 1 + 2"),
            }

            // bar()
            match &args[2] {
                Expr::Call(n, a, _) => {
                    assert_eq!(n, "bar");
                    assert!(a.is_empty());
                }
                _ => panic!("Expected call bar()"),
            }
        }
        _ => panic!("Expected new-expression"),
    }
}

#[test]
fn test_new_expr_with_nested_new_and_call_args() {
    let expr = parse_expr_from_source("new Foo(g(1,2), new Bar(3));");

    match expr {
        Expr::New(name, args, _) => {
            assert_eq!(name, "Foo");
            assert_eq!(args.len(), 2);

            // g(1, 2)
            match &args[0] {
                Expr::Call(n, a, _) => {
                    assert_eq!(n, "g");
                    assert_eq!(a.len(), 2);
                    match &a[0] {
                        Expr::Int(1, _) => {}
                        _ => panic!("Expected integer 1"),
                    }
                    match &a[1] {
                        Expr::Int(2, _) => {}
                        _ => panic!("Expected integer 2"),
                    }
                }
                _ => panic!("Expected call g(1,2)"),
            }

            // new Bar(3)
            match &args[1] {
                Expr::New(n, a, _) => {
                    assert_eq!(n, "Bar");
                    assert_eq!(a.len(), 1);
                    match &a[0] {
                        Expr::Int(3, _) => {}
                        _ => panic!("Expected integer 3"),
                    }
                }
                _ => panic!("Expected new Bar(3)"),
            }
        }
        _ => panic!("Expected new-expression"),
    }
}
