//! Integration tests for HIR lowering
//!
//! These tests verify that AST nodes are correctly lowered to HIR nodes
//! with proper span preservation and structure.

use codespan::Span;
use veil_ast as ast;
use veil_hir::ids::ModuleId;
use veil_hir::{HirExprKind, HirItemKind, HirStmtKind, lower_program};

#[test]
fn test_empty_program() {
    let ast_program = ast::Program {
        imports: vec![],
        stmts: vec![],
        functions: vec![],
        structs: vec![],
        enums: vec![],
        impls: vec![],
        ffi_functions: vec![],
        ffi_variables: vec![],
        tests: vec![],
    };

    let module_id = ModuleId::new(0);
    let result = lower_program(&ast_program, module_id);
    assert!(result.is_ok());

    let hir_program = result.unwrap();
    assert_eq!(hir_program.module_id, module_id);
    assert!(hir_program.items.is_empty());
}

#[test]
fn test_simple_function() {
    let function = ast::Function {
        name: "test_fn".to_string(),
        generic_params: vec![],
        params: vec![("x".to_string(), ast::Type::I32)],
        return_type: ast::Type::I32,
        body: vec![ast::Stmt::Return(
            ast::Expr::Var(
                "x".to_string(),
                ast::ExprInfo {
                    span: Span::new(0, 1),
                    ty: ast::Type::I32,
                    is_tail: false,
                },
            ),
            Span::new(0, 10),
        )],
        span: Span::new(0, 20),
        visibility: ast::Visibility::Private,
    };

    let ast_program = ast::Program {
        imports: vec![],
        stmts: vec![],
        functions: vec![function],
        structs: vec![],
        enums: vec![],
        impls: vec![],
        ffi_functions: vec![],
        ffi_variables: vec![],
        tests: vec![],
    };

    let module_id = ModuleId::new(0);
    let result = lower_program(&ast_program, module_id);
    assert!(result.is_ok());

    let hir_program = result.unwrap();
    assert_eq!(hir_program.items.len(), 1);

    if let HirItemKind::Function(hir_function) = &hir_program.items[0].kind {
        assert_eq!(hir_function.name, "test_fn");
        assert_eq!(hir_function.params.len(), 1);
        assert_eq!(hir_function.params[0].name, "x");
        assert_eq!(hir_function.return_type, veil_hir::HirType::I32);
    } else {
        panic!("Expected function item");
    }
}

#[test]
fn test_struct_lowering() {
    let struct_def = ast::StructDef {
        name: "Point".to_string(),
        generic_params: vec![],
        fields: vec![
            ast::StructField {
                name: "x".to_string(),
                ty: ast::Type::I32,
                span: Span::new(0, 5),
            },
            ast::StructField {
                name: "y".to_string(),
                ty: ast::Type::I32,
                span: Span::new(5, 10),
            },
        ],
        span: Span::new(0, 20),
        visibility: ast::Visibility::Public,
        repr: None,
    };

    let ast_program = ast::Program {
        imports: vec![],
        stmts: vec![],
        functions: vec![],
        structs: vec![struct_def],
        enums: vec![],
        impls: vec![],
        ffi_functions: vec![],
        ffi_variables: vec![],
        tests: vec![],
    };

    let module_id = ModuleId::new(0);
    let result = lower_program(&ast_program, module_id);
    assert!(result.is_ok());

    let hir_program = result.unwrap();
    assert_eq!(hir_program.items.len(), 1);

    if let HirItemKind::Struct(hir_struct) = &hir_program.items[0].kind {
        assert_eq!(hir_struct.name, "Point");
        assert_eq!(hir_struct.fields.len(), 2);
        assert_eq!(hir_struct.fields[0].name, "x");
        assert_eq!(hir_struct.fields[1].name, "y");
        assert_eq!(hir_struct.fields[0].ty, veil_hir::HirType::I32);
        assert_eq!(hir_struct.fields[1].ty, veil_hir::HirType::I32);
    } else {
        panic!("Expected struct item");
    }
}

#[test]
fn test_expression_lowering() {
    // Test binary operation lowering
    let expr = ast::Expr::BinOp(
        Box::new(ast::Expr::Int(
            1,
            ast::ExprInfo {
                span: Span::new(0, 1),
                ty: ast::Type::I32,
                is_tail: false,
            },
        )),
        ast::BinOp::Add,
        Box::new(ast::Expr::Int(
            2,
            ast::ExprInfo {
                span: Span::new(2, 3),
                ty: ast::Type::I32,
                is_tail: false,
            },
        )),
        ast::ExprInfo {
            span: Span::new(0, 3),
            ty: ast::Type::I32,
            is_tail: false,
        },
    );

    let function = ast::Function {
        name: "test_expr".to_string(),
        generic_params: vec![],
        params: vec![],
        return_type: ast::Type::I32,
        body: vec![ast::Stmt::Return(expr, Span::new(0, 10))],
        span: Span::new(0, 20),
        visibility: ast::Visibility::Private,
    };

    let ast_program = ast::Program {
        imports: vec![],
        stmts: vec![],
        functions: vec![function],
        structs: vec![],
        enums: vec![],
        impls: vec![],
        ffi_functions: vec![],
        ffi_variables: vec![],
        tests: vec![],
    };

    let module_id = ModuleId::new(0);
    let result = lower_program(&ast_program, module_id);
    assert!(result.is_ok());

    let hir_program = result.unwrap();
    assert_eq!(hir_program.items.len(), 1);

    if let HirItemKind::Function(hir_function) = &hir_program.items[0].kind {
        assert_eq!(hir_function.body.stmts.len(), 1);

        if let HirStmtKind::Return(Some(return_expr)) = &hir_function.body.stmts[0].kind {
            if let HirExprKind::Binary { op, lhs, rhs } = return_expr.kind.as_ref() {
                assert_eq!(*op, veil_hir::HirBinaryOp::Add);

                if let HirExprKind::Int(1) = lhs.kind.as_ref() {
                    // Left operand is correct
                } else {
                    panic!("Expected left operand to be Int(1)");
                }

                if let HirExprKind::Int(2) = rhs.kind.as_ref() {
                    // Right operand is correct
                } else {
                    panic!("Expected right operand to be Int(2)");
                }
            } else {
                panic!("Expected binary operation");
            }
        } else {
            panic!("Expected return statement");
        }
    } else {
        panic!("Expected function item");
    }
}

#[test]
fn test_pattern_lowering() {
    let _pattern = ast::Pattern::Variable("x".to_string(), Span::new(0, 1));

    let var_stmt = ast::Stmt::Var(
        "x".to_string(),
        Some(ast::Type::I32),
        ast::Expr::Int(
            42,
            ast::ExprInfo {
                span: Span::new(5, 7),
                ty: ast::Type::I32,
                is_tail: false,
            },
        ),
        false, // not mutable
        Span::new(0, 10),
    );

    let function = ast::Function {
        name: "test_pattern".to_string(),
        generic_params: vec![],
        params: vec![],
        return_type: ast::Type::Void,
        body: vec![var_stmt],
        span: Span::new(0, 20),
        visibility: ast::Visibility::Private,
    };

    let ast_program = ast::Program {
        imports: vec![],
        stmts: vec![],
        functions: vec![function],
        structs: vec![],
        enums: vec![],
        impls: vec![],
        ffi_functions: vec![],
        ffi_variables: vec![],
        tests: vec![],
    };

    let module_id = ModuleId::new(0);
    let result = lower_program(&ast_program, module_id);
    assert!(result.is_ok());

    let hir_program = result.unwrap();

    if let HirItemKind::Function(hir_function) = &hir_program.items[0].kind {
        assert_eq!(hir_function.body.stmts.len(), 1);

        if let HirStmtKind::Var {
            name,
            ty,
            init,
            is_mutable,
        } = &hir_function.body.stmts[0].kind
        {
            assert_eq!(name, "x");
            assert!(!is_mutable);

            assert!(ty.is_some());
            assert_eq!(ty.as_ref().unwrap(), &veil_hir::HirType::I32);

            if let HirExprKind::Int(42) = init.kind.as_ref() {
                // Correct initialization value
            } else {
                panic!("Expected initialization to Int(42)");
            }
        } else {
            panic!("Expected let statement");
        }
    } else {
        panic!("Expected function item");
    }
}

#[test]
fn test_span_preservation() {
    let function = ast::Function {
        name: "test_spans".to_string(),
        generic_params: vec![],
        params: vec![],
        return_type: ast::Type::Void,
        body: vec![],
        span: Span::new(10, 50),
        visibility: ast::Visibility::Private,
    };

    let ast_program = ast::Program {
        imports: vec![],
        stmts: vec![],
        functions: vec![function],
        structs: vec![],
        enums: vec![],
        impls: vec![],
        ffi_functions: vec![],
        ffi_variables: vec![],
        tests: vec![],
    };

    let module_id = ModuleId::new(0);
    let result = lower_program(&ast_program, module_id);
    assert!(result.is_ok());

    let hir_program = result.unwrap();

    // Check that spans are preserved in the span map
    if let HirItemKind::Function(hir_function) = &hir_program.items[0].kind {
        let span = hir_program.span_map.get(hir_function.id);
        assert!(span.is_some());
        assert_eq!(span.unwrap(), Span::new(10, 50));
    } else {
        panic!("Expected function item");
    }
}
