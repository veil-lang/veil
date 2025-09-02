use veil_diagnostics::Files;
use veil_hir::*;
use veil_resolve::SymbolTable;
use veil_typeck::TypeChecker;

#[test]
fn typed_snapshot_golden() {
    // Setup diagnostics file id
    let mut files = Files::<String>::new();
    let fid = files.add("typed_snapshot.veil".to_string(), String::new());

    // Build a simple HIR program:
    // fn main() {
    //     let a: i32 = 1;
    //     [1, 2]
    // }
    let mut program = HirProgram {
        module_id: ModuleId::new(0),
        items: vec![HirItem {
            id: NodeId::new(1),
            visibility: HirVisibility::Private,
            kind: HirItemKind::Function(HirFunction {
                id: NodeId::new(1),
                name: "main".to_string(),
                symbol_id: None,
                generic_params: vec![],
                params: vec![],
                return_type: HirType::Void,
                body: HirBlock {
                    id: NodeId::new(2),
                    stmts: vec![HirStmt {
                        id: NodeId::new(100),
                        kind: HirStmtKind::Let {
                            pattern: HirPattern {
                                id: NodeId::new(3),
                                kind: Box::new(HirPatternKind::Variable("a".to_string())),
                            },
                            ty: Some(HirType::I32),
                            init: Some(HirExpr {
                                id: NodeId::new(4),
                                kind: Box::new(HirExprKind::Int(1)),
                            }),
                        },
                    }],
                    expr: Some(Box::new(HirExpr {
                        id: NodeId::new(7),
                        kind: Box::new(HirExprKind::ArrayLiteral(vec![
                            HirExpr {
                                id: NodeId::new(5),
                                kind: Box::new(HirExprKind::Int(1)),
                            },
                            HirExpr {
                                id: NodeId::new(6),
                                kind: Box::new(HirExprKind::Int(2)),
                            },
                        ])),
                    })),
                },
            }),
        }],
        span_map: SpanMap::new(),
    };

    let symbol_table = SymbolTable::new();
    let mut checker = TypeChecker::new(symbol_table, fid, Some(program.module_id));

    checker
        .check_program(&mut program)
        .expect("typeck should succeed");

    let snapshot = checker.format_typed_snapshot();
    let expected = "\
TypedSnapshot
Types:
  ty_0 = I32
  ty_1 = Array(I32)
NodeTypes:
  node_3 -> ty_0
  node_4 -> ty_0
  node_5 -> ty_0
  node_6 -> ty_0
  node_7 -> ty_1
";
    assert_eq!(snapshot, expected, "typed snapshot should match golden");
}

#[test]
fn division_fixit_integer_operands_ve0010() {
    // Setup diagnostics file id
    let mut files = Files::<String>::new();
    let fid = files.add("divfix_int.veil".to_string(), String::new());

    let symbol_table = SymbolTable::new();
    let mut checker = TypeChecker::new(symbol_table, fid, Some(ModuleId::new(0)));

    // Build expression: 1 / 2
    let mut expr = HirExpr {
        id: NodeId::new(10),
        kind: Box::new(HirExprKind::Binary {
            op: HirBinaryOp::Div,
            lhs: Box::new(HirExpr {
                id: NodeId::new(11),
                kind: Box::new(HirExprKind::Int(1)),
            }),
            rhs: Box::new(HirExpr {
                id: NodeId::new(12),
                kind: Box::new(HirExprKind::Int(2)),
            }),
        }),
    };

    // Provide span for root division node so diagnostic includes code + fix-it note
    let mut span_map = SpanMap::new();
    span_map.insert(NodeId::new(10), codespan::Span::new(0, 1));
    checker.context.span_map = Some(span_map);

    let _ = checker
        .check_expr(&mut expr)
        .expect("typeck expr should succeed");

    // Assert VE0010 and a fix-it note recommending '//'
    assert!(
        checker
            .errors
            .iter()
            .any(|d| d.code.as_deref() == Some("VE0010")),
        "expected VE0010 for integer '/'"
    );
    assert!(
        checker
            .errors
            .iter()
            .any(|d| d.notes.iter().any(|n| n.contains("//"))),
        "expected help note suggesting '//' for integer division"
    );
}

#[test]
fn division_fixit_mixed_int_float_ve0011() {
    // Setup diagnostics file id
    let mut files = Files::<String>::new();
    let fid = files.add("divfix_mixed.veil".to_string(), String::new());

    let symbol_table = SymbolTable::new();
    let mut checker = TypeChecker::new(symbol_table, fid, Some(ModuleId::new(0)));

    // Build expression: 1 / 2.0
    let mut expr = HirExpr {
        id: NodeId::new(20),
        kind: Box::new(HirExprKind::Binary {
            op: HirBinaryOp::Div,
            lhs: Box::new(HirExpr {
                id: NodeId::new(21),
                kind: Box::new(HirExprKind::Int(1)),
            }),
            rhs: Box::new(HirExpr {
                id: NodeId::new(22),
                kind: Box::new(HirExprKind::Float(2.0)),
            }),
        }),
    };

    // Provide span for root division node so diagnostic includes code + fix-it note
    let mut span_map = SpanMap::new();
    span_map.insert(NodeId::new(20), codespan::Span::new(0, 1));
    checker.context.span_map = Some(span_map);

    let _ = checker
        .check_expr(&mut expr)
        .expect("typeck expr should succeed");

    // Assert VE0011 and a fix-it note recommending casting to float
    assert!(
        checker
            .errors
            .iter()
            .any(|d| d.code.as_deref() == Some("VE0011")),
        "expected VE0011 for mixed int/float '/'"
    );
    assert!(
        checker.errors.iter().any(|d| d
            .notes
            .iter()
            .any(|n| n.contains("as f32") || n.contains("as f64"))),
        "expected help note suggesting cast to f32/f64"
    );
}
