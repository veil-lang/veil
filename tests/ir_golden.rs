use std::env;
use std::fs;
use std::path::{Path, PathBuf};

use veil_hir::{
    HirBlock, HirExpr, HirExprKind, HirFunction, HirItem, HirItemKind, HirParam, HirProgram,
    HirStmt, HirStmtKind, HirType, HirVisibility, ModuleId, NodeId, SpanMap,
};
use veil_ir as ir;

fn manifest_path() -> PathBuf {
    PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into()))
}

fn golden_dir() -> PathBuf {
    manifest_path().join("tests").join("golden").join("ir")
}

fn ensure_dir(p: &Path) {
    if let Err(e) = fs::create_dir_all(p) {
        panic!("Failed to create directory {}: {}", p.display(), e);
    }
}

fn normalize_for_compare(s: &str) -> String {
    let s = s.replace("\r\n", "\n");
    s.lines()
        .map(|l| l.trim_end())
        .collect::<Vec<_>>()
        .join("\n")
        .trim_end()
        .to_string()
}

fn write_if_update_enabled(path: &Path, content: &str) {
    if env::var("UPDATE_GOLDEN").unwrap_or_default() == "1" {
        ensure_dir(path.parent().unwrap());
        fs::write(path, content).expect("Failed to update golden file");
        eprintln!("Updated golden: {}", path.display());
    }
}

fn assert_matches_golden(test_name: &str, actual: &str) {
    let path = golden_dir().join(format!("{}.golden.ir", test_name));
    let actual = normalize_for_compare(actual);

    if !path.exists() && env::var("UPDATE_GOLDEN").unwrap_or_default() != "1" {
        panic!(
            "Golden file missing: {}.\nRun with UPDATE_GOLDEN=1 to create it.",
            path.display()
        );
    }

    if !path.exists() {
        write_if_update_enabled(&path, &actual);
        // After write, ensure exists for subsequent compare or exit early
        assert!(path.exists(), "Failed to create golden file");
    }

    let expected =
        normalize_for_compare(&fs::read_to_string(&path).expect("Failed to read golden file"));
    if expected != actual {
        // Optionally auto-update if requested
        write_if_update_enabled(&path, &actual);

        // Produce a compact diff snippet
        let (exp_lines, act_lines) = (
            expected.lines().collect::<Vec<_>>(),
            actual.lines().collect::<Vec<_>>(),
        );
        let mut first_diff = None;
        let max = exp_lines.len().max(act_lines.len());
        for i in 0..max {
            let el = exp_lines.get(i).copied().unwrap_or("<EOF>");
            let al = act_lines.get(i).copied().unwrap_or("<EOF>");
            if el != al {
                first_diff = Some((i + 1, el, al));
                break;
            }
        }
        if let Some((line, exp, act)) = first_diff {
            panic!(
                "Golden mismatch for {} at line {}.\nExpected: {}\nActual  : {}\nGolden: {}\n",
                test_name,
                line,
                exp,
                act,
                path.display()
            );
        } else {
            // Different but no first diff? Fallback panic.
            panic!(
                "Golden mismatch for {} but could not compute first differing line.\nGolden: {}",
                test_name,
                path.display()
            );
        }
    }
}

// Simple ID generator for tests
fn nid(c: &mut u32) -> NodeId {
    let id = *c;
    *c += 1;
    NodeId::new(id)
}

// Build: fn main() -> i32 { return 42; }
fn build_hir_const_return() -> HirProgram {
    let mut c = 0u32;

    let ret_expr = HirExpr {
        id: nid(&mut c),
        kind: Box::new(HirExprKind::Int(42)),
    };

    let ret_stmt = HirStmt {
        id: nid(&mut c),
        kind: HirStmtKind::Return(Some(ret_expr)),
    };

    let body = HirBlock {
        id: nid(&mut c),
        stmts: vec![ret_stmt],
        expr: None,
    };

    let fun = HirFunction {
        id: nid(&mut c),
        name: "main".to_string(),
        symbol_id: None,
        generic_params: vec![],
        params: vec![],
        return_type: HirType::I32,
        body,
    };

    let item = HirItem {
        id: nid(&mut c),
        kind: HirItemKind::Function(fun),
        visibility: HirVisibility::Public,
    };

    HirProgram {
        module_id: ModuleId::new(0),
        items: vec![item],
        span_map: SpanMap::new(),
    }
}

// Build:
// fn add(x: i32, y: i32) -> i32 { return x + y; }
// fn main() -> i32 { return add(2, 3); }
fn build_hir_add_and_call() -> HirProgram {
    let mut c = 0u32;

    // add function
    let px = HirParam {
        id: nid(&mut c),
        name: "x".into(),
        symbol_id: None,
        ty: HirType::I32,
    };
    let py = HirParam {
        id: nid(&mut c),
        name: "y".into(),
        symbol_id: None,
        ty: HirType::I32,
    };
    let add_body = {
        let x_var = HirExpr {
            id: nid(&mut c),
            kind: Box::new(HirExprKind::Variable("x".into())),
        };
        let y_var = HirExpr {
            id: nid(&mut c),
            kind: Box::new(HirExprKind::Variable("y".into())),
        };
        let add_expr = HirExpr {
            id: nid(&mut c),
            kind: Box::new(HirExprKind::Binary {
                op: veil_hir::HirBinaryOp::Add,
                lhs: Box::new(x_var),
                rhs: Box::new(y_var),
            }),
        };
        let ret_stmt = HirStmt {
            id: nid(&mut c),
            kind: HirStmtKind::Return(Some(add_expr)),
        };
        HirBlock {
            id: nid(&mut c),
            stmts: vec![ret_stmt],
            expr: None,
        }
    };
    let add_fn = HirFunction {
        id: nid(&mut c),
        name: "add".into(),
        symbol_id: None,
        generic_params: vec![],
        params: vec![px, py],
        return_type: HirType::I32,
        body: add_body,
    };

    // main function: return add(2, 3)
    let main_body = {
        let callee = HirExpr {
            id: nid(&mut c),
            kind: Box::new(HirExprKind::Variable("add".into())),
        };
        let arg2 = HirExpr {
            id: nid(&mut c),
            kind: Box::new(HirExprKind::Int(2)),
        };
        let arg3 = HirExpr {
            id: nid(&mut c),
            kind: Box::new(HirExprKind::Int(3)),
        };
        let call = HirExpr {
            id: nid(&mut c),
            kind: Box::new(HirExprKind::Call {
                func: Box::new(callee),
                args: vec![arg2, arg3],
            }),
        };
        let ret_stmt = HirStmt {
            id: nid(&mut c),
            kind: HirStmtKind::Return(Some(call)),
        };
        HirBlock {
            id: nid(&mut c),
            stmts: vec![ret_stmt],
            expr: None,
        }
    };
    let main_fn = HirFunction {
        id: nid(&mut c),
        name: "main".into(),
        symbol_id: None,
        generic_params: vec![],
        params: vec![],
        return_type: HirType::I32,
        body: main_body,
    };

    let items = vec![
        HirItem {
            id: nid(&mut c),
            kind: HirItemKind::Function(add_fn),
            visibility: HirVisibility::Public,
        },
        HirItem {
            id: nid(&mut c),
            kind: HirItemKind::Function(main_fn),
            visibility: HirVisibility::Public,
        },
    ];

    HirProgram {
        module_id: ModuleId::new(0),
        items,
        span_map: SpanMap::new(),
    }
}

#[test]
fn ir_golden_const_return() {
    let hir = build_hir_const_return();
    let prog = ir::lower_from_hir(&hir);
    let pretty = prog.to_pretty_string();
    assert_matches_golden("const_return", &pretty);
}

// Build: fn main() -> void { return; }
fn build_hir_return_void() -> HirProgram {
    let mut c = 0u32;

    let ret_stmt = HirStmt {
        id: nid(&mut c),
        kind: HirStmtKind::Return(None),
    };

    let body = HirBlock {
        id: nid(&mut c),
        stmts: vec![ret_stmt],
        expr: None,
    };

    let fun = HirFunction {
        id: nid(&mut c),
        name: "main".to_string(),
        symbol_id: None,
        generic_params: vec![],
        params: vec![],
        return_type: HirType::Void,
        body,
    };

    let item = HirItem {
        id: nid(&mut c),
        kind: HirItemKind::Function(fun),
        visibility: HirVisibility::Public,
    };

    HirProgram {
        module_id: ModuleId::new(0),
        items: vec![item],
        span_map: SpanMap::new(),
    }
}

#[test]
fn ir_golden_return_void_no_expr() {
    let hir = build_hir_return_void();
    let prog = ir::lower_from_hir(&hir);
    let pretty = prog.to_pretty_string();
    assert_matches_golden("return_void_no_expr", &pretty);
}

#[test]
fn ir_golden_add_and_call() {
    let hir = build_hir_add_and_call();
    let prog = ir::lower_from_hir(&hir);
    let pretty = prog.to_pretty_string();
    assert_matches_golden("add_and_call", &pretty);
}

// Build:
// fn main() -> i32 {
//   if true { }
//   return 5;
// }
fn build_hir_if_const_then_return() -> HirProgram {
    let mut c = 0u32;

    let cond = HirExpr {
        id: nid(&mut c),
        kind: Box::new(HirExprKind::Bool(true)),
    };

    let then_block = HirBlock {
        id: nid(&mut c),
        stmts: vec![],
        expr: None,
    };

    let if_expr = HirExpr {
        id: nid(&mut c),
        kind: Box::new(HirExprKind::If {
            condition: Box::new(cond),
            then_branch: then_block,
            else_branch: None,
        }),
    };

    let if_stmt = HirStmt {
        id: nid(&mut c),
        kind: HirStmtKind::Expr(if_expr),
    };

    let five_expr = HirExpr {
        id: nid(&mut c),
        kind: Box::new(HirExprKind::Int(5)),
    };

    let ret_stmt = HirStmt {
        id: nid(&mut c),
        kind: HirStmtKind::Return(Some(five_expr)),
    };

    let body = HirBlock {
        id: nid(&mut c),
        stmts: vec![if_stmt, ret_stmt],
        expr: None,
    };

    let fun = HirFunction {
        id: nid(&mut c),
        name: "main".to_string(),
        symbol_id: None,
        generic_params: vec![],
        params: vec![],
        return_type: HirType::I32,
        body,
    };

    let item = HirItem {
        id: nid(&mut c),
        kind: HirItemKind::Function(fun),
        visibility: HirVisibility::Public,
    };

    HirProgram {
        module_id: ModuleId::new(0),
        items: vec![item],
        span_map: SpanMap::new(),
    }
}

#[test]
fn ir_golden_if_const_then_return() {
    let hir = build_hir_if_const_then_return();
    let prog = ir::lower_from_hir(&hir);
    let pretty = prog.to_pretty_string();
    assert_matches_golden("if_const_then_return", &pretty);
}

// --------------------------
// Logical AND / OR builders
// --------------------------

fn build_hir_logical_and() -> HirProgram {
    let mut c = 0u32;

    // true && false;
    let t = HirExpr {
        id: nid(&mut c),
        kind: Box::new(HirExprKind::Bool(true)),
    };
    let f = HirExpr {
        id: nid(&mut c),
        kind: Box::new(HirExprKind::Bool(false)),
    };
    let and_expr = HirExpr {
        id: nid(&mut c),
        kind: Box::new(HirExprKind::Binary {
            op: veil_hir::HirBinaryOp::And,
            lhs: Box::new(t),
            rhs: Box::new(f),
        }),
    };

    let eval_stmt = HirStmt {
        id: nid(&mut c),
        kind: HirStmtKind::Expr(and_expr),
    };

    let ret_stmt = HirStmt {
        id: nid(&mut c),
        kind: HirStmtKind::Return(Some(HirExpr {
            id: nid(&mut c),
            kind: Box::new(HirExprKind::Int(0)),
        })),
    };

    let body = HirBlock {
        id: nid(&mut c),
        stmts: vec![eval_stmt, ret_stmt],
        expr: None,
    };

    let fun = HirFunction {
        id: nid(&mut c),
        name: "main".to_string(),
        symbol_id: None,
        generic_params: vec![],
        params: vec![],
        return_type: HirType::I32,
        body,
    };

    let item = HirItem {
        id: nid(&mut c),
        kind: HirItemKind::Function(fun),
        visibility: HirVisibility::Public,
    };

    HirProgram {
        module_id: ModuleId::new(0),
        items: vec![item],
        span_map: SpanMap::new(),
    }
}

fn build_hir_logical_or() -> HirProgram {
    let mut c = 0u32;

    // false || true;
    let f = HirExpr {
        id: nid(&mut c),
        kind: Box::new(HirExprKind::Bool(false)),
    };
    let t = HirExpr {
        id: nid(&mut c),
        kind: Box::new(HirExprKind::Bool(true)),
    };
    let or_expr = HirExpr {
        id: nid(&mut c),
        kind: Box::new(HirExprKind::Binary {
            op: veil_hir::HirBinaryOp::Or,
            lhs: Box::new(f),
            rhs: Box::new(t),
        }),
    };

    let eval_stmt = HirStmt {
        id: nid(&mut c),
        kind: HirStmtKind::Expr(or_expr),
    };

    let ret_stmt = HirStmt {
        id: nid(&mut c),
        kind: HirStmtKind::Return(Some(HirExpr {
            id: nid(&mut c),
            kind: Box::new(HirExprKind::Int(0)),
        })),
    };

    let body = HirBlock {
        id: nid(&mut c),
        stmts: vec![eval_stmt, ret_stmt],
        expr: None,
    };

    let fun = HirFunction {
        id: nid(&mut c),
        name: "main".to_string(),
        symbol_id: None,
        generic_params: vec![],
        params: vec![],
        return_type: HirType::I32,
        body,
    };

    let item = HirItem {
        id: nid(&mut c),
        kind: HirItemKind::Function(fun),
        visibility: HirVisibility::Public,
    };

    HirProgram {
        module_id: ModuleId::new(0),
        items: vec![item],
        span_map: SpanMap::new(),
    }
}

#[test]
fn ir_golden_logical_and() {
    let hir = build_hir_logical_and();
    let prog = ir::lower_from_hir(&hir);
    let pretty = prog.to_pretty_string();
    assert_matches_golden("logical_and", &pretty);
}

#[test]
fn ir_golden_logical_or() {
    let hir = build_hir_logical_or();
    let prog = ir::lower_from_hir(&hir);
    let pretty = prog.to_pretty_string();
    assert_matches_golden("logical_or", &pretty);
}

// -------------
// Match (int)
// -------------

fn build_hir_match_int() -> HirProgram {
    let mut c = 0u32;

    // match 2 { 1 => 10, 2 => 20, _ => 0 }; return 0;
    let scrut = HirExpr {
        id: nid(&mut c),
        kind: Box::new(HirExprKind::Int(2)),
    };

    let pat1 = veil_hir::HirPattern {
        id: nid(&mut c),
        kind: Box::new(veil_hir::HirPatternKind::Literal(Box::new(HirExpr {
            id: nid(&mut c),
            kind: Box::new(HirExprKind::Int(1)),
        }))),
    };
    let arm1 = veil_hir::HirMatchArm {
        id: nid(&mut c),
        pattern: pat1,
        guard: None,
        body: veil_hir::HirMatchArmBody::Expr(HirExpr {
            id: nid(&mut c),
            kind: Box::new(HirExprKind::Int(10)),
        }),
    };

    let pat2 = veil_hir::HirPattern {
        id: nid(&mut c),
        kind: Box::new(veil_hir::HirPatternKind::Literal(Box::new(HirExpr {
            id: nid(&mut c),
            kind: Box::new(HirExprKind::Int(2)),
        }))),
    };
    let arm2 = veil_hir::HirMatchArm {
        id: nid(&mut c),
        pattern: pat2,
        guard: None,
        body: veil_hir::HirMatchArmBody::Expr(HirExpr {
            id: nid(&mut c),
            kind: Box::new(HirExprKind::Int(20)),
        }),
    };

    // Default arm: use wildcard pattern
    let pat_default = veil_hir::HirPattern {
        id: nid(&mut c),
        kind: Box::new(veil_hir::HirPatternKind::Wildcard),
    };
    let arm_default = veil_hir::HirMatchArm {
        id: nid(&mut c),
        pattern: pat_default,
        guard: None,
        body: veil_hir::HirMatchArmBody::Expr(HirExpr {
            id: nid(&mut c),
            kind: Box::new(HirExprKind::Int(0)),
        }),
    };

    let match_expr = HirExpr {
        id: nid(&mut c),
        kind: Box::new(HirExprKind::Match {
            expr: Box::new(scrut),
            arms: vec![arm1, arm2, arm_default],
        }),
    };

    let match_stmt = HirStmt {
        id: nid(&mut c),
        kind: HirStmtKind::Expr(match_expr),
    };

    let ret_stmt = HirStmt {
        id: nid(&mut c),
        kind: HirStmtKind::Return(Some(HirExpr {
            id: nid(&mut c),
            kind: Box::new(HirExprKind::Int(0)),
        })),
    };

    let body = HirBlock {
        id: nid(&mut c),
        stmts: vec![match_stmt, ret_stmt],
        expr: None,
    };

    let fun = HirFunction {
        id: nid(&mut c),
        name: "main".to_string(),
        symbol_id: None,
        generic_params: vec![],
        params: vec![],
        return_type: HirType::I32,
        body,
    };

    let item = HirItem {
        id: nid(&mut c),
        kind: HirItemKind::Function(fun),
        visibility: HirVisibility::Public,
    };

    HirProgram {
        module_id: ModuleId::new(0),
        items: vec![item],
        span_map: SpanMap::new(),
    }
}

#[test]
fn ir_golden_match_int() {
    let hir = build_hir_match_int();
    let prog = ir::lower_from_hir(&hir);
    let pretty = prog.to_pretty_string();
    assert_matches_golden("match_int", &pretty);
}

// ---------
// For loop
// ---------

fn build_hir_for_loop() -> HirProgram {
    let mut c = 0u32;

    // for x in it { } ; return 0;
    let pattern = veil_hir::HirPattern {
        id: nid(&mut c),
        kind: Box::new(veil_hir::HirPatternKind::Variable("x".to_string())),
    };
    let iter = HirExpr {
        id: nid(&mut c),
        kind: Box::new(HirExprKind::Variable("it".to_string())),
    };
    let empty_body = HirBlock {
        id: nid(&mut c),
        stmts: vec![],
        expr: None,
    };
    let for_expr = HirExpr {
        id: nid(&mut c),
        kind: Box::new(HirExprKind::For {
            pattern,
            iter: Box::new(iter),
            body: empty_body,
        }),
    };

    let for_stmt = HirStmt {
        id: nid(&mut c),
        kind: HirStmtKind::Expr(for_expr),
    };
    let ret_stmt = HirStmt {
        id: nid(&mut c),
        kind: HirStmtKind::Return(Some(HirExpr {
            id: nid(&mut c),
            kind: Box::new(HirExprKind::Int(0)),
        })),
    };

    let body = HirBlock {
        id: nid(&mut c),
        stmts: vec![for_stmt, ret_stmt],
        expr: None,
    };

    let fun = HirFunction {
        id: nid(&mut c),
        name: "main".to_string(),
        symbol_id: None,
        generic_params: vec![],
        params: vec![],
        return_type: HirType::I32,
        body,
    };

    let item = HirItem {
        id: nid(&mut c),
        kind: HirItemKind::Function(fun),
        visibility: HirVisibility::Public,
    };

    HirProgram {
        module_id: ModuleId::new(0),
        items: vec![item],
        span_map: SpanMap::new(),
    }
}

#[test]
fn ir_golden_for_loop() {
    let hir = build_hir_for_loop();
    let prog = ir::lower_from_hir(&hir);
    let pretty = prog.to_pretty_string();
    assert_matches_golden("for_loop", &pretty);
}
