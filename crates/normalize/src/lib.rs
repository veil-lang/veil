#![forbid(unsafe_code)]
#![deny(rust_2018_idioms)]
#![deny(unused_must_use)]

//! Veil Normalize Pass
//!
//! This crate implements desugaring of higher-level HIR constructs into a more
//! regular subset (Normalized HIR), while preserving source spans for accurate
//! diagnostics in later passes.
//!
//! Initial coverage (M6 scope):
//! - Pipeline operator: `a |> b` => `b(a)`; chains `a |> b |> c` => `c(b(a))`
//! - Postfix increment/decrement: `x++` / `x--` normalized into blocks using
//!   `let __tmp = x; x = x (+|-) 1; __tmp` to preserve expression value semantics
//!
//! Span Preservation:
//! - New nodes produced by normalization inherit the span of the original node,
//!   unless a more precise mapping is practical. The SpanMap in the program is
//!   updated for all synthesized nodes.

use std::collections::HashMap;

use veil_hir::{
    HirBinaryOp, HirBlock, HirExpr, HirExprKind, HirItem, HirItemKind, HirMatchArm,
    HirMatchArmBody, HirProgram, HirStmt, HirStmtKind, HirType, NodeId, SpanMap,
};

/// Normalize the HIR program in-place:
/// - Desugar pipeline `|>` to nested function calls
/// - Desugar postfix increment/decrement to block-based sequences preserving value semantics
pub fn normalize_program(program: &mut HirProgram) {
    let mut norm = Normalizer::new(&program.span_map);
    for item in &mut program.items {
        norm.normalize_item(item);
    }
    // Replace span map with merged map including new synthesized nodes
    program.span_map = norm.into_span_map();
}

struct Normalizer {
    /// Next fresh NodeId to assign for synthesized nodes
    next_id: u32,
    /// Span map clone to append new entries (we do not drop existing mappings)
    span_map: SpanMap,
    /// Cache of spans by old node id for quick lookup
    span_lookup: HashMap<NodeId, veil_diagnostics::Span>,
}

impl Normalizer {
    fn new(existing: &SpanMap) -> Self {
        let mut span_lookup = HashMap::new();
        for (k, v) in existing.spans.iter() {
            span_lookup.insert(*k, *v);
        }
        let max_id = span_lookup.keys().map(|n| n.as_u32()).max().unwrap_or(0);
        Self {
            next_id: max_id.saturating_add(1),
            span_map: existing.clone(),
            span_lookup,
        }
    }

    fn into_span_map(self) -> SpanMap {
        self.span_map
    }

    fn fresh_id(&mut self) -> NodeId {
        let id = NodeId::new(self.next_id);
        self.next_id = self.next_id.saturating_add(1);
        id
    }

    fn span_of(&self, id: NodeId) -> Option<veil_diagnostics::Span> {
        self.span_lookup.get(&id).copied()
    }

    fn set_span(&mut self, id: NodeId, span: veil_diagnostics::Span) {
        self.span_map.insert(id, span);
        self.span_lookup.insert(id, span);
    }

    fn _inherit_span(&mut self, from: NodeId, to: NodeId) {
        if let Some(sp) = self.span_of(from) {
            self.set_span(to, sp);
        }
    }

    // --------------------------
    // Normalization entry points
    // --------------------------

    fn normalize_item(&mut self, item: &mut HirItem) {
        match &mut item.kind {
            HirItemKind::Function(f) => {
                self.normalize_block(&mut f.body);
            }
            HirItemKind::Impl(im) => {
                for m in &mut im.methods {
                    self.normalize_block(&mut m.body);
                }
            }
            HirItemKind::Struct(_)
            | HirItemKind::Enum(_)
            | HirItemKind::FfiFunction(_)
            | HirItemKind::FfiVariable(_)
            | HirItemKind::Test(_)
            | HirItemKind::Import(_) => {
                // No normalization needed at item level for these in M6 scope
            }
        }
    }

    fn normalize_block(&mut self, block: &mut HirBlock) {
        for stmt in &mut block.stmts {
            self.normalize_stmt(stmt);
        }
        if let Some(expr) = &mut block.expr {
            let mut tmp = HirExpr {
                id: expr.id,
                kind: expr.kind.clone(),
            };
            self.normalize_expr(&mut tmp);
            *expr = Box::new(tmp);
        }
    }

    fn normalize_stmt(&mut self, stmt: &mut HirStmt) {
        match &mut stmt.kind {
            HirStmtKind::Expr(expr) => {
                let mut tmp = HirExpr {
                    id: expr.id,
                    kind: expr.kind.clone(),
                };
                self.normalize_expr(&mut tmp);
                *expr = tmp;
            }
            HirStmtKind::Let { init, .. } => {
                if let Some(e) = init {
                    let mut tmp = HirExpr {
                        id: e.id,
                        kind: e.kind.clone(),
                    };
                    self.normalize_expr(&mut tmp);
                    *e = tmp;
                }
            }
            HirStmtKind::Assign { lhs, rhs } => {
                // Assignments: normalize both sides
                let mut l = HirExpr {
                    id: lhs.id,
                    kind: lhs.kind.clone(),
                };
                self.normalize_expr(&mut l);
                *lhs = l;

                let mut r = HirExpr {
                    id: rhs.id,
                    kind: rhs.kind.clone(),
                };
                self.normalize_expr(&mut r);
                *rhs = r;
            }
            HirStmtKind::Return(e) | HirStmtKind::Break(e) => {
                if let Some(expr) = e {
                    let mut tmp = HirExpr {
                        id: expr.id,
                        kind: expr.kind.clone(),
                    };
                    self.normalize_expr(&mut tmp);
                    *expr = tmp;
                }
            }
            HirStmtKind::Continue => {}
        }
    }

    fn normalize_expr(&mut self, expr: &mut HirExpr) {
        use HirExprKind::*;
        match &mut *expr.kind {
            // Leaf literals/variables: no change
            Int(_) | Float(_) | Bool(_) | String(_) | Char(_) | None | Void | Variable(_) => {}

            // Pipeline: expr |> func => Call { func, args: [expr] }
            Pipeline { expr: piped, func } => {
                // Normalize children first
                let mut left = HirExpr {
                    id: piped.id,
                    kind: piped.kind.clone(),
                };
                self.normalize_expr(&mut left);

                let mut right = HirExpr {
                    id: func.id,
                    kind: func.kind.clone(),
                };
                self.normalize_expr(&mut right);

                // Build call node
                let call_id = expr.id; // reuse the current node id
                let call = HirExpr {
                    id: call_id,
                    kind: Box::new(HirExprKind::Call {
                        func: Box::new(right),
                        args: vec![left],
                    }),
                };

                // Span: inherit from original pipeline node onto call (its id remains the same)
                if let Some(sp) = self.span_of(expr.id) {
                    self.set_span(call_id, sp);
                }

                *expr = call;
            }

            // Postfix increment/decrement normalization (preserve value semantics)
            PostfixIncrement(inner) | PostfixDecrement(inner) => {
                // Normalize inner first
                let mut target = HirExpr {
                    id: inner.id,
                    kind: inner.kind.clone(),
                };
                self.normalize_expr(&mut target);

                let op = match &*expr.kind {
                    PostfixIncrement(_) => HirBinaryOp::Add,
                    PostfixDecrement(_) => HirBinaryOp::Sub,
                    _ => unreachable!(),
                };

                // Construct:
                // {
                //   let __tmpN = target;
                //   target = target (+|-) 1;
                //   __tmpN
                // }
                let block_id = expr.id; // reuse id for outer block
                let tmp_name = format!("__tmp{}", block_id.as_u32());
                // Spans
                let outer_span = self.span_of(expr.id);

                // 1 literal
                let one_expr_id = self.fresh_id();
                let one_expr = HirExpr {
                    id: one_expr_id,
                    kind: Box::new(HirExprKind::Int(1)),
                };
                if let Some(sp) = outer_span {
                    self.set_span(one_expr_id, sp);
                }

                // tmp pattern
                let tmp_pat_id = self.fresh_id();
                let tmp_pattern = veil_hir::HirPattern {
                    id: tmp_pat_id,
                    kind: Box::new(veil_hir::HirPatternKind::Variable(tmp_name.clone())),
                };
                if let Some(sp) = outer_span {
                    self.set_span(tmp_pat_id, sp);
                }

                // let __tmp = target;
                let let_id = self.fresh_id();
                let let_stmt = HirStmt {
                    id: let_id,
                    kind: HirStmtKind::Let {
                        pattern: tmp_pattern,
                        ty: Option::<HirType>::None,
                        init: Some(target.clone()),
                    },
                };
                if let Some(sp) = outer_span {
                    self.set_span(let_id, sp);
                }

                // rhs: target (+|-) 1
                let rhs_id = self.fresh_id();
                let rhs = HirExpr {
                    id: rhs_id,
                    kind: Box::new(HirExprKind::Binary {
                        op,
                        lhs: Box::new(target.clone()),
                        rhs: Box::new(one_expr),
                    }),
                };
                if let Some(sp) = outer_span {
                    self.set_span(rhs_id, sp);
                }

                // assign: target = (target (+|-) 1)
                let assign_id = self.fresh_id();
                let assign_stmt = HirStmt {
                    id: assign_id,
                    kind: HirStmtKind::Assign {
                        lhs: target.clone(),
                        rhs,
                    },
                };
                if let Some(sp) = outer_span {
                    self.set_span(assign_id, sp);
                }

                // tail expr: __tmp
                let tmp_var_id = self.fresh_id();
                let tmp_var = HirExpr {
                    id: tmp_var_id,
                    kind: Box::new(HirExprKind::Variable(tmp_name)),
                };
                if let Some(sp) = outer_span {
                    self.set_span(tmp_var_id, sp);
                }

                let block = HirExpr {
                    id: block_id,
                    kind: Box::new(HirExprKind::Block(HirBlock {
                        id: self.fresh_id(),
                        stmts: vec![let_stmt, assign_stmt],
                        expr: Some(Box::new(tmp_var)),
                    })),
                };

                if let Some(sp) = outer_span {
                    self.set_span(block_id, sp);
                }

                *expr = block;
            }

            // Field/index access: normalize inner
            FieldAccess { base, .. } => {
                let mut b = HirExpr {
                    id: base.id,
                    kind: base.kind.clone(),
                };
                self.normalize_expr(&mut b);
                *base = Box::new(b);
            }
            Index { base, index } => {
                let mut b = HirExpr {
                    id: base.id,
                    kind: base.kind.clone(),
                };
                self.normalize_expr(&mut b);
                *base = Box::new(b);

                let mut i = HirExpr {
                    id: index.id,
                    kind: index.kind.clone(),
                };
                self.normalize_expr(&mut i);
                *index = Box::new(i);
            }

            // Calls: normalize callee and args
            Call { func, args } => {
                let mut f = HirExpr {
                    id: func.id,
                    kind: func.kind.clone(),
                };
                self.normalize_expr(&mut f);
                *func = Box::new(f);

                for a in args.iter_mut() {
                    let mut tmp = HirExpr {
                        id: a.id,
                        kind: a.kind.clone(),
                    };
                    self.normalize_expr(&mut tmp);
                    *a = tmp;
                }
            }
            MethodCall {
                receiver,
                args,
                method: _,
            } => {
                let mut recv = HirExpr {
                    id: receiver.id,
                    kind: receiver.kind.clone(),
                };
                self.normalize_expr(&mut recv);
                *receiver = Box::new(recv);

                for a in args.iter_mut() {
                    let mut tmp = HirExpr {
                        id: a.id,
                        kind: a.kind.clone(),
                    };
                    self.normalize_expr(&mut tmp);
                    *a = tmp;
                }
            }

            // Postfix '?' lowering to early-return:
            // {
            //   let __optN = inner;
            //   if (__optN == none) { return none; }
            //   __optN
            // }
            PostfixQuestion(inner) => {
                // Normalize inner first
                let mut normalized_inner = HirExpr {
                    id: inner.id,
                    kind: inner.kind.clone(),
                };
                self.normalize_expr(&mut normalized_inner);

                let outer_span = self.span_of(expr.id);
                let block_expr_id = expr.id; // reuse the current node id for the outer block expr
                let block_id = self.fresh_id();

                // Temporary binding name
                let tmp_name = format!("__opt{}", block_expr_id.as_u32());

                // Build: let __opt = <inner>;
                let tmp_pat_id = self.fresh_id();
                let tmp_pattern = veil_hir::HirPattern {
                    id: tmp_pat_id,
                    kind: Box::new(veil_hir::HirPatternKind::Variable(tmp_name.clone())),
                };
                if let Some(sp) = outer_span {
                    self.set_span(tmp_pat_id, sp);
                }

                let let_id = self.fresh_id();
                let let_stmt = HirStmt {
                    id: let_id,
                    kind: HirStmtKind::Let {
                        pattern: tmp_pattern,
                        ty: Option::<HirType>::None,
                        init: Some(normalized_inner),
                    },
                };
                if let Some(sp) = outer_span {
                    self.set_span(let_id, sp);
                }

                // Build condition: (__opt == none)
                let tmp_var_for_cond_id = self.fresh_id();
                let tmp_var_for_cond = HirExpr {
                    id: tmp_var_for_cond_id,
                    kind: Box::new(HirExprKind::Variable(tmp_name.clone())),
                };
                if let Some(sp) = outer_span {
                    self.set_span(tmp_var_for_cond_id, sp);
                }

                let none_expr_id = self.fresh_id();
                let none_expr = HirExpr {
                    id: none_expr_id,
                    kind: Box::new(HirExprKind::None),
                };
                if let Some(sp) = outer_span {
                    self.set_span(none_expr_id, sp);
                }

                let cond_id = self.fresh_id();
                let cond_expr = HirExpr {
                    id: cond_id,
                    kind: Box::new(HirExprKind::Binary {
                        op: HirBinaryOp::Eq,
                        lhs: Box::new(tmp_var_for_cond),
                        rhs: Box::new(none_expr),
                    }),
                };
                if let Some(sp) = outer_span {
                    self.set_span(cond_id, sp);
                }

                // Then branch: { return none; }
                let then_block_id = self.fresh_id();
                let then_return_none_id = self.fresh_id();
                let then_return_none_expr = HirExpr {
                    id: self.fresh_id(),
                    kind: Box::new(HirExprKind::None),
                };
                let then_return_stmt = HirStmt {
                    id: then_return_none_id,
                    kind: HirStmtKind::Return(Some(then_return_none_expr)),
                };
                let then_block = HirBlock {
                    id: then_block_id,
                    stmts: vec![then_return_stmt],
                    expr: Option::<Box<HirExpr>>::None,
                };
                if let Some(sp) = outer_span {
                    self.set_span(then_block_id, sp);
                    self.set_span(then_return_none_id, sp);
                }

                // If expression as a statement: if (__opt == none) { return none; }
                let if_expr_id = self.fresh_id();
                let if_expr = HirExpr {
                    id: if_expr_id,
                    kind: Box::new(HirExprKind::If {
                        condition: Box::new(cond_expr),
                        then_branch: then_block,
                        else_branch: Option::<HirBlock>::None,
                    }),
                };
                if let Some(sp) = outer_span {
                    self.set_span(if_expr_id, sp);
                }

                let if_stmt_id = self.fresh_id();
                let if_stmt = HirStmt {
                    id: if_stmt_id,
                    kind: HirStmtKind::Expr(if_expr),
                };
                if let Some(sp) = outer_span {
                    self.set_span(if_stmt_id, sp);
                }

                // Tail expression: __opt
                let tmp_var_tail_id = self.fresh_id();
                let tmp_var_tail = HirExpr {
                    id: tmp_var_tail_id,
                    kind: Box::new(HirExprKind::Variable(tmp_name)),
                };
                if let Some(sp) = outer_span {
                    self.set_span(tmp_var_tail_id, sp);
                }

                // Outer block expression
                let block = HirExpr {
                    id: block_expr_id,
                    kind: Box::new(HirExprKind::Block(HirBlock {
                        id: block_id,
                        stmts: vec![let_stmt, if_stmt],
                        expr: Some(Box::new(tmp_var_tail)),
                    })),
                };
                if let Some(sp) = outer_span {
                    self.set_span(block_expr_id, sp);
                    self.set_span(block_id, sp);
                }

                *expr = block;
            }

            // Unary ops: normalize inner
            Unary { expr: inner, .. } => {
                let mut i = HirExpr {
                    id: inner.id,
                    kind: inner.kind.clone(),
                };
                self.normalize_expr(&mut i);
                *inner = Box::new(i);
            }

            // Binary ops: normalize both sides
            Binary { lhs, rhs, .. } => {
                let mut l = HirExpr {
                    id: lhs.id,
                    kind: lhs.kind.clone(),
                };
                self.normalize_expr(&mut l);
                *lhs = Box::new(l);

                let mut r = HirExpr {
                    id: rhs.id,
                    kind: rhs.kind.clone(),
                };
                self.normalize_expr(&mut r);
                *rhs = Box::new(r);
            }

            // Control flow: normalize nested blocks/exprs
            If {
                condition,
                then_branch,
                else_branch,
            } => {
                let mut c = HirExpr {
                    id: condition.id,
                    kind: condition.kind.clone(),
                };
                self.normalize_expr(&mut c);
                *condition = Box::new(c);

                self.normalize_block(then_branch);
                if let Some(else_b) = else_branch {
                    self.normalize_block(else_b);
                }
            }
            Match { expr: mexpr, arms } => {
                let mut me = HirExpr {
                    id: mexpr.id,
                    kind: mexpr.kind.clone(),
                };
                self.normalize_expr(&mut me);
                *mexpr = Box::new(me);

                for HirMatchArm {
                    pattern: _,
                    guard,
                    body,
                    ..
                } in arms.iter_mut()
                {
                    if let Some(g) = guard {
                        let mut ge = HirExpr {
                            id: g.id,
                            kind: g.kind.clone(),
                        };
                        self.normalize_expr(&mut ge);
                        *g = ge;
                    }
                    match body {
                        HirMatchArmBody::Expr(e) => {
                            let mut ee = HirExpr {
                                id: e.id,
                                kind: e.kind.clone(),
                            };
                            self.normalize_expr(&mut ee);
                            *e = ee;
                        }
                        HirMatchArmBody::Block(b) => {
                            self.normalize_block(b);
                        }
                    }
                }
            }
            Loop { body } | Spawn(body) | UnsafeBlock(body) | Block(body) => {
                self.normalize_block(body);
            }
            While { condition, body } => {
                let mut c = HirExpr {
                    id: condition.id,
                    kind: condition.kind.clone(),
                };
                self.normalize_expr(&mut c);
                *condition = Box::new(c);
                self.normalize_block(body);
            }
            For { iter, body, .. } => {
                let mut it = HirExpr {
                    id: iter.id,
                    kind: iter.kind.clone(),
                };
                self.normalize_expr(&mut it);
                *iter = Box::new(it);
                self.normalize_block(body);
            }

            // Memory ops and casts
            Ref(e) | Deref(e) | Await(e) => {
                let mut i = HirExpr {
                    id: e.id,
                    kind: e.kind.clone(),
                };
                self.normalize_expr(&mut i);
                *e = Box::new(i);
            }
            Cast { expr: inner, .. } => {
                let mut i = HirExpr {
                    id: inner.id,
                    kind: inner.kind.clone(),
                };
                self.normalize_expr(&mut i);
                *inner = Box::new(i);
            }

            // Constructors and tuples/arrays
            StructLiteral { fields, .. } => {
                for (_n, e) in fields.iter_mut() {
                    let mut tmp = HirExpr {
                        id: e.id,
                        kind: e.kind.clone(),
                    };
                    self.normalize_expr(&mut tmp);
                    *e = tmp;
                }
            }
            ArrayLiteral(elements) | TupleLiteral(elements) => {
                for e in elements.iter_mut() {
                    let mut tmp = HirExpr {
                        id: e.id,
                        kind: e.kind.clone(),
                    };
                    self.normalize_expr(&mut tmp);
                    *e = tmp;
                }
            }

            // Template strings
            Template { parts } => {
                for p in parts.iter_mut() {
                    if let veil_hir::HirTemplateStringPart::Expr(e) = p {
                        let mut tmp = HirExpr {
                            id: e.id,
                            kind: e.kind.clone(),
                        };
                        self.normalize_expr(&mut tmp);
                        *e = tmp;
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use veil_hir::*;

    fn span0() -> codespan::Span {
        codespan::Span::new(0, 1)
    }

    fn program_with_item(item: HirItem) -> HirProgram {
        let mut span_map = SpanMap::new();
        span_map.insert(item.id, span0());
        HirProgram {
            module_id: ModuleId::new(0),
            items: vec![item],
            span_map,
        }
    }

    #[test]
    fn normalize_pipeline_to_call() {
        // Build: fn main() { a |> b }
        let a_id = NodeId::new(10);
        let b_id = NodeId::new(11);
        let pipe_id = NodeId::new(12);
        let block_id = NodeId::new(13);
        let func_id = NodeId::new(1);

        let expr = HirExpr {
            id: pipe_id,
            kind: Box::new(HirExprKind::Pipeline {
                expr: Box::new(HirExpr {
                    id: a_id,
                    kind: Box::new(HirExprKind::Variable("a".to_string())),
                }),
                func: Box::new(HirExpr {
                    id: b_id,
                    kind: Box::new(HirExprKind::Variable("b".to_string())),
                }),
            }),
        };

        let mut program = program_with_item(HirItem {
            id: func_id,
            visibility: HirVisibility::Private,
            kind: HirItemKind::Function(HirFunction {
                id: func_id,
                name: "main".to_string(),
                symbol_id: None,
                generic_params: vec![],
                params: vec![],
                return_type: HirType::Void,
                body: HirBlock {
                    id: block_id,
                    stmts: vec![],
                    expr: Some(Box::new(expr)),
                },
            }),
        });

        // Add spans
        program.span_map.insert(a_id, span0());
        program.span_map.insert(b_id, span0());
        program.span_map.insert(pipe_id, span0());
        program.span_map.insert(block_id, span0());

        normalize_program(&mut program);

        // Inspect normalized expr
        let HirItemKind::Function(f) = &program.items[0].kind else {
            panic!("not function")
        };
        let Some(e) = &f.body.expr else {
            panic!("no expr")
        };
        match &*e.kind {
            HirExprKind::Call { func, args } => {
                match &*func.kind {
                    HirExprKind::Variable(name) => assert_eq!(name, "b"),
                    _ => panic!("func should be variable b"),
                }
                assert_eq!(args.len(), 1);
                match &*args[0].kind {
                    HirExprKind::Variable(name) => assert_eq!(name, "a"),
                    _ => panic!("arg should be variable a"),
                }
            }
            _ => panic!("pipeline should be normalized to call"),
        }
    }

    #[test]
    fn normalize_postfix_increment_variable() {
        // Build: fn main() { x++ }
        let x_id = NodeId::new(20);
        let post_id = NodeId::new(21);
        let block_id = NodeId::new(22);
        let func_id = NodeId::new(2);

        let expr = HirExpr {
            id: post_id,
            kind: Box::new(HirExprKind::PostfixIncrement(Box::new(HirExpr {
                id: x_id,
                kind: Box::new(HirExprKind::Variable("x".to_string())),
            }))),
        };

        let mut program = program_with_item(HirItem {
            id: func_id,
            visibility: HirVisibility::Private,
            kind: HirItemKind::Function(HirFunction {
                id: func_id,
                name: "main".to_string(),
                symbol_id: None,
                generic_params: vec![],
                params: vec![],
                return_type: HirType::Void,
                body: HirBlock {
                    id: block_id,
                    stmts: vec![HirStmt {
                        id: NodeId::new(30),
                        kind: HirStmtKind::Expr(expr),
                    }],
                    expr: None,
                },
            }),
        });

        // Add spans
        program.span_map.insert(x_id, span0());
        program.span_map.insert(post_id, span0());
        program.span_map.insert(block_id, span0());

        normalize_program(&mut program);

        let HirItemKind::Function(f) = &program.items[0].kind else {
            panic!("not function")
        };
        match &f.body.stmts[0].kind {
            HirStmtKind::Expr(e) => {
                // Expect block after normalization
                match &*e.kind {
                    HirExprKind::Block(b) => {
                        assert_eq!(b.stmts.len(), 2, "let tmp; assign");
                        assert!(b.expr.is_some(), "tail expression exists");
                    }
                    _ => panic!("postfix increment should become block"),
                }
            }
            _ => panic!("first stmt should be expr"),
        }
    }

    #[test]
    fn normalize_pipeline_chain_to_nested_calls() {
        // Build: fn main() { a |> b |> c } => c(b(a))
        let a_id = NodeId::new(40);
        let b_id = NodeId::new(41);
        let c_id = NodeId::new(42);
        let pipe1_id = NodeId::new(43); // (a |> b)
        let pipe2_id = NodeId::new(44); // (.. |> c)
        let block_id = NodeId::new(45);
        let func_id = NodeId::new(3);

        let expr = HirExpr {
            id: pipe2_id,
            kind: Box::new(HirExprKind::Pipeline {
                expr: Box::new(HirExpr {
                    id: pipe1_id,
                    kind: Box::new(HirExprKind::Pipeline {
                        expr: Box::new(HirExpr {
                            id: a_id,
                            kind: Box::new(HirExprKind::Variable("a".to_string())),
                        }),
                        func: Box::new(HirExpr {
                            id: b_id,
                            kind: Box::new(HirExprKind::Variable("b".to_string())),
                        }),
                    }),
                }),
                func: Box::new(HirExpr {
                    id: c_id,
                    kind: Box::new(HirExprKind::Variable("c".to_string())),
                }),
            }),
        };

        let mut program = program_with_item(HirItem {
            id: func_id,
            visibility: HirVisibility::Private,
            kind: HirItemKind::Function(HirFunction {
                id: func_id,
                name: "main".to_string(),
                symbol_id: None,
                generic_params: vec![],
                params: vec![],
                return_type: HirType::Void,
                body: HirBlock {
                    id: block_id,
                    stmts: vec![],
                    expr: Some(Box::new(expr)),
                },
            }),
        });

        // Spans
        program.span_map.insert(a_id, span0());
        program.span_map.insert(b_id, span0());
        program.span_map.insert(c_id, span0());
        program.span_map.insert(pipe1_id, span0());
        program.span_map.insert(pipe2_id, span0());
        program.span_map.insert(block_id, span0());

        normalize_program(&mut program);

        // Expect c(b(a))
        let HirItemKind::Function(f) = &program.items[0].kind else {
            panic!("not function")
        };
        let Some(e) = &f.body.expr else {
            panic!("no expr")
        };
        match &*e.kind {
            HirExprKind::Call {
                func: cfunc,
                args: cargs,
            } => {
                // c(...)
                match &*cfunc.kind {
                    HirExprKind::Variable(name) => assert_eq!(name, "c"),
                    _ => panic!("outer func should be 'c'"),
                }
                assert_eq!(cargs.len(), 1);
                // b(a)
                match &*cargs[0].kind {
                    HirExprKind::Call {
                        func: bfunc,
                        args: bargs,
                    } => {
                        match &*bfunc.kind {
                            HirExprKind::Variable(name) => assert_eq!(name, "b"),
                            _ => panic!("inner func should be 'b'"),
                        }
                        assert_eq!(bargs.len(), 1);
                        match &*bargs[0].kind {
                            HirExprKind::Variable(name) => assert_eq!(name, "a"),
                            _ => panic!("innermost arg should be 'a'"),
                        }
                    }
                    _ => panic!("expected b(a) as arg to c(...)"),
                }
            }
            _ => panic!("pipeline chain should normalize to nested function calls"),
        }
    }

    #[test]
    fn normalize_postfix_question_to_early_return() {
        // Build: fn main() { x? }
        let x_id = NodeId::new(60);
        let postq_id = NodeId::new(61);
        let block_id = NodeId::new(62);
        let func_id = NodeId::new(4);

        let expr = HirExpr {
            id: postq_id,
            kind: Box::new(HirExprKind::PostfixQuestion(Box::new(HirExpr {
                id: x_id,
                kind: Box::new(HirExprKind::Variable("x".to_string())),
            }))),
        };

        let mut program = program_with_item(HirItem {
            id: func_id,
            visibility: HirVisibility::Private,
            kind: HirItemKind::Function(HirFunction {
                id: func_id,
                name: "main".to_string(),
                symbol_id: None,
                generic_params: vec![],
                params: vec![],
                return_type: HirType::Void, // normalization does not enforce type here
                body: HirBlock {
                    id: block_id,
                    stmts: vec![],
                    expr: Some(Box::new(expr)),
                },
            }),
        });

        // Spans
        program.span_map.insert(x_id, span0());
        program.span_map.insert(postq_id, span0());
        program.span_map.insert(block_id, span0());

        normalize_program(&mut program);

        // After normalization, expect a block with:
        // let __optN = x;
        // if (__optN == none) { return none; }
        // __optN
        let HirItemKind::Function(f) = &program.items[0].kind else {
            panic!("not function")
        };
        let Some(e) = &f.body.expr else {
            panic!("no expr")
        };
        match &*e.kind {
            HirExprKind::Block(b) => {
                assert_eq!(b.stmts.len(), 2, "expected let temp and if-return");
                // stmt[0] is let
                match &b.stmts[0].kind {
                    HirStmtKind::Let { pattern, init, .. } => {
                        // pattern variable name and init should exist
                        match &*pattern.kind {
                            HirPatternKind::Variable(name) => {
                                assert!(name.starts_with("__opt"), "temp should be __optN");
                            }
                            _ => panic!("let pattern should be variable"),
                        }
                        assert!(init.is_some(), "let must initialize temp with inner expr");
                    }
                    _ => panic!("first stmt should be let"),
                }
                // stmt[1] is if-expression wrapped as statement
                match &b.stmts[1].kind {
                    HirStmtKind::Expr(if_expr) => match &*if_expr.kind {
                        HirExprKind::If {
                            condition,
                            then_branch,
                            else_branch,
                        } => {
                            assert!(else_branch.is_none(), "no else branch expected");
                            // condition is temp == none
                            match &*condition.kind {
                                HirExprKind::Binary { op, lhs, rhs } => {
                                    assert!(
                                        matches!(op, HirBinaryOp::Eq),
                                        "expected equality check"
                                    );
                                    // rhs none
                                    assert!(
                                        matches!(&*rhs.kind, HirExprKind::None),
                                        "rhs should be none"
                                    );
                                    // lhs is temp var
                                    assert!(
                                        matches!(&*lhs.kind, HirExprKind::Variable(_)),
                                        "lhs should be temp variable"
                                    );
                                }
                                _ => panic!("condition should be binary eq"),
                            }
                            // then branch returns none
                            assert_eq!(
                                then_branch.stmts.len(),
                                1,
                                "then branch should have one stmt"
                            );
                            match &then_branch.stmts[0].kind {
                                HirStmtKind::Return(Some(ret_expr)) => {
                                    assert!(
                                        matches!(&*ret_expr.kind, HirExprKind::None),
                                        "return none"
                                    );
                                }
                                _ => panic!("then branch should be return none"),
                            }
                        }
                        _ => panic!("second stmt should be if-expression"),
                    },
                    _ => panic!("second stmt should be expr (if)"),
                }
                // tail expr is temp variable
                let tail = b.expr.as_ref().expect("tail expr expected");
                assert!(
                    matches!(&*tail.kind, HirExprKind::Variable(_)),
                    "tail should be temp variable"
                );
            }
            _ => panic!("postfix '?' should normalize to block with early-return pattern"),
        }
    }
}
