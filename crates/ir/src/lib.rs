#![forbid(unsafe_code)]
#![allow(rust_2018_idioms)]
#![deny(unused_must_use)]

//! Veil IR (Intermediate Representation)
//!
//! Minimal, structured IR with a deterministic pretty-printer and lowering
//! from post-normalize, post-mono HIR (preferred) and a temporary AST path.
//!
//! Status:
//! - IR data model: Program, Function, Block, Instructions (minimal), Terminator
//! - Deterministic print: stable order (functions sorted by name, blocks by id, inst order preserved)
//! - Lowering: HIR → IR (basic coverage), legacy AST → IR skeleton retained for debug
//!
//! Roadmap:
//! - Flesh out instruction set and types
//! - Add golden IR snapshots and round-trip tests

use std::fmt;

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use veil_ast as ast;
use veil_hir as hir;

// ===== Core IR Types =====

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub struct BlockId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub struct ValueId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub struct LocalId(pub u32);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ProgramIR {
    // Use Vec for function storage; display sorts deterministically by name.
    pub functions: Vec<FunctionIR>,
    // Future: globals, data segments, cfg metadata, etc.
}

impl Default for ProgramIR {
    fn default() -> Self {
        Self::new()
    }
}

impl ProgramIR {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
        }
    }

    pub fn to_pretty_string(&self) -> String {
        let mut out = String::new();

        // Sort functions by name for deterministic printing
        let mut fns_sorted = self.functions.clone();
        fns_sorted.sort_by(|a, b| a.name.cmp(&b.name));

        for (i, f) in fns_sorted.iter().enumerate() {
            if i > 0 {
                out.push('\n');
            }
            out.push_str(&f.to_pretty_string());
        }

        out
    }
}

impl fmt::Display for ProgramIR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.to_pretty_string())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionIR {
    pub name: String,
    pub params: Vec<ParamIR>,
    pub ret: TypeIR,
    pub entry: BlockId,
    // Function-local slots (for future Load/Store); printed deterministically
    pub locals: Vec<LocalIR>,
    // Deterministic block order: we use a simple Vec and sort by BlockId in print
    pub blocks: Vec<BlockIR>,
    // Named values are mapped deterministically on emit; internal mapping may use IndexMap when expanded
    pub debug_names: IndexMap<ValueId, String>,
}

impl FunctionIR {
    pub fn new<N: Into<String>>(name: N, params: Vec<ParamIR>, ret: TypeIR) -> Self {
        let entry = BlockId(0);
        let entry_block = BlockIR {
            id: entry,
            insts: Vec::new(),
            results: Vec::new(),
            term: TerminatorIR::Return { value: None },
        };
        Self {
            name: name.into(),
            params,
            ret,
            entry,
            locals: Vec::new(),
            blocks: vec![entry_block],
            debug_names: IndexMap::new(),
        }
    }

    pub fn to_pretty_string(&self) -> String {
        let mut s = String::new();
        // Header
        s.push_str("fn ");
        s.push_str(&self.name);
        s.push('(');
        for (i, p) in self.params.iter().enumerate() {
            if i > 0 {
                s.push_str(", ");
            }
            s.push_str(&p.name);
            s.push_str(": ");
            s.push_str(&p.ty.to_string());
        }
        s.push_str(") -> ");
        s.push_str(&self.ret.to_string());
        s.push_str(" {\n");
        // Locals elided in pretty printer

        // Blocks sorted by id
        let mut blocks = self.blocks.clone();
        blocks.sort_by_key(|b| b.id);

        for b in blocks {
            s.push_str(&format!("  bb{}:\n", b.id.0));
            for (idx, inst) in b.insts.iter().enumerate() {
                let rid = b.results.get(idx).copied().unwrap_or(ValueId(idx as u32));
                s.push_str(&format!("    %{} = ", rid.0));
                s.push_str(&inst.to_string());
                s.push('\n');
            }
            s.push_str("    ");
            s.push_str(&b.term.to_string());
            s.push('\n');
        }

        s.push('}');
        s
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ParamIR {
    pub name: String,
    pub ty: TypeIR,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LocalIR {
    pub id: LocalId,
    pub ty: TypeIR,
    pub debug_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BlockIR {
    pub id: BlockId,
    pub insts: Vec<InstIR>,
    pub results: Vec<ValueId>,
    pub term: TerminatorIR,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum InstIR {
    // Minimal seed set; will expand.
    Nop,
    // Constants
    ConstInt {
        value: i64,
    },
    ConstFloat {
        value: f64,
    },
    ConstStr {
        value: String,
    },
    // Example arithmetic
    Add {
        lhs: ValueId,
        rhs: ValueId,
    },
    Sub {
        lhs: ValueId,
        rhs: ValueId,
    },
    Mul {
        lhs: ValueId,
        rhs: ValueId,
    },
    Div {
        lhs: ValueId,
        rhs: ValueId,
    },
    // Extended arithmetic and bitwise (feature-gated)
    Mod {
        lhs: ValueId,
        rhs: ValueId,
    },
    // Bitwise ops
    BitAnd {
        lhs: ValueId,
        rhs: ValueId,
    },
    BitOr {
        lhs: ValueId,
        rhs: ValueId,
    },
    BitXor {
        lhs: ValueId,
        rhs: ValueId,
    },
    // Shifts
    Shl {
        lhs: ValueId,
        rhs: ValueId,
    },
    Shr {
        lhs: ValueId,
        rhs: ValueId,
    },
    // Comparisons (integer/float semantics decided by types upstream)
    CmpEq {
        lhs: ValueId,
        rhs: ValueId,
    },
    CmpNe {
        lhs: ValueId,
        rhs: ValueId,
    },
    CmpLt {
        lhs: ValueId,
        rhs: ValueId,
    },
    CmpLe {
        lhs: ValueId,
        rhs: ValueId,
    },
    CmpGt {
        lhs: ValueId,
        rhs: ValueId,
    },
    CmpGe {
        lhs: ValueId,
        rhs: ValueId,
    },
    // Select (ternary) producing a value without branching at the IR level
    Select {
        cond: ValueId,
        then_v: ValueId,
        else_v: ValueId,
    },
    // Unary ops
    Not {
        value: ValueId,
    },
    Neg {
        value: ValueId,
    },
    Pos {
        value: ValueId,
    },
    // Cast to a target IR type
    Cast {
        value: ValueId,
        ty: TypeIR,
    },
    // Memory
    Load {
        local: LocalId,
    },
    Store {
        local: LocalId,
        value: ValueId,
    },
    // Call
    Call {
        callee: String,
        args: Vec<ValueId>,
    },
    // Format a string using a printf-style format and arguments; result is a String
    Format {
        fmt: String,
        args: Vec<ValueId>,
    },
}

impl fmt::Display for InstIR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use InstIR::*;
        match self {
            Nop => write!(f, "nop"),
            ConstInt { value } => write!(f, "const.i64 {}", value),
            ConstFloat { value } => write!(f, "const.f64 {}", value),
            ConstStr { value } => write!(f, "const.str {:?}", value),
            Add { lhs, rhs } => write!(f, "add %{}, %{}", lhs.0, rhs.0),
            Sub { lhs, rhs } => write!(f, "sub %{}, %{}", lhs.0, rhs.0),
            Mul { lhs, rhs } => write!(f, "mul %{}, %{}", lhs.0, rhs.0),
            Div { lhs, rhs } => write!(f, "div %{}, %{}", lhs.0, rhs.0),
            Mod { lhs, rhs } => write!(f, "mod %{}, %{}", lhs.0, rhs.0),
            BitAnd { lhs, rhs } => write!(f, "band %{}, %{}", lhs.0, rhs.0),
            BitOr { lhs, rhs } => write!(f, "bor %{}, %{}", lhs.0, rhs.0),
            BitXor { lhs, rhs } => write!(f, "bxor %{}, %{}", lhs.0, rhs.0),
            Shl { lhs, rhs } => write!(f, "shl %{}, %{}", lhs.0, rhs.0),
            Shr { lhs, rhs } => write!(f, "shr %{}, %{}", lhs.0, rhs.0),
            CmpEq { lhs, rhs } => write!(f, "icmp.eq %{}, %{}", lhs.0, rhs.0),
            CmpNe { lhs, rhs } => write!(f, "icmp.ne %{}, %{}", lhs.0, rhs.0),
            CmpLt { lhs, rhs } => write!(f, "icmp.lt %{}, %{}", lhs.0, rhs.0),
            CmpLe { lhs, rhs } => write!(f, "icmp.le %{}, %{}", lhs.0, rhs.0),
            CmpGt { lhs, rhs } => write!(f, "icmp.gt %{}, %{}", lhs.0, rhs.0),
            CmpGe { lhs, rhs } => write!(f, "icmp.ge %{}, %{}", lhs.0, rhs.0),
            Select {
                cond,
                then_v,
                else_v,
            } => write!(f, "select %{}, %{}, %{}", cond.0, then_v.0, else_v.0),
            Not { value } => write!(f, "not %{}", value.0),
            Neg { value } => write!(f, "neg %{}", value.0),
            Pos { value } => write!(f, "pos %{}", value.0),
            Cast { value, ty } => write!(f, "cast %{} -> {}", value.0, ty),
            Load { local } => write!(f, "load %l{}", local.0),
            Store { local, value } => write!(f, "store %l{}, %{}", local.0, value.0),
            Call { callee, args } => {
                write!(f, "call {}(", callee)?;
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "%{}", a.0)?;
                }
                write!(f, ")")
            }
            Format { fmt, args } => {
                write!(f, "format {:?}(", fmt)?;
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "%{}", a.0)?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TerminatorIR {
    Return {
        value: Option<ValueId>,
    },
    Branch {
        cond: ValueId,
        then_bb: BlockId,
        else_bb: BlockId,
    },
    Jump {
        bb: BlockId,
    },
}

impl fmt::Display for TerminatorIR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TerminatorIR::Return { value } => {
                if let Some(v) = value {
                    write!(f, "ret %{}", v.0)
                } else {
                    write!(f, "ret")
                }
            }
            TerminatorIR::Branch {
                cond,
                then_bb,
                else_bb,
            } => {
                write!(f, "br %{}, bb{}, bb{}", cond.0, then_bb.0, else_bb.0)
            }
            TerminatorIR::Jump { bb } => {
                write!(f, "jmp bb{}", bb.0)
            }
        }
    }
}

/// Minimal IR type set; intentionally compact for the first step.
/// Will expand to post-mono concrete types.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TypeIR {
    Void,
    I32,
    I64,
    F32,
    F64,
    Bool,
    String,
    Ptr(Box<TypeIR>),
    // For early stages, allow unknown/opaque names to carry through deterministically
    Opaque(String),
}

impl fmt::Display for TypeIR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TypeIR::*;
        match self {
            Void => write!(f, "void"),
            I32 => write!(f, "i32"),
            I64 => write!(f, "i64"),
            F32 => write!(f, "f32"),
            F64 => write!(f, "f64"),
            Bool => write!(f, "bool"),
            String => write!(f, "string"),
            Ptr(inner) => write!(f, "*{}", inner),
            Opaque(name) => write!(f, "opaque<{}>", name),
        }
    }
}

// Module-scope loop context used by lowering control-flow
struct LoopCtx {
    break_bb: BlockId,
    continue_bb: BlockId,
}

// ===== Lowering (Preferred: HIR → IR) =====

struct LoweringCtx<'a> {
    // Global function return types, for inference without hardcoded names
    function_ret_types: &'a std::collections::HashMap<String, TypeIR>,
    // Functions that support template string expansion tol per-function state
    blocks: Vec<BlockIR>,
    cur_bb: BlockId,
    next_val: u32,
    locals: std::collections::HashMap<String, ValueId>,
    local_slots: std::collections::HashMap<String, LocalId>,
    locals_meta: Vec<LocalIR>,
    debug_names: IndexMap<ValueId, String>,
    loops: Vec<LoopCtx>,
}

impl<'a> LoweringCtx<'a> {
    fn new(function_ret_types: &'a std::collections::HashMap<String, TypeIR>) -> Self {
        Self {
            function_ret_types,
            blocks: Vec::new(),
            cur_bb: BlockId(0),
            next_val: 0,
            locals: std::collections::HashMap::new(),
            local_slots: std::collections::HashMap::new(),
            locals_meta: Vec::new(),
            debug_names: IndexMap::new(),
            loops: Vec::new(),
        }
    }

    fn new_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len() as u32);
        self.blocks.push(BlockIR {
            id,
            insts: Vec::new(),
            results: Vec::new(),
            term: TerminatorIR::Return { value: None },
        });
        id
    }

    fn get_block_mut(&mut self, id: BlockId) -> &mut BlockIR {
        // Ids are sequential and correspond to indices
        &mut self.blocks[id.0 as usize]
    }

    fn emit(&mut self, bb: BlockId, inst: InstIR) -> ValueId {
        let id = ValueId(self.next_val);
        self.next_val = self.next_val.saturating_add(1);
        let block = self.get_block_mut(bb);
        block.insts.push(inst);
        block.results.push(id);
        id
    }

    fn set_term(&mut self, bb: BlockId, term: TerminatorIR) {
        let block = self.get_block_mut(bb);
        block.term = term;
    }

    fn infer_type_from_hir_expr_with_context(&self, expr: &hir::HirExpr) -> TypeIR {
        infer_type_from_hir_expr_with_context(
            expr,
            &self.locals_meta,
            &self.local_slots,
            self.function_ret_types,
        )
    }

    fn infer_format_specifier_for_expr(&self, expr: &hir::HirExpr) -> String {
        infer_format_specifier_for_expr(
            expr,
            &self.locals_meta,
            &self.local_slots,
            self.function_ret_types,
        )
    }

    fn resolve_final_callee_name(&self, expr: &hir::HirExpr) -> Option<String> {
        use hir::HirExprKind as K;
        match &*expr.kind {
            K::Variable(name) => Some(name.clone()),
            K::Pipeline { expr: _, func } => {
                // In pipeline, func determines the callee
                self.resolve_final_callee_name(func)
            }
            _ => None,
        }
    }

    fn lower_template_string(&mut self, parts: &[hir::HirTemplateStringPart]) -> ValueId {
        let mut fmt = String::new();

        let mut exprs: Vec<&hir::HirExpr> = Vec::new();

        for p in parts {
            match p {
                hir::HirTemplateStringPart::String(s) => fmt.push_str(s),

                hir::HirTemplateStringPart::Expr(ex) => {
                    let spec = self.infer_format_specifier_for_expr(ex);

                    fmt.push_str(&spec);

                    exprs.push(ex);
                }
            }
        }

        let mut argv = Vec::new();

        for ex in exprs {
            if let Some(v) = self.lower_expr(ex) {
                argv.push(v);
            }
        }

        self.emit(self.cur_bb, InstIR::Format { fmt, args: argv })
    }

    fn lower_expr(&mut self, expr: &hir::HirExpr) -> Option<ValueId> {
        use hir::HirBinaryOp::*;
        use hir::HirExprKind as K;
        match &*expr.kind {
            K::Int(v) => Some(self.emit(self.cur_bb, InstIR::ConstInt { value: *v })),
            K::Float(v) => Some(self.emit(self.cur_bb, InstIR::ConstFloat { value: *v })),
            K::Bool(b) => {
                let n = if *b { 1 } else { 0 };
                Some(self.emit(self.cur_bb, InstIR::ConstInt { value: n }))
            }
            K::String(s) => Some(self.emit(self.cur_bb, InstIR::ConstStr { value: s.clone() })),
            K::Template { parts } => Some(self.lower_template_string(parts)),
            K::Variable(name) => {
                if let Some(lid) = self.local_slots.get(name) {
                    Some(self.emit(self.cur_bb, InstIR::Load { local: *lid }))
                } else {
                    self.locals.get(name).copied()
                }
            }
            K::Binary { op, lhs, rhs } => {
                match op {
                    // Short-circuiting logical AND: (l && r)
                    And => {
                        let l = self.lower_expr(lhs)?;

                        // Create blocks for rhs evaluation and merge
                        let rhs_bb = self.new_block();
                        let merge_bb = self.new_block();
                        // Branch on l: if true, evaluate rhs; else skip to merge
                        self.set_term(
                            self.cur_bb,
                            TerminatorIR::Branch {
                                cond: l,
                                then_bb: rhs_bb,
                                else_bb: merge_bb,
                            },
                        );
                        // Lower rhs in rhs_bb
                        self.cur_bb = rhs_bb;

                        let r = self.lower_expr(rhs).unwrap_or_else(|| {
                            self.emit(self.cur_bb, InstIR::ConstInt { value: 0 })
                        });

                        self.set_term(self.cur_bb, TerminatorIR::Jump { bb: merge_bb });
                        // Merge: select(l ? r : 0)
                        self.cur_bb = merge_bb;
                        let zero = self.emit(self.cur_bb, InstIR::ConstInt { value: 0 });
                        Some(self.emit(
                            self.cur_bb,
                            InstIR::Select {
                                cond: l,
                                then_v: r,
                                else_v: zero,
                            },
                        ))
                    }
                    // Short-circuiting logical OR: (l || r)
                    Or => {
                        let l = self.lower_expr(lhs)?;

                        let rhs_bb = self.new_block();
                        let merge_bb = self.new_block();
                        // If l is true, skip rhs and produce 1; else evaluate rhs
                        self.set_term(
                            self.cur_bb,
                            TerminatorIR::Branch {
                                cond: l,
                                then_bb: merge_bb,
                                else_bb: rhs_bb,
                            },
                        );
                        // Lower rhs when needed
                        self.cur_bb = rhs_bb;

                        let r = self.lower_expr(rhs).unwrap_or_else(|| {
                            self.emit(self.cur_bb, InstIR::ConstInt { value: 0 })
                        });

                        self.set_term(self.cur_bb, TerminatorIR::Jump { bb: merge_bb });
                        // Merge: select(l ? 1 : r)
                        self.cur_bb = merge_bb;
                        let one = self.emit(self.cur_bb, InstIR::ConstInt { value: 1 });
                        Some(self.emit(
                            self.cur_bb,
                            InstIR::Select {
                                cond: l,
                                then_v: one,
                                else_v: r,
                            },
                        ))
                    }
                    _ => {
                        let l = self.lower_expr(lhs)?;
                        let r = self.lower_expr(rhs)?;

                        let inst = match op {
                            Add => InstIR::Add { lhs: l, rhs: r },
                            Sub => InstIR::Sub { lhs: l, rhs: r },
                            Mul => InstIR::Mul { lhs: l, rhs: r },
                            Div | IDiv => InstIR::Div { lhs: l, rhs: r },
                            Mod => InstIR::Mod { lhs: l, rhs: r },
                            BitAnd => InstIR::BitAnd { lhs: l, rhs: r },
                            BitOr => InstIR::BitOr { lhs: l, rhs: r },
                            BitXor => InstIR::BitXor { lhs: l, rhs: r },
                            Shl => InstIR::Shl { lhs: l, rhs: r },
                            Shr => InstIR::Shr { lhs: l, rhs: r },
                            Eq => InstIR::CmpEq { lhs: l, rhs: r },
                            Ne => InstIR::CmpNe { lhs: l, rhs: r },
                            Lt => InstIR::CmpLt { lhs: l, rhs: r },
                            Le => InstIR::CmpLe { lhs: l, rhs: r },
                            Gt => InstIR::CmpGt { lhs: l, rhs: r },
                            Ge => InstIR::CmpGe { lhs: l, rhs: r },
                            _ => InstIR::Nop,
                        };
                        Some(self.emit(self.cur_bb, inst))
                    }
                }
            }

            K::Call { func, args } => {
                if let Some(name) = self.resolve_final_callee_name(func) {
                    // Default lowering for other calls

                    let mut argv = Vec::new();

                    for a in args {
                        if let Some(v) = self.lower_expr(a) {
                            argv.push(v);
                        }
                    }

                    Some(self.emit(
                        self.cur_bb,
                        InstIR::Call {
                            callee: name,

                            args: argv,
                        },
                    ))
                } else {
                    None
                }
            }

            // Unary operations
            K::Unary { op, expr } => {
                let v = self
                    .lower_expr(expr)
                    .unwrap_or_else(|| self.emit(self.cur_bb, InstIR::Nop));
                use hir::HirUnaryOp::*;
                let inst = match op {
                    Not => InstIR::Not { value: v },
                    Minus => InstIR::Neg { value: v },
                    Plus => InstIR::Pos { value: v },
                    PreInc => InstIR::Pos { value: v }, // TODO: Implement proper pre-increment
                    PostInc => InstIR::Pos { value: v }, // TODO: Implement proper post-increment
                    PreDec => InstIR::Neg { value: v }, // TODO: Implement proper pre-decrement
                    PostDec => InstIR::Neg { value: v }, // TODO: Implement proper post-decrement
                };
                Some(self.emit(self.cur_bb, inst))
            }
            // Casts
            K::Cast { expr, ty } => {
                let v = self
                    .lower_expr(expr)
                    .unwrap_or_else(|| self.emit(self.cur_bb, InstIR::Nop));
                let ity = map_hir_type_to_ir(ty);
                Some(self.emit(self.cur_bb, InstIR::Cast { value: v, ty: ity }))
            }
            // Control-flow expressions (If/While/Loop) are handled in statement lowering
            _ => None,
        }
    }

    fn lower_block(&mut self, blk: &hir::HirBlock) {
        for stmt in &blk.stmts {
            if let Some(_ret) = self.lower_stmt(stmt) {
                // Early return; keep terminator set by caller
                return;
            }
        }
        if let Some(expr) = &blk.expr {
            let _ = self.lower_expr(expr);
        }
    }

    fn lower_if_stmt(
        &mut self,
        cond: &hir::HirExpr,
        then_branch: &hir::HirBlock,
        else_branch: &Option<hir::HirBlock>,
    ) {
        // Evaluate condition in current block
        let cval = self
            .lower_expr(cond)
            .unwrap_or_else(|| self.emit(self.cur_bb, InstIR::Nop));

        // Create CFG nodes
        let then_bb = self.new_block();
        let else_bb = self.new_block();
        let merge_bb = self.new_block();

        // Branch out of current block
        self.set_term(
            self.cur_bb,
            TerminatorIR::Branch {
                cond: cval,
                then_bb,
                else_bb,
            },
        );

        // Lower THEN
        self.cur_bb = then_bb;
        self.lower_block(then_branch);
        // Ensure jump to merge if not already returned/branched (we conservatively overwrite)
        self.set_term(self.cur_bb, TerminatorIR::Jump { bb: merge_bb });

        // Lower ELSE
        self.cur_bb = else_bb;
        if let Some(eb) = else_branch {
            self.lower_block(eb);
        }
        self.set_term(self.cur_bb, TerminatorIR::Jump { bb: merge_bb });

        // Continue at merge
        self.cur_bb = merge_bb;
    }

    fn lower_while_stmt(&mut self, condition: &hir::HirExpr, body: &hir::HirBlock) {
        // Create blocks
        let cond_bb = self.new_block();
        let loop_bb = self.new_block();
        let after_bb = self.new_block();

        // Jump from current to cond
        self.set_term(self.cur_bb, TerminatorIR::Jump { bb: cond_bb });

        // Lower condition
        self.cur_bb = cond_bb;

        let cval = self
            .lower_expr(condition)
            .unwrap_or_else(|| self.emit(self.cur_bb, InstIR::Nop));

        self.set_term(
            self.cur_bb,
            TerminatorIR::Branch {
                cond: cval,
                then_bb: loop_bb,
                else_bb: after_bb,
            },
        );

        // Push loop ctx
        self.loops.push(LoopCtx {
            break_bb: after_bb,
            continue_bb: cond_bb,
        });

        // Lower body
        self.cur_bb = loop_bb;
        self.lower_block(body);
        // Continue to re-check condition
        self.set_term(self.cur_bb, TerminatorIR::Jump { bb: cond_bb });

        // Pop loop ctx and continue at after
        self.loops.pop();
        self.cur_bb = after_bb;
    }

    fn lower_stmt(&mut self, stmt: &hir::HirStmt) -> Option<Option<ValueId>> {
        use hir::HirExprKind as K;
        use hir::HirStmtKind as S;
        match &stmt.kind {
            S::Let { pattern, ty, init } => {
                if let hir::HirPatternKind::Variable(name) = &*pattern.kind {
                    // Allocate a unique local slot for this binding
                    let new_local = if name == "_" {
                        // Always allocate new unique local for '_' (discard variables)
                        LocalId(self.locals_meta.len() as u32)
                    } else if let Some(&existing_local) = self.local_slots.get(name) {
                        // Reuse existing local for the same variable name
                        existing_local
                    } else {
                        // Allocate new unique local ID
                        let new_id = LocalId(self.locals_meta.len() as u32);
                        self.local_slots.insert(name.clone(), new_id);
                        new_id
                    };
                    // Record LocalIR with best-effort type from HIR or infer from init
                    let local_ty = if let Some(ty) = ty {
                        map_hir_type_to_ir(ty)
                    } else if let Some(init_expr) = init {
                        // Try to infer type from the initialization expression
                        self.infer_type_from_hir_expr_with_context(init_expr)
                    } else {
                        // Default to i32 for untyped, uninitialized locals
                        TypeIR::I32
                    };
                    // Only add to locals_meta if this is a new local
                    if !self.locals_meta.iter().any(|l| l.id == new_local) {
                        self.locals_meta.push(LocalIR {
                            id: new_local,
                            ty: local_ty,
                            debug_name: Some(name.clone()),
                        });
                    }

                    // Initialize with RHS if provided
                    if let Some(init_expr) = init {
                        if let Some(v) = self.lower_expr(init_expr) {
                            let _ = self.emit(
                                self.cur_bb,
                                InstIR::Store {
                                    local: new_local,
                                    value: v,
                                },
                            );
                        }
                    }
                }
                None
            }
            S::Expr(e) => {
                // Surface-level control-flow expressions handled here
                match &*e.kind {
                    K::If {
                        condition,
                        then_branch,
                        else_branch,
                    } => {
                        self.lower_if_stmt(condition, then_branch, else_branch);
                        None
                    }
                    K::While { condition, body } => {
                        self.lower_while_stmt(condition, body);
                        None
                    }
                    // Desugar a simple match into a branch chain for literal patterns without guards
                    K::Match { expr, arms } => {
                        // Evaluate scrutinee
                        let scrut = self.lower_expr(expr).unwrap_or_else(|| {
                            self.emit(self.cur_bb, InstIR::ConstInt { value: 0 })
                        });

                        let next_bb = self.cur_bb;
                        let merge_bb = self.new_block();
                        let mut arm_blocks: Vec<(Option<ValueId>, BlockId)> = Vec::new();

                        // Create a basic block per arm and record literal value when applicable
                        for arm in arms {
                            let arm_bb = self.new_block();
                            // Support literal and wildcard arms with optional guards; non-literal structured patterns fall back to default behavior
                            let lit_val = match &*arm.pattern.kind {
                                hir::HirPatternKind::Literal(boxed) => match &*boxed.kind {
                                    K::Int(v) => {
                                        Some(self.emit(arm_bb, InstIR::ConstInt { value: *v }))
                                    }
                                    K::Bool(b) => {
                                        let n = if *b { 1 } else { 0 };
                                        Some(self.emit(arm_bb, InstIR::ConstInt { value: n }))
                                    }
                                    _ => None,
                                },
                                _ => None,
                            };
                            arm_blocks.push((lit_val, arm_bb));
                        }

                        // Chain branches comparing scrutinee to each literal arm; default to first arm if non-literal/else
                        let mut cursor = next_bb;
                        for (i, (lit, arm_bb)) in arm_blocks.iter().enumerate() {
                            // Determine the fallthrough compare block (or default to this arm if last)
                            let fallthrough = if i + 1 < arm_blocks.len() {
                                self.new_block()
                            } else {
                                *arm_bb
                            };
                            if let Some(lv) = lit {
                                // Compare scrutinee == literal
                                let cmp = self.emit(
                                    cursor,
                                    InstIR::CmpEq {
                                        lhs: scrut,
                                        rhs: *lv,
                                    },
                                );
                                // If the arm has a guard, evaluate it in a dedicated block
                                if let Some(guard_expr) = &arms[i].guard {
                                    let guard_bb = self.new_block();
                                    self.set_term(
                                        cursor,
                                        TerminatorIR::Branch {
                                            cond: cmp,
                                            then_bb: guard_bb,
                                            else_bb: fallthrough,
                                        },
                                    );
                                    // Lower guard in guard_bb
                                    let guard_cur = guard_bb;
                                    self.cur_bb = guard_cur;

                                    let g = self.lower_expr(guard_expr).unwrap_or_else(|| {
                                        self.emit(self.cur_bb, InstIR::ConstInt { value: 0 })
                                    });

                                    self.set_term(
                                        self.cur_bb,
                                        TerminatorIR::Branch {
                                            cond: g,
                                            then_bb: *arm_bb,
                                            else_bb: fallthrough,
                                        },
                                    );
                                } else {
                                    // No guard: branch directly to arm or fallthrough
                                    self.set_term(
                                        cursor,
                                        TerminatorIR::Branch {
                                            cond: cmp,
                                            then_bb: *arm_bb,
                                            else_bb: fallthrough,
                                        },
                                    );
                                }
                                cursor = fallthrough;
                            } else {
                                // Wildcard/default arm: jump directly
                                self.set_term(cursor, TerminatorIR::Jump { bb: *arm_bb });
                                cursor = *arm_bb;
                            }
                        }

                        // Lower each arm body and jump to merge
                        for (idx, (_lit, arm_bb)) in arm_blocks.iter().enumerate() {
                            self.cur_bb = *arm_bb;
                            match &arms[idx].body {
                                hir::HirMatchArmBody::Expr(be) => {
                                    let _ = self.lower_expr(be);
                                }
                                hir::HirMatchArmBody::Block(bb) => {
                                    self.lower_block(bb);
                                }
                            }
                            self.set_term(self.cur_bb, TerminatorIR::Jump { bb: merge_bb });
                        }
                        // Continue after match
                        self.cur_bb = merge_bb;
                        None
                    }

                    // Desugar a for-loop to while using opaque iterator helpers:
                    // iter_has_next(iter) -> bool, iter_next(iter) -> T
                    K::For {
                        pattern,
                        iter,
                        body,
                    } => {
                        // Evaluate iterator
                        let iter_v = self
                            .lower_expr(iter)
                            .unwrap_or_else(|| self.emit(self.cur_bb, InstIR::Nop));

                        // Blocks: cond -> loop -> after
                        let cond_bb = self.new_block();
                        let loop_bb = self.new_block();
                        let after_bb = self.new_block();
                        self.set_term(self.cur_bb, TerminatorIR::Jump { bb: cond_bb });

                        // Condition: has_next = iter_has_next(iter)
                        self.cur_bb = cond_bb;
                        let has = self.emit(
                            self.cur_bb,
                            InstIR::Call {
                                callee: "iter_has_next".to_string(),
                                args: vec![iter_v],
                            },
                        );
                        self.set_term(
                            self.cur_bb,
                            TerminatorIR::Branch {
                                cond: has,
                                then_bb: loop_bb,
                                else_bb: after_bb,
                            },
                        );

                        // Body: next_val = iter_next(iter); bind if pattern is variable
                        self.cur_bb = loop_bb;
                        let next_item = self.emit(
                            self.cur_bb,
                            InstIR::Call {
                                callee: "iter_next".to_string(),
                                args: vec![iter_v],
                            },
                        );
                        if let hir::HirPatternKind::Variable(name) = &*pattern.kind {
                            // Bind pattern name to next_item for body scope
                            self.locals.insert(name.clone(), next_item);
                            self.debug_names.insert(next_item, name.clone());
                        }
                        self.lower_block(body);
                        self.set_term(self.cur_bb, TerminatorIR::Jump { bb: cond_bb });

                        // After
                        self.cur_bb = after_bb;
                        None
                    }

                    _ => {
                        let _ = self.lower_expr(e);
                        None
                    }
                }
            }

            S::Assign { lhs, rhs } => {
                // If assigning to a local slot, emit a Store; otherwise just evaluate RHS
                if let hir::HirExprKind::Variable(var_name) = &*lhs.kind {
                    if let Some(lid) = self.local_slots.get(var_name).copied() {
                        if let Some(v) = self.lower_expr(rhs) {
                            let _ = self.emit(
                                self.cur_bb,
                                InstIR::Store {
                                    local: lid,
                                    value: v,
                                },
                            );
                            return None;
                        }
                    }
                }

                let _ = self.lower_expr(rhs);
                None
            }

            S::Return(opt) => {
                let v = opt.as_ref().and_then(|e| self.lower_expr(e));

                // Set terminator on current block now
                self.set_term(self.cur_bb, TerminatorIR::Return { value: v });

                Some(v)
            }

            S::Break(opt) => {
                // Evaluate expression for side effects, ignore value in this minimal IR
                if let Some(expr) = opt {
                    let _ = self.lower_expr(expr);
                }
                if let Some(top) = self.loops.last() {
                    self.set_term(self.cur_bb, TerminatorIR::Jump { bb: top.break_bb });
                }
                None
            }
            S::Continue => {
                if let Some(top) = self.loops.last() {
                    self.set_term(
                        self.cur_bb,
                        TerminatorIR::Jump {
                            bb: top.continue_bb,
                        },
                    );
                }
                None
            }
        }
    }
}

/// Entry point for IR lowering from post-normalize, post-monomorphized HIR.
/// This covers a minimal subset: literals, variables, binary ops (+,-,*,/),
/// calls (with simple variable callee), let/expr/return statements.
pub fn lower_from_hir(program: &hir::HirProgram) -> ProgramIR {
    let mut ir = ProgramIR::new();

    // Build a function -> return type map from HIR to guide expression type inference
    let mut function_ret_types: std::collections::HashMap<String, TypeIR> =
        std::collections::HashMap::new();
    for item in &program.items {
        if let hir::HirItemKind::Function(func) = &item.kind {
            function_ret_types.insert(func.name.clone(), map_hir_type_to_ir(&func.return_type));
        }
    }

    for item in &program.items {
        if let hir::HirItemKind::Function(func) = &item.kind {
            let name = func.name.clone();
            let params = func
                .params
                .iter()
                .map(|p| ParamIR {
                    name: p.name.clone(),
                    ty: map_hir_type_to_ir(&p.ty),
                })
                .collect::<Vec<_>>();
            let ret = map_hir_type_to_ir(&func.return_type);

            let mut f_ir = FunctionIR::new(name, params, ret);

            // Create LoweringCtx for this function
            let mut ctx = LoweringCtx::new(&function_ret_types);

            // Initialize context with function's entry block
            ctx.blocks = f_ir.blocks;
            ctx.cur_bb = f_ir.entry;

            // Seed parameter value IDs so they can be referenced
            for p in &func.params {
                let id = ValueId(ctx.next_val);
                ctx.next_val = ctx.next_val.saturating_add(1);
                ctx.debug_names.insert(id, p.name.clone());
                ctx.locals.insert(p.name.clone(), id);
            }

            // Lower body statements and optional tail expr
            ctx.lower_block(&func.body);

            // Finalize terminator only if the current block still has the default placeholder
            match ctx.blocks[ctx.cur_bb.0 as usize].term {
                TerminatorIR::Return { value: None } => {
                    // Default placeholder: replace with an explicit return (still None here)
                    ctx.set_term(ctx.cur_bb, TerminatorIR::Return { value: None });
                }
                _ => {
                    // Do not overwrite non-default terminators (explicit return, branch, or jump)
                }
            }

            // Update function IR with results from context
            f_ir.blocks = ctx.blocks;
            f_ir.locals = ctx.locals_meta;
            f_ir.debug_names = ctx.debug_names;

            ir.functions.push(f_ir);
        }
    }

    ir
}

// ===== Lowering (Temporary: AST → IR Skeleton) =====

/// Entry point for IR lowering (temporary path):
/// - In the target architecture, this will accept post-monomorphized HIR.
/// - For now, this scaffolds a ProgramIR from AST, creating one entry block per function
///   with an empty body and a `ret`.
pub fn lower_from_ast(program: &ast::ast::Program) -> ProgramIR {
    // Helper: emit an instruction into a block and produce a fresh ValueId
    fn emit(block: &mut BlockIR, next_val: &mut u32, inst: InstIR) -> ValueId {
        block.insts.push(inst);
        let id = ValueId(*next_val);
        *next_val = next_val.saturating_add(1);
        id
    }

    // Helper: lower an AST expression to IR, producing a ValueId when applicable
    fn lower_expr(
        expr: &ast::ast::Expr,
        block: &mut BlockIR,
        next_val: &mut u32,
        locals: &mut std::collections::HashMap<String, ValueId>,
    ) -> Option<ValueId> {
        match expr {
            ast::ast::Expr::Int(v, _info) => {
                Some(emit(block, next_val, InstIR::ConstInt { value: *v as i64 }))
            }
            ast::ast::Expr::Int64(v, _info) => {
                Some(emit(block, next_val, InstIR::ConstInt { value: *v }))
            }
            ast::ast::Expr::Bool(b, _info) => {
                let n = if *b { 1 } else { 0 };
                Some(emit(block, next_val, InstIR::ConstInt { value: n }))
            }
            ast::ast::Expr::Str(s, _info) => {
                Some(emit(block, next_val, InstIR::ConstStr { value: s.clone() }))
            }
            ast::ast::Expr::Var(name, _info) => locals.get(name).copied(),
            ast::ast::Expr::BinOp(l, op, r, _info) => {
                let lhs = lower_expr(l, block, next_val, locals)?;
                let rhs = lower_expr(r, block, next_val, locals)?;
                let op_s = format!("{:?}", op);
                let inst = if op_s == "Add" {
                    InstIR::Add { lhs, rhs }
                } else if op_s == "Sub" {
                    InstIR::Sub { lhs, rhs }
                } else if op_s == "Mul" {
                    InstIR::Mul { lhs, rhs }
                } else if op_s == "Div" {
                    InstIR::Div { lhs, rhs }
                } else {
                    InstIR::Nop
                };
                Some(emit(block, next_val, inst))
            }
            ast::ast::Expr::Call(name, args, _info) => {
                let mut argv = Vec::new();
                for a in args {
                    if let Some(v) = lower_expr(a, block, next_val, locals) {
                        argv.push(v);
                    }
                }
                Some(emit(
                    block,
                    next_val,
                    InstIR::Call {
                        callee: name.clone(),
                        args: argv,
                    },
                ))
            }
            _ => None,
        }
    }

    // Helper: lower a statement; returns Some(ret_val) when encountering `return`
    fn lower_stmt(
        stmt: &ast::ast::Stmt,
        block: &mut BlockIR,
        next_val: &mut u32,
        locals: &mut std::collections::HashMap<String, ValueId>,
        debug_names: &mut IndexMap<ValueId, String>,
    ) -> Option<Option<ValueId>> {
        match stmt {
            ast::ast::Stmt::Let(name, _ty, expr, _span, _vis) => {
                if let Some(v) = lower_expr(expr, block, next_val, locals) {
                    locals.insert(name.clone(), v);
                    debug_names.insert(v, name.clone());
                }
                None
            }
            ast::ast::Stmt::Expr(expr, _span) => {
                let _ = lower_expr(expr, block, next_val, locals);
                None
            }
            ast::ast::Stmt::Return(expr, _span) => {
                let v = lower_expr(expr, block, next_val, locals);
                Some(v)
            }
            _ => None,
        }
    }

    let mut ir = ProgramIR::new();

    // Deterministic: iterate functions and produce a stable order later on printing
    for func in &program.functions {
        let name = func.name.clone();
        let params = func
            .params
            .iter()
            .map(|(n, t)| ParamIR {
                name: n.clone(),
                ty: map_ast_type_to_ir(t),
            })
            .collect::<Vec<_>>();
        let ret = map_ast_type_to_ir(&func.return_type);

        let mut f_ir = FunctionIR::new(name, params, ret);
        let entry_block = f_ir.blocks.get_mut(0).expect("entry block exists");

        // Local lowering state (per function)
        let mut next_val: u32 = 0;
        let mut locals: std::collections::HashMap<String, ValueId> =
            std::collections::HashMap::new();

        // Seed parameter value IDs so they can be referenced as variables
        // (IR currently doesn't model parameter defs as instructions)
        for (pname, _pty) in &func.params {
            let id = ValueId(next_val);
            next_val = next_val.saturating_add(1);
            f_ir.debug_names.insert(id, pname.clone());
            locals.insert(pname.clone(), id);
        }

        // Lower statements; on return, set terminator
        let mut ret_captured: Option<Option<ValueId>> = None;
        for stmt in &func.body {
            if let Some(r) = lower_stmt(
                stmt,
                entry_block,
                &mut next_val,
                &mut locals,
                &mut f_ir.debug_names,
            ) {
                ret_captured = Some(r);
                break;
            }
        }

        if let Some(ret_val) = ret_captured {
            entry_block.term = TerminatorIR::Return { value: ret_val };
        }

        ir.functions.push(f_ir);
    }

    ir
}

fn map_ast_type_to_ir(t: &ast::ast::Type) -> TypeIR {
    use ast::ast::Type as A;
    match t {
        A::Void => TypeIR::Void,
        A::I32 => TypeIR::I32,
        A::I64 => TypeIR::I64,
        A::F32 => TypeIR::F32,
        A::F64 => TypeIR::F64,
        A::Bool => TypeIR::Bool,
        A::String => TypeIR::String,
        // Keep others opaque for now; this will be refined in real HIR→IR lowering
        other => TypeIR::Opaque(format!("{:?}", other)),
    }
}

fn map_hir_type_to_ir(t: &hir::HirType) -> TypeIR {
    use hir::HirType as H;
    match t {
        H::Void => TypeIR::Void,
        H::I32 => TypeIR::I32,
        H::I64 => TypeIR::I64,
        H::F32 => TypeIR::F32,
        H::F64 => TypeIR::F64,
        H::Bool => TypeIR::Bool,
        H::String => TypeIR::String,
        H::Pointer(inner) => TypeIR::Ptr(Box::new(map_hir_type_to_ir(inner))),
        // Keep others opaque to preserve determinism; refined after type layout decisions
        other => TypeIR::Opaque(format!("{:?}", other)),
    }
}

/// Infer IR type from HIR expression for local variable type inference

fn infer_type_from_hir_expr_with_context(
    expr: &hir::HirExpr,

    locals_meta: &[LocalIR],

    local_slots: &std::collections::HashMap<String, LocalId>,

    function_ret_types: &std::collections::HashMap<String, TypeIR>,
) -> TypeIR {
    use hir::HirExprKind as K;
    match &*expr.kind {
        K::Int(_) => TypeIR::I64,   // HIR Int is i64
        K::Float(_) => TypeIR::F64, // HIR Float is f64
        K::Bool(_) => TypeIR::Bool,
        K::String(_) => TypeIR::String,
        K::Char(_) => TypeIR::I32, // Treat char as i32 for now
        K::None => TypeIR::I32,    // Default for None
        K::Void => TypeIR::Void,
        K::Binary { .. } => TypeIR::I64, // Most binary ops result in i64
        K::Unary { .. } => TypeIR::I64,  // Most unary ops result in i64

        K::Call { func, .. } => {
            // Infer return type by consulting the HIR-derived function return-type map
            if let K::Variable(func_name) = &*func.kind {
                if let Some(ret_ty) = function_ret_types.get(func_name) {
                    ret_ty.clone()
                } else {
                    // Fall back to i32 when the callee is unknown or external
                    TypeIR::I32
                }
            } else {
                // Non-variable function expressions default to i32
                TypeIR::I32
            }
        }

        K::Variable(name) => {
            // Look up the variable type from already processed locals
            if let Some(&local_id) = local_slots.get(name) {
                if let Some(local_meta) = locals_meta.iter().find(|l| l.id == local_id) {
                    local_meta.ty.clone()
                } else {
                    TypeIR::I32 // Fallback if not found
                }
            } else {
                TypeIR::I32 // Default for unknown variables
            }
        }
        K::Pipeline { .. } => TypeIR::I32, // Pipeline results default to i32
        _ => TypeIR::I32,                  // Safe default for other expressions
    }
}

/// Infer the appropriate printf format specifier for an expression in a template string.

/// This uses local type information whenever available to avoid hardcoding function names.

fn infer_format_specifier_for_expr(
    expr: &hir::HirExpr,
    locals_meta: &[LocalIR],
    local_slots: &std::collections::HashMap<String, LocalId>,
    function_ret_types: &std::collections::HashMap<String, TypeIR>,
) -> String {
    let ty =
        infer_type_from_hir_expr_with_context(expr, locals_meta, local_slots, function_ret_types);

    match ty {
        TypeIR::String => "%s".to_string(),
        TypeIR::F32 | TypeIR::F64 => "%f".to_string(),
        TypeIR::Bool | TypeIR::I32 => "%d".to_string(),
        TypeIR::I64 => "%lld".to_string(),
        TypeIR::Ptr(_) | TypeIR::Opaque(_) => "%p".to_string(),
        TypeIR::Void => "%d".to_string(),
    }
}

// ===== Tests =====

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deterministic_function_print_order() {
        // Construct a small program with two functions out of order
        let mut p = ProgramIR::new();
        let f_b = FunctionIR::new(
            "zeta",
            vec![ParamIR {
                name: "x".into(),
                ty: TypeIR::I32,
            }],
            TypeIR::I32,
        );
        let f_a = FunctionIR::new("alpha", vec![], TypeIR::Void);
        p.functions.push(f_b);
        p.functions.push(f_a);

        let printed = p.to_pretty_string();
        let a_idx = printed.find("fn alpha(").expect("alpha present");
        let z_idx = printed.find("fn zeta(").expect("zeta present");
        assert!(
            a_idx < z_idx,
            "alpha must print before zeta for determinism"
        );
    }

    #[test]
    fn display_roundtrip_includes_blocks_and_ret() {
        let mut f = FunctionIR::new("main", vec![], TypeIR::I32);
        // Attach a simple instruction and a return value to verify formatting
        let bb0 = f.blocks.get_mut(0).unwrap();
        bb0.insts.push(InstIR::ConstInt { value: 42 });
        // Record the produced ValueId for the inst so printer uses the actual ValueId number
        bb0.results.push(ValueId(0));
        bb0.term = TerminatorIR::Return {
            value: Some(ValueId(0)),
        };

        let s = f.to_pretty_string();
        assert!(s.contains("fn main() -> i32 {"));
        assert!(s.contains("bb0:"));
        assert!(s.contains("%0 = const.i64 42"));
        assert!(s.contains("ret %0"));
        assert!(s.trim_end().ends_with('}'));
    }
}
