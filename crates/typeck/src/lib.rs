//! Type checker for the Veil programming language operating on HIR
//!
//! This crate provides type checking functionality that operates on the HIR
//! (High-level Intermediate Representation) rather than the AST. It integrates
//! with the resolver to use resolved symbol information for accurate type checking.
//!
//! - Typed-HIR attachments (TypeIds) with deterministic snapshots
//! - Division diagnostics with fix-its for '/' vs '//' and '/=' vs '//='
//! - Postfix '?' typing and return-type validation
//! - Unions/intersections typing and basic match exhaustiveness warnings
//! - Trait reference validation for dyn trait types and impl trait_ref existence

mod expr;
mod pattern;
mod stmt;

use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic as ReportingDiagnostic;
use std::collections::{BTreeMap, HashMap};
use veil_diagnostics::{Diag, Span};
use veil_hir::{
    HirBlock, HirExpr, HirExprKind, HirFunction, HirImpl, HirItem, HirProgram, HirType, ModuleId,
    NodeId, SpanMap, SymbolId, TypeId,
};
use veil_resolve::{Symbol, SymbolKind, SymbolTable};

/// Type checking context that maintains type information and resolved symbols
#[derive(Debug)]
pub struct TypeCheckContext {
    /// Symbol table from resolution phase
    pub symbol_table: SymbolTable,
    /// Local variable types for current scope
    pub local_variables: HashMap<String, HirType>,
    /// Current function's return type
    pub current_return_type: Option<HirType>,
    /// Whether we're inside a loop (for break/continue)
    pub in_loop: bool,
    /// Whether we're inside a safe block
    pub in_safe: bool,
    /// Current module being type checked
    pub current_module: Option<ModuleId>,
    /// File being processed
    pub file_id: FileId,
    /// Break types for current loop stack
    pub break_types: Vec<HirType>,
    /// Whether we're inferring return type
    pub inferring_return_type: bool,
    /// Inferred return type (if any)
    pub inferred_return_type: Option<HirType>,
    /// Type unification constraints
    pub constraints: Vec<TypeConstraint>,
    /// Per-node type map for expressions/statements
    pub node_types: HashMap<NodeId, HirType>,
    /// Per-node interned type id map
    pub node_type_ids: HashMap<NodeId, TypeId>,
    /// Type interner for producing stable TypeIds
    pub type_interner: TypeInterner,
    /// Span map for diagnostics
    pub span_map: Option<SpanMap>,
}

/// Type constraint for unification
#[derive(Debug, Clone)]
pub struct TypeConstraint {
    pub lhs: HirType,
    pub rhs: HirType,
    pub location: NodeId,
    pub reason: String,
}

/// Simple, local type interner used by type checking to assign stable TypeIds.
#[derive(Debug, Default)]
pub struct TypeInterner {
    types: Vec<(TypeId, HirType)>,
    next: u32,
}

impl TypeInterner {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            next: 0,
        }
    }

    pub fn get_or_intern(&mut self, ty: &HirType) -> TypeId {
        if let Some((id, _)) = self.types.iter().find(|(_, t)| t == ty) {
            return *id;
        }
        let id = TypeId::new(self.next);
        self.next += 1;
        self.types.push((id, ty.clone()));
        id
    }

    pub fn get(&self, id: TypeId) -> Option<&HirType> {
        self.types
            .iter()
            .find_map(|(tid, t)| if *tid == id { Some(t) } else { None })
    }
}

impl TypeCheckContext {
    pub fn new(
        symbol_table: SymbolTable,
        file_id: FileId,
        current_module: Option<ModuleId>,
    ) -> Self {
        Self {
            symbol_table,
            local_variables: HashMap::new(),
            current_return_type: None,
            in_loop: false,
            in_safe: false,
            current_module,
            file_id,
            break_types: Vec::new(),
            inferring_return_type: false,
            inferred_return_type: None,
            constraints: Vec::new(),
            node_types: HashMap::new(),
            node_type_ids: HashMap::new(),
            type_interner: TypeInterner::new(),
            span_map: None,
        }
    }

    /// Get symbol by ID
    pub fn get_symbol(&self, symbol_id: SymbolId) -> Option<&Symbol> {
        self.symbol_table.get(symbol_id)
    }

    /// Get symbol by name in current scope
    pub fn get_symbol_by_name(&self, name: &str) -> Option<&Symbol> {
        // Use the SymbolTable API to find all candidates by name, then
        // prioritize those in the current module, followed by those
        // accessible from the current module (visibility), finally
        // fall back to the first candidate if ambiguous.
        let candidates = self.symbol_table.find_by_name(name);
        if candidates.is_empty() {
            return None;
        }
        if let Some(module_id) = self.current_module {
            if let Some(sym) = candidates.iter().find(|s| s.module_id == module_id) {
                return Some(*sym);
            }
            if let Some(sym) = candidates
                .iter()
                .find(|s| s.is_accessible_from(module_id, module_id))
            {
                return Some(*sym);
            }
        }
        Some(candidates[0])
    }

    /// Record the computed type for a node, and attach a TypeId
    pub fn set_node_type(&mut self, node_id: NodeId, ty: HirType) {
        let ty_id = self.type_interner.get_or_intern(&ty);
        self.node_types.insert(node_id, ty);
        self.node_type_ids.insert(node_id, ty_id);
    }

    /// Retrieve the computed type for a node, if any
    pub fn get_node_type(&self, node_id: NodeId) -> Option<&HirType> {
        self.node_types.get(&node_id)
    }

    /// Retrieve the interned TypeId for a node, if any
    pub fn get_node_type_id(&self, node_id: NodeId) -> Option<TypeId> {
        self.node_type_ids.get(&node_id).copied()
    }

    /// Lookup a HIR type by its TypeId
    pub fn get_type_by_id(&self, id: TypeId) -> Option<&HirType> {
        self.type_interner.get(id)
    }

    /// Get source span for a node id, if available
    pub fn span_of(&self, node_id: NodeId) -> Option<Span> {
        self.span_map.as_ref().and_then(|m| m.get(node_id))
    }

    /// Add a type constraint
    pub fn add_constraint(&mut self, lhs: HirType, rhs: HirType, location: NodeId, reason: String) {
        self.constraints.push(TypeConstraint {
            lhs,
            rhs,
            location,
            reason,
        });
    }

    /// Check if type is compatible with another
    pub fn types_compatible(&self, left: &HirType, right: &HirType) -> bool {
        use HirType::*;

        match (left, right) {
            // Exact matches
            (I8, I8) | (I16, I16) | (I32, I32) | (I64, I64) => true,
            (U8, U8) | (U16, U16) | (U32, U32) | (U64, U64) => true,
            (F32, F32) | (F64, F64) => true,
            (Bool, Bool) | (String, String) | (Char, Char) => true,
            (Void, Void) | (Never, Never) => true,

            // Array compatibility
            (Array(left_elem), Array(right_elem)) => self.types_compatible(left_elem, right_elem),

            // Tuple compatibility
            (Tuple(left_elems), Tuple(right_elems)) => {
                left_elems.len() == right_elems.len()
                    && left_elems
                        .iter()
                        .zip(right_elems.iter())
                        .all(|(l, r)| self.types_compatible(l, r))
            }

            // Optional compatibility
            (Optional(left_inner), Optional(right_inner)) => {
                self.types_compatible(left_inner, right_inner)
            }

            // Reference compatibility
            (Reference(left_inner, left_mut), Reference(right_inner, right_mut)) => {
                left_mut == right_mut && self.types_compatible(left_inner, right_inner)
            }

            // Function compatibility
            (Function(left_params, left_ret), Function(right_params, right_ret)) => {
                left_params.len() == right_params.len()
                    && left_params
                        .iter()
                        .zip(right_params.iter())
                        .all(|(l, r)| self.types_compatible(l, r))
                    && self.types_compatible(left_ret, right_ret)
            }

            // Struct/Enum compatibility by name (for now)
            (Struct(left_name), Struct(right_name)) => left_name == right_name,
            (Enum(left_name), Enum(right_name)) => left_name == right_name,

            // Generic compatibility
            (Generic(left_ref), Generic(right_ref)) => left_ref.name == right_ref.name,
            (GenericInstance(left_ref, left_args), GenericInstance(right_ref, right_args)) => {
                left_ref.name == right_ref.name
                    && left_args.len() == right_args.len()
                    && left_args
                        .iter()
                        .zip(right_args.iter())
                        .all(|(l, r)| self.types_compatible(l, r))
            }

            // Union types (at least one compatible)
            (Union(left_types), right) => {
                left_types.iter().any(|lt| self.types_compatible(lt, right))
            }
            (left, Union(right_types)) => {
                right_types.iter().any(|rt| self.types_compatible(left, rt))
            }

            // Intersection types (all must be compatible)
            (Intersection(left_types), right) => {
                left_types.iter().all(|lt| self.types_compatible(lt, right))
            }
            (left, Intersection(right_types)) => {
                right_types.iter().all(|rt| self.types_compatible(left, rt))
            }

            // Unknown type is compatible with anything (for error recovery)
            (Unknown, _) | (_, Unknown) => true,

            // Everything else is incompatible
            _ => false,
        }
    }
}

/// Main type checker for HIR programs
#[derive(Debug)]
pub struct TypeChecker {
    pub context: TypeCheckContext,
    pub errors: Vec<Diag>,
}

/// Typed snapshot of computed types for golden testing
#[derive(Debug, Clone)]
pub struct TypedSnapshot {
    pub node_types: BTreeMap<NodeId, TypeId>,
    pub types: BTreeMap<TypeId, HirType>,
}

impl TypedSnapshot {
    /// Deterministic formatting for golden snapshots
    pub fn to_pretty_string(&self) -> String {
        let mut s = String::new();
        s.push_str("TypedSnapshot\n");
        s.push_str("Types:\n");
        for (tid, ty) in &self.types {
            s.push_str(&format!("  {} = {:?}\n", tid, ty));
        }
        s.push_str("NodeTypes:\n");
        for (nid, tid) in &self.node_types {
            s.push_str(&format!("  {} -> {}\n", nid, tid));
        }
        s
    }
}

impl TypeChecker {
    /// Build a deterministic typed snapshot of NodeId->TypeId and the referenced types table
    pub fn typed_snapshot(&self) -> TypedSnapshot {
        let mut node_types: BTreeMap<NodeId, TypeId> = BTreeMap::new();
        for (nid, tid) in &self.context.node_type_ids {
            node_types.insert(*nid, *tid);
        }

        // Collect only the types referenced by nodes to keep snapshots minimal/stable
        let mut types: BTreeMap<TypeId, HirType> = BTreeMap::new();
        for tid in node_types.values() {
            if let Some(ty) = self.context.get_type_by_id(*tid).cloned() {
                types.entry(*tid).or_insert(ty);
            }
        }

        TypedSnapshot { node_types, types }
    }

    /// Convenience: format the typed snapshot for golden tests
    pub fn format_typed_snapshot(&self) -> String {
        self.typed_snapshot().to_pretty_string()
    }
    /// Create a new type checker with resolved symbols
    pub fn new(
        symbol_table: SymbolTable,
        file_id: FileId,
        current_module: Option<ModuleId>,
    ) -> Self {
        Self {
            context: TypeCheckContext::new(symbol_table, file_id, current_module),
            errors: Vec::new(),
        }
    }

    /// Expose the computed HIR type for a node, if recorded
    pub fn node_type(&self, node: NodeId) -> Option<&HirType> {
        self.context.get_node_type(node)
    }

    /// Expose the interned TypeId for a node, if recorded
    pub fn node_type_id(&self, node: NodeId) -> Option<TypeId> {
        self.context.get_node_type_id(node)
    }

    /// Lookup a HIR type by its TypeId, if present in the interner
    pub fn type_by_id(&self, id: TypeId) -> Option<&HirType> {
        self.context.get_type_by_id(id)
    }

    /// Type check a complete HIR program
    pub fn check_program(&mut self, program: &mut HirProgram) -> Result<(), Vec<Diag>> {
        // Wire span map for diagnostics
        self.context.span_map = Some(program.span_map.clone());
        // Type check all items
        for item in &mut program.items {
            self.check_item(item)?;
        }

        // Resolve type constraints
        self.resolve_constraints()?;

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    /// Type check a single HIR item
    pub fn check_item(&mut self, item: &mut HirItem) -> Result<(), Vec<Diag>> {
        use veil_hir::HirItemKind::*;

        match &mut item.kind {
            Function(func) => self.check_function(func),
            Struct(_) => Ok(()), // Structs are checked during resolution
            Enum(_) => Ok(()),   // Enums are checked during resolution
            Impl(impl_block) => self.check_impl(impl_block),
            Import(_) => Ok(()),      // Imports are handled during resolution
            FfiFunction(_) => Ok(()), // FFI functions are pre-checked
            FfiVariable(_) => Ok(()), // FFI variables are pre-checked
            Test(_) => Ok(()),        // Test items are not type-checked here
        }
    }

    /// Type check a function
    pub fn check_function(&mut self, func: &mut HirFunction) -> Result<(), Vec<Diag>> {
        // Set up function context
        let old_return_type = self.context.current_return_type.clone();
        self.context.current_return_type = Some(func.return_type.clone());

        // Validate trait usages in signature (dyn Trait in params/return)
        for p in &func.params {
            self.validate_type(&p.ty, p.id);
        }
        self.validate_type(&func.return_type, func.id);

        // If return type is void and we're not explicitly typed, infer it
        if func.return_type == HirType::Void && !func.generic_params.is_empty() {
            self.context.inferring_return_type = true;
        }

        // Type check function body
        self.check_block(&mut func.body)?;

        // Check if inferred return type differs from declared
        if let Some(inferred) = &self.context.inferred_return_type
            && !self.context.types_compatible(&func.return_type, inferred)
        {
            let diagnostic: Diag = ReportingDiagnostic::error().with_message(format!(
                "Function declared return type {:?} but inferred {:?}",
                func.return_type, inferred
            ));
            self.errors.push(diagnostic);
        }

        // Validate postfix '?' obligations (if any) against the function return type
        {
            let mut question_sites: Vec<NodeId> = Vec::new();
            self.collect_question_sites_in_block(&func.body, &mut question_sites);
            if !question_sites.is_empty() {
                for site in question_sites {
                    // Type recorded for a PostfixQuestion node is the unwrapped type
                    if let Some(unwrap_ty) = self.context.get_node_type(site).cloned() {
                        match &func.return_type {
                            HirType::Optional(ret_inner) => {
                                if !self.context.types_compatible(&unwrap_ty, ret_inner) {
                                    self.error_with_fix(
                                                site,
                                                Some("VE0020"),
                                                format!(
                                                    "postfix '?' unwrap type {:?} is incompatible with function return type {:?}",
                                                    unwrap_ty, func.return_type
                                                ),
                                                format!(
                                                    "Change return type to {:?}? or adjust the expression type",
                                                    unwrap_ty
                                                ),
                                            );
                                }
                            }
                            other => {
                                self.error_with_fix(
                                            site,
                                            Some("VE0021"),
                                            format!(
                                                "postfix '?' requires function to return an optional type, but function returns {:?}",
                                                other
                                            ),
                                            format!(
                                                "Change function return type to {:?}? to propagate None automatically",
                                                unwrap_ty
                                            ),
                                        );
                            }
                        }
                    }
                }
            }
        }

        // Restore context
        self.context.current_return_type = old_return_type;
        self.context.inferring_return_type = false;
        self.context.inferred_return_type = None;

        Ok(())
    }

    /// Type check a block
    pub fn check_block(&mut self, block: &mut veil_hir::HirBlock) -> Result<HirType, Vec<Diag>> {
        let mut last_type = HirType::Void;

        for stmt in &mut block.stmts {
            last_type = self.check_stmt(stmt)?;
        }

        if let Some(expr) = &mut block.expr {
            let ty = self.check_expr(expr)?;
            // Record the computed type and attach a TypeId for the tail expression
            self.context.set_node_type(expr.id, ty.clone());
            last_type = ty;
        }

        Ok(last_type)
    }

    /// Resolve all type constraints
    fn resolve_constraints(&mut self) -> Result<(), Vec<Diag>> {
        // Simple constraint resolution - in a full implementation,
        // this would use unification algorithms
        for constraint in &self.context.constraints {
            if !self
                .context
                .types_compatible(&constraint.lhs, &constraint.rhs)
            {
                let diagnostic: Diag = ReportingDiagnostic::error().with_message(format!(
                    "Type mismatch: {} (expected: {:?}, found: {:?})",
                    constraint.reason, constraint.lhs, constraint.rhs
                ));
                self.errors.push(diagnostic);
            }
        }

        Ok(())
    }

    /// Add an error to the error list at a specific node (with span if available)
    pub fn error_at(&mut self, node: NodeId, message: impl Into<String>) {
        if let Some(span) = self.context.span_of(node) {
            let d = veil_diagnostics::error(message.into(), self.context.file_id, span);
            self.errors.push(d);
        } else {
            self.error(message.into());
        }
    }

    /// Add a warning to the error list at a specific node (with span if available)
    pub fn warning_at(&mut self, node: NodeId, message: impl Into<String>) {
        if let Some(span) = self.context.span_of(node) {
            let d = veil_diagnostics::warning(message.into(), self.context.file_id, span);
            self.errors.push(d);
        } else {
            self.warning(message.into());
        }
    }

    /// Add an error with a fix-it note (help) attached
    pub fn error_with_fix(
        &mut self,
        node: NodeId,
        code: Option<&str>,
        message: impl Into<String>,
        fix: impl Into<String>,
    ) {
        if let Some(span) = self.context.span_of(node) {
            let mut d = if let Some(c) = code {
                veil_diagnostics::error_with(c, message.into(), self.context.file_id, span)
            } else {
                veil_diagnostics::error(message.into(), self.context.file_id, span)
            };
            d = d.with_notes(vec![veil_diagnostics::help(fix.into())]);
            self.errors.push(d);
        } else {
            self.error(message.into());
        }
    }

    /// Fallback: Add an error without span
    pub fn error(&mut self, message: String) {
        let diagnostic: Diag = ReportingDiagnostic::error().with_message(message);
        self.errors.push(diagnostic);
    }

    /// Fallback: Add a warning without span
    /// Add a warning to the error list
    pub fn warning(&mut self, message: String) {
        let diagnostic: Diag = ReportingDiagnostic::warning().with_message(message);
        self.errors.push(diagnostic);
    }

    /// Validate a HIR type for trait-related rules (dyn Trait)
    fn validate_type(&mut self, ty: &HirType, at_node: NodeId) {
        match ty {
            HirType::DynTrait(trait_ref) => {
                // VE0100: dyn trait references must resolve to a trait symbol
                let is_trait = self
                    .context
                    .symbol_table
                    .find_by_name_and_kind(&trait_ref.name, SymbolKind::Trait)
                    .is_some();
                if !is_trait {
                    self.error_with_fix(
                        at_node,
                        Some("VE0100"),
                        format!("unknown trait '{}'", trait_ref.name),
                        "Ensure the trait is declared and imported, or fix the name",
                    );
                }
            }
            HirType::Optional(inner)
            | HirType::Pointer(inner)
            | HirType::Reference(inner, _)
            | HirType::Array(inner)
            | HirType::SizedArray(inner, _) => self.validate_type(inner, at_node),
            HirType::Tuple(elems) => {
                for e in elems {
                    self.validate_type(e, at_node);
                }
            }
            HirType::Function(params, ret) => {
                for p in params {
                    self.validate_type(p, at_node);
                }
                self.validate_type(ret, at_node);
            }
            HirType::Union(ts) | HirType::Intersection(ts) => {
                for t in ts {
                    self.validate_type(t, at_node);
                }
            }
            _ => {}
        }
    }

    /// Type-check impl blocks: validate trait_ref exists and method duplicates
    fn check_impl(&mut self, imp: &mut HirImpl) -> Result<(), Vec<Diag>> {
        // Validate trait_ref existence if provided
        if let Some(trait_type) = &imp.trait_ref {
            if let HirType::Unresolved(trait_name) = trait_type {
                let is_trait = self
                    .context
                    .symbol_table
                    .find_by_name_and_kind(trait_name, SymbolKind::Trait)
                    .is_some();
                if !is_trait {
                    // VE0101: unresolved trait in impl
                    self.error_with_fix(
                        imp.id,
                        Some("VE0101"),
                        format!("unknown trait '{:?}' in impl", trait_name),
                        "Declare/import the trait or correct the name",
                    );
                }
            }
        }

        // Check duplicate methods in impl
        use std::collections::HashSet;
        let mut seen: HashSet<&str> = HashSet::new();
        for m in &imp.methods {
            if !seen.insert(m.name.as_str()) {
                // Duplicate method name in impl block
                self.error_at(m.id, format!("duplicate method '{}' in impl", m.name));
            }
        }

        // Type check methods
        for m in &mut imp.methods {
            let _ = self.check_function(m);
        }

        Ok(())
    }

    /// Shared integer-type predicate usable by expr/stmt checkers
    pub(crate) fn is_integer_type(&self, ty: &HirType) -> bool {
        matches!(
            ty,
            HirType::I8
                | HirType::I16
                | HirType::I32
                | HirType::I64
                | HirType::U8
                | HirType::U16
                | HirType::U32
                | HirType::U64
        )
    }
}

// Helper traversal to collect all PostfixQuestion sites in a block/expr tree.
impl TypeChecker {
    fn collect_question_sites_in_block(&mut self, block: &HirBlock, out: &mut Vec<NodeId>) {
        for stmt in &block.stmts {
            match &stmt.kind {
                veil_hir::HirStmtKind::Expr(expr) => self.collect_question_sites_in_expr(expr, out),
                veil_hir::HirStmtKind::Let { init, .. } => {
                    if let Some(e) = init {
                        self.collect_question_sites_in_expr(e, out);
                    }
                }
                veil_hir::HirStmtKind::Assign { lhs, rhs } => {
                    self.collect_question_sites_in_expr(lhs, out);
                    self.collect_question_sites_in_expr(rhs, out);
                }
                veil_hir::HirStmtKind::Return(expr_opt)
                | veil_hir::HirStmtKind::Break(expr_opt) => {
                    if let Some(e) = expr_opt {
                        self.collect_question_sites_in_expr(e, out);
                    }
                }
                veil_hir::HirStmtKind::Continue => {}
            }
        }
        if let Some(expr) = &block.expr {
            self.collect_question_sites_in_expr(expr, out);
        }
    }

    fn collect_question_sites_in_expr(&mut self, expr: &HirExpr, out: &mut Vec<NodeId>) {
        use HirExprKind::*;
        match &*expr.kind {
            PostfixQuestion(inner) => {
                // Record the site, then continue traversal into inner to catch nested uses
                out.push(expr.id);
                self.collect_question_sites_in_expr(inner, out);
            }

            // Literals and simple variables: nothing to traverse
            Int(_) | Float(_) | Bool(_) | String(_) | Char(_) | None | Variable(_) => {}

            FieldAccess { base, .. } => self.collect_question_sites_in_expr(base, out),

            Index { base, index } => {
                self.collect_question_sites_in_expr(base, out);
                self.collect_question_sites_in_expr(index, out);
            }

            Call { func, args } => {
                self.collect_question_sites_in_expr(func, out);
                for a in args {
                    self.collect_question_sites_in_expr(a, out);
                }
            }

            MethodCall { receiver, args, .. } => {
                self.collect_question_sites_in_expr(receiver, out);
                for a in args {
                    self.collect_question_sites_in_expr(a, out);
                }
            }

            Unary { expr: inner, .. } => self.collect_question_sites_in_expr(inner, out),

            Binary { lhs, rhs, .. } => {
                self.collect_question_sites_in_expr(lhs, out);
                self.collect_question_sites_in_expr(rhs, out);
            }

            If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.collect_question_sites_in_expr(condition, out);
                self.collect_question_sites_in_block(then_branch, out);
                if let Some(else_b) = else_branch {
                    self.collect_question_sites_in_block(else_b, out);
                }
            }

            Match { expr: mexpr, arms } => {
                self.collect_question_sites_in_expr(mexpr, out);
                for arm in arms {
                    if let Some(g) = &arm.guard {
                        self.collect_question_sites_in_expr(g, out);
                    }
                    match &arm.body {
                        veil_hir::HirMatchArmBody::Expr(e) => {
                            self.collect_question_sites_in_expr(e, out)
                        }
                        veil_hir::HirMatchArmBody::Block(b) => {
                            self.collect_question_sites_in_block(b, out)
                        }
                    }
                }
            }

            Loop { body } | Spawn(body) | UnsafeBlock(body) | Block(body) => {
                self.collect_question_sites_in_block(body, out);
            }

            While { condition, body } => {
                self.collect_question_sites_in_expr(condition, out);
                self.collect_question_sites_in_block(body, out);
            }

            For {
                pattern: _,
                iter,
                body,
            } => {
                self.collect_question_sites_in_expr(iter, out);
                self.collect_question_sites_in_block(body, out);
            }

            Ref(inner) | Deref(inner) | Await(inner) => {
                self.collect_question_sites_in_expr(inner, out);
            }

            Cast { expr: inner, .. } => self.collect_question_sites_in_expr(inner, out),

            StructLiteral { fields, .. } => {
                for (_n, e) in fields {
                    self.collect_question_sites_in_expr(e, out);
                }
            }

            ArrayLiteral(elems) | TupleLiteral(elems) => {
                for e in elems {
                    self.collect_question_sites_in_expr(e, out);
                }
            }

            Pipeline { expr: piped, func } => {
                self.collect_question_sites_in_expr(piped, out);
                self.collect_question_sites_in_expr(func, out);
            }

            PostfixIncrement(inner) | PostfixDecrement(inner) => {
                self.collect_question_sites_in_expr(inner, out);
            }

            Template { parts } => {
                for p in parts {
                    if let veil_hir::HirTemplateStringPart::Expr(e) = p {
                        self.collect_question_sites_in_expr(e, out);
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_type_compatibility() {
        let symbol_table = SymbolTable::new();
        let mut _files = veil_diagnostics::Files::<String>::new();
        let fid = _files.add("test.veil".to_string(), String::new());
        let context = TypeCheckContext::new(symbol_table, fid, None);

        // Basic type compatibility
        assert!(context.types_compatible(&HirType::I32, &HirType::I32));
        assert!(!context.types_compatible(&HirType::I32, &HirType::String));

        // Array compatibility
        assert!(context.types_compatible(
            &HirType::Array(Box::new(HirType::I32)),
            &HirType::Array(Box::new(HirType::I32))
        ));
        assert!(!context.types_compatible(
            &HirType::Array(Box::new(HirType::I32)),
            &HirType::Array(Box::new(HirType::String))
        ));
    }

    #[test]
    fn test_constraint_system() {
        let symbol_table = SymbolTable::new();
        let mut _files = veil_diagnostics::Files::<String>::new();
        let fid = _files.add("test.veil".to_string(), String::new());
        let mut context = TypeCheckContext::new(symbol_table, fid, None);

        context.add_constraint(
            HirType::I32,
            HirType::String,
            NodeId::new(1),
            "test constraint".to_string(),
        );

        assert_eq!(context.constraints.len(), 1);
        assert_eq!(context.constraints[0].lhs, HirType::I32);
        assert_eq!(context.constraints[0].rhs, HirType::String);
    }

    #[test]
    fn test_type_interning_dedup() {
        // Same logical type should map to same TypeId
        let mut interner = TypeInterner::new();
        let id1 = interner.get_or_intern(&HirType::I32);
        let id2 = interner.get_or_intern(&HirType::I32);
        let id3 = interner.get_or_intern(&HirType::F64);

        assert_eq!(id1, id2);
        assert_ne!(id1, id3);
        assert_eq!(interner.get(id1), Some(&HirType::I32));
        assert_eq!(interner.get(id3), Some(&HirType::F64));
    }

    #[test]
    fn test_node_type_id_storage_and_lookup() {
        let symbol_table = SymbolTable::new();
        let mut files = veil_diagnostics::Files::<String>::new();
        let fid = files.add("test.veil".to_string(), String::new());

        let mut ctx = TypeCheckContext::new(symbol_table, fid, None);
        let n = NodeId::new(7);

        // Store a node type and ensure both type and TypeId are recorded
        ctx.set_node_type(n, HirType::Bool);

        assert_eq!(ctx.get_node_type(n), Some(&HirType::Bool));
        let tid = ctx.get_node_type_id(n).expect("expected a TypeId for node");
        assert_eq!(ctx.get_type_by_id(tid), Some(&HirType::Bool));
    }

    #[test]
    fn test_typed_snapshot_basic() {
        let symbol_table = SymbolTable::new();
        let mut files = veil_diagnostics::Files::<String>::new();
        let fid = files.add("snapshot.veil".to_string(), String::new());
        let mut checker = TypeChecker::new(symbol_table, fid, None);

        // Record two nodes with the same logical type to test interning deduplication
        checker.context.set_node_type(NodeId::new(1), HirType::I32);
        checker.context.set_node_type(NodeId::new(2), HirType::I32);

        let snap = checker.typed_snapshot();
        assert_eq!(
            snap.types.len(),
            1,
            "Expected deduped single type in interner"
        );
        assert_eq!(snap.node_types.len(), 2, "Expected two node->type entries");

        let formatted = snap.to_pretty_string();
        assert!(formatted.contains("TypedSnapshot"));
        assert!(formatted.contains("Types:"));
        assert!(formatted.contains("I32"));
        assert!(formatted.contains("NodeTypes:"));
        // Don't rely on specific TypeId numbering, just ensure node mappings are present
        assert!(formatted.contains("node_1 -> "));
        assert!(formatted.contains("node_2 -> "));
    }

    #[test]
    fn test_division_fixit_code_and_note_for_integers() {
        let symbol_table = SymbolTable::new();
        let mut files = veil_diagnostics::Files::<String>::new();
        let fid = files.add("divfix.veil".to_string(), String::new());
        let mut checker = TypeChecker::new(symbol_table, fid, None);

        // a: i32 / b: i32  -> should trigger VE0010 with a fix-it to use '//'
        let mut expr = veil_hir::HirExpr {
            id: NodeId::new(10),
            kind: Box::new(veil_hir::HirExprKind::Binary {
                op: veil_hir::HirBinaryOp::Div,
                lhs: Box::new(veil_hir::HirExpr {
                    id: NodeId::new(11),
                    kind: Box::new(veil_hir::HirExprKind::Int(1)),
                }),
                rhs: Box::new(veil_hir::HirExpr {
                    id: NodeId::new(12),
                    kind: Box::new(veil_hir::HirExprKind::Int(2)),
                }),
            }),
        };

        // Provide a span for the division node so the diagnostic carries a code and fix-it note
        let mut span_map = SpanMap::new();
        span_map.insert(NodeId::new(10), Span::new(0, 1));
        checker.context.span_map = Some(span_map);

        let _ = checker.check_expr(&mut expr);
        assert!(
            checker
                .errors
                .iter()
                .any(|d| d.code.as_deref() == Some("VE0010")),
            "expected VE0010 diagnostic for integer '/' recommending '//' fix-it"
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
    fn test_idiv_integers_type_ok() {
        let symbol_table = SymbolTable::new();
        let mut files = veil_diagnostics::Files::<String>::new();
        let fid = files.add("idiv_ints.veil".to_string(), String::new());
        let mut checker = TypeChecker::new(symbol_table, fid, None);

        // 6 // 3 -> Ok(i32) with no diagnostics
        let mut expr = veil_hir::HirExpr {
            id: NodeId::new(30),
            kind: Box::new(veil_hir::HirExprKind::Binary {
                op: veil_hir::HirBinaryOp::IDiv,
                lhs: Box::new(veil_hir::HirExpr {
                    id: NodeId::new(31),
                    kind: Box::new(veil_hir::HirExprKind::Int(6)),
                }),
                rhs: Box::new(veil_hir::HirExpr {
                    id: NodeId::new(32),
                    kind: Box::new(veil_hir::HirExprKind::Int(3)),
                }),
            }),
        };

        // Span is optional here; no error expected
        let ty = checker.check_expr(&mut expr).expect("type check ok");
        assert_eq!(
            ty,
            HirType::I32,
            "expected i32 type for integer '//' division"
        );
        assert!(
            checker.errors.is_empty(),
            "no diagnostics expected for integer '//' division"
        );
    }

    #[test]
    fn test_idiv_floats_emit_ve0014() {
        let symbol_table = SymbolTable::new();
        let mut files = veil_diagnostics::Files::<String>::new();
        let fid = files.add("idiv_floats.veil".to_string(), String::new());
        let mut checker = TypeChecker::new(symbol_table, fid, None);

        // 1.0 // 2.0 -> VE0014: use '/'
        let mut expr = veil_hir::HirExpr {
            id: NodeId::new(40),
            kind: Box::new(veil_hir::HirExprKind::Binary {
                op: veil_hir::HirBinaryOp::IDiv,
                lhs: Box::new(veil_hir::HirExpr {
                    id: NodeId::new(41),
                    kind: Box::new(veil_hir::HirExprKind::Float(1.0)),
                }),
                rhs: Box::new(veil_hir::HirExpr {
                    id: NodeId::new(42),
                    kind: Box::new(veil_hir::HirExprKind::Float(2.0)),
                }),
            }),
        };

        // Provide a span for actionable diagnostic
        let mut span_map = SpanMap::new();
        span_map.insert(NodeId::new(40), Span::new(0, 1));
        checker.context.span_map = Some(span_map);

        let _ = checker.check_expr(&mut expr);
        assert!(
            checker
                .errors
                .iter()
                .any(|d| d.code.as_deref() == Some("VE0014")),
            "expected VE0014 for '//' with float operands suggesting use of '/'"
        );
    }

    #[test]
    fn test_idiv_mixed_types_emit_ve0015() {
        let symbol_table = SymbolTable::new();
        let mut files = veil_diagnostics::Files::<String>::new();
        let fid = files.add("idiv_mixed.veil".to_string(), String::new());
        let mut checker = TypeChecker::new(symbol_table, fid, None);

        // 1 // 2.0 -> VE0015: both operands must be integers or use '/'
        let mut expr = veil_hir::HirExpr {
            id: NodeId::new(50),
            kind: Box::new(veil_hir::HirExprKind::Binary {
                op: veil_hir::HirBinaryOp::IDiv,
                lhs: Box::new(veil_hir::HirExpr {
                    id: NodeId::new(51),
                    kind: Box::new(veil_hir::HirExprKind::Int(1)),
                }),
                rhs: Box::new(veil_hir::HirExpr {
                    id: NodeId::new(52),
                    kind: Box::new(veil_hir::HirExprKind::Float(2.0)),
                }),
            }),
        };

        // Provide span for actionable diagnostic
        let mut span_map = SpanMap::new();
        span_map.insert(NodeId::new(50), Span::new(0, 1));
        checker.context.span_map = Some(span_map);

        let _ = checker.check_expr(&mut expr);
        assert!(
            checker
                .errors
                .iter()
                .any(|d| d.code.as_deref() == Some("VE0015")),
            "expected VE0015 for '//' with mixed int/float operands"
        );
    }

    #[test]
    fn test_postfix_question_requires_optional_return_type() {
        // Symbol table with variable `x: i32?`
        let mut symbol_table = SymbolTable::new();
        let sym_id = symbol_table.next_symbol_id();
        let symbol = veil_resolve::Symbol::new(
            sym_id,
            "x".to_string(),
            veil_resolve::SymbolKind::Variable,
            ModuleId::new(0),
            NodeId::new(0),
        )
        .with_type(HirType::Optional(Box::new(HirType::I32)));
        symbol_table.insert(symbol);

        let mut files = veil_diagnostics::Files::<String>::new();
        let fid = files.add("qmark.veil".to_string(), String::new());
        let mut checker = TypeChecker::new(symbol_table, fid, None);

        // Function returns i32, but body uses `x?` which propagates None => require i32?
        let body = veil_hir::HirBlock {
            id: NodeId::new(20),
            stmts: vec![],
            expr: Some(Box::new(veil_hir::HirExpr {
                id: NodeId::new(22),
                kind: Box::new(veil_hir::HirExprKind::PostfixQuestion(Box::new(
                    veil_hir::HirExpr {
                        id: NodeId::new(21),
                        kind: Box::new(veil_hir::HirExprKind::Variable("x".to_string())),
                    },
                ))),
            })),
        };

        let mut func = veil_hir::HirFunction {
            id: NodeId::new(30),
            name: "f".to_string(),
            symbol_id: None,
            generic_params: vec![],
            params: vec![],
            return_type: HirType::I32, // Non-optional
            body,
        };

        // Provide span for the `?` site to ensure diagnostic carries code and fix-it
        let mut span_map = SpanMap::new();
        span_map.insert(NodeId::new(22), Span::new(0, 1));
        checker.context.span_map = Some(span_map);

        let res = checker.check_function(&mut func);
        assert!(res.is_ok());

        assert!(
            checker
                .errors
                .iter()
                .any(|d| d.code.as_deref() == Some("VE0021")),
            "expected VE0021: postfix '?' requires optional function return type"
        );
    }

    #[test]
    fn test_dyn_trait_unknown_yields_ve0100() {
        let symbol_table = SymbolTable::new();
        let mut files = veil_diagnostics::Files::<String>::new();
        let fid = files.add("dyn_trait.veil".to_string(), String::new());
        let mut checker = TypeChecker::new(symbol_table, fid, None);

        // fn f(x: dyn Foo) -> void {}
        let mut func = veil_hir::HirFunction {
            id: NodeId::new(100),
            name: "f".to_string(),
            symbol_id: None,
            generic_params: vec![],
            params: vec![veil_hir::HirParam {
                id: NodeId::new(101),
                name: "x".to_string(),
                symbol_id: None,
                ty: HirType::DynTrait(veil_hir::HirTraitRef::new("Foo".to_string())),
            }],
            return_type: HirType::Void,
            body: veil_hir::HirBlock {
                id: NodeId::new(102),
                stmts: vec![],
                expr: None,
            },
        };

        let mut span_map = SpanMap::new();
        span_map.insert(NodeId::new(101), Span::new(0, 1));
        checker.context.span_map = Some(span_map);

        let _ = checker.check_function(&mut func);

        assert!(
            checker
                .errors
                .iter()
                .any(|d| d.code.as_deref() == Some("VE0100")),
            "expected VE0100 for unknown dyn trait"
        );
    }

    #[test]
    fn test_impl_unknown_trait_yields_ve0101() {
        let symbol_table = SymbolTable::new();
        let mut files = veil_diagnostics::Files::<String>::new();
        let fid = files.add("impl_trait.veil".to_string(), String::new());
        let mut checker = TypeChecker::new(symbol_table, fid, None);

        let mut imp = veil_hir::HirImpl {
            id: NodeId::new(200),
            target_type: HirType::Struct("S".to_string()),
            trait_ref: Some(HirType::Unresolved("Foo".to_string())),
            trait_symbol_id: None,
            methods: vec![],
        };

        let mut span_map = SpanMap::new();
        span_map.insert(NodeId::new(200), Span::new(0, 1));
        checker.context.span_map = Some(span_map);

        let _ = checker.check_impl(&mut imp);

        assert!(
            checker
                .errors
                .iter()
                .any(|d| d.code.as_deref() == Some("VE0101")),
            "expected VE0101 for unknown trait in impl"
        );
    }

    #[test]
    fn test_impl_duplicate_methods_reported() {
        let symbol_table = SymbolTable::new();
        let mut files = veil_diagnostics::Files::<String>::new();
        let fid = files.add("impl_dupe.veil".to_string(), String::new());
        let mut checker = TypeChecker::new(symbol_table, fid, None);

        let m1 = veil_hir::HirFunction {
            id: NodeId::new(300),
            name: "m".to_string(),
            symbol_id: None,
            generic_params: vec![],
            params: vec![],
            return_type: HirType::Void,
            body: veil_hir::HirBlock {
                id: NodeId::new(301),
                stmts: vec![],
                expr: None,
            },
        };
        let m2 = veil_hir::HirFunction {
            id: NodeId::new(302),
            name: "m".to_string(),
            symbol_id: None,
            generic_params: vec![],
            params: vec![],
            return_type: HirType::Void,
            body: veil_hir::HirBlock {
                id: NodeId::new(303),
                stmts: vec![],
                expr: None,
            },
        };

        let mut imp = veil_hir::HirImpl {
            id: NodeId::new(304),
            target_type: HirType::Struct("S".to_string()),
            trait_ref: None,
            trait_symbol_id: None,
            methods: vec![m1, m2],
        };

        let _ = checker.check_impl(&mut imp);
        assert!(
            !checker.errors.is_empty(),
            "expected duplicate method diagnostic"
        );
    }
}
