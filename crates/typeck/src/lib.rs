//! Type checker for the Veil programming language operating on HIR
//!
//! This crate provides type checking functionality that operates on the HIR
//! (High-level Intermediate Representation) rather than the AST. It integrates
//! with the resolver to use resolved symbol information for accurate type checking.

mod expr;
mod pattern;
mod stmt;

use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic as ReportingDiagnostic;
use std::collections::HashMap;
use veil_diagnostics::Diag;
use veil_hir::{HirFunction, HirItem, HirProgram, HirType, ModuleId, NodeId, SymbolId};
use veil_resolve::{Symbol, SymbolTable};

/// Type checking context that maintains type information and resolved symbols
#[derive(Debug)]
pub struct TypeCheckContext {
    /// Symbol table from resolution phase
    pub symbol_table: SymbolTable,
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
}

/// Type constraint for unification
#[derive(Debug, Clone)]
pub struct TypeConstraint {
    pub lhs: HirType,
    pub rhs: HirType,
    pub location: NodeId,
    pub reason: String,
}

impl TypeCheckContext {
    pub fn new(
        symbol_table: SymbolTable,
        file_id: FileId,
        current_module: Option<ModuleId>,
    ) -> Self {
        Self {
            symbol_table,
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

    /// Record the computed type for a node
    pub fn set_node_type(&mut self, node_id: NodeId, ty: HirType) {
        self.node_types.insert(node_id, ty);
    }

    /// Retrieve the computed type for a node, if any
    pub fn get_node_type(&self, node_id: NodeId) -> Option<&HirType> {
        self.node_types.get(&node_id)
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
            (Generic(left_name), Generic(right_name)) => left_name == right_name,
            (GenericInstance(left_name, left_args), GenericInstance(right_name, right_args)) => {
                left_name == right_name
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

impl TypeChecker {
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

    /// Type check a complete HIR program
    pub fn check_program(&mut self, program: &mut HirProgram) -> Result<(), Vec<Diag>> {
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
            Struct(_) => Ok(()),      // Structs are checked during resolution
            Enum(_) => Ok(()),        // Enums are checked during resolution
            Impl(_) => Ok(()),        // Impls will be handled separately
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

        // If return type is void and we're not explicitly typed, infer it
        if func.return_type == HirType::Void && !func.generic_params.is_empty() {
            self.context.inferring_return_type = true;
        }

        // Type check function body
        self.check_block(&mut func.body)?;

        // Check if inferred return type differs from declared
        if let Some(inferred) = &self.context.inferred_return_type {
            if !self.context.types_compatible(&func.return_type, inferred) {
                let diagnostic: Diag = ReportingDiagnostic::error().with_message(format!(
                    "Function declared return type {:?} but inferred {:?}",
                    func.return_type, inferred
                ));
                self.errors.push(diagnostic);
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
            last_type = self.check_expr(expr)?;
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

    /// Add an error to the error list
    pub fn error(&mut self, message: String) {
        let diagnostic: Diag = ReportingDiagnostic::error().with_message(message);
        self.errors.push(diagnostic);
    }

    /// Add a warning to the error list
    pub fn warning(&mut self, message: String) {
        let diagnostic: Diag = ReportingDiagnostic::warning().with_message(message);
        self.errors.push(diagnostic);
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
}
