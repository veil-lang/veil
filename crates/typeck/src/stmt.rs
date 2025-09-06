//! Statement type checking for HIR
//!
//! This module implements type checking for HIR statements, ensuring that
//! variable declarations, assignments, and control flow are well-typed.

use crate::TypeChecker;
use veil_diagnostics::Diag;
use veil_hir::{HirStmt, HirStmtKind, HirType};

impl TypeChecker {
    /// Type check a statement and return the type it produces (if any)
    pub fn check_stmt(&mut self, stmt: &mut HirStmt) -> Result<HirType, Vec<Diag>> {
        match &mut stmt.kind {
            HirStmtKind::Expr(expr) => {
                // Expression statements evaluate to their expression type
                self.check_expr(expr)
            }

            HirStmtKind::Const { name, ty, init } => {
                // Type check the initializer
                let init_type = self.check_expr(init)?;

                // Check type annotation if present
                if let Some(annotation) = ty
                    && !self.strict_type_match(annotation, &init_type)
                {
                    self.error(format!(
                        "Type annotation {:?} does not match initializer type {:?}",
                        annotation, init_type
                    ));
                }

                // Determine the constant's type
                let const_type = ty.clone().unwrap_or(init_type);

                // Store constant type and mutability (always immutable) in local context for future lookups
                self.context
                    .define_local_variable(name.clone(), const_type.clone(), false);

                Ok(const_type)
            }

            HirStmtKind::Var {
                name,
                ty,
                init,
                is_mutable,
            } => {
                // Type check the initializer
                let init_type = self.check_expr(init)?;

                // Check type annotation if present
                if let Some(annotation) = ty
                    && !self.strict_type_match(annotation, &init_type)
                {
                    self.error(format!(
                        "Type annotation {:?} does not match initializer type {:?}",
                        annotation, init_type
                    ));
                }

                // Determine the variable's type
                let var_type = ty.clone().unwrap_or(init_type);

                // Store variable type and mutability in local context for future lookups
                self.context
                    .define_local_variable(name.clone(), var_type.clone(), *is_mutable);

                Ok(var_type)
            }

            HirStmtKind::Assign { lhs, rhs } => {
                let target_type = self.check_expr(lhs)?;
                let value_type = self.check_expr(rhs)?;

                // Check mutability: assignment target must be a mutable variable
                if let veil_hir::HirExprKind::Variable(var_name) = &*lhs.kind {
                    match self.context.is_variable_mutable(var_name) {
                        Some(true) => {
                            // Variable is mutable, assignment is allowed
                        }
                        Some(false) => {
                            self.error(format!(
                                "Cannot assign to immutable variable '{}'",
                                var_name
                            ));
                        }
                        None => {
                            self.error(format!(
                                "Cannot assign to undefined variable '{}'",
                                var_name
                            ));
                        }
                    }
                } else {
                    // For now, only support assignment to simple variables
                    // TODO: Add support for field assignments, array indexing, etc.
                    self.error("Assignment target must be a variable".to_string());
                }

                // Record types for both sides to attach TypeIds in the Typed-HIR map
                self.context.set_node_type(lhs.id, target_type.clone());
                self.context.set_node_type(rhs.id, value_type.clone());

                if !self.context.types_compatible(&target_type, &value_type) {
                    self.error(format!(
                        "Cannot assign {:?} to {:?}",
                        value_type, target_type
                    ));
                }

                Ok(HirType::Void) // Assignments don't produce values
            }

            HirStmtKind::Return(return_expr) => {
                if let Some(expr) = return_expr {
                    let return_type = self.check_expr(expr)?;

                    // Check against expected return type
                    if let Some(expected_return) = &self.context.current_return_type {
                        if !self.context.types_compatible(expected_return, &return_type) {
                            self.error(format!(
                                "Return type mismatch: expected {:?}, found {:?}",
                                expected_return, return_type
                            ));
                        }
                    } else if self.context.inferring_return_type {
                        // Infer return type from this return statement
                        self.context.inferred_return_type = Some(return_type);
                    }
                } else {
                    // Empty return - check if function expects void
                    if let Some(expected_return) = &self.context.current_return_type
                        && *expected_return != HirType::Void
                    {
                        self.error(format!(
                            "Empty return in function expecting {:?}",
                            expected_return
                        ));
                    }
                }

                Ok(HirType::Never) // Return statements don't produce values in their context
            }

            HirStmtKind::Break(break_expr) => {
                if !self.context.in_loop {
                    self.error("Break statement outside of loop".to_string());
                }

                if let Some(expr) = break_expr {
                    let break_type = self.check_expr(expr)?;
                    self.context.break_types.push(break_type);
                }

                Ok(HirType::Never) // Break statements don't produce values in their context
            }

            HirStmtKind::Continue => {
                if !self.context.in_loop {
                    self.error("Continue statement outside of loop".to_string());
                }

                Ok(HirType::Never) // Continue statements don't produce values
            }
        }
    }

    /// Check that a pattern is compatible with a given type
    pub(crate) fn check_pattern_against_type(
        &mut self,
        pattern: &veil_hir::HirPattern,
        expected_type: &HirType,
    ) -> Result<(), Vec<Diag>> {
        use veil_hir::{HirExprKind, HirPatternKind};

        // Helper: does the pattern shape match the given type (union/intersection aware)
        fn matches_shape(p: &veil_hir::HirPattern, t: &HirType) -> bool {
            match &*p.kind {
                HirPatternKind::Wildcard | HirPatternKind::Variable(_) => true,
                HirPatternKind::Literal(expr) => {
                    match &*expr.kind {
                        HirExprKind::Int(_) => matches!(
                            t,
                            HirType::I8
                                | HirType::I16
                                | HirType::I32
                                | HirType::I64
                                | HirType::U8
                                | HirType::U16
                                | HirType::U32
                                | HirType::U64
                        ),
                        HirExprKind::Float(_) => matches!(t, HirType::F32 | HirType::F64),
                        HirExprKind::Bool(_) => matches!(t, HirType::Bool),
                        HirExprKind::String(_) => matches!(t, HirType::String),
                        HirExprKind::Char(_) => matches!(t, HirType::Char),
                        HirExprKind::None => matches!(t, HirType::Optional(_)),
                        _ => true, // Fallback for complex literal exprs
                    }
                }
                HirPatternKind::Struct { name, .. } => matches!(t, HirType::Struct(n) if n == name),
                HirPatternKind::EnumVariant { name, .. } => {
                    matches!(t, HirType::Enum(n) if n == name)
                }
                HirPatternKind::Tuple(elems) => {
                    matches!(t, HirType::Tuple(ts) if ts.len() == elems.len())
                }
                HirPatternKind::Array(_elems) => {
                    matches!(t, HirType::Array(_) | HirType::SizedArray(_, _))
                }
                HirPatternKind::Range { .. } => {
                    matches!(
                        t,
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
        }

        // Union/intersection dispatcher for quick shape compatibility
        let shape_ok = match expected_type {
            HirType::Union(types) => types.iter().any(|t| matches_shape(pattern, t)),
            HirType::Intersection(types) => types.iter().all(|t| matches_shape(pattern, t)),
            other => matches_shape(pattern, other),
        };

        if !shape_ok {
            self.error(format!(
                "Pattern does not match expected type shape: pattern={:?}, expected={:?}",
                pattern.kind, expected_type
            ));
            return Ok(());
        }

        // For destructuring patterns, attempt shallow recursive checks where possible.
        match (&*pattern.kind, expected_type) {
            // Wildcard/variable: always OK
            (HirPatternKind::Wildcard, _) | (HirPatternKind::Variable(_), _) => {}

            // Literal: we already did shape check; nothing more to enforce now.
            (HirPatternKind::Literal(_), _) => {}

            // Tuple: check arity and recurse element-wise when type carries element info
            (HirPatternKind::Tuple(pelems), HirType::Tuple(telems))
                if pelems.len() == telems.len() =>
            {
                for (p, t) in pelems.iter().zip(telems.iter()) {
                    self.check_pattern_against_type(p, t)?;
                }
            }
            (HirPatternKind::Tuple(_), HirType::Union(types)) => {
                // If union contains a tuple type, prefer the first matching arity to drill in
                if let Some(telems) = types.iter().find_map(|t| {
                    if let HirType::Tuple(ts) = t {
                        Some(ts)
                    } else {
                        None
                    }
                }) && let HirPatternKind::Tuple(pelems) = &*pattern.kind
                    && pelems.len() == telems.len()
                {
                    for (p, t) in pelems.iter().zip(telems.iter()) {
                        self.check_pattern_against_type(p, t)?;
                    }
                }
            }

            // Array: no element typing available here; accept after shape check
            (HirPatternKind::Array(_), HirType::Array(_) | HirType::SizedArray(_, _)) => {}

            // Struct: without field type metadata at this pass, accept after shape check
            (HirPatternKind::Struct { .. }, HirType::Struct(_)) => {}

            // Enum variant: without variant payload typing here, accept after shape check
            (HirPatternKind::EnumVariant { .. }, HirType::Enum(_)) => {}

            // Range patterns: already shape-checked (integers)
            (HirPatternKind::Range { .. }, _) => {}

            // Union/intersection already handled in shape_ok; nothing deeper to do generically
            (_, HirType::Union(_)) | (_, HirType::Intersection(_)) => {}

            // Fallback: nothing more to enforce
            _ => {}
        }

        Ok(())
    }

    /// Strict type matching for explicit type annotations
    /// This is more restrictive than general type compatibility
    fn strict_type_match(&self, expected: &HirType, actual: &HirType) -> bool {
        use HirType::*;

        match (expected, actual) {
            // Exact matches are always allowed
            (I8, I8) | (I16, I16) | (I32, I32) | (I64, I64) => true,
            (U8, U8) | (U16, U16) | (U32, U32) | (U64, U64) => true,
            (F32, F32) | (F64, F64) => true,
            (Bool, Bool) | (String, String) | (Char, Char) => true,
            (Void, Void) | (Never, Never) => true,

            // Allow some safe numeric promotions for explicit annotations
            // Integer widening (only upward)
            (I16, I8) | (I32, I8) | (I64, I8) => true,
            (I32, I16) | (I64, I16) => true,
            (I64, I32) => true,
            (U16, U8) | (U32, U8) | (U64, U8) => true,
            (U32, U16) | (U64, U16) => true,
            (U64, U32) => true,

            // Float widening
            (F64, F32) => true,

            // Integer to float (safe)
            (F32, I8) | (F32, I16) | (F32, U8) | (F32, U16) => true,
            (F64, I8) | (F64, I16) | (F64, I32) | (F64, U8) | (F64, U16) | (F64, U32) => true,

            // Range compatibility
            (Range, Range) => true,

            // Everything else is incompatible for explicit annotations
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use veil_hir::*;
    use veil_resolve::SymbolTable;

    #[test]
    fn test_let_statement_type_checking() {
        let symbol_table = SymbolTable::new();
        let mut files = veil_diagnostics::Files::<String>::new();
        let fid = files.add("test.veil".to_string(), String::new());
        let mut checker = TypeChecker::new(symbol_table, fid, None);

        // Create a simple var statement: var x = 42;
        let mut stmt = HirStmt {
            id: NodeId::new(1),
            kind: HirStmtKind::Var {
                name: "x".to_string(),
                ty: Some(HirType::I32),
                init: HirExpr {
                    id: NodeId::new(3),
                    kind: Box::new(HirExprKind::Int(42)),
                },
                is_mutable: false,
            },
        };

        let result = checker.check_stmt(&mut stmt);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), HirType::I32);
    }

    #[test]
    fn test_type_mismatch_in_let() {
        let symbol_table = SymbolTable::new();
        let mut files = veil_diagnostics::Files::<String>::new();
        let fid = files.add("test.veil".to_string(), String::new());
        let mut checker = TypeChecker::new(symbol_table, fid, None);

        // Create a let statement with type mismatch: let x: bool = 42;
        // Create var statement with mismatched type: var x: bool = 42;
        let mut stmt = HirStmt {
            id: NodeId::new(1),
            kind: HirStmtKind::Var {
                name: "x".to_string(),
                ty: Some(HirType::Bool),
                init: HirExpr {
                    id: NodeId::new(3),
                    kind: Box::new(HirExprKind::Int(42)),
                },
                is_mutable: false,
            },
        };

        let result = checker.check_stmt(&mut stmt);
        assert!(result.is_ok()); // Should succeed but with errors
        assert!(!checker.errors.is_empty()); // Should have type mismatch error
    }
}
