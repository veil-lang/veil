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

            HirStmtKind::Let { pattern, ty, init } => {
                // Type check the initializer if present
                let init_type = if let Some(init_expr) = init {
                    Some(self.check_expr(init_expr)?)
                } else {
                    None
                };

                // Check type annotation if present
                if let Some(annotation) = ty
                    && let Some(ref init_ty) = init_type
                    && !self.context.types_compatible(annotation, init_ty)
                {
                    self.error(format!(
                        "Type annotation {:?} does not match initializer type {:?}",
                        annotation, init_ty
                    ));
                }

                // Determine the variable's type
                let var_type = ty.clone().or(init_type).unwrap_or(HirType::Unknown);

                // Record the variable type on the pattern node (Typed-HIR attachment)
                self.context.set_node_type(pattern.id, var_type.clone());

                // Store variable type in local context for future lookups
                if let veil_hir::HirPatternKind::Variable(var_name) = &*pattern.kind {
                    self.context
                        .local_variables
                        .insert(var_name.clone(), var_type.clone());
                }

                // Check pattern compatibility with the type
                self.check_pattern_against_type(pattern, &var_type)?;

                Ok(HirType::Void) // Let statements don't produce values
            }

            HirStmtKind::Assign { lhs, rhs } => {
                let target_type = self.check_expr(lhs)?;
                let value_type = self.check_expr(rhs)?;

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

        // Create a simple let statement: let x = 42;
        let mut stmt = HirStmt {
            id: NodeId::new(1),
            kind: HirStmtKind::Let {
                pattern: HirPattern {
                    id: NodeId::new(2),
                    kind: Box::new(HirPatternKind::Variable("x".to_string())),
                },
                ty: Some(HirType::I32),
                init: Some(HirExpr {
                    id: NodeId::new(3),
                    kind: Box::new(HirExprKind::Int(42)),
                }),
            },
        };

        let result = checker.check_stmt(&mut stmt);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), HirType::Void);
    }

    #[test]
    fn test_type_mismatch_in_let() {
        let symbol_table = SymbolTable::new();
        let mut files = veil_diagnostics::Files::<String>::new();
        let fid = files.add("test.veil".to_string(), String::new());
        let mut checker = TypeChecker::new(symbol_table, fid, None);

        // Create a let statement with type mismatch: let x: bool = 42;
        let mut stmt = HirStmt {
            id: NodeId::new(1),
            kind: HirStmtKind::Let {
                pattern: HirPattern {
                    id: NodeId::new(2),
                    kind: Box::new(HirPatternKind::Variable("x".to_string())),
                },
                ty: Some(HirType::Bool),
                init: Some(HirExpr {
                    id: NodeId::new(3),
                    kind: Box::new(HirExprKind::Int(42)),
                }),
            },
        };

        let result = checker.check_stmt(&mut stmt);
        assert!(result.is_ok()); // Should succeed but with errors
        assert!(!checker.errors.is_empty()); // Should have type mismatch error
    }
}
