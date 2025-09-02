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
                if let Some(annotation) = ty {
                    if let Some(ref init_ty) = init_type {
                        if !self.context.types_compatible(annotation, init_ty) {
                            self.error(format!(
                                "Type annotation {:?} does not match initializer type {:?}",
                                annotation, init_ty
                            ));
                        }
                    }
                }

                // Determine the variable's type
                let var_type = ty.clone().or(init_type).unwrap_or(HirType::Unknown);

                // Check pattern compatibility with the type
                self.check_pattern_against_type(pattern, &var_type)?;

                Ok(HirType::Void) // Let statements don't produce values
            }

            HirStmtKind::Assign { lhs, rhs } => {
                let target_type = self.check_expr(lhs)?;
                let value_type = self.check_expr(rhs)?;

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
                    if let Some(expected_return) = &self.context.current_return_type {
                        if *expected_return != HirType::Void {
                            self.error(format!(
                                "Empty return in function expecting {:?}",
                                expected_return
                            ));
                        }
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
    fn check_pattern_against_type(
        &mut self,
        _pattern: &veil_hir::HirPattern,
        _expected_type: &HirType,
    ) -> Result<(), Vec<Diag>> {
        // TODO(M5): Implement full pattern typing rules. For now, accept all patterns.
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
