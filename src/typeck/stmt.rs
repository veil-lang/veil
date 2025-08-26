use crate::ast;
use crate::ast::{Stmt, Type};
use crate::typeck::{Context, TypeChecker};
use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic;

impl TypeChecker {
    pub fn check_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Vec<Diagnostic<FileId>>> {
        match stmt {
            Stmt::Let(name, decl_ty, expr, var_span, _) => {
                let expr_ty = if let Some(decl_ty) = decl_ty {
                    self.check_expr_with_expected(expr, decl_ty)
                        .unwrap_or(Type::Unknown)
                } else {
                    self.check_expr(expr).unwrap_or(Type::Unknown)
                };

                if expr_ty == Type::Void {
                    self.report_error("Cannot assign void expression to variable", *var_span);
                    return Ok(());
                }

                let final_ty = if let Some(decl_ty) = decl_ty {
                    if expr_ty == Type::NoneType {
                        match decl_ty {
                            Type::Optional(_) => decl_ty.clone(),
                            _ => {
                                self.report_error(
                                    &format!("Cannot assign None to non-optional type {}", decl_ty),
                                    expr.span(),
                                );
                                decl_ty.clone()
                            }
                        }
                    } else if !Self::is_convertible(&expr_ty, decl_ty) {
                        self.report_error(
                            &format!("Cannot convert {} to {}", expr_ty, decl_ty),
                            expr.span(),
                        );
                        decl_ty.clone()
                    } else {
                        decl_ty.clone()
                    }
                } else {
                    if expr_ty == Type::NoneType {
                        self.report_error(
                            "Cannot infer type from None literal - specify type annotation",
                            expr.span(),
                        );
                        Type::Unknown
                    } else {
                        expr_ty
                    }
                };

                self.context.variables.insert(name.clone(), final_ty);
            }
            Stmt::Var(name, decl_ty, _) => {
                let ty = decl_ty.clone().unwrap_or(Type::Unknown);
                self.context.variables.insert(name.clone(), ty);
            }
            Stmt::Expr(expr, _) => {
                self.check_expr(expr)?;
            }
            Stmt::Block(stmts, _) => self.check_block(stmts)?,
            Stmt::If(cond, then_branch, else_branch, _) => {
                let cond_ty = self.check_expr(cond).unwrap_or(Type::Unknown);
                self.expect_type(&cond_ty, &Type::Bool, cond.span())?;

                self.check_block(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.check_block(else_branch)?;
                }
            }
            Stmt::Return(expr, _) => {
                let expr_ty = self.check_expr(expr).unwrap_or(Type::Unknown);

                if self.context.inferring_return_type && self.context.inferred_return_type.is_none()
                {
                    self.context.inferred_return_type = Some(expr_ty.clone());
                    self.context.current_return_type = expr_ty;
                } else {
                    let expected_type = self.context.current_return_type.clone();
                    self.expect_type(&expr_ty, &expected_type, expr.span())?;
                }
            }
            Stmt::Defer(expr, span) => {
                let expr_ty = self.check_expr(expr)?;

                if expr_ty != Type::Void {
                    self.report_error("Defer expects void-returning expression", *span);
                }
            }
            Stmt::While(cond, body, _) => {
                let cond_ty = self.check_expr(cond)?;
                self.expect_type(&cond_ty, &Type::Bool, cond.span())?;
                let previous_in_loop = self.context.in_loop;
                self.context.in_loop = true;
                self.check_block(body)?;
                self.context.in_loop = previous_in_loop;
            }
            Stmt::Loop(body, _) => {
                let previous_in_loop = self.context.in_loop;
                self.context.in_loop = true;
                self.check_block(body)?;
                self.context.in_loop = previous_in_loop;
            }
            Stmt::For(name, index_var, range, step, body, _) => {
                self.check_expr(range)?;

                if let Some(step_expr) = step {
                    let step_type = self.check_expr(step_expr)?;
                    if !matches!(step_type, Type::I32 | Type::I64 | Type::U32 | Type::U64) {
                        self.report_error("Step value must be an integer", step_expr.span());
                    }
                }

                let old_variables = self.context.variables.clone();
                self.context.variables.insert(name.clone(), Type::I32);
                if let Some(idx_var) = index_var {
                    self.context.variables.insert(idx_var.clone(), Type::I32);
                }
                let previous_in_loop = self.context.in_loop;
                self.context.in_loop = true;
                self.check_block(body)?;
                self.context.in_loop = previous_in_loop;
                self.context.variables = old_variables;
            }
            Stmt::Break(expr, span) => {
                if !self.context.in_loop {
                    self.report_error("Break statement outside of loop", *span);
                }
                if let Some(expr) = expr {
                    let break_type = self.check_expr(expr)?;
                    self.context.break_types.push(break_type);
                } else {
                    self.context.break_types.push(Type::Void);
                }
            }
            Stmt::Continue(span) => {
                if !self.context.in_loop {
                    self.report_error("Continue statement outside of loop", *span);
                }
            }
        }
        Ok(())
    }

    pub fn check_block(&mut self, stmts: &mut [Stmt]) -> Result<(), Vec<Diagnostic<FileId>>> {
        for stmt in stmts {
            self.check_stmt(stmt)?;
        }
        Ok(())
    }

    pub fn check_impl_block(
        &mut self,
        impl_block: &mut ast::ImplBlock,
    ) -> Result<(), Vec<Diagnostic<FileId>>> {
        let target_type = self.parse_type_name(&impl_block.target_type);

        for method in &mut impl_block.methods {
            for (param_name, param_type) in &mut method.params {
                if param_name == "self" {
                    *param_type = target_type.clone();
                }
            }

            if method.name == "constructor" {
                let params: Vec<Type> = method.params.iter().map(|(_, t)| t.clone()).collect();
                let constructor_name = format!("{}.constructor", impl_block.target_type);
                self.functions
                    .insert(constructor_name, (params, method.return_type.clone()));
            }

            self.context.current_return_type = method.return_type.clone();
            self.check_function(method)?;
        }

        Ok(())
    }

    pub fn check_test(&mut self, test: &mut ast::Test) -> Result<(), Vec<Diagnostic<FileId>>> {
        let mut local_ctx = Context::new();
        local_ctx.current_return_type = Type::Void;
        local_ctx.struct_defs = self.context.struct_defs.clone();
        local_ctx.enum_defs = self.context.enum_defs.clone();
        local_ctx.enum_def_map = self.context.enum_def_map.clone();
        local_ctx.variables = self.context.variables.clone();

        let original_ctx = std::mem::replace(&mut self.context, local_ctx);

        for stmt in &mut test.stmts {
            self.check_stmt(stmt)?;
        }

        self.context = original_ctx;

        Ok(())
    }
}
