use crate::ast;
use crate::ast::Type;
use crate::codegen::CompileError;
use crate::codegen::c::CBackend;

impl CBackend {
    pub fn emit_stmt(&mut self, stmt: &ast::Stmt) -> Result<(), CompileError> {
        match stmt {
            ast::Stmt::Let(name, ty, expr, _, _) => {
                if let ast::Expr::Match(expr, arms, info) = expr {
                    let var_type = ty.clone().unwrap_or_else(|| info.ty.clone());
                    let c_ty = self.type_to_c(&var_type);
                    let temp_var = format!("_match_result_{}", info.span.start());
                    self.body.push_str(&format!("{} {} = 0;\n", c_ty, temp_var));
                    
                    // Generate code for the expression being matched
                    let expr_code = self.emit_expr(expr)?;
                    let matched_type = expr.get_type();
                    let temp_input = format!("_match_input_{}", info.span.start());
                    let c_input_type = self.type_to_c(&matched_type);
                    self.body.push_str(&format!("{} {} = {};\n", c_input_type, temp_input, expr_code));
                    
                    let mut match_code = String::new();
                    self.emit_match_switch_with_result(
                        &temp_input,
                        &temp_var,
                        arms,
                        &mut match_code,
                    )?;
                    self.body.push_str(&match_code);
                    self.body
                        .push_str(&format!("{} {} = {};\n", c_ty, name, temp_var));
                    self.variables.borrow_mut().insert(name.clone(), var_type);
                    return Ok(());
                }
                let var_type = if let Some(ty) = ty {
                    ty.clone()
                } else {
                    match expr {
                        ast::Expr::Int(_, _) => Type::I32,
                        ast::Expr::F32(_, _) => Type::F32,
                        ast::Expr::Bool(_, _) => Type::Bool,
                        ast::Expr::Str(_, _) => Type::String,
                        ast::Expr::Call(func_name, _, _) => {
                            if func_name.starts_with("<method>.") {
                                expr.get_type()
                            } else {
                                self.functions_map.get(func_name).cloned().ok_or_else(|| {
                                    CompileError::CodegenError {
                                        message: format!("Undefined function '{}'", func_name),
                                        span: Some(expr.span()),
                                        file_id: self.file_id,
                                    }
                                })?
                            }
                        }
                        _ => expr.get_type(),
                    }
                };

                if let Type::Optional(inner) = &var_type {
                    self.ensure_optional_type(inner);
                }

                let c_ty = self.type_to_c(&var_type);
                let expr_code = self.emit_expr_with_optional_context(expr, &var_type)?;
                self.body
                    .push_str(&format!("{} {} = {};\n", c_ty, name, expr_code));
                self.variables.borrow_mut().insert(name.clone(), var_type);
            }
            ast::Stmt::Return(expr, _) => {
                if let ast::Expr::Void(_) = expr {
                    let current_func = self.body.rsplit_once("(").and_then(|(before, _)| {
                        before.rsplit_once(' ').map(|(_, name)| name.trim())
                    });
                    let ret_type = self.functions_map.get(current_func.unwrap_or(""));
                    if current_func.unwrap_or("") == "main" || ret_type == Some(&Type::I32) {
                        self.body.push_str("return 0;\n");
                        return Ok(());
                    } else if ret_type == Some(&Type::Void) {
                        self.body.push_str("return;\n");
                        return Ok(());
                    } else {
                        return Ok(());
                    }
                }
                let ret_type = if let Some(func_name) = &self.current_function {
                    self.functions_map
                        .get(func_name)
                        .cloned()
                        .unwrap_or(Type::Void)
                } else {
                    Type::Void
                };
                let expr_code = self.emit_expr_with_optional_context(expr, &ret_type)?;
                self.body.push_str(&format!("return {};\n", expr_code));
            }
            ast::Stmt::Expr(expr, _) => {
                let expr_code = self.emit_expr(expr)?;
                if expr_code.starts_with('{') {
                    self.body.push_str(&expr_code);
                } else if !expr_code.ends_with(';') {
                    self.body.push_str(&format!("{};\n", expr_code));
                } else {
                    self.body.push_str(&format!("{}\n", expr_code));
                }
            }
            ast::Stmt::Block(stmts, _) => {
                for s in stmts {
                    self.emit_stmt(s)?;
                }
            }
            ast::Stmt::While(cond, body, _) => {
                let cond_code = self.emit_expr(cond)?;
                self.body.push_str(&format!("while ({}) {{\n", cond_code));
                for stmt in body {
                    self.emit_stmt(stmt)?;
                }
                self.body.push_str("}\n");
            }
            ast::Stmt::Loop(body, _) => {
                self.body.push_str("while (1) {\n");
                for stmt in body {
                    self.emit_stmt(stmt)?;
                }
                self.body.push_str("}\n");
            }
            ast::Stmt::For(var_name, index_var, range, step, body, _) => {
                if let ast::Expr::Range(start_expr, end_expr, range_type, _) = range {
                    if let ast::Expr::InfiniteRange(_, _) = end_expr.as_ref() {
                        let start_code = self.emit_expr(start_expr)?;
                        let step_code = if let Some(step_expr) = step {
                            self.emit_expr(step_expr)?
                        } else {
                            "1".to_string()
                        };
                        let loop_var = if var_name.is_empty() {
                            "_unused_var"
                        } else {
                            var_name
                        };

                        if let Some(index_name) = index_var {
                            self.body.push_str(&format!(
                                "for (int {var} = {start}, {idx} = 0; ; {var} += {step}, {idx}++) {{\n",
                                var = loop_var,
                                idx = index_name,
                                start = start_code,
                                step = step_code
                            ));
                        } else {
                            self.body.push_str(&format!(
                                "for (int {var} = {start}; ; {var} += {step}) {{\n",
                                var = loop_var,
                                start = start_code,
                                step = step_code
                            ));
                        }
                    } else if let ast::Expr::InfiniteRange(_, _) = start_expr.as_ref() {
                        let end_code = self.emit_expr(end_expr)?;
                        let step_code = if let Some(step_expr) = step {
                            self.emit_expr(step_expr)?
                        } else {
                            "1".to_string()
                        };
                        let loop_var = if var_name.is_empty() {
                            "_unused_var"
                        } else {
                            var_name
                        };

                        if let Some(index_name) = index_var {
                            self.body.push_str(&format!(
                                "for (int {var} = 0, {idx} = 0; {var} < {end}; {var} += {step}, {idx}++) {{\n",
                                var = loop_var,
                                idx = index_name,
                                end = end_code,
                                step = step_code
                            ));
                        } else {
                            self.body.push_str(&format!(
                                "for (int {var} = 0; {var} < {end}; {var} += {step}) {{\n",
                                var = loop_var,
                                end = end_code,
                                step = step_code
                            ));
                        }
                    } else {
                        let start_code = self.emit_expr(start_expr)?;
                        let end_code = self.emit_expr(end_expr)?;

                        let loop_var = if var_name.is_empty() {
                            "_unused_var"
                        } else {
                            var_name
                        };

                        let step_code = if let Some(step_expr) = step {
                            self.emit_expr(step_expr)?
                        } else {
                            "1".to_string()
                        };

                        let is_reversed =
                            if let (ast::Expr::Int(start_val, _), ast::Expr::Int(end_val, _)) =
                                (start_expr.as_ref(), end_expr.as_ref())
                            {
                                start_val > end_val
                            } else {
                                false
                            };

                        if is_reversed {
                            let condition = match range_type {
                                ast::RangeType::Exclusive => format!("{} > {}", loop_var, end_code),
                                ast::RangeType::Inclusive => {
                                    format!("{} >= {}", loop_var, end_code)
                                }
                                _ => {
                                    return Err(CompileError::CodegenError {
                                        message: format!(
                                            "Unsupported range type for reversed range: {:?}",
                                            range_type
                                        ),
                                        span: None,
                                        file_id: self.file_id,
                                    });
                                }
                            };

                            if let Some(index_name) = index_var {
                                self.body.push_str(&format!(
                                    "for (int {var} = {start}, {idx} = 0; {condition}; {var} -= {step}, {idx}++) {{\n",
                                    var = loop_var,
                                    idx = index_name,
                                    start = start_code,
                                    condition = condition,
                                    step = step_code
                                ));
                            } else {
                                self.body.push_str(&format!(
                                    "for (int {var} = {start}; {condition}; {var} -= {step}) {{\n",
                                    var = loop_var,
                                    start = start_code,
                                    condition = condition,
                                    step = step_code
                                ));
                            }
                        } else {
                            let condition = match range_type {
                                ast::RangeType::Exclusive => format!("{} < {}", loop_var, end_code),
                                ast::RangeType::Inclusive => {
                                    format!("{} <= {}", loop_var, end_code)
                                }
                                _ => {
                                    return Err(CompileError::CodegenError {
                                        message: format!(
                                            "Unsupported range type for normal range: {:?}",
                                            range_type
                                        ),
                                        span: None,
                                        file_id: self.file_id,
                                    });
                                }
                            };

                            if let Some(index_name) = index_var {
                                self.body.push_str(&format!(
                                    "for (int {var} = {start}, {idx} = 0; {condition}; {var} += {step}, {idx}++) {{\n",
                                    var = loop_var,
                                    idx = index_name,
                                    start = start_code,
                                    condition = condition,
                                    step = step_code
                                ));
                            } else {
                                self.body.push_str(&format!(
                                    "for (int {var} = {start}; {condition}; {var} += {step}) {{\n",
                                    var = loop_var,
                                    start = start_code,
                                    condition = condition,
                                    step = step_code
                                ));
                            }
                        }
                    }
                } else if let ast::Expr::InfiniteRange(range_type, _) = range {
                    let loop_var = if var_name.is_empty() {
                        "_unused_var"
                    } else {
                        var_name
                    };

                    self.includes.borrow_mut().insert("<stdlib.h>".to_string());
                    self.includes.borrow_mut().insert("<time.h>".to_string());

                    let unique_id = format!("{}_{}", loop_var, self.body.len());

                    match range_type {
                        ast::RangeType::Infinite => {
                            self.body.push_str(&format!(
                                "{{\n\
                                 static int seeded_{} = 0;\n\
                                 if (!seeded_{}) {{\n\
                                     srand(time(NULL) + {});\n\
                                     seeded_{} = 1;\n\
                                 }}\n\
                                 for (int k = 0; k < 5; k++) rand();\n\
                                 long long {var};\n\
                                 int scale = rand() % 4;\n\
                                 switch(scale) {{\n\
                                     case 0: {var} = rand() % 1000; break; \n\
                                     case 1: {var} = rand() % 1000000; break; \n\
                                     case 2: {var} = ((long long)rand() << 16) | rand(); break; \n\
                                     case 3: {var} = ((long long)rand() << 32) | ((long long)rand() << 16) | rand(); break; \n\
                                 }}\n\
                                 if (rand() % 2) {var} = -{var};  \n",
                                unique_id, unique_id, unique_id.len() * 23, unique_id, var = loop_var
                            ));
                            if let Some(index_name) = index_var {
                                self.body.push_str(&format!("int {} = 0;\n", index_name));
                            }
                            self.body.push_str("for (;;) {\n");
                        }
                        ast::RangeType::InfiniteUp => {
                            self.body.push_str(&format!(
                                "{{\n\
                                 static int seeded_{} = 0;\n\
                                 if (!seeded_{}) {{\n\
                                     srand(time(NULL) + {}); \n\
                                     seeded_{} = 1;\n\
                                 }}\n\
                                 for (int k = 0; k < 7; k++) rand(); \n\
                                 long long {var};\n\
                                 int scale = rand() % 4; \n\
                                 switch(scale) {{\n\
                                     case 0: {var} = rand() % 1000; break;\n\
                                     case 1: {var} = rand() % 1000000; break;\n\
                                     case 2: {var} = ((long long)rand() << 16) | rand(); break; \n\
                                     case 3: {var} = ((long long)rand() << 32) | ((long long)rand() << 16) | rand(); break;\n\
                                 }}\n",
                                unique_id, unique_id, unique_id.len() * 31, unique_id, var = loop_var
                            ));
                            if let Some(index_name) = index_var {
                                self.body.push_str(&format!("int {} = 0;\n", index_name));
                            }
                            self.body.push_str("for (;;) {\n");
                        }
                        ast::RangeType::InfiniteDown => {
                            self.body.push_str(&format!(
                                "{{\n\
                                 static int seeded_{} = 0;\n\
                                 if (!seeded_{}) {{\n\
                                     srand(time(NULL) + {}); \n\
                                     seeded_{} = 1;\n\
                                 }}\n\
                                 for (int k = 0; k < 11; k++) rand(); \n\
                                 long long {var};\n\
                                 int scale = rand() % 4;  \n\
                                 switch(scale) {{\n\
                                     case 0: {var} = -(rand() % 1000); break; \n\
                                     case 1: {var} = -(rand() % 1000000); break; \n\
                                     case 2: {var} = -(((long long)rand() << 16) | rand()); break; \n\
                                     case 3: {var} = -(((long long)rand() << 32) | ((long long)rand() << 16) | rand()); break; \n\
                                 }}\n",
                                unique_id, unique_id, unique_id.len() * 43, unique_id, var = loop_var
                            ));
                            if let Some(index_name) = index_var {
                                self.body.push_str(&format!("int {} = 0;\n", index_name));
                            }
                            self.body.push_str("for (;;) {\n");
                        }
                        _ => {
                            return Err(CompileError::CodegenError {
                                message: format!("Invalid infinite range type: {:?}", range_type),
                                span: None,
                                file_id: self.file_id,
                            });
                        }
                    }
                } else {
                    let _range_code = self.emit_expr(range)?;
                    let _loop_var = if var_name.is_empty() {
                        "_unused_var"
                    } else {
                        var_name
                    };

                    self.body.push_str("/* Unsupported range type */\n");
                }

                for stmt in body {
                    self.emit_stmt(stmt)?;
                }

                if let ast::Expr::InfiniteRange(range_type, _) = range {
                    let loop_var = if var_name.is_empty() {
                        "_unused_var"
                    } else {
                        var_name
                    };

                    match range_type {
                        ast::RangeType::InfiniteUp => {
                            self.body.push_str(&format!("    {}++;\n", loop_var));
                            if let Some(index_name) = index_var {
                                self.body.push_str(&format!("    {}++;\n", index_name));
                            }
                        }
                        ast::RangeType::InfiniteDown => {
                            self.body.push_str(&format!("    {}--;\n", loop_var));
                            if let Some(index_name) = index_var {
                                self.body.push_str(&format!("    {}++;\n", index_name));
                            }
                        }
                        ast::RangeType::Infinite => {
                            self.body.push_str(&format!("    {}++;\n", loop_var));
                            if let Some(index_name) = index_var {
                                self.body.push_str(&format!("    {}++;\n", index_name));
                            }
                        }
                        _ => {}
                    }
                }

                self.body.push_str("}\n");

                if let ast::Expr::InfiniteRange(_, _) = range {
                    self.body.push_str("}\n");
                }
            }
            ast::Stmt::Break(expr, _) => {
                let in_loop_expr =
                    self.current_loop_result.is_some() && self.current_loop_break.is_some();

                if in_loop_expr {
                    let result_var = self.current_loop_result.as_ref().unwrap().clone();
                    let break_label = self.current_loop_break.as_ref().unwrap().clone();

                    if let Some(expr) = expr {
                        let expr_code = self.emit_expr(expr)?;
                        self.body.push_str(&format!(
                            "{{ {} = {}; goto {}; }}\n",
                            result_var, expr_code, break_label
                        ));
                    } else {
                        self.body.push_str(&format!("goto {};\n", break_label));
                    }
                } else {
                    if let Some(_expr) = expr {
                        self.body
                            .push_str("break; /* break with value in regular loop */\n");
                    } else {
                        self.body.push_str("break;\n");
                    }
                }
            }
            ast::Stmt::Continue(_) => {
                self.body.push_str("continue;\n");
            }
            ast::Stmt::If(cond, then_branch, else_branch, _) => {
                let cond_code = self.emit_expr(cond)?;
                self.body.push_str(&format!("if ({}) {{\n", cond_code));

                for stmt in then_branch {
                    self.emit_stmt(stmt)?;
                }
                self.body.push('}');

                if let Some(else_body) = else_branch {
                    self.body.push_str(" else {\n");
                    for stmt in else_body {
                        self.emit_stmt(stmt)?;
                    }
                    self.body.push('}');
                }

                self.body.push('\n');
            }
            _ => unimplemented!(),
        }
        Ok(())
    }

    pub fn emit_stmt_to_string(&mut self, stmt: &ast::Stmt) -> Result<String, CompileError> {
        let original_body = std::mem::take(&mut self.body);
        self.emit_stmt(stmt)?;
        let result = std::mem::replace(&mut self.body, original_body);
        Ok(result)
    }
}
