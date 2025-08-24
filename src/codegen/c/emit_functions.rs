use crate::ast;
use crate::ast::Type;
use crate::codegen::CompileError;
use crate::codegen::c::CBackend;

impl CBackend {
    pub fn emit_functions(&mut self, program: &ast::Program) -> Result<(), CompileError> {
        for func in &program.functions {
            self.functions_map
                .insert(func.name.clone(), func.return_type.clone());
        }

        for func in &program.functions {
            let return_type = if func.name == "main" {
                "int".to_string()
            } else {
                self.type_to_c(&func.return_type)
            };
            let func_name = if func.name == "main" {
                func.name.clone()
            } else if func.name.starts_with("ve_method_") {
                func.name.clone()
            } else {
                format!("ve_fn_{}", func.name)
            };

            let params = func
                .params
                .iter()
                .map(|(name, ty)| format!("{} {}", self.type_to_c(ty), name))
                .collect::<Vec<_>>()
                .join(", ");
            self.header
                .push_str(&format!("{} {}({});\n", return_type, func_name, params));
        }

        for impl_block in &program.impls {
            for method in &impl_block.methods {
                let return_type = self.type_to_c(&method.return_type);
                let sanitized_type = if impl_block.target_type.starts_with("[]") {
                    let inner = &impl_block.target_type[2..];
                    format!("array_{}", inner)
                } else {
                    impl_block.target_type.clone()
                };
                let method_name = format!("ve_method_{}_{}", sanitized_type, method.name);

                let params = method
                    .params
                    .iter()
                    .map(|(name, ty)| format!("{} {}", self.type_to_c(ty), name))
                    .collect::<Vec<_>>()
                    .join(", ");
                self.header
                    .push_str(&format!("{} {}({});\n", return_type, method_name, params));
            }
        }

        for func in &program.functions {
            self.emit_function(func)?;
        }

        Ok(())
    }

    pub fn emit_function(&mut self, func: &ast::Function) -> Result<(), CompileError> {
        self.current_function = Some(func.name.clone());

        if let Type::Optional(inner) = &func.return_type {
            self.ensure_optional_type(inner);
        }
        for (_, param_type) in &func.params {
            if let Type::Optional(inner) = param_type {
                self.ensure_optional_type(inner);
            }
        }

        let return_type = if func.name == "main" {
            "int".to_string()
        } else {
            self.type_to_c(&func.return_type)
        };

        let func_name = if func.name == "main" {
            func.name.clone()
        } else if func.name.starts_with("ve_method_") {
            func.name.clone()
        } else {
            format!("ve_fn_{}", func.name)
        };

        let is_generic = !func.generic_params.is_empty();
        let mut param_strings = Vec::new();

        for (name, ty) in &func.params {
            let c_ty = if is_generic && matches!(ty, Type::Generic(_)) {
                "void*".to_string()
            } else {
                self.type_to_c(ty)
            };

            param_strings.push(format!("{} {}", c_ty, name));
            self.variables.borrow_mut().insert(name.clone(), ty.clone());
        }

        let params = param_strings.join(", ");

        self.body
            .push_str(&format!("{} {}({}) {{\n", return_type, func_name, params));

        if func.name == "main" {
            self.body.push_str("ve_arena_enter();\n");
        }

        let mut stmts = func.body.iter().peekable();
        let mut has_tail_return = false;
        while let Some(stmt) = stmts.next() {
            let is_last = stmts.peek().is_none();
            if is_last {
                if let ast::Stmt::Expr(expr, _) = stmt {
                    if expr.get_info().is_tail {
                        let expr_code = self.emit_expr(expr)?;
                        if func.name == "main" {
                            self.body.push_str("ve_arena_exit();\n");
                            self.body.push_str(
                                "#ifdef VE_DEBUG_MEMORY\n    ve_arena_stats();\n#endif\n",
                            );
                            self.body.push_str("    ve_arena_cleanup();\n");
                        }
                        self.body.push_str(&format!("return {};\n", expr_code));
                        has_tail_return = true;
                        continue;
                    }
                }
            }
            self.emit_stmt(stmt)?;
        }

        if !has_tail_return {
            if func.name == "main" {
                let last_is_return = func
                    .body
                    .last()
                    .is_some_and(|s| matches!(s, ast::Stmt::Return(..)));

                if !last_is_return {
                    self.body.push_str("ve_arena_exit();\n");
                    self.body
                        .push_str("#ifdef VE_DEBUG_MEMORY\n    ve_arena_stats();\n#endif\n");
                    self.body.push_str("    ve_arena_cleanup();\n");
                    self.body.push_str("    return 0;\n");
                }
            } else if func.return_type == Type::Void {
                self.body.push_str("    return;\n");
            }
        }

        self.body.push_str("}\n\n");
        Ok(())
    }

    pub fn emit_impl_block(&mut self, impl_block: &ast::ImplBlock) -> Result<(), CompileError> {
        for method in &impl_block.methods {
            let sanitized_type = if impl_block.target_type.starts_with("[]") {
                let inner = &impl_block.target_type[2..];
                format!("array_{}", inner)
            } else {
                impl_block.target_type.clone()
            };
            let mangled_name = format!("ve_method_{}_{}", sanitized_type, method.name);

            if impl_block.target_type.ends_with("[]")
                && self.generate_optimized_array_method(impl_block, method)?
            {
                continue;
            }

            let mut impl_function = method.clone();
            impl_function.name = mangled_name;

            self.emit_function(&impl_function)?;
        }

        Ok(())
    }

    pub fn generate_optimized_array_method(
        &mut self,
        impl_block: &ast::ImplBlock,
        method: &ast::Function,
    ) -> Result<bool, CompileError> {
        let inner_type = if impl_block.target_type.starts_with("[]") {
            &impl_block.target_type[2..]
        } else {
            return Ok(false);
        };

        let sanitized_type = format!("array_{}", inner_type);
        let mangled_name = format!("ve_method_{}_{}", sanitized_type, method.name);

        match method.name.as_str() {
            "length" => {
                self.body
                    .push_str(&format!("int {}(ve_Array* self) {{\n", mangled_name));
                self.body
                    .push_str("    return (int)ve_array_length(self);\n");
                self.body.push_str("}\n\n");
                Ok(true)
            }
            "append" => {
                let element_type = self.get_c_type_for_veil_type(inner_type);
                let append_func = self.get_append_function_for_type(inner_type);

                self.body.push_str(&format!(
                    "ve_Array* {}(ve_Array* self, {} item) {{\n",
                    mangled_name, element_type
                ));
                self.body
                    .push_str(&format!("    return {}(self, item);\n", append_func));
                self.body.push_str("}\n\n");
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    pub fn get_c_type_for_veil_type(&self, veil_type: &str) -> String {
        match veil_type {
            "i32" => "int".to_string(),
            "string" => "const char*".to_string(),
            "bool" => "bool".to_string(),
            "f32" => "float".to_string(),
            "f64" => "double".to_string(),
            _ => "void*".to_string(),
        }
    }

    pub fn get_append_function_for_type(&self, veil_type: &str) -> String {
        match veil_type {
            "i32" => "ve_array_append_i32".to_string(),
            "string" => "ve_array_append_string".to_string(),
            "bool" => "ve_array_append_bool".to_string(),
            _ => "ve_array_append_element".to_string(),
        }
    }

    pub fn generate_struct_to_str_functions<'a, I>(&mut self, structs: I)
    where
        I: IntoIterator<Item = &'a ast::StructDef>,
    {
        for struct_def in structs {
            let struct_name = &struct_def.name;
            self.header.push_str(&format!(
                "static char* ve_{}_to_str(const ve_{} *obj) {{\n",
                struct_name, struct_name
            ));
            self.header
                .push_str("    char *buffer = ve_arena_alloc(256);\n");
            self.header.push_str(&format!(
                "    if (!buffer) return \"<failed to allocate memory for {}>\";\n",
                struct_name
            ));
            let mut format_parts = Vec::new();
            let mut args = Vec::new();
            format_parts.push(format!("{}{{", struct_name));
            for (i, field) in struct_def.fields.iter().enumerate() {
                let (fmt, arg) = match field.ty {
                    Type::I32 => ("%d", format!("obj->{}", field.name)),
                    Type::Bool => (
                        "%s",
                        format!("(obj->{} ? \"true\" : \"false\")", field.name),
                    ),
                    Type::String => (
                        "%s",
                        format!("(obj->{} ? obj->{} : \"null\")", field.name, field.name),
                    ),
                    Type::Pointer(_) | Type::RawPtr => {
                        ("%p", format!("(void*)obj->{}", field.name))
                    }
                    Type::Struct(ref s) => ("%s", format!("ve_{}_to_str(&obj->{})", s, field.name)),
                    _ => ("?", "\"<unknown type>\"".to_string()),
                };
                if i > 0 {
                    format_parts.push(format!(" {}:{}", field.name, fmt));
                } else {
                    format_parts.push(format!("{}:{}", field.name, fmt));
                }
                args.push(arg);
            }
            format_parts.push("}".to_string());
            let format_str = format_parts.join("");
            self.header
                .push_str(&format!("    sprintf(buffer, \"{}\"", format_str));
            for arg in args {
                self.header.push_str(&format!(", {}", arg));
            }
            self.header.push_str(");\n");
            self.header.push_str("    return buffer;\n");
            self.header.push_str("}\n\n");
        }
    }
}
