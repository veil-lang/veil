use crate::ast;
use crate::ast::Type;
use crate::codegen::CompileError;
use crate::codegen::c::CBackend;

impl CBackend {
    pub fn emit_struct(&mut self, struct_def: &ast::StructDef) -> Result<(), CompileError> {
        let struct_name = format!("ve_{}", struct_def.name);
        let mut struct_code = format!("typedef struct {} {{\n", struct_name);

        for field in &struct_def.fields {
            let field_type = self.type_to_c(&field.ty);
            struct_code.push_str(&format!("    {} {};\n", field_type, field.name));
        }

        struct_code.push_str(&format!("}} {};\n\n", struct_name));
        self.header.push_str(&struct_code);

        Ok(())
    }

    pub fn emit_enum(&mut self, enum_def: &ast::EnumDef) -> Result<(), CompileError> {
        if !enum_def.generic_params.is_empty() {
            return Ok(());
        }

        self.emit_enum_impl(enum_def, &[], &enum_def.name)
    }

    fn sanitize_member_name(&self, name: &str) -> String {
        match name.to_lowercase().as_str() {
            "int" => "int_val".to_string(),
            "char" => "char_val".to_string(),
            "float" => "float_val".to_string(),
            "double" => "double_val".to_string(),
            "struct" => "struct_val".to_string(),
            "union" => "union_val".to_string(),
            "enum" => "enum_val".to_string(),
            "const" => "const_val".to_string(),
            "static" => "static_val".to_string(),
            "void" => "void_val".to_string(),
            "if" => "if_val".to_string(),
            "else" => "else_val".to_string(),
            "while" => "while_val".to_string(),
            "for" => "for_val".to_string(),
            "return" => "return_val".to_string(),
            "break" => "break_val".to_string(),
            "continue" => "continue_val".to_string(),
            "switch" => "switch_val".to_string(),
            "case" => "case_val".to_string(),
            "default" => "default_val".to_string(),
            "typedef" => "typedef_val".to_string(),
            "sizeof" => "sizeof_val".to_string(),
            _ => name.to_lowercase(),
        }
    }

    pub fn emit_enum_impl(
        &mut self,
        enum_def: &ast::EnumDef,
        type_args: &[Type],
        base_name: &str,
    ) -> Result<(), CompileError> {
        let enum_name = format!("ve_{}", base_name);

        let is_simple_enum = enum_def.variants.iter().all(|v| v.data.is_none());

        if is_simple_enum {
            self.header.push_str(&format!("typedef enum {{\n"));
            for variant in &enum_def.variants {
                if let Some(value) = variant.value {
                    self.header.push_str(&format!(
                        "    {}_{} = {},\n",
                        enum_name, variant.name, value
                    ));
                } else {
                    self.header
                        .push_str(&format!("    {}_{},\n", enum_name, variant.name));
                }
            }
            self.header.push_str(&format!("}} {};\n\n", enum_name));
            return Ok(());
        }

        self.header.push_str(&format!("typedef enum {{\n"));
        for (i, variant) in enum_def.variants.iter().enumerate() {
            self.header
                .push_str(&format!("    {}_{} = {},\n", enum_name, variant.name, i));
        }
        self.header.push_str(&format!("}} {}_Tag;\n\n", enum_name));

        self.header
            .push_str(&format!("typedef struct {} {{\n", enum_name));
        self.header
            .push_str(&format!("    {}_Tag tag;\n", enum_name));
        self.header.push_str("    union {\n");

        for variant in &enum_def.variants {
            if let Some(data) = &variant.data {
                let mut fields: Vec<(String, &ast::Type)> = Vec::new();
                match data {
                    crate::ast::EnumVariantData::Tuple(types) => {
                        for (i, ty) in types.iter().enumerate() {
                            let concrete_ty = if let Some(idx) = enum_def
                                .generic_params
                                .iter()
                                .position(|gp| gp == &ty.to_string())
                            {
                                type_args.get(idx).unwrap_or(ty)
                            } else {
                                ty
                            };
                            fields.push((format!("field{}", i), concrete_ty));
                        }
                    }
                    crate::ast::EnumVariantData::Struct(struct_fields) => {
                        for f in struct_fields.iter() {
                            let concrete_ty = if let Some(idx) = enum_def
                                .generic_params
                                .iter()
                                .position(|gp| gp == &f.ty.to_string())
                            {
                                type_args.get(idx).unwrap_or(&f.ty)
                            } else {
                                &f.ty
                            };
                            fields.push((self.sanitize_member_name(&f.name), concrete_ty));
                        }
                    }
                }

                if !fields.is_empty() {
                    self.header.push_str("        struct {\n");
                    for (name, ty) in fields.iter() {
                        let c_type = self.type_to_c(ty);
                        self.header
                            .push_str(&format!("            {} {};\n", c_type, name));
                    }
                    self.header
                        .push_str(&format!("        }} {};\n", self.sanitize_member_name(&variant.name)));
                }
            }
        }

        self.header.push_str("    } data;\n");
        self.header.push_str(&format!("}} {};\n\n", enum_name));

        for variant in &enum_def.variants {
            if let Some(data) = &variant.data {
                let mut params: Vec<String> = Vec::new();
                let mut assignments: Vec<(String, String)> = Vec::new();
                match data {
                    crate::ast::EnumVariantData::Tuple(types) => {
                        for (i, ty) in types.iter().enumerate() {
                            let concrete_ty = if let Some(idx) = enum_def
                                .generic_params
                                .iter()
                                .position(|gp| gp == &ty.to_string())
                            {
                                type_args.get(idx).unwrap_or(ty)
                            } else {
                                ty
                            };
                            let cname = self.type_to_c(concrete_ty);
                            params.push(format!("{} field{}", cname, i));
                            assignments.push((format!("field{}", i), format!("field{}", i)));
                        }
                    }
                    crate::ast::EnumVariantData::Struct(struct_fields) => {
                        for f in struct_fields.iter() {
                            let concrete_ty = if let Some(idx) = enum_def
                                .generic_params
                                .iter()
                                .position(|gp| gp == &f.ty.to_string())
                            {
                                type_args.get(idx).unwrap_or(&f.ty)
                            } else {
                                &f.ty
                            };
                            let cname = self.type_to_c(concrete_ty);
                            let param_name = self.sanitize_member_name(&f.name);
                            params.push(format!("{} {}", cname, param_name));
                            assignments.push((param_name.clone(), param_name));
                        }
                    }
                }

                if !params.is_empty() {
                    self.header.push_str(&format!(
                        "static {} {}_{}_new({}) {{\n",
                        enum_name,
                        enum_name,
                        variant.name,
                        params.join(", ")
                    ));
                    self.header
                        .push_str(&format!("    {} result;\n", enum_name));
                    self.header.push_str(&format!(
                        "    result.tag = {}_{};\n",
                        enum_name, variant.name
                    ));

                    for (field_member, param_name) in assignments.iter() {
                        self.header.push_str(&format!(
                            "    result.data.{}.{} = {};\n",
                            self.sanitize_member_name(&variant.name),
                            field_member,
                            param_name
                        ));
                    }

                    self.header.push_str("    return result;\n");
                    self.header.push_str("}\n\n");
                } else {
                    self.header.push_str(&format!(
                        "static {} {}_{}_new() {{\n",
                        enum_name, enum_name, variant.name
                    ));
                    self.header
                        .push_str(&format!("    {} result;\n", enum_name));
                    self.header.push_str(&format!(
                        "    result.tag = {}_{};\n",
                        enum_name, variant.name
                    ));
                    self.header.push_str("    return result;\n");
                    self.header.push_str("}\n\n");
                }
            } else {
                self.header.push_str(&format!(
                    "static {} {}_{}_new() {{\n",
                    enum_name, enum_name, variant.name
                ));
                self.header
                    .push_str(&format!("    {} result;\n", enum_name));
                self.header.push_str(&format!(
                    "    result.tag = {}_{};\n",
                    enum_name, variant.name
                ));
                self.header.push_str("    return result;\n");
                self.header.push_str("}\n\n");
            }
        }

        Ok(())
    }

    pub fn emit_generic_enum_instance(
        &mut self,
        enum_def: &ast::EnumDef,
        args: &[Type],
    ) -> Result<(), CompileError> {
        let mut name = enum_def.name.clone();
        for arg in args {
            name.push('_');
            name.push_str(&self.type_to_c_name(arg));
        }

        self.emit_enum_impl(enum_def, args, &name)
    }
}
