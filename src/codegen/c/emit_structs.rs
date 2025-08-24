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
            if let Some(data_types) = &variant.data {
                if !data_types.is_empty() {
                    self.header.push_str("        struct {\n");
                    for (i, ty) in data_types.iter().enumerate() {
                        let concrete_ty = if let Some(idx) = enum_def
                            .generic_params
                            .iter()
                            .position(|gp| gp == &ty.to_string())
                        {
                            type_args.get(idx).unwrap_or(ty)
                        } else {
                            ty
                        };
                        let c_type = self.type_to_c(concrete_ty);
                        self.header
                            .push_str(&format!("            {} field{};\n", c_type, i));
                    }
                    self.header
                        .push_str(&format!("        }} {};\n", variant.name.to_lowercase()));
                }
            }
        }

        self.header.push_str("    } data;\n");
        self.header.push_str(&format!("}} {};\n\n", enum_name));

        for variant in &enum_def.variants {
            if let Some(data_types) = &variant.data {
                if !data_types.is_empty() {
                    let mut params = Vec::new();
                    for (i, ty) in data_types.iter().enumerate() {
                        let concrete_ty = if let Some(idx) = enum_def
                            .generic_params
                            .iter()
                            .position(|gp| gp == &ty.to_string())
                        {
                            type_args.get(idx).unwrap_or(ty)
                        } else {
                            ty
                        };
                        params.push(format!("{} field{}", self.type_to_c(concrete_ty), i));
                    }

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

                    for (i, _) in data_types.iter().enumerate() {
                        self.header.push_str(&format!(
                            "    result.data.{}.field{} = field{};\n",
                            variant.name.to_lowercase(),
                            i,
                            i
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
