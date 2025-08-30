mod diagnostic;
mod expr;
mod pattern;
mod stmt;

use super::ast::{self, Type};
use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic;
use std::collections::HashMap;

#[derive(Debug)]
struct Context {
    variables: HashMap<String, Type>,
    current_return_type: Type,
    in_safe: bool,
    in_loop: bool,
    break_types: Vec<Type>,
    struct_defs: HashMap<String, Vec<(String, Type)>>,
    enum_defs: HashMap<String, Vec<String>>,
    enum_def_map: HashMap<String, ast::EnumDef>,
    inferring_return_type: bool,
    inferred_return_type: Option<Type>,
}

impl Context {
    fn new() -> Self {
        Context {
            variables: HashMap::new(),
            current_return_type: Type::Void,
            in_safe: false,
            in_loop: false,
            break_types: Vec::new(),
            struct_defs: HashMap::new(),
            enum_defs: HashMap::new(),
            enum_def_map: HashMap::new(),
            inferring_return_type: false,
            inferred_return_type: None,
        }
    }
}

#[derive(Debug)]
pub struct TypeChecker {
    errors: Vec<Diagnostic<FileId>>,
    context: Context,
    functions: HashMap<String, (Vec<Type>, Type)>,
    file_id: FileId,
    enums: Vec<ast::EnumDef>,
    impls: Vec<ast::ImplBlock>,
}

impl TypeChecker {
    pub fn new(
        file_id: FileId,
        imported_functions: HashMap<String, (Vec<Type>, Type)>,
        imported_structs: Vec<ast::StructDef>,
        imported_ffi_vars: Vec<ast::FfiVariable>,
    ) -> Self {
        let mut checker = TypeChecker {
            file_id,
            errors: Vec::new(),
            context: Context::new(),
            functions: imported_functions,
            enums: Vec::new(),
            impls: Vec::new(),
        };

        for struct_def in imported_structs {
            let fields: Vec<(String, Type)> = struct_def
                .fields
                .iter()
                .map(|f| (f.name.clone(), f.ty.clone()))
                .collect();
            checker
                .context
                .struct_defs
                .insert(struct_def.name.clone(), fields);
        }

        for ffi_var in imported_ffi_vars {
            checker.context.variables.insert(ffi_var.name, ffi_var.ty);
        }

        checker
    }

    pub fn check(&mut self, program: &mut ast::Program) -> Result<(), Vec<Diagnostic<FileId>>> {
        self.enums = program.enums.clone();
        self.impls = program.impls.clone();

        for ffi in &program.ffi_functions {
            self.functions.insert(
                ffi.name.clone(),
                (ffi.params.clone(), ffi.return_type.clone()),
            );
        }

        for ffi_var in &program.ffi_variables {
            self.context
                .variables
                .insert(ffi_var.name.clone(), ffi_var.ty.clone());
        }

        for struct_def in &program.structs {
            let fields: Vec<(String, Type)> = struct_def
                .fields
                .iter()
                .map(|f| (f.name.clone(), f.ty.clone()))
                .collect();
            self.context
                .struct_defs
                .insert(struct_def.name.clone(), fields);
        }

        for enum_def in &program.enums {
            let variants: Vec<String> = enum_def.variants.iter().map(|v| v.name.clone()).collect();
            self.context
                .enum_defs
                .insert(enum_def.name.clone(), variants);
            self.context
                .enum_def_map
                .insert(enum_def.name.clone(), enum_def.clone());
        }
        for func in &program.functions {
            let params: Vec<Type> = func.params.iter().map(|(_, t)| t.clone()).collect();
            self.functions
                .insert(func.name.clone(), (params, func.return_type.clone()));
        }

        for impl_block in &program.impls {
            let _target_type = self.parse_type_name(&impl_block.target_type);

            for method in &impl_block.methods {
                if method.name == "constructor" {
                    let params: Vec<Type> = method.params.iter().map(|(_, t)| t.clone()).collect();
                    let constructor_name = format!("{}.constructor", impl_block.target_type);
                    self.functions
                        .insert(constructor_name, (params, method.return_type.clone()));
                } else {
                    let params: Vec<Type> = method.params.iter().map(|(_, t)| t.clone()).collect();
                    let method_key = format!("{}.{}", impl_block.target_type, method.name);
                    self.functions
                        .insert(method_key, (params, method.return_type.clone()));
                }
            }
        }

        for stmt in &mut program.stmts {
            self.check_stmt(stmt)?;
        }

        for func in &mut program.functions {
            self.context.current_return_type = func.return_type.clone();
            if let Err(e) = self.check_function(func) {
                return Err(e);
            }
        }

        for impl_block in &mut program.impls {
            self.check_impl_block(impl_block)?;
        }

        for test in &mut program.tests {
            self.check_test(test)?;
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }
    fn check_function(&mut self, func: &mut ast::Function) -> Result<(), Vec<Diagnostic<FileId>>> {
        let mut local_ctx = Context::new();
        local_ctx.current_return_type = func.return_type.clone();
        local_ctx.struct_defs = self.context.struct_defs.clone();
        local_ctx.enum_defs = self.context.enum_defs.clone();
        local_ctx.enum_def_map = self.context.enum_def_map.clone();

        local_ctx.variables = self.context.variables.clone();

        for (name, ty) in &func.params {
            local_ctx.variables.insert(name.clone(), ty.clone());
        }

        let original_ctx = std::mem::replace(&mut self.context, local_ctx);

        let mut inferred_return_type: Option<Type> = None;
        let explicit_return_type = func.return_type != Type::Void;

        if !explicit_return_type {
            self.context.inferring_return_type = true;
        }

        for stmt in &mut func.body {
            self.check_stmt(stmt)?;

            if !explicit_return_type && self.context.inferred_return_type.is_some() {
                if inferred_return_type.is_none() {
                    inferred_return_type = self.context.inferred_return_type.clone();
                    self.context.current_return_type = inferred_return_type.clone().unwrap();
                    func.return_type = inferred_return_type.clone().unwrap();

                    if let Some((params, _)) = self.functions.get(&func.name).cloned() {
                        self.functions.insert(
                            func.name.clone(),
                            (params, inferred_return_type.clone().unwrap()),
                        );
                    }
                }
            }
        }

        self.context = original_ctx;

        Ok(())
    }

    fn parse_type_name(&self, type_name: &str) -> Type {
        if type_name.starts_with("[]") {
            let inner_type_name = &type_name[2..];
            let inner_type = self.parse_type_name(inner_type_name);
            return Type::Array(Box::new(inner_type));
        }

        if type_name.ends_with("[]") {
            let inner_type_name = &type_name[..type_name.len() - 2];
            let inner_type = self.parse_type_name(inner_type_name);
            return Type::Array(Box::new(inner_type));
        }

        match type_name {
            "string" => Type::String,
            "i32" => Type::I32,
            "i64" => Type::I64,
            "i8" => Type::I8,
            "i16" => Type::I16,
            "u8" => Type::U8,
            "u16" => Type::U16,
            "u32" => Type::U32,
            "u64" => Type::U64,
            "f32" => Type::F32,
            "f64" => Type::F64,
            "bool" => Type::Bool,
            "void" => Type::Void,
            _ => {
                if self.context.struct_defs.contains_key(type_name) {
                    Type::Struct(type_name.to_string())
                } else if self.context.enum_defs.contains_key(type_name) {
                    Type::Enum(type_name.to_string())
                } else if type_name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                    Type::Generic(type_name.to_string())
                } else {
                    Type::Unknown
                }
            }
        }
    }
}
