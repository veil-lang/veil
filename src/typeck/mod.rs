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
    // Full struct definitions (keeps generics) for generic-aware lookups
    struct_def_map: HashMap<String, ast::StructDef>,
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
            struct_def_map: HashMap::new(),
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
            // Preserve full struct definition (including generic params)
            checker
                .struct_def_map
                .insert(struct_def.name.clone(), struct_def.clone());
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
            // Track full struct definitions for generic-aware operations
            self.struct_def_map
                .insert(struct_def.name.clone(), struct_def.clone());
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
            let _target_type = impl_block
                .target_type_parsed
                .clone()
                .unwrap_or_else(|| self.parse_type_name(&impl_block.target_type));

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
            self.check_function(func)?
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

            if !explicit_return_type
                && self.context.inferred_return_type.is_some()
                && inferred_return_type.is_none()
            {
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

        self.context = original_ctx;

        Ok(())
    }

    fn parse_type_name(&self, type_name: &str) -> Type {
        let s = type_name.trim();

        // Handle array prefix '[]T' and suffix 'T[]'
        if let Some(inner) = s.strip_prefix("[]") {
            let inner_type = self.parse_type_name(inner);
            return Type::Array(Box::new(inner_type));
        }

        if let Some(inner) = s.strip_suffix("[]") {
            let inner_type = self.parse_type_name(inner);
            return Type::Array(Box::new(inner_type));
        }

        // Handle generic instance like Foo<Bar, Baz[]>
        if let Some(lt_idx) = s.find('<') {
            if s.ends_with('>') {
                let base = s[..lt_idx].trim();
                let args_src = &s[lt_idx + 1..s.len() - 1];

                // Split args by commas, respecting nested angle brackets
                let mut depth = 0;
                let mut buf = String::new();
                let mut parts: Vec<String> = Vec::new();
                for ch in args_src.chars() {
                    match ch {
                        '<' => {
                            depth += 1;
                            buf.push(ch);
                        }
                        '>' => {
                            if depth > 0 {
                                depth -= 1;
                            }
                            buf.push(ch);
                        }
                        ',' if depth == 0 => {
                            parts.push(buf.trim().to_string());
                            buf.clear();
                        }
                        _ => buf.push(ch),
                    }
                }
                if !buf.trim().is_empty() {
                    parts.push(buf.trim().to_string());
                }

                let args: Vec<Type> = parts
                    .into_iter()
                    .map(|p| self.parse_type_name(&p))
                    .collect();

                if self.context.struct_defs.contains_key(base)
                    || self.context.enum_defs.contains_key(base)
                {
                    return Type::GenericInstance(base.to_string(), args);
                } else if base
                    .chars()
                    .next()
                    .map(|c| c.is_uppercase())
                    .unwrap_or(false)
                {
                    // Fallback to generic instance with unknown base if it looks like a type name
                    return Type::GenericInstance(base.to_string(), args);
                }
            }
        }

        match s {
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
                if self.context.struct_defs.contains_key(s) {
                    Type::Struct(s.to_string())
                } else if self.context.enum_defs.contains_key(s) {
                    Type::Enum(s.to_string())
                } else if s.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                    Type::Generic(s.to_string())
                } else {
                    Type::Unknown
                }
            }
        }
    }
}
