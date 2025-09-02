#![forbid(unsafe_code)]
#![allow(rust_2018_idioms)]
#![deny(unused_must_use)]

use std::collections::{HashMap, HashSet};
use std::fmt;

use veil_ast::ast;
use veil_ast::ast::{
    AstTransformer, AstVisitor, GenericCallCollector, GenericCallTransformer, Type,
};

/// Errors that can occur during monomorphization.
#[derive(Debug)]
pub enum MonoError {
    UnresolvedGenericType(String),
    Unsupported(String),
    Internal(String),
}

impl fmt::Display for MonoError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MonoError::UnresolvedGenericType(name) => {
                write!(f, "Unresolved generic type parameter: {}", name)
            }
            MonoError::Unsupported(msg) => write!(f, "Unsupported monomorphization: {}", msg),
            MonoError::Internal(msg) => write!(f, "Internal monomorphization error: {}", msg),
        }
    }
}

impl std::error::Error for MonoError {}

/// Monomorphization options (reserved for future expansion).
#[derive(Debug, Clone)]
pub struct MonomorphizeOptions {
    // Placeholder for future flags, e.g., array/impl specialization
}

impl Default for MonomorphizeOptions {
    fn default() -> Self {
        MonomorphizeOptions {}
    }
}

/// Name mapping compatible with backend's type_to_c_name, used for deterministic
/// concrete struct/impl names (e.g., GenericInstance Foo<T=int> => "Foo_i32").
fn type_to_backend_name(ty: &Type) -> String {
    match ty {
        Type::I32 => "i32".to_string(),
        Type::Bool => "bool".to_string(),
        Type::String => "string".to_string(),
        Type::Void => "void".to_string(),
        Type::F32 => "f32".to_string(),
        Type::F64 => "f64".to_string(),
        Type::I8 => "i8".to_string(),
        Type::I16 => "i16".to_string(),
        Type::I64 => "i64".to_string(),
        Type::U8 => "u8".to_string(),
        Type::U16 => "u16".to_string(),
        Type::U32 => "u32".to_string(),
        Type::U64 => "u64".to_string(),
        Type::Struct(name) | Type::Enum(name) | Type::Generic(name) => name.clone(),
        Type::GenericInstance(name, args) => {
            let mut s = name.clone();
            for arg in args {
                s.push('_');
                s.push_str(&type_to_backend_name(arg));
            }
            s
        }
        Type::Array(inner) => format!("array_{}", type_to_backend_name(inner)),
        Type::Optional(inner) => format!("optional_{}", type_to_backend_name(inner)),
        Type::NoneType => "none".to_string(),
        Type::Pointer(inner) => format!("ptr_{}", type_to_backend_name(inner)),
        Type::SizedArray(inner, n) => format!("arr{n}_{}", type_to_backend_name(inner)),
        Type::CChar => "c_char".to_string(),
        Type::CInt => "c_int".to_string(),
        Type::CSize => "c_size".to_string(),
        Type::RawPtr => "rawptr".to_string(),
        Type::Any => "any".to_string(),
        Type::Unknown => "unknown".to_string(),
        Type::Ellipsis => "variadic".to_string(),
        Type::Function(args, ret) => {
            let args_str = args
                .iter()
                .map(|t| type_to_backend_name(t))
                .collect::<Vec<_>>()
                .join("_");
            let ret_str = type_to_backend_name(ret);
            format!("fn_{}_ret_{}", args_str, ret_str)
        }
    }
}

/// Convert a simple backend type name back to an AST Type for array specialization.
/// Only common cases are handled (i32/string/bool), otherwise assume a concrete struct.
fn backend_name_to_type(name: &str) -> ast::Type {
    match name {
        "i32" => ast::Type::I32,
        "string" => ast::Type::String,
        "bool" => ast::Type::Bool,
        "f32" => ast::Type::F32,
        "f64" => ast::Type::F64,
        "i8" => ast::Type::I8,
        "i16" => ast::Type::I16,
        "i64" => ast::Type::I64,
        "u8" => ast::Type::U8,
        "u16" => ast::Type::U16,
        "u32" => ast::Type::U32,
        "u64" => ast::Type::U64,
        _ => ast::Type::Struct(name.to_string()),
    }
}

/// Monomorphizer
///
/// Implements:
/// - Function monomorphization (call-driven)
/// - Generic struct instantiation (concrete defs appended)
/// - Impl specialization:
///   - Array impls (T[] / []T) specialized per concrete element type
///   - Generic struct impls specialized per concrete instances discovered
#[derive(Debug, Default, Clone, serde::Serialize)]
pub struct MonoMetadata {
    pub functions_instantiated: Vec<String>,
    pub struct_instances: Vec<String>,
    pub array_impls: Vec<String>,
    pub generic_impls: Vec<String>,
}

pub struct Monomorphizer {
    _options: MonomorphizeOptions,
    fn_cache: std::cell::RefCell<std::collections::HashSet<(String, Vec<Type>)>>,
    struct_cache: std::cell::RefCell<std::collections::HashSet<String>>,
    array_impl_cache: std::cell::RefCell<std::collections::HashSet<String>>,
    generic_impl_cache: std::cell::RefCell<std::collections::HashSet<String>>,
}

impl Monomorphizer {
    pub fn monomorphize_with_metadata(
        &self,
        program: &ast::Program,
    ) -> Result<(ast::Program, MonoMetadata), MonoError> {
        let out = self.monomorphize_program(program)?;

        // Best-effort metadata extraction (deterministic and cacheable)
        let mut meta = MonoMetadata::default();

        // Functions: detect instantiated generics via our mangling convention "__"
        for f in &out.functions {
            if f.name.contains("__") {
                meta.functions_instantiated.push(f.name.clone());
            }
        }

        // Struct instances: names derived from backend-style type_to_c_name/type_to_backend_name
        for s in &out.structs {
            if s.name.contains('_') {
                meta.struct_instances.push(s.name.clone());
            }
        }

        // Impl blocks: array specializations and generic-struct specializations
        for ib in &out.impls {
            if ib.target_type.starts_with("array_") {
                meta.array_impls.push(ib.target_type.clone());
            } else if ib.target_type.contains('_') {
                meta.generic_impls.push(ib.target_type.clone());
            }
        }

        meta.functions_instantiated.sort();
        meta.functions_instantiated.dedup();
        meta.struct_instances.sort();
        meta.struct_instances.dedup();
        meta.array_impls.sort();
        meta.array_impls.dedup();
        meta.generic_impls.sort();
        meta.generic_impls.dedup();

        Ok((out, meta))
    }
    pub fn new(options: MonomorphizeOptions) -> Self {
        Self {
            _options: options,
            fn_cache: Default::default(),
            struct_cache: Default::default(),
            array_impl_cache: Default::default(),
            generic_impl_cache: Default::default(),
        }
    }

    /// Entry point: monomorphize a whole Program (AST) and return a transformed Program.
    pub fn monomorphize_program(&self, program: &ast::Program) -> Result<ast::Program, MonoError> {
        // 1) Collect all generic functions present in the program
        let all_generic_functions: Vec<_> = program
            .functions
            .iter()
            .filter(|f| !f.generic_params.is_empty())
            .collect();

        // 2) Collect generic call sites + any instances referenced in expression types
        let mut collector = GenericCallCollector::with_functions(
            &all_generic_functions
                .iter()
                .map(|f| (*f).clone())
                .collect::<Vec<_>>(),
        );
        collector.visit_program(program);

        // 3) Begin with non-generic functions as the base set
        let mut new_functions = program
            .functions
            .iter()
            .filter(|f| f.generic_params.is_empty())
            .cloned()
            .collect::<Vec<_>>();

        // Transformer that will remap generic call names -> instantiated names
        let mut transformer = GenericCallTransformer::new();

        // Map for looking up generic function definitions by name
        let generic_func_map: HashMap<_, _> = all_generic_functions
            .iter()
            .map(|f| (f.name.clone(), *f))
            .collect();

        // Avoid duplicate instantiation of the same (function_name, inferred_type_args)
        let mut seen_ref = self.fn_cache.borrow_mut();

        // 4) For each call site, infer and instantiate
        for (func_name, call_arg_types) in &collector.generic_calls {
            if let Some(gen_func) = generic_func_map.get(func_name) {
                // Infer generic args by unifying each declared param type with the call arg type.
                let mut subst: HashMap<String, Type> = HashMap::new();
                let mut ok = true;

                for ((_, param_ty), arg_ty) in gen_func.params.iter().zip(call_arg_types.iter()) {
                    if !unify_types(param_ty, arg_ty, &subst) {
                        ok = false;
                        break;
                    }
                }
                if !ok {
                    continue;
                }

                // Produce ordered type args according to the function's generic parameter list.
                let mut inferred: Vec<Type> = Vec::new();
                for gp in &gen_func.generic_params {
                    if let Some(t) = subst.get(gp) {
                        inferred.push(t.clone());
                    } else {
                        ok = false;
                        break;
                    }
                }
                if !ok {
                    continue;
                }

                if !seen_ref.insert((func_name.clone(), inferred.clone())) {
                    continue;
                }

                // Instantiate function
                let mono_func = instantiate_generic_function(gen_func, &inferred)?;
                let mono_name = mono_func.name.clone();

                // Record mapping for call rewriting
                transformer.add_mapping(func_name.clone(), call_arg_types.clone(), mono_name);
                new_functions.push(mono_func);
            }
        }

        // 5) Rewrite calls in the program to refer to instantiated names
        let mut out_program = transformer.transform_program(ast::Program {
            functions: new_functions,
            ..program.clone()
        });

        // 6) Discover array inner types and generic struct instances across program
        let mut concrete_array_inners: HashSet<String> = HashSet::new();

        fn collect_types_in_expr(expr: &ast::Expr, out: &mut HashSet<String>) {
            use ast::Expr;
            match expr {
                Expr::ArrayInit(elems, _) => {
                    for e in elems {
                        collect_types_in_expr(e, out);
                    }
                }
                Expr::ArrayAccess(a, _, _) => {
                    collect_types_in_expr(a, out);
                }
                Expr::Call(_, args, info) => {
                    if let ast::Type::Array(inner) = &info.ty {
                        out.insert(type_to_backend_name(inner));
                    }
                    for a in args {
                        collect_types_in_expr(a, out);
                    }
                }
                Expr::New(_, args, _) => {
                    for a in args {
                        collect_types_in_expr(a, out);
                    }
                }
                Expr::BinOp(l, _, r, _) => {
                    collect_types_in_expr(l, out);
                    collect_types_in_expr(r, out);
                }
                Expr::UnaryOp(_, e, _) => collect_types_in_expr(e, out),
                Expr::StructInit(_, fields, _) => {
                    for (_, e) in fields {
                        collect_types_in_expr(e, out);
                    }
                }
                Expr::FieldAccess(e, _, _) => collect_types_in_expr(e, out),
                Expr::Match(_, arms, _) => {
                    for arm in arms {
                        match &arm.body {
                            ast::MatchArmBody::Expr(e) => collect_types_in_expr(e, out),
                            ast::MatchArmBody::Block(stmts) => {
                                for s in stmts {
                                    collect_types_in_stmt(s, out);
                                }
                            }
                        }
                    }
                }
                Expr::Cast(e, _, _) => collect_types_in_expr(e, out),
                Expr::Deref(e, _) => collect_types_in_expr(e, out),
                Expr::TemplateStr(parts, _) => {
                    for part in parts {
                        if let ast::TemplateStrPart::Expression(e) = part {
                            collect_types_in_expr(e, out);
                        }
                    }
                }
                Expr::SafeBlock(stmts, _) | Expr::Loop(stmts, _) => {
                    for s in stmts {
                        collect_types_in_stmt(s, out);
                    }
                }
                Expr::If(cond, then_b, else_b, _) => {
                    collect_types_in_expr(cond, out);
                    for s in then_b {
                        collect_types_in_stmt(s, out);
                    }
                    if let Some(stmts) = else_b {
                        for s in stmts {
                            collect_types_in_stmt(s, out);
                        }
                    }
                }
                _ => {}
            }
        }

        fn collect_types_in_stmt(stmt: &ast::Stmt, out: &mut HashSet<String>) {
            match stmt {
                ast::Stmt::Let(_, ty_opt, expr, _, _) => {
                    if let Some(ty) = ty_opt {
                        if let ast::Type::Array(inner) = ty {
                            out.insert(type_to_backend_name(inner));
                        }
                    }
                    collect_types_in_expr(expr, out);
                }
                ast::Stmt::Expr(expr, _) | ast::Stmt::Return(expr, _) => {
                    collect_types_in_expr(expr, out);
                }
                ast::Stmt::If(cond, then_b, else_b, _) => {
                    collect_types_in_expr(cond, out);
                    for s in then_b {
                        collect_types_in_stmt(s, out);
                    }
                    if let Some(stmts) = else_b {
                        for s in stmts {
                            collect_types_in_stmt(s, out);
                        }
                    }
                }
                ast::Stmt::Block(stmts, _) | ast::Stmt::Loop(stmts, _) => {
                    for s in stmts {
                        collect_types_in_stmt(s, out);
                    }
                }
                ast::Stmt::While(cond, body, _) => {
                    collect_types_in_expr(cond, out);
                    for s in body {
                        collect_types_in_stmt(s, out);
                    }
                }
                ast::Stmt::For(_, _, range, step, body, _) => {
                    collect_types_in_expr(range, out);
                    if let Some(st) = step {
                        collect_types_in_expr(st, out);
                    }
                    for s in body {
                        collect_types_in_stmt(s, out);
                    }
                }
                ast::Stmt::Break(expr_opt, _) => {
                    if let Some(e) = expr_opt {
                        collect_types_in_expr(e, out);
                    }
                }
                _ => {}
            }
        }

        for f in &out_program.functions {
            if let ast::Type::Array(inner) = &f.return_type {
                concrete_array_inners.insert(type_to_backend_name(inner));
            }
            for (_, p) in &f.params {
                if let ast::Type::Array(inner) = p {
                    concrete_array_inners.insert(type_to_backend_name(inner));
                }
            }
            for s in &f.body {
                collect_types_in_stmt(s, &mut concrete_array_inners);
            }
        }
        for s in &out_program.stmts {
            collect_types_in_stmt(s, &mut concrete_array_inners);
        }
        for t in &out_program.tests {
            for s in &t.stmts {
                collect_types_in_stmt(s, &mut concrete_array_inners);
            }
        }

        // 7) Generic struct instantiation from usages
        let mut generic_structs: HashMap<String, ast::StructDef> = out_program
            .structs
            .iter()
            .filter(|s| !s.generic_params.is_empty())
            .map(|s| (s.name.clone(), s.clone()))
            .collect();

        fn collect_struct_instances_in_expr(
            expr: &ast::Expr,
            out: &mut HashSet<ast::Type>,
            generic_structs: &HashMap<String, ast::StructDef>,
        ) {
            let ty = expr.get_type();
            if let ast::Type::GenericInstance(name, _) = &ty {
                if generic_structs.contains_key(name) {
                    out.insert(ty);
                }
            }

            use ast::Expr;
            match expr {
                Expr::Call(_, args, _) => {
                    for a in args {
                        collect_struct_instances_in_expr(a, out, generic_structs);
                    }
                }
                Expr::BinOp(l, _, r, _) => {
                    collect_struct_instances_in_expr(l, out, generic_structs);
                    collect_struct_instances_in_expr(r, out, generic_structs);
                }
                Expr::UnaryOp(_, e, _) => collect_struct_instances_in_expr(e, out, generic_structs),
                Expr::StructInit(_, fields, _) => {
                    for (_, e) in fields {
                        collect_struct_instances_in_expr(e, out, generic_structs);
                    }
                }
                Expr::ArrayInit(elems, _) => {
                    for e in elems {
                        collect_struct_instances_in_expr(e, out, generic_structs);
                    }
                }
                Expr::ArrayAccess(a, b, _) => {
                    collect_struct_instances_in_expr(a, out, generic_structs);
                    collect_struct_instances_in_expr(b, out, generic_structs);
                }
                Expr::Cast(e, _, _) => collect_struct_instances_in_expr(e, out, generic_structs),
                Expr::Assign(a, b, _) => {
                    collect_struct_instances_in_expr(a, out, generic_structs);
                    collect_struct_instances_in_expr(b, out, generic_structs);
                }
                Expr::Deref(e, _) => collect_struct_instances_in_expr(e, out, generic_structs),
                Expr::Range(a, b, _, _) => {
                    collect_struct_instances_in_expr(a, out, generic_structs);
                    collect_struct_instances_in_expr(b, out, generic_structs);
                }
                Expr::FieldAccess(e, _, _) => {
                    collect_struct_instances_in_expr(e, out, generic_structs)
                }
                Expr::TemplateStr(parts, _) => {
                    for part in parts {
                        if let ast::TemplateStrPart::Expression(e) = part {
                            collect_struct_instances_in_expr(e, out, generic_structs);
                        }
                    }
                }
                Expr::Match(e, arms, _) => {
                    collect_struct_instances_in_expr(e, out, generic_structs);
                    for arm in arms {
                        match &arm.body {
                            ast::MatchArmBody::Expr(e) => {
                                collect_struct_instances_in_expr(e, out, generic_structs)
                            }
                            ast::MatchArmBody::Block(stmts) => {
                                for s in stmts {
                                    collect_struct_instances_in_stmt(s, out, generic_structs);
                                }
                            }
                        }
                    }
                }
                Expr::SafeBlock(stmts, _) | Expr::Loop(stmts, _) => {
                    for s in stmts {
                        collect_struct_instances_in_stmt(s, out, generic_structs);
                    }
                }
                Expr::If(cond, then_b, else_b, _) => {
                    collect_struct_instances_in_expr(cond, out, generic_structs);
                    for s in then_b {
                        collect_struct_instances_in_stmt(s, out, generic_structs);
                    }
                    if let Some(stmts) = else_b {
                        for s in stmts {
                            collect_struct_instances_in_stmt(s, out, generic_structs);
                        }
                    }
                }
                _ => {}
            }
        }

        fn collect_struct_instances_in_stmt(
            stmt: &ast::Stmt,
            out: &mut HashSet<ast::Type>,
            generic_structs: &HashMap<String, ast::StructDef>,
        ) {
            match stmt {
                ast::Stmt::Let(_, ty_opt, expr, _, _) => {
                    if let Some(ty) = ty_opt {
                        if let ast::Type::GenericInstance(name, _) = ty {
                            if generic_structs.contains_key(name) {
                                out.insert(ty.clone());
                            }
                        }
                    }
                    collect_struct_instances_in_expr(expr, out, generic_structs);
                }
                ast::Stmt::Expr(expr, _) | ast::Stmt::Return(expr, _) => {
                    collect_struct_instances_in_expr(expr, out, generic_structs);
                }
                ast::Stmt::If(cond, then_b, else_b, _) => {
                    collect_struct_instances_in_expr(cond, out, generic_structs);
                    for s in then_b {
                        collect_struct_instances_in_stmt(s, out, generic_structs);
                    }
                    if let Some(stmts) = else_b {
                        for s in stmts {
                            collect_struct_instances_in_stmt(s, out, generic_structs);
                        }
                    }
                }
                ast::Stmt::Block(stmts, _) | ast::Stmt::Loop(stmts, _) => {
                    for s in stmts {
                        collect_struct_instances_in_stmt(s, out, generic_structs);
                    }
                }
                ast::Stmt::While(cond, body, _) => {
                    collect_struct_instances_in_expr(cond, out, generic_structs);
                    for s in body {
                        collect_struct_instances_in_stmt(s, out, generic_structs);
                    }
                }
                ast::Stmt::For(_, _, range, step, body, _) => {
                    collect_struct_instances_in_expr(range, out, generic_structs);
                    if let Some(st) = step {
                        collect_struct_instances_in_expr(st, out, generic_structs);
                    }
                    for s in body {
                        collect_struct_instances_in_stmt(s, out, generic_structs);
                    }
                }
                ast::Stmt::Break(expr_opt, _) => {
                    if let Some(e) = expr_opt {
                        collect_struct_instances_in_expr(e, out, generic_structs);
                    }
                }
                _ => {}
            }
        }

        let mut struct_instances: HashSet<ast::Type> = HashSet::new();
        for f in &out_program.functions {
            // From signatures
            if let ast::Type::GenericInstance(name, _) = &f.return_type {
                if generic_structs.contains_key(name) {
                    struct_instances.insert(f.return_type.clone());
                }
            }
            for (_, p) in &f.params {
                if let ast::Type::GenericInstance(name, _) = p {
                    if generic_structs.contains_key(name) {
                        struct_instances.insert(p.clone());
                    }
                }
            }
            // From bodies
            for s in &f.body {
                collect_struct_instances_in_stmt(s, &mut struct_instances, &generic_structs);
            }
        }
        for s in &out_program.stmts {
            collect_struct_instances_in_stmt(s, &mut struct_instances, &generic_structs);
        }
        for t in &out_program.tests {
            for s in &t.stmts {
                collect_struct_instances_in_stmt(s, &mut struct_instances, &generic_structs);
            }
        }

        // 8) Instantiate concrete struct definitions from generic ones
        let mut extra_structs: Vec<ast::StructDef> = Vec::new();
        let mut seen_struct_names = HashSet::new();

        for ty in &struct_instances {
            if let ast::Type::GenericInstance(name, args) = ty {
                if let Some(def) = generic_structs.get(name) {
                    let mut new_fields = Vec::new();
                    for f in &def.fields {
                        let new_ty = substitute_type_using_struct_generics(&f.ty, def, args)?;
                        new_fields.push(ast::StructField {
                            name: f.name.clone(),
                            ty: new_ty,
                            span: f.span,
                        });
                    }
                    let concrete_name = type_to_backend_name(ty);
                    if seen_struct_names.insert(concrete_name.clone())
                        && !self.struct_cache.borrow().contains(&concrete_name)
                    {
                        self.struct_cache.borrow_mut().insert(concrete_name.clone());
                        extra_structs.push(ast::StructDef {
                            name: concrete_name,
                            generic_params: vec![],
                            fields: new_fields,
                            span: def.span,
                            visibility: def.visibility.clone(),
                            repr: def.repr.clone(),
                        });
                    }
                }
            }
        }

        // 9) Specialize impl blocks: arrays and generic structs
        let mut new_impls: Vec<ast::ImplBlock> = Vec::new();
        let existing_impl_targets: HashSet<String> = out_program
            .impls
            .iter()
            .map(|ib| ib.target_type.clone())
            .collect();

        // Prefer some common array inners
        concrete_array_inners.insert("i32".to_string());
        concrete_array_inners.insert("string".to_string());
        concrete_array_inners.insert("bool".to_string());

        let mut created_array_impls: HashSet<String> = HashSet::new();

        for impl_block in &out_program.impls {
            // (a) Array impls of the form "T[]" or "[]T"
            let is_generic_array_impl =
                impl_block.target_type.contains("[]") && impl_block.target_type.contains('T');

            if is_generic_array_impl {
                let concrete_inners: Vec<String> = concrete_array_inners
                    .iter()
                    .filter(|s| *s != "T")
                    .cloned()
                    .collect();

                for inner_name in &concrete_inners {
                    if created_array_impls.contains(inner_name)
                        || self.array_impl_cache.borrow().contains(inner_name)
                    {
                        continue;
                    }
                    if existing_impl_targets.contains(&format!("{}[]", inner_name))
                        || existing_impl_targets.contains(&format!("[]{}", inner_name))
                    {
                        continue;
                    }
                    created_array_impls.insert(inner_name.clone());
                    self.array_impl_cache
                        .borrow_mut()
                        .insert(inner_name.clone());
                    let mut type_map = HashMap::new();
                    let concrete_type = backend_name_to_type(inner_name);

                    type_map.insert("T".to_string(), concrete_type.clone());

                    let mut new_methods = Vec::new();
                    for m in &impl_block.methods {
                        let mut new_m = m.clone();
                        new_m.return_type = substitute_type(&m.return_type, &type_map)?;
                        let mut new_params = Vec::new();
                        for (n, t) in &m.params {
                            new_params.push((n.clone(), substitute_type(t, &type_map)?));
                        }
                        new_m.params = new_params;

                        let mut new_body = Vec::new();
                        for stmt in &m.body {
                            new_body.push(substitute_stmt(stmt, &type_map)?);
                        }
                        new_m.body = new_body;
                        new_methods.push(new_m);
                    }

                    let target_name = format!("array_{}", inner_name);
                    new_impls.push(ast::ImplBlock {
                        target_type: target_name,
                        target_type_parsed: Some(ast::Type::Array(Box::new(concrete_type))),
                        methods: new_methods,
                        span: impl_block.span,
                    });
                }
                // Don't keep the original generic array impl
            } else if impl_block.target_type.contains('<') && impl_block.target_type.ends_with('>')
            {
                // (b) Generic struct impls: impl Foo<T> { ... } -> specialize per instance of Foo<X>
                let s = impl_block.target_type.as_str();
                if let Some(lt) = s.find('<') {
                    let base = s[..lt].trim();
                    // let params_src = &s[lt + 1..s.len() - 1]; // not used directly
                    for inst in &struct_instances {
                        if let ast::Type::GenericInstance(inst_base, inst_args) = inst {
                            if inst_base == base {
                                if let Some(def) = generic_structs.get(base) {
                                    let mut new_methods = Vec::new();
                                    for m in &impl_block.methods {
                                        let mut new_m = m.clone();
                                        new_m.return_type = substitute_type_using_struct_generics(
                                            &m.return_type,
                                            def,
                                            inst_args,
                                        )?;
                                        let mut new_params = Vec::new();
                                        for (n, t) in &m.params {
                                            new_params.push((
                                                n.clone(),
                                                substitute_type_using_struct_generics(
                                                    t, def, inst_args,
                                                )?,
                                            ));
                                        }
                                        new_m.params = new_params;

                                        // Substitute inside statements using a struct generic map
                                        let mut map = HashMap::new();
                                        for (gp, arg) in
                                            def.generic_params.iter().zip(inst_args.iter())
                                        {
                                            map.insert(gp.clone(), arg.clone());
                                        }
                                        let mut new_body = Vec::new();
                                        for stmt in &m.body {
                                            new_body.push(substitute_stmt(stmt, &map)?);
                                        }
                                        new_m.body = new_body;

                                        new_methods.push(new_m);
                                    }

                                    let concrete_name =
                                        type_to_backend_name(&ast::Type::GenericInstance(
                                            inst_base.clone(),
                                            inst_args.clone(),
                                        ));
                                    if !self.generic_impl_cache.borrow().contains(&concrete_name) {
                                        self.generic_impl_cache
                                            .borrow_mut()
                                            .insert(concrete_name.clone());
                                        new_impls.push(ast::ImplBlock {
                                            target_type: concrete_name,
                                            target_type_parsed: None, // optional
                                            methods: new_methods,
                                            span: impl_block.span,
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                // Keep non-generic, non-array impls as-is
                new_impls.push(impl_block.clone());
            }
        }

        // 10) Finalize program with concrete structs and impls
        out_program.structs.extend(extra_structs);
        out_program.impls = new_impls;

        Ok(out_program)
    }
}

/// Attempt to unify a generic pattern type with a concrete call-site type.
/// Fills `subst` with inferred generic parameter -> concrete type mappings.
///
/// Returns true when the types can be unified, false otherwise.
fn unify_types(pattern: &Type, concrete: &Type, subst: &HashMap<String, Type>) -> bool {
    // Convert immutable subst to mutable for recursion; helper delegates to internal fn
    fn go(pattern: &Type, concrete: &Type, subst: &mut HashMap<String, Type>) -> bool {
        match (pattern, concrete) {
            // Bind generic to the concrete type (if not already bound) or ensure consistency
            (Type::Generic(name), c) => {
                if let Some(existing) = subst.get(name) {
                    existing == c
                } else {
                    subst.insert(name.clone(), c.clone());
                    true
                }
            }
            // Optional patterns: accept both Optional(X) vs Optional(Y) and Optional(X) vs Y
            (Type::Optional(p), Type::Optional(c)) => go(p, c, subst),
            (Type::Optional(p), c) => go(p, c, subst),

            // Structural recursion
            (Type::Array(p), Type::Array(c)) => go(p, c, subst),
            (Type::Pointer(p), Type::Pointer(c)) => go(p, c, subst),
            (Type::SizedArray(p, n1), Type::SizedArray(c, n2)) => n1 == n2 && go(p, c, subst),

            (Type::GenericInstance(n1, a1), Type::GenericInstance(n2, a2)) => {
                if n1 != n2 || a1.len() != a2.len() {
                    return false;
                }
                for (pp, cc) in a1.iter().zip(a2.iter()) {
                    if !go(pp, cc, subst) {
                        return false;
                    }
                }
                true
            }

            // Allow exact concrete matches to pass
            (p, c) if p == c => true,

            // Keep inference simple: only identical shapes or generic binds
            _ => false,
        }
    }

    let mut owned = subst.clone();
    go(pattern, concrete, &mut owned)
}

/// Instantiate a generic function with concrete type arguments, producing a monomorphic function.
/// The new function name is mangled to encode the concrete types.
fn instantiate_generic_function(
    generic_func: &ast::Function,
    type_args: &[Type],
) -> Result<ast::Function, MonoError> {
    // Generate a deterministic mangled name based on type args
    let mangled_name = {
        let mut s = generic_func.name.clone();
        s.push_str("__");
        for (i, t) in type_args.iter().enumerate() {
            if i > 0 {
                s.push('_');
            }
            s.push_str(&type_to_mono_suffix(t));
        }
        s
    };

    // Build substitution map generic_param -> concrete type
    let mut type_map: HashMap<String, Type> = HashMap::new();
    for (gp, arg) in generic_func.generic_params.iter().zip(type_args.iter()) {
        type_map.insert(gp.clone(), arg.clone());
    }

    // Substitute in parameters, return type, and body
    let substituted_params = generic_func
        .params
        .iter()
        .map(|(name, ty)| Ok::<_, MonoError>((name.clone(), substitute_type(ty, &type_map)?)))
        .collect::<Result<Vec<_>, _>>()?;

    let substituted_return_type = substitute_type(&generic_func.return_type, &type_map)?;

    let substituted_body = generic_func
        .body
        .iter()
        .map(|stmt| substitute_stmt(stmt, &type_map))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(ast::Function {
        name: mangled_name,
        generic_params: Vec::new(),
        params: substituted_params,
        return_type: substituted_return_type,
        body: substituted_body,
        span: generic_func.span,
        visibility: generic_func.visibility.clone(),
    })
}

fn substitute_type_using_struct_generics(
    ty: &ast::Type,
    def: &ast::StructDef,
    args: &[ast::Type],
) -> Result<ast::Type, MonoError> {
    let mut map = HashMap::new();
    for (gp, arg) in def.generic_params.iter().zip(args.iter()) {
        map.insert(gp.clone(), arg.clone());
    }
    substitute_type(ty, &map)
}

fn substitute_type(
    ty: &ast::Type,
    type_map: &HashMap<String, ast::Type>,
) -> Result<ast::Type, MonoError> {
    match ty {
        ast::Type::Generic(name) => type_map
            .get(name)
            .cloned()
            .ok_or_else(|| MonoError::UnresolvedGenericType(name.clone())),
        ast::Type::Array(inner) => Ok(ast::Type::Array(Box::new(substitute_type(
            inner, type_map,
        )?))),
        ast::Type::SizedArray(inner, size) => Ok(ast::Type::SizedArray(
            Box::new(substitute_type(inner, type_map)?),
            *size,
        )),
        ast::Type::GenericInstance(name, args) => {
            let mut substituted_args = Vec::new();
            for arg in args {
                substituted_args.push(substitute_type(arg, type_map)?);
            }
            Ok(ast::Type::GenericInstance(name.clone(), substituted_args))
        }
        ast::Type::Optional(inner) => Ok(ast::Type::Optional(Box::new(substitute_type(
            inner, type_map,
        )?))),
        ast::Type::Pointer(inner) => Ok(ast::Type::Pointer(Box::new(substitute_type(
            inner, type_map,
        )?))),
        _ => Ok(ty.clone()),
    }
}

fn substitute_stmt(
    stmt: &ast::Stmt,
    type_map: &HashMap<String, ast::Type>,
) -> Result<ast::Stmt, MonoError> {
    match stmt {
        ast::Stmt::Let(name, ty_opt, expr, span, visibility) => {
            let new_ty = if let Some(ty) = ty_opt.as_ref() {
                Some(substitute_type(ty, type_map)?)
            } else {
                None
            };
            let new_expr = substitute_expr(expr, type_map)?;
            Ok(ast::Stmt::Let(
                name.clone(),
                new_ty,
                new_expr,
                *span,
                visibility.clone(),
            ))
        }
        ast::Stmt::Expr(expr, span) => Ok(ast::Stmt::Expr(substitute_expr(expr, type_map)?, *span)),
        ast::Stmt::Return(expr, span) => {
            Ok(ast::Stmt::Return(substitute_expr(expr, type_map)?, *span))
        }
        // Best-effort: other stmt kinds are cloned (they typically won't carry generic types directly)
        _ => Ok(stmt.clone()),
    }
}

fn substitute_expr(
    expr: &ast::Expr,
    type_map: &HashMap<String, ast::Type>,
) -> Result<ast::Expr, MonoError> {
    match expr {
        ast::Expr::Call(name, args, info) => {
            let new_args = args
                .iter()
                .map(|arg| substitute_expr(arg, type_map))
                .collect::<Result<Vec<_>, _>>()?;
            let new_info = ast::ExprInfo {
                span: info.span,
                ty: substitute_type(&info.ty, type_map)?,
                is_tail: info.is_tail,
            };
            Ok(ast::Expr::Call(name.clone(), new_args, new_info))
        }
        ast::Expr::BinOp(left, op, right, info) => {
            let new_info = ast::ExprInfo {
                span: info.span,
                ty: substitute_type(&info.ty, type_map)?,
                is_tail: info.is_tail,
            };
            Ok(ast::Expr::BinOp(
                Box::new(substitute_expr(left, type_map)?),
                op.clone(),
                Box::new(substitute_expr(right, type_map)?),
                new_info,
            ))
        }
        ast::Expr::Var(name, info) => {
            let new_info = ast::ExprInfo {
                span: info.span,
                ty: substitute_type(&info.ty, type_map)?,
                is_tail: info.is_tail,
            };
            Ok(ast::Expr::Var(name.clone(), new_info))
        }
        ast::Expr::TemplateStr(parts, info) => {
            let new_parts = parts
                .iter()
                .map(|part| match part {
                    ast::TemplateStrPart::Literal(text) => Ok::<ast::TemplateStrPart, MonoError>(
                        ast::TemplateStrPart::Literal(text.clone()),
                    ),
                    ast::TemplateStrPart::Expression(expr) => Ok(ast::TemplateStrPart::Expression(
                        Box::new(substitute_expr(expr, type_map)?),
                    )),
                })
                .collect::<Result<Vec<_>, _>>()?;
            let new_info = ast::ExprInfo {
                span: info.span,
                ty: substitute_type(&info.ty, type_map)?,
                is_tail: info.is_tail,
            };
            Ok(ast::Expr::TemplateStr(new_parts, new_info))
        }
        ast::Expr::Cast(inner, ty, info) => {
            let new_inner = substitute_expr(inner, type_map)?;
            let new_ty = substitute_type(ty, type_map)?;
            let new_info = ast::ExprInfo {
                span: info.span,
                ty: new_ty.clone(),
                is_tail: info.is_tail,
            };
            Ok(ast::Expr::Cast(Box::new(new_inner), new_ty, new_info))
        }
        // Ensure FieldAccess types are concretized (e.g., self.arr: T[] -> i32[])
        ast::Expr::FieldAccess(obj, field_name, info) => {
            let new_obj = Box::new(substitute_expr(obj, type_map)?);
            let new_info = ast::ExprInfo {
                span: info.span,
                ty: substitute_type(&info.ty, type_map)?,
                is_tail: info.is_tail,
            };
            Ok(ast::Expr::FieldAccess(
                new_obj,
                field_name.clone(),
                new_info,
            ))
        }
        // Ensure ArrayAccess element types are concretized (e.g., (T[])[idx] -> i32)
        ast::Expr::ArrayAccess(array, index, info) => {
            let new_array = Box::new(substitute_expr(array, type_map)?);
            let new_index = Box::new(substitute_expr(index, type_map)?);
            let new_info = ast::ExprInfo {
                span: info.span,
                ty: substitute_type(&info.ty, type_map)?,
                is_tail: info.is_tail,
            };
            Ok(ast::Expr::ArrayAccess(new_array, new_index, new_info))
        }
        // For literals and other leaf nodes: rebuild with substituted type info where possible
        _ => {
            let info = expr.get_info();
            let new_info = ast::ExprInfo {
                span: info.span,
                ty: substitute_type(&info.ty, type_map)?,
                is_tail: info.is_tail,
            };
            match expr {
                ast::Expr::Int(value, _) => Ok(ast::Expr::Int(*value, new_info)),
                ast::Expr::Int64(value, _) => Ok(ast::Expr::Int64(*value, new_info)),
                ast::Expr::Bool(value, _) => Ok(ast::Expr::Bool(*value, new_info)),
                ast::Expr::Str(value, _) => Ok(ast::Expr::Str(value.clone(), new_info)),
                ast::Expr::F32(value, _) => Ok(ast::Expr::F32(*value, new_info)),
                ast::Expr::F64(value, _) => Ok(ast::Expr::F64(*value, new_info)),
                ast::Expr::Void(_) => Ok(ast::Expr::Void(new_info)),
                _ => Ok(expr.clone()),
            }
        }
    }
}

/// Deterministic suffix used for mangling monomorphized function names.
///
/// Mirrors the backend's approach (conceptually), but intentionally simplified.
/// Must remain stable across runs for deterministic builds/snapshots.
fn type_to_mono_suffix(ty: &Type) -> String {
    match ty {
        Type::I32 => "i32".to_string(),
        Type::Bool => "bool".to_string(),
        Type::String => "string".to_string(),
        Type::Void => "void".to_string(),
        Type::F32 => "f32".to_string(),
        Type::F64 => "f64".to_string(),
        Type::I8 => "i8".to_string(),
        Type::I16 => "i16".to_string(),
        Type::I64 => "i64".to_string(),
        Type::U8 => "u8".to_string(),
        Type::U16 => "u16".to_string(),
        Type::U32 => "u32".to_string(),
        Type::U64 => "u64".to_string(),
        Type::Struct(name) | Type::Enum(name) | Type::Generic(name) => name.clone(),
        Type::GenericInstance(name, args) => {
            let mut s = name.clone();
            for arg in args {
                s.push('_');
                s.push_str(&type_to_mono_suffix(arg));
            }
            s
        }
        Type::Array(inner) => format!("array_{}", type_to_mono_suffix(inner)),
        Type::Optional(inner) => format!("opt_{}", type_to_mono_suffix(inner)),
        Type::NoneType => "none".to_string(),
        Type::Pointer(inner) => format!("ptr_{}", type_to_mono_suffix(inner)),
        Type::SizedArray(inner, n) => format!("arr{n}_{}", type_to_mono_suffix(inner)),
        Type::CChar => "c_char".to_string(),
        Type::CInt => "c_int".to_string(),
        Type::CSize => "c_size".to_string(),
        Type::RawPtr => "rawptr".to_string(),
        Type::Any => "any".to_string(),
        Type::Unknown => "unknown".to_string(),
        Type::Ellipsis => "variadic".to_string(),
        Type::Function(args, ret) => {
            let args_str = args
                .iter()
                .map(|t| type_to_mono_suffix(t))
                .collect::<Vec<_>>()
                .join("_");
            let ret_str = type_to_mono_suffix(ret);
            format!("fn_{}_ret_{}", args_str, ret_str)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use veil_ast::ast::{Expr, ExprInfo, Function, Stmt, Type};

    fn info(ty: Type) -> ExprInfo {
        ExprInfo {
            span: Default::default(),
            ty,
            is_tail: false,
        }
    }

    #[test]
    fn monomorphize_simple_generic_function() {
        // fn id<T>(x: T) -> T { return x }
        let id_fn = Function {
            name: "id".to_string(),
            generic_params: vec!["T".to_string()],
            params: vec![("x".to_string(), Type::Generic("T".to_string()))],
            return_type: Type::Generic("T".to_string()),
            body: vec![Stmt::Return(
                Expr::Var("x".to_string(), info(Type::Generic("T".to_string()))),
                Default::default(),
            )],
            span: Default::default(),
            visibility: Default::default(),
        };

        // main uses id(1)
        let call = Expr::Call(
            "id".to_string(),
            vec![Expr::Int(1, info(Type::I32))],
            info(Type::I32),
        );

        let main_fn = Function {
            name: "main".to_string(),
            generic_params: vec![],
            params: vec![],
            return_type: Type::I32,
            body: vec![Stmt::Return(call, Default::default())],
            span: Default::default(),
            visibility: Default::default(),
        };

        let program = ast::Program {
            functions: vec![id_fn.clone(), main_fn.clone()],
            ..Default::default()
        };

        let mono = Monomorphizer::new(Default::default());
        let out = mono.monomorphize_program(&program).expect("mono ok");

        // Should contain a monomorphized version of id
        assert!(out.functions.iter().any(|f| f.name.starts_with("id__")));
        assert!(out.functions.iter().any(|f| f.generic_params.is_empty()));
    }
}
