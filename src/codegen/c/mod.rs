mod emit_expr;
mod emit_functions;
mod emit_stmt;
mod emit_structs;
mod mem_analysis;
mod utils;

use crate::ast::{AstTransformer, GenericCallTransformer, Type};
use crate::{
    ast,
    ast::AstVisitor,
    codegen::{CodegenConfig, CompileError},
};
use codespan::FileId;
use std::cell::RefCell;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::path::Path;

pub struct CBackend {
    config: CodegenConfig,
    header: String,
    body: String,
    file_id: FileId,
    includes: RefCell<BTreeSet<String>>,
    variables: RefCell<HashMap<String, Type>>,
    functions_map: HashMap<String, Type>,
    functions_map_ast: Option<HashMap<String, ast::Function>>,
    ffi_functions: HashSet<String>,
    struct_defs: HashMap<String, Vec<(String, Type)>>,
    imported_structs: Vec<ast::StructDef>,
    enum_defs: HashMap<String, ast::EnumDef>,
    memory_analysis: MemoryAnalysis,
    is_test_mode: bool,
    current_function: Option<String>,
    generated_optional_types: HashSet<String>,
    current_loop_result: Option<String>,
    current_loop_break: Option<String>,
    temp_counter: usize,
}

#[derive(Debug, Default)]
struct MemoryAnalysis {
    estimated_arena_size: usize,
    string_allocations: usize,
    array_allocations: usize,
    struct_allocations: usize,
    max_function_depth: usize,
    total_functions: usize,
}

impl CBackend {
    fn substitute_type_using_struct_generics(
        &self,
        ty: &ast::Type,
        def: &ast::StructDef,
        args: &[ast::Type],
    ) -> Result<ast::Type, CompileError> {
        let mut type_map = std::collections::HashMap::new();
        for (gp, arg) in def.generic_params.iter().zip(args.iter()) {
            type_map.insert(gp.clone(), arg.clone());
        }
        self.substitute_type(ty, &type_map)
    }

    pub fn new(
        config: CodegenConfig,
        file_id: FileId,
        imported_functions: HashMap<String, (Vec<Type>, Type)>,
        imported_structs: Vec<ast::StructDef>,
        imported_ffi_vars: Vec<ast::FfiVariable>,
        is_test_mode: bool,
    ) -> Self {
        let mut variables = HashMap::new();
        for ffi_var in imported_ffi_vars {
            variables.insert(ffi_var.name, ffi_var.ty);
        }

        let mut functions_map = HashMap::new();

        for (name, (_params, return_type)) in imported_functions {
            functions_map.insert(name, return_type);
        }

        Self {
            config,
            header: String::new(),
            body: String::new(),
            file_id,
            functions_map_ast: None,
            includes: RefCell::new(BTreeSet::new()),
            variables: RefCell::new(variables),
            functions_map,
            ffi_functions: HashSet::new(),
            struct_defs: HashMap::new(),
            imported_structs,
            enum_defs: HashMap::new(),
            memory_analysis: MemoryAnalysis::default(),
            is_test_mode,
            current_function: None,
            generated_optional_types: HashSet::new(),
            current_loop_result: None,
            current_loop_break: None,
            temp_counter: 0,
        }
    }

    pub fn compile(
        &mut self,
        program: &ast::Program,
        output_path: &Path,
    ) -> Result<(), CompileError> {
        let mut program = program.clone();
        // Removed redundant monomorphize_generics call
        if let Err(e) = crate::ast::merge_impl_blocks(&mut program) {
            return Err(CompileError::CodegenError {
                message: e,
                span: None,
                file_id: self.file_id,
            });
        }
        let program = self.monomorphize_generics(&program)?;

        self.analyze_memory_requirements(&program);
        self.emit_header();
        self.generate_ffi_declarations(&program)?;

        let imported_structs = self.imported_structs.clone();
        for struct_def in &imported_structs {
            self.emit_struct(struct_def)?;
        }

        let imported_struct_names: std::collections::HashSet<String> =
            imported_structs.iter().map(|s| s.name.clone()).collect();

        for struct_def in &program.structs {
            let fields: Vec<(String, Type)> = struct_def
                .fields
                .iter()
                .map(|f| (f.name.clone(), f.ty.clone()))
                .collect();
            self.struct_defs.insert(struct_def.name.clone(), fields);
        }
        for enum_def in &program.enums {
            self.enum_defs
                .insert(enum_def.name.clone(), enum_def.clone());
        }

        // Emit simple enums first (they have no dependencies)
        for enum_def in &program.enums {
            let is_simple_enum = enum_def.variants.iter().all(|v| v.data.is_none());
            if is_simple_enum {
                self.emit_enum(enum_def)?;
            }
        }

        // Then emit complex enums (they may depend on simple enums)
        for enum_def in &program.enums {
            let is_simple_enum = enum_def.variants.iter().all(|v| v.data.is_none());
            if !is_simple_enum {
                self.emit_enum(enum_def)?;
            }
        }

        for struct_def in &program.structs {
            if !imported_struct_names.contains(&struct_def.name) {
                self.emit_struct(struct_def)?;
            }
        }
        use crate::ast::Expr;
        fn collect_generic_enum_instances(expr: &Expr, out: &mut Vec<Type>) {
            fn add_if_generic_instance(ty: &Type, out: &mut Vec<Type>) {
                if let Type::GenericInstance(_, _) = ty {
                    out.push(ty.clone());
                }
            }

            match expr {
                Expr::EnumConstruct(_, _, _, info) => {
                    add_if_generic_instance(&info.ty, out);
                }
                Expr::Call(_, args, info) => {
                    add_if_generic_instance(&info.ty, out);
                    for arg in args {
                        collect_generic_enum_instances(arg, out);
                    }
                }
                Expr::Match(_, arms, info) => {
                    add_if_generic_instance(&info.ty, out);
                    for arm in arms {
                        match &arm.body {
                            crate::ast::MatchArmBody::Expr(e) => {
                                collect_generic_enum_instances(e, out)
                            }
                            crate::ast::MatchArmBody::Block(stmts) => {
                                for s in stmts {
                                    if let crate::ast::Stmt::Expr(e, _) = s {
                                        collect_generic_enum_instances(e, out);
                                    }
                                }
                            }
                        }
                    }
                }
                Expr::SafeBlock(stmts, _) => {
                    for s in stmts {
                        if let crate::ast::Stmt::Expr(e, _) = s {
                            collect_generic_enum_instances(e, out);
                        }
                    }
                }
                Expr::TemplateStr(parts, _) => {
                    for part in parts {
                        if let crate::ast::TemplateStrPart::Expression(e) = part {
                            collect_generic_enum_instances(e, out);
                        }
                    }
                }
                Expr::BinOp(l, _, r, _) => {
                    collect_generic_enum_instances(l, out);
                    collect_generic_enum_instances(r, out);
                }
                Expr::UnaryOp(_, e, _) => collect_generic_enum_instances(e, out),
                Expr::StructInit(_, fields, _) => {
                    for (_, e) in fields {
                        collect_generic_enum_instances(e, out);
                    }
                }
                Expr::ArrayInit(elems, _) => {
                    for e in elems {
                        collect_generic_enum_instances(e, out);
                    }
                }
                Expr::ArrayAccess(a, b, _) => {
                    collect_generic_enum_instances(a, out);
                    collect_generic_enum_instances(b, out);
                }
                Expr::Cast(e, _, _) => collect_generic_enum_instances(e, out),
                Expr::Assign(a, b, _) => {
                    collect_generic_enum_instances(a, out);
                    collect_generic_enum_instances(b, out);
                }
                Expr::Deref(e, _) => collect_generic_enum_instances(e, out),
                Expr::Range(a, b, _, _) => {
                    collect_generic_enum_instances(a, out);
                    collect_generic_enum_instances(b, out);
                }
                Expr::FieldAccess(e, _, _) => collect_generic_enum_instances(e, out),
                _ => {}
            }
        }
        let mut generic_enum_instances = Vec::new();

        for func in &program.functions {
            if let Type::GenericInstance(_, _) = &func.return_type {
                generic_enum_instances.push(func.return_type.clone());
            }
            for stmt in &func.body {
                if let crate::ast::Stmt::Expr(e, _) = stmt {
                    collect_generic_enum_instances(e, &mut generic_enum_instances);
                }
                if let crate::ast::Stmt::Let(_, Some(ty), _, _, _) = stmt
                    && let Type::GenericInstance(_, _) = ty
                {
                    generic_enum_instances.push(ty.clone());
                }
            }
        }

        for stmt in &program.stmts {
            if let crate::ast::Stmt::Expr(e, _) = stmt {
                collect_generic_enum_instances(e, &mut generic_enum_instances);
            }
            if let crate::ast::Stmt::Let(_, Some(ty), _, _, _) = stmt
                && let Type::GenericInstance(_, _) = ty
            {
                generic_enum_instances.push(ty.clone());
            }
        }
        generic_enum_instances.sort_by(|a, b| format!("{:?}", a).cmp(&format!("{:?}", b)));
        generic_enum_instances.dedup();

        for ty in &generic_enum_instances {
            if let Type::GenericInstance(name, args) = ty
                && let Some(enum_def) = program.enums.iter().find(|e| &e.name == name)
            {
                self.emit_generic_enum_instance(enum_def, args)?;
            }
        }

        self.functions_map = program
            .functions
            .iter()
            .map(|f| (f.name.clone(), f.return_type.clone()))
            .collect();
        for ffi in &program.ffi_functions {
            self.functions_map
                .insert(ffi.name.clone(), ffi.return_type.clone());
        }

        self.functions_map_ast = Some(
            program
                .functions
                .iter()
                .map(|f| (f.name.clone(), f.clone()))
                .collect(),
        );

        self.emit_globals(&program)?;

        self.emit_functions(&program)?;

        for impl_block in &program.impls {
            self.emit_impl_block(impl_block)?;
        }

        if self.is_test_mode {
            self.emit_tests(&program)?;
        }

        let imported_structs = self.imported_structs.clone();
        let mut all_structs: Vec<&ast::StructDef> = Vec::new();
        let mut seen_names = std::collections::HashSet::new();

        for struct_def in imported_structs.iter() {
            if seen_names.insert(struct_def.name.clone()) {
                all_structs.push(struct_def);
            }
        }

        for struct_def in program.structs.iter() {
            if seen_names.insert(struct_def.name.clone()) {
                all_structs.push(struct_def);
            }
        }

        self.generate_struct_to_str_functions(all_structs);
        self.emit_main_if_missing(&program)?;
        self.write_output(output_path)?;
        Ok(())
    }

    fn monomorphize_generics(&self, program: &ast::Program) -> Result<ast::Program, CompileError> {
        let all_generic_functions: Vec<_> = program
            .functions
            .iter()
            .filter(|f| !f.generic_params.is_empty())
            .collect();

        let mut collector = ast::GenericCallCollector::with_functions(
            &all_generic_functions
                .iter()
                .map(|f| (*f).clone())
                .collect::<Vec<_>>(),
        );
        collector.visit_program(program);

        let mut new_functions = program
            .functions
            .iter()
            .filter(|f| f.generic_params.is_empty())
            .cloned()
            .collect::<Vec<_>>();
        let mut transformer = GenericCallTransformer::new();

        let generic_func_map: std::collections::HashMap<_, _> = all_generic_functions
            .iter()
            .map(|f| (f.name.clone(), *f))
            .collect();

        let mut seen: HashSet<(String, Vec<Type>)> = HashSet::new();

        // Helper to unify a generic pattern type against a concrete call-site type.
        fn unify_types(
            pattern: &Type,
            concrete: &Type,
            subst: &mut std::collections::HashMap<String, Type>,
        ) -> bool {
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
                (Type::Optional(p), Type::Optional(c)) => unify_types(p, c, subst),
                (Type::Optional(p), c) => unify_types(p, c, subst),
                // Structural recursion
                (Type::Array(p), Type::Array(c)) => unify_types(p, c, subst),
                (Type::Pointer(p), Type::Pointer(c)) => unify_types(p, c, subst),
                (Type::SizedArray(p, n1), Type::SizedArray(c, n2)) => {
                    n1 == n2 && unify_types(p, c, subst)
                }
                (Type::GenericInstance(n1, a1), Type::GenericInstance(n2, a2)) => {
                    if n1 != n2 || a1.len() != a2.len() {
                        return false;
                    }
                    for (pp, cc) in a1.iter().zip(a2.iter()) {
                        if !unify_types(pp, cc, subst) {
                            return false;
                        }
                    }
                    true
                }
                // Allow exact concrete matches to pass
                (p, c) if p == c => true,
                // Permit some numeric/compat conversions to still infer the generic (e.g., i32 vs u32)
                // For inference we keep it simple: only identical shapes or generic binds
                _ => false,
            }
        }

        for (func_name, call_arg_types) in &collector.generic_calls {
            if let Some(gen_func) = generic_func_map.get(func_name) {
                // Infer generic args by unifying each declared param type against the call arg type.
                let mut subst: std::collections::HashMap<String, Type> =
                    std::collections::HashMap::new();
                let mut ok = true;
                for ((_, param_ty), arg_ty) in gen_func.params.iter().zip(call_arg_types.iter()) {
                    if !unify_types(param_ty, arg_ty, &mut subst) {
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

                if !seen.insert((func_name.clone(), inferred.clone())) {
                    continue;
                }

                let mono_func = self.instantiate_generic_function(gen_func, &inferred)?;
                let mono_name = mono_func.name.clone();

                transformer.add_mapping(func_name.clone(), call_arg_types.clone(), mono_name);
                new_functions.push(mono_func);
            }
        }

        let transformed_program = transformer.transform_program(ast::Program {
            functions: new_functions.clone(),
            ..program.clone()
        });

        let mut concrete_array_inners: std::collections::HashSet<String> =
            std::collections::HashSet::new();

        fn collect_types_in_expr(
            expr: &ast::Expr,
            out: &mut std::collections::HashSet<String>,
            c: &CBackend,
        ) {
            use ast::Expr;
            match expr {
                Expr::ArrayInit(elems, _) => {
                    for e in elems {
                        collect_types_in_expr(e, out, c);
                    }
                }
                Expr::ArrayAccess(a, _, _) => {
                    collect_types_in_expr(a, out, c);
                }
                Expr::Call(_, args, info) => {
                    if let ast::Type::Array(inner) = &info.ty {
                        out.insert(c.type_to_c_name(inner));
                    }
                    for a in args {
                        collect_types_in_expr(a, out, c);
                    }
                }
                Expr::New(_, args, _) => {
                    for a in args {
                        collect_types_in_expr(a, out, c);
                    }
                }
                Expr::BinOp(l, _, r, _) => {
                    collect_types_in_expr(l, out, c);
                    collect_types_in_expr(r, out, c);
                }
                Expr::UnaryOp(_, e, _) => collect_types_in_expr(e, out, c),
                Expr::StructInit(_, fields, _) => {
                    for (_, e) in fields {
                        collect_types_in_expr(e, out, c);
                    }
                }
                Expr::FieldAccess(e, _, _) => collect_types_in_expr(e, out, c),
                Expr::Match(_, arms, _) => {
                    for arm in arms {
                        match &arm.body {
                            ast::MatchArmBody::Expr(e) => collect_types_in_expr(e, out, c),
                            ast::MatchArmBody::Block(stmts) => {
                                for s in stmts {
                                    collect_types_in_stmt(s, out, c);
                                }
                            }
                        }
                    }
                }
                Expr::Cast(e, _, _) => collect_types_in_expr(e, out, c),
                Expr::Deref(e, _) => collect_types_in_expr(e, out, c),
                _ => {}
            }
        }

        fn collect_types_in_stmt(
            stmt: &ast::Stmt,
            out: &mut std::collections::HashSet<String>,
            c: &CBackend,
        ) {
            match stmt {
                ast::Stmt::Let(_, ty_opt, expr, _, _) => {
                    if let Some(ty) = ty_opt
                        && let ast::Type::Array(inner) = ty
                    {
                        out.insert(c.type_to_c_name(inner));
                    }
                    collect_types_in_expr(expr, out, c);
                }
                ast::Stmt::Expr(expr, _) => collect_types_in_expr(expr, out, c),
                ast::Stmt::Return(expr, _) => collect_types_in_expr(expr, out, c),
                ast::Stmt::If(cond, then_branch, else_branch, _) => {
                    collect_types_in_expr(cond, out, c);
                    for s in then_branch {
                        collect_types_in_stmt(s, out, c);
                    }
                    if let Some(else_b) = else_branch {
                        for s in else_b {
                            collect_types_in_stmt(s, out, c);
                        }
                    }
                }
                ast::Stmt::Block(stmts, _) => {
                    for s in stmts {
                        collect_types_in_stmt(s, out, c);
                    }
                }
                ast::Stmt::While(cond, body, _) => {
                    collect_types_in_expr(cond, out, c);
                    for s in body {
                        collect_types_in_stmt(s, out, c);
                    }
                }
                ast::Stmt::Loop(body, _) => {
                    for s in body {
                        collect_types_in_stmt(s, out, c);
                    }
                }
                ast::Stmt::For(_, _, range, step, body, _) => {
                    collect_types_in_expr(range, out, c);
                    if let Some(st) = step {
                        collect_types_in_expr(st, out, c);
                    }
                    for s in body {
                        collect_types_in_stmt(s, out, c);
                    }
                }
                ast::Stmt::Break(expr_opt, _) => {
                    if let Some(e) = expr_opt {
                        collect_types_in_expr(e, out, c);
                    }
                }
                _ => {}
            }
        }

        for func in &transformed_program.functions {
            if let ast::Type::Array(inner) = &func.return_type {
                concrete_array_inners.insert(self.type_to_c_name(inner));
            }
            for (_, p) in &func.params {
                if let ast::Type::Array(inner) = p {
                    concrete_array_inners.insert(self.type_to_c_name(inner));
                }
            }
            for stmt in &func.body {
                collect_types_in_stmt(stmt, &mut concrete_array_inners, self);
            }
        }

        for stmt in &program.stmts {
            collect_types_in_stmt(stmt, &mut concrete_array_inners, self);
        }

        for struct_def in &program.structs {
            for field in &struct_def.fields {
                if let ast::Type::Array(inner) = &field.ty {
                    concrete_array_inners.insert(self.type_to_c_name(inner));
                }
            }
        }
        // Also consider imported struct fields for array element monomorphization
        for struct_def in &self.imported_structs {
            for field in &struct_def.fields {
                if let ast::Type::Array(inner) = &field.ty {
                    concrete_array_inners.insert(self.type_to_c_name(inner));
                }
            }
        }

        // 1) Specialize array impls with generic T to concrete element types discovered
        let mut new_impls: Vec<ast::ImplBlock> = Vec::new();
        let existing_impl_targets: std::collections::HashSet<String> = program
            .impls
            .iter()
            .map(|ib| ib.target_type.clone())
            .collect();

        for impl_block in &program.impls {
            if impl_block.target_type.contains('T') && impl_block.target_type.contains("[]") {
                for inner in &concrete_array_inners {
                    let t1 = format!("[]{}", inner);
                    let t2 = format!("{}[]", inner);
                    if existing_impl_targets.contains(&t1) || existing_impl_targets.contains(&t2) {
                        continue;
                    }
                    let mut type_map = std::collections::HashMap::new();
                    let concrete_type = if inner == "i32" {
                        ast::Type::I32
                    } else if inner == "string" {
                        ast::Type::String
                    } else if inner == "bool" {
                        ast::Type::Bool
                    } else {
                        ast::Type::Struct(inner.clone())
                    };
                    type_map.insert("T".to_string(), concrete_type.clone());

                    let mut new_methods = Vec::new();
                    for m in &impl_block.methods {
                        let mut new_m = m.clone();
                        new_m.return_type = self.substitute_type(&m.return_type, &type_map)?;
                        let mut new_params = Vec::new();
                        for (n, t) in &m.params {
                            new_params.push((n.clone(), self.substitute_type(t, &type_map)?));
                        }
                        new_m.params = new_params;
                        new_methods.push(new_m);
                    }

                    new_impls.push(ast::ImplBlock {
                        target_type: format!("[]{}", inner),
                        target_type_parsed: Some(ast::Type::Array(Box::new(concrete_type))),
                        methods: new_methods,
                        span: impl_block.span,
                    });
                }
                // Do not push the original generic array impl
            } else if !impl_block.target_type.contains('<') {
                // Keep non-generic impls as-is; generic struct impls will be specialized below
                new_impls.push(impl_block.clone());
            }
        }

        // 2) Collect generic struct definitions in this program and imports (parametric)
        let mut generic_structs: std::collections::HashMap<String, ast::StructDef> = program
            .structs
            .iter()
            .filter(|s| !s.generic_params.is_empty())
            .map(|s| (s.name.clone(), s.clone()))
            .collect();
        // Extend with imported generic structs (avoid overriding local defs)
        for s in &self.imported_structs {
            if !s.generic_params.is_empty() && !generic_structs.contains_key(&s.name) {
                generic_structs.insert(s.name.clone(), s.clone());
            }
        }

        // 3) Discover concrete generic struct instances used in signatures AND expressions
        let mut struct_instances: std::collections::HashSet<ast::Type> =
            std::collections::HashSet::new();

        let mut add_if_struct_instance = |ty: &ast::Type| {
            if let ast::Type::GenericInstance(name, _args) = ty {
                if generic_structs.contains_key(name) {
                    struct_instances.insert(ty.clone());
                }
            }
        };

        // From function/impl signatures
        for f in &transformed_program.functions {
            add_if_struct_instance(&f.return_type);
            for (_, p) in &f.params {
                add_if_struct_instance(p);
            }
        }
        for ib in &new_impls {
            for m in &ib.methods {
                add_if_struct_instance(&m.return_type);
                for (_, p) in &m.params {
                    add_if_struct_instance(p);
                }
            }
        }

        // Helper to traverse expressions/statements and collect GenericInstance struct usages
        fn collect_struct_instances_in_expr(
            expr: &ast::Expr,
            out: &mut std::collections::HashSet<ast::Type>,
            generic_structs: &std::collections::HashMap<String, ast::StructDef>,
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
            out: &mut std::collections::HashSet<ast::Type>,
            generic_structs: &std::collections::HashMap<String, ast::StructDef>,
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

        // From function bodies and top-level statements
        for f in &transformed_program.functions {
            for stmt in &f.body {
                collect_struct_instances_in_stmt(stmt, &mut struct_instances, &generic_structs);
            }
        }
        for stmt in &program.stmts {
            collect_struct_instances_in_stmt(stmt, &mut struct_instances, &generic_structs);
        }
        // Also collect from test bodies (since tests drive most examples)
        for test in &program.tests {
            for stmt in &test.stmts {
                collect_struct_instances_in_stmt(stmt, &mut struct_instances, &generic_structs);
            }
        }

        // 4) Instantiate concrete struct definitions from generic ones
        let mut extra_structs: Vec<ast::StructDef> = Vec::new();
        let mut seen_struct_names = std::collections::HashSet::new();

        for ty in &struct_instances {
            if let ast::Type::GenericInstance(name, args) = ty {
                if let Some(def) = generic_structs.get(name) {
                    let mut new_fields = Vec::new();
                    for f in &def.fields {
                        let new_ty =
                            self.substitute_type_using_struct_generics(&f.ty, def, args)?;
                        new_fields.push(ast::StructField {
                            name: f.name.clone(),
                            ty: new_ty,
                            span: f.span,
                        });
                    }
                    let concrete_name = self.type_to_c_name(ty);
                    if seen_struct_names.insert(concrete_name.clone()) {
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

        // 5) Specialize generic struct impl blocks (e.g., impl ArrayIter<T> { ... }) to concrete instances
        for impl_block in &program.impls {
            if impl_block.target_type.contains('<') && impl_block.target_type.ends_with('>') {
                let s = impl_block.target_type.as_str();
                if let Some(lt) = s.find('<') {
                    let base = s[..lt].trim();
                    let params_src = &s[lt + 1..s.len() - 1];
                    let _param_names: Vec<String> = params_src
                        .split(',')
                        .map(|p| p.trim().to_string())
                        .collect();

                    for ty in &struct_instances {
                        if let ast::Type::GenericInstance(inst_base, inst_args) = ty {
                            if inst_base == base {
                                if let Some(def) = generic_structs.get(base) {
                                    let mut new_methods = Vec::new();
                                    for m in &impl_block.methods {
                                        let mut new_m = m.clone();
                                        new_m.return_type = self
                                            .substitute_type_using_struct_generics(
                                                &m.return_type,
                                                def,
                                                inst_args,
                                            )?;
                                        let mut new_params = Vec::new();
                                        for (n, t) in &m.params {
                                            new_params.push((
                                                n.clone(),
                                                self.substitute_type_using_struct_generics(
                                                    t, def, inst_args,
                                                )?,
                                            ));
                                        }
                                        new_m.params = new_params;
                                        new_methods.push(new_m);
                                    }

                                    new_impls.push(ast::ImplBlock {
                                        target_type: self.type_to_c_name(ty),
                                        target_type_parsed: Some(ty.clone()),
                                        methods: new_methods,
                                        span: impl_block.span,
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }

        let mut final_program = transformed_program.clone();
        // Append specialized concrete structs
        if !extra_structs.is_empty() {
            let mut combined_structs = final_program.structs.clone();
            for s in extra_structs {
                if !combined_structs.iter().any(|e| e.name == s.name) {
                    combined_structs.push(s);
                }
            }
            final_program.structs = combined_structs;
        }
        // Use specialized impls (arrays + generic struct impls)
        final_program.impls = new_impls;

        Ok(final_program)
    }

    fn instantiate_generic_function(
        &self,
        generic_func: &ast::Function,
        type_args: &[ast::Type],
    ) -> Result<ast::Function, CompileError> {
        let mut type_map = std::collections::HashMap::new();

        for (i, param) in generic_func.generic_params.iter().enumerate() {
            if let Some(ty) = type_args.get(i) {
                type_map.insert(param.clone(), ty.clone());
            }
        }

        let mangled_name = format!(
            "{}_{}",
            generic_func.name,
            type_args
                .iter()
                .map(|t| self.type_to_c_name(t))
                .collect::<Vec<_>>()
                .join("_")
        );

        let substituted_params = generic_func
            .params
            .iter()
            .map(|(name, ty)| {
                let new_type = self.substitute_type(ty, &type_map)?;
                Ok::<(String, ast::Type), CompileError>((name.clone(), new_type))
            })
            .collect::<Result<Vec<_>, _>>()?;

        let substituted_return_type = self.substitute_type(&generic_func.return_type, &type_map)?;

        let substituted_body = generic_func
            .body
            .iter()
            .map(|stmt| self.substitute_stmt(stmt, &type_map))
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

    fn substitute_type(
        &self,
        ty: &ast::Type,
        type_map: &std::collections::HashMap<String, ast::Type>,
    ) -> Result<ast::Type, CompileError> {
        match ty {
            ast::Type::Generic(name) => {
                type_map
                    .get(name)
                    .cloned()
                    .ok_or_else(|| CompileError::CodegenError {
                        message: format!("Unresolved generic type parameter: {}", name),
                        span: None,
                        file_id: self.file_id,
                    })
            }
            ast::Type::Array(inner) => Ok(ast::Type::Array(Box::new(
                self.substitute_type(inner, type_map)?,
            ))),
            ast::Type::SizedArray(inner, size) => Ok(ast::Type::SizedArray(
                Box::new(self.substitute_type(inner, type_map)?),
                *size,
            )),
            ast::Type::GenericInstance(name, args) => {
                let mut substituted_args = Vec::new();
                for arg in args {
                    substituted_args.push(self.substitute_type(arg, type_map)?);
                }
                Ok(ast::Type::GenericInstance(name.clone(), substituted_args))
            }
            ast::Type::Optional(inner) => Ok(ast::Type::Optional(Box::new(
                self.substitute_type(inner, type_map)?,
            ))),
            ast::Type::Pointer(inner) => Ok(ast::Type::Pointer(Box::new(
                self.substitute_type(inner, type_map)?,
            ))),
            _ => Ok(ty.clone()),
        }
    }

    fn substitute_stmt(
        &self,
        stmt: &ast::Stmt,
        type_map: &std::collections::HashMap<String, ast::Type>,
    ) -> Result<ast::Stmt, CompileError> {
        match stmt {
            ast::Stmt::Let(name, ty_opt, expr, span, visibility) => {
                let new_ty = if let Some(ty) = ty_opt.as_ref() {
                    Some(self.substitute_type(ty, type_map)?)
                } else {
                    None
                };
                let new_expr = self.substitute_expr(expr, type_map)?;
                Ok(ast::Stmt::Let(
                    name.clone(),
                    new_ty,
                    new_expr,
                    *span,
                    visibility.clone(),
                ))
            }
            ast::Stmt::Expr(expr, span) => Ok(ast::Stmt::Expr(
                self.substitute_expr(expr, type_map)?,
                *span,
            )),
            ast::Stmt::Return(expr, span) => {
                let new_expr = self.substitute_expr(expr, type_map)?;
                Ok(ast::Stmt::Return(new_expr, *span))
            }
            _ => Ok(stmt.clone()),
        }
    }

    fn substitute_expr(
        &self,
        expr: &ast::Expr,
        type_map: &std::collections::HashMap<String, ast::Type>,
    ) -> Result<ast::Expr, CompileError> {
        match expr {
            ast::Expr::Call(name, args, info) => {
                let new_args = args
                    .iter()
                    .map(|arg| self.substitute_expr(arg, type_map))
                    .collect::<Result<Vec<_>, _>>()?;
                let new_info = ast::ExprInfo {
                    span: info.span,
                    ty: self.substitute_type(&info.ty, type_map)?,
                    is_tail: info.is_tail,
                };
                Ok(ast::Expr::Call(name.clone(), new_args, new_info))
            }
            ast::Expr::BinOp(left, op, right, info) => {
                let new_info = ast::ExprInfo {
                    span: info.span,
                    ty: self.substitute_type(&info.ty, type_map)?,
                    is_tail: info.is_tail,
                };
                Ok(ast::Expr::BinOp(
                    Box::new(self.substitute_expr(left, type_map)?),
                    op.clone(),
                    Box::new(self.substitute_expr(right, type_map)?),
                    new_info,
                ))
            }
            ast::Expr::Var(name, info) => {
                let new_info = ast::ExprInfo {
                    span: info.span,
                    ty: self.substitute_type(&info.ty, type_map)?,
                    is_tail: info.is_tail,
                };
                Ok(ast::Expr::Var(name.clone(), new_info))
            }
            ast::Expr::TemplateStr(parts, info) => {
                let new_parts = parts
                    .iter()
                    .map(|part| match part {
                        ast::TemplateStrPart::Literal(text) => {
                            Ok::<ast::TemplateStrPart, CompileError>(ast::TemplateStrPart::Literal(
                                text.clone(),
                            ))
                        }
                        ast::TemplateStrPart::Expression(expr) => {
                            Ok(ast::TemplateStrPart::Expression(Box::new(
                                self.substitute_expr(expr, type_map)?,
                            )))
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let new_info = ast::ExprInfo {
                    span: info.span,
                    ty: self.substitute_type(&info.ty, type_map)?,
                    is_tail: info.is_tail,
                };
                Ok(ast::Expr::TemplateStr(new_parts, new_info))
            }
            ast::Expr::Cast(inner, ty, info) => {
                let new_inner = self.substitute_expr(inner, type_map)?;
                let new_ty = self.substitute_type(ty, type_map)?;
                let new_info = ast::ExprInfo {
                    span: info.span,
                    ty: new_ty.clone(),
                    is_tail: info.is_tail,
                };
                Ok(ast::Expr::Cast(Box::new(new_inner), new_ty, new_info))
            }
            _ => {
                let new_info = ast::ExprInfo {
                    span: expr.get_info().span,
                    ty: self.substitute_type(&expr.get_info().ty, type_map)?,
                    is_tail: expr.get_info().is_tail,
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

    fn emit_tests(&mut self, program: &ast::Program) -> Result<(), CompileError> {
        if self.is_test_mode {
            self.body.push_str("static int ve_test_failed = 0;\n");
            self.body.push_str("static int ve_test_passed = 0;\n");
            self.body
                .push_str("static const char* ve_current_test = \"\";\n\n");

            self.body
                .push_str("void ve_test_panic(const char* msg) {\n");
            self.body.push_str("    ve_arena_enter();\n");
            self.body.push_str("    printf(\"Panic: %s\\n\", msg);\n");
            self.body.push_str("    ve_arena_exit();\n");
            self.body.push_str("    ve_test_failed++;\n");
            self.body.push_str("}\n\n");
        }

        for test in &program.tests {
            let func_name = format!("ve_test_{}", test.name);
            self.body.push_str(&format!("int {}() {{\n", func_name));
            self.body
                .push_str(&format!("    ve_current_test = \"{}\";\n", test.name));

            for stmt in &test.stmts {
                self.emit_stmt(stmt)?;
            }

            self.body.push_str("    ve_test_passed++;\n");
            self.body.push_str("    return 0;\n");
            self.body.push_str("}\n\n");
        }

        Ok(())
    }

    fn generate_ffi_declarations(&mut self, program: &ast::Program) -> Result<(), CompileError> {
        let mut ffi_decls = String::new();
        for ffi in &program.ffi_functions {
            self.ffi_functions.insert(ffi.name.clone());

            if ffi.name.starts_with("ve_") {
                continue;
            }

            let ret = self.type_to_c_ffi(&ffi.return_type);
            let params = ffi
                .params
                .iter()
                .map(|ty| self.type_to_c_ffi(ty))
                .collect::<Vec<String>>()
                .join(", ");

            let param_str = if params.is_empty() { "void" } else { &params };

            if let Some(header) = ffi.metadata.as_ref().and_then(|m| m.get("header")) {
                self.includes.borrow_mut().insert(format!("<{}>", header));
            }

            if let Some(link) = ffi.metadata.as_ref().and_then(|m| m.get("link")) {
                self.header
                    .push_str(&format!("#pragma comment(lib, \"{}\")\n", link));
            }

            if let Some(no_emit_decl) = ffi.metadata.as_ref().and_then(|m| m.get("no_emit_decl"))
                && no_emit_decl == "true"
            {
                continue;
            }

            ffi_decls.push_str(&format!("extern {} {}({});\n", ret, ffi.name, param_str));
        }

        if !ffi_decls.is_empty() {
            self.header.push_str(&ffi_decls);
            self.header.push('\n');
        }

        Ok(())
    }
    fn emit_header(&mut self) {
        self.header.push_str(&format!(
            "// Generated by Veil Compiler (target: {})\n",
            self.config.target_triple
        ));
        self.header
            .push_str("#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n");
        self.header.push_str("#include <stdbool.h>\n");
        self.header.push_str("#include <math.h>\n");
        self.header.push_str("#include <stdint.h>\n");
        self.header.push_str("#include <time.h>\n");

        for include in self.includes.borrow().iter() {
            self.header.push_str(&format!("#include {}\n", include));
        }
        self.header.push('\n');

        self.header.push_str("typedef unsigned char u8;\n");
        self.header.push_str("typedef unsigned short u16;\n");
        self.header.push_str("typedef unsigned int u32;\n");
        self.header.push_str("typedef unsigned long long u64;\n");
        self.header.push_str("typedef signed char i8;\n");
        self.header.push_str("typedef signed short i16;\n");
        self.header.push_str("typedef signed int i32;\n");
        self.header.push_str("typedef signed long long i64;\n");

        self.header.push_str("typedef unsigned char ve_u8;\n");
        self.header.push_str("typedef unsigned short ve_u16;\n");
        self.header.push_str("typedef unsigned int ve_u32;\n");
        self.header.push_str("typedef unsigned long long ve_u64;\n");
        self.header.push_str("typedef signed char ve_i8;\n");
        self.header.push_str("typedef signed short ve_i16;\n");
        self.header.push_str("typedef signed int ve_i32;\n");
        self.header.push_str("typedef signed long long ve_i64;\n");
        self.header.push_str("typedef float ve_f32;\n");
        self.header.push_str("typedef double ve_f64;\n");
        self.header.push_str("typedef size_t ve_size_t;\n\n");
        self.header.push_str("typedef struct {\n");
        self.header.push_str("    void* data;\n");
        self.header.push_str("    size_t length;\n");
        self.header.push_str("    size_t capacity;\n");
        self.header.push_str("} ve_Array;\n\n");

        self.emit_arena_system();
        self.emit_utility_functions();
    }

    fn emit_arena_system(&mut self) {
        self.header.push_str("typedef struct {\n");
        self.header.push_str("    char* memory;\n");
        self.header.push_str("    size_t used;\n");
        self.header.push_str("    size_t capacity;\n");
        self.header.push_str("} ve_Arena;\n\n");

        self.header
            .push_str("static void** ve_malloc_ptrs = NULL;\n");
        self.header.push_str("static size_t ve_malloc_count = 0;\n");
        self.header
            .push_str("static size_t ve_malloc_capacity = 0;\n");
        self.header
            .push_str("static void ve_track_malloc(void* ptr) {\n");
        self.header
            .push_str("    if (ve_malloc_count >= ve_malloc_capacity) {\n");
        self.header.push_str(
            "        ve_malloc_capacity = ve_malloc_capacity ? ve_malloc_capacity * 2 : 16;\n",
        );
        self.header.push_str("        ve_malloc_ptrs = realloc(ve_malloc_ptrs, sizeof(void*) * ve_malloc_capacity);\n");
        self.header.push_str("    }\n");
        self.header
            .push_str("    ve_malloc_ptrs[ve_malloc_count++] = ptr;\n");
        self.header.push_str("}\n\n");

        let thread_local_keyword = if cfg!(target_os = "windows") && cfg!(target_env = "msvc") {
            "__declspec(thread)"
        } else if cfg!(any(target_env = "gnu", target_env = "musl"))
            || cfg!(target_os = "linux")
            || cfg!(target_os = "macos")
        {
            "__thread"
        } else {
            "_Thread_local"
        };

        self.header.push_str(&format!(
            "static {} ve_Arena ve_temp_arena = {{0}};\n",
            thread_local_keyword
        ));
        self.header.push_str(&format!(
            "static {} int ve_arena_depth = 0;\n\n",
            thread_local_keyword
        ));

        self.header.push_str("static void ve_arena_enter() {\n");
        self.header.push_str("    ve_arena_depth++;\n");
        self.header
            .push_str("    if (ve_arena_depth == 1 && !ve_temp_arena.memory) {\n");
        self.header.push_str(&format!(
            "        size_t arena_size = {};\n",
            self.memory_analysis.estimated_arena_size
        ));
        self.header
            .push_str("        ve_temp_arena.memory = malloc(arena_size);\n");
        self.header
            .push_str("        if (!ve_temp_arena.memory) {\n");
        self.header
            .push_str("            fprintf(stderr, \"Failed to allocate arena memory\\n\");\n");
        self.header.push_str("            exit(1);\n");
        self.header.push_str("        }\n");
        self.header
            .push_str("        ve_temp_arena.capacity = arena_size;\n");
        self.header.push_str("        ve_temp_arena.used = 0;\n");
        self.header.push_str("    }\n");
        self.header.push_str("}\n\n");

        self.header.push_str("static void ve_arena_exit() {\n");
        self.header.push_str("    ve_arena_depth--;\n");
        self.header.push_str("    if (ve_arena_depth == 0) {\n");
        self.header.push_str("        ve_temp_arena.used = 0;\n");
        self.header.push_str("    }\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("char* ve_arena_alloc(size_t size) {\n");
        self.header.push_str("    if (!ve_temp_arena.memory) {\n");
        self.header.push_str("        ve_arena_enter();\n");
        self.header.push_str("    }\n");
        self.header.push_str("    \n");
        self.header.push_str("    size = (size + 7) & ~7;\n");
        self.header.push_str("    \n");
        self.header
            .push_str("    if (ve_temp_arena.used + size >= ve_temp_arena.capacity) {\n");
        self.header
            .push_str("        if (size <= ve_temp_arena.capacity / 2) {\n");
        self.header
            .push_str("            size_t new_capacity = ve_temp_arena.capacity * 2;\n");
        self.header.push_str(
            "            char* new_memory = realloc(ve_temp_arena.memory, new_capacity);\n",
        );
        self.header.push_str("            if (new_memory) {\n");
        self.header
            .push_str("                ve_temp_arena.memory = new_memory;\n");
        self.header
            .push_str("                ve_temp_arena.capacity = new_capacity;\n");
        self.header.push_str("            } else {\n");
        self.header
            .push_str("                char* ptr = malloc(size);\n");
        self.header
            .push_str("                if (ptr) ve_track_malloc(ptr);\n");
        self.header.push_str("                return ptr;\n");
        self.header.push_str("            }\n");
        self.header.push_str("        } else {\n");
        self.header
            .push_str("            char* ptr = malloc(size);\n");
        self.header
            .push_str("            if (ptr) ve_track_malloc(ptr);\n");
        self.header.push_str("            return ptr;\n");
        self.header.push_str("        }\n");
        self.header.push_str("    }\n");
        self.header.push_str("    \n");
        self.header
            .push_str("    char* result = ve_temp_arena.memory + ve_temp_arena.used;\n");
        self.header.push_str("    ve_temp_arena.used += size;\n");
        self.header.push_str("    return result;\n");
        self.header.push_str("}\n\n");

        self.header.push_str("static void ve_arena_cleanup() {\n");
        self.header
            .push_str("    for (size_t i = 0; i < ve_malloc_count; i++) {\n");
        self.header.push_str("        free(ve_malloc_ptrs[i]);\n");
        self.header.push_str("    }\n");
        self.header.push_str("    free(ve_malloc_ptrs);\n");
        self.header.push_str("    ve_malloc_ptrs = NULL;\n");
        self.header.push_str("    ve_malloc_count = 0;\n");
        self.header.push_str("    ve_malloc_capacity = 0;\n");
        self.header.push_str("    if (ve_temp_arena.memory) {\n");
        self.header
            .push_str("        free(ve_temp_arena.memory);\n");
        self.header
            .push_str("        ve_temp_arena.memory = NULL;\n");
        self.header.push_str("        ve_temp_arena.used = 0;\n");
        self.header
            .push_str("        ve_temp_arena.capacity = 0;\n");
        self.header.push_str("    }\n");
        self.header.push_str("}\n\n");

        self.emit_array_functions();
    }

    fn emit_array_functions(&mut self) {
        self.includes.borrow_mut().insert("<string.h>".to_string());

        self.header.push_str(
            "static ve_Array* ve_array_create(void* data, size_t length, size_t element_size) {\n",
        );
        self.header
            .push_str("    ve_Array* arr = ve_arena_alloc(sizeof(ve_Array));\n");
        self.header.push_str("    if (arr) {\n");
        self.header.push_str("        arr->data = data;\n");
        self.header.push_str("        arr->length = length;\n");
        self.header.push_str("        arr->capacity = length;\n");
        self.header.push_str("    }\n");
        self.header.push_str("    return arr;\n");
        self.header.push_str("}\n\n");

        self.header.push_str("static ve_Array* ve_array_create_empty(size_t element_size, size_t initial_capacity) {\n");
        self.header
            .push_str("    if (initial_capacity == 0) initial_capacity = 4;\n");
        self.header
            .push_str("    void* data = ve_arena_alloc(initial_capacity * element_size);\n");
        self.header
            .push_str("    ve_Array* arr = ve_arena_alloc(sizeof(ve_Array));\n");
        self.header.push_str("    if (arr && data) {\n");
        self.header.push_str("        arr->data = data;\n");
        self.header.push_str("        arr->length = 0;\n");
        self.header
            .push_str("        arr->capacity = initial_capacity;\n");
        self.header.push_str("    }\n");
        self.header.push_str("    return arr;\n");
        self.header.push_str("}\n\n");

        self.header.push_str("static ve_Array* ve_array_ensure_capacity(ve_Array* arr, size_t required_capacity, size_t element_size) {\n");
        self.header
            .push_str("    if (!arr || arr->capacity >= required_capacity) return arr;\n");
        self.header
            .push_str("    size_t new_capacity = arr->capacity;\n");
        self.header
            .push_str("    while (new_capacity < required_capacity) {\n");
        self.header
            .push_str("        new_capacity = new_capacity == 0 ? 4 : new_capacity * 2;\n");
        self.header.push_str("    }\n");
        self.header
            .push_str("    void* new_data = ve_arena_alloc(new_capacity * element_size);\n");
        self.header.push_str("    if (new_data && arr->data) {\n");
        self.header
            .push_str("        memcpy(new_data, arr->data, arr->length * element_size);\n");
        self.header.push_str("    }\n");
        self.header.push_str("    arr->data = new_data;\n");
        self.header.push_str("    arr->capacity = new_capacity;\n");
        self.header.push_str("    return arr;\n");
        self.header.push_str("}\n\n");

        self.header.push_str("static ve_Array* ve_array_append_element(ve_Array* arr, void* element, size_t element_size) {\n");
        self.header.push_str("    if (!arr) {\n");
        self.header
            .push_str("        arr = ve_array_create_empty(element_size, 4);\n");
        self.header.push_str("    }\n");
        self.header
            .push_str("    arr = ve_array_ensure_capacity(arr, arr->length + 1, element_size);\n");
        self.header
            .push_str("    if (arr && arr->data && element) {\n");
        self.header
            .push_str("        char* dest = ((char*)arr->data) + (arr->length * element_size);\n");
        self.header
            .push_str("        memcpy(dest, element, element_size);\n");
        self.header.push_str("        arr->length++;\n");
        self.header.push_str("    }\n");
        self.header.push_str("    return arr;\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static size_t ve_array_length(ve_Array* arr) {\n");
        self.header.push_str("    return arr ? arr->length : 0;\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static void* ve_array_data(ve_Array* arr) {\n");
        self.header.push_str("    return arr ? arr->data : NULL;\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static ve_Array* ve_array_append_i32(ve_Array* arr, int value) {\n");
        self.header
            .push_str("    return ve_array_append_element(arr, &value, sizeof(int));\n");
        self.header.push_str("}\n\n");

        self.header.push_str(
            "static ve_Array* ve_array_append_string(ve_Array* arr, const char* value) {\n",
        );
        self.header
            .push_str("    return ve_array_append_element(arr, &value, sizeof(const char*));\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static ve_Array* ve_array_append_bool(ve_Array* arr, bool value) {\n");
        self.header
            .push_str("    return ve_array_append_element(arr, &value, sizeof(bool));\n");
        self.header.push_str("}\n\n");
    }

    fn emit_utility_functions(&mut self) {
        let to_str_functions = [
            ("int", "int", "%d", "12"),
            ("float", "float", "%g", "32"),
            ("double", "double", "%g", "32"),
            ("i8", "ve_i8", "%d", "8"),
            ("i16", "ve_i16", "%d", "8"),
            ("i64", "ve_i64", "%lld", "24"),
            ("u8", "ve_u8", "%u", "8"),
            ("u16", "ve_u16", "%u", "8"),
            ("u32", "ve_u32", "%u", "16"),
            ("u64", "ve_u64", "%llu", "24"),
            ("size_t", "ve_size_t", "%zu", "24"),
        ];

        for (name, c_type, fmt, size) in &to_str_functions {
            let cast = if name.contains("u8")
                || name.contains("u16")
                || name.contains("i8")
                || name.contains("i16")
            {
                if name.contains("u") {
                    "(unsigned int)"
                } else {
                    "(int)"
                }
            } else {
                ""
            };

            self.header.push_str(&format!(
                "static char* ve_{}_to_str({} num) {{\n    char* buffer = ve_arena_alloc({});\n    sprintf(buffer, \"{}\", {}num);\n    return buffer;\n}}\n\n",
                name, c_type, size, fmt, cast
            ));
        }

        self.header
            .push_str("static char* ve_bool_to_str(bool b) {\n");
        self.header
            .push_str("    const char* val = b ? \"true\" : \"false\";\n");
        self.header.push_str("    size_t len = strlen(val) + 1;\n");
        self.header
            .push_str("    char* buffer = ve_arena_alloc(len);\n");
        self.header
            .push_str("    if (buffer) memcpy(buffer, val, len);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static char* ve_ptr_to_str(void* ptr) {\n");
        self.header
            .push_str("    char* buffer = ve_arena_alloc(20);\n");
        self.header.push_str("    sprintf(buffer, \"%p\", ptr);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static char* ve_array_to_str(ve_Array* arr) {\n");
        self.header
            .push_str("    if (!arr) return \"[null array]\";\n");
        self.header.push_str("    size_t len = arr->length;\n");
        self.header.push_str("    if (len == 0) return \"[]\";\n");
        self.header
            .push_str("    char* buffer = ve_arena_alloc(64 + len * 16);\n");
        self.header
            .push_str("    sprintf(buffer, \"[array of %zu elements]\", len);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static char* ve_array_i32_to_str(ve_Array* arr) {\n");
        self.header
            .push_str("    if (!arr) return \"[null array]\";\n");
        self.header.push_str("    size_t len = arr->length;\n");
        self.header.push_str("    if (len == 0) return \"[]\";\n");
        self.header
            .push_str("    size_t buffer_size = 16 + len * 16;\n");
        self.header
            .push_str("    char* buffer = ve_arena_alloc(buffer_size);\n");
        self.header.push_str("    strcpy(buffer, \"[\");\n");
        self.header
            .push_str("    for (size_t i = 0; i < len; i++) {\n");
        self.header
            .push_str("        if (i > 0) strcat(buffer, \", \");\n");
        self.header.push_str("        char temp[16];\n");
        self.header
            .push_str("        sprintf(temp, \"%d\", ((int*)ve_array_data(arr))[i]);\n");
        self.header.push_str("        strcat(buffer, temp);\n");
        self.header.push_str("    }\n");
        self.header.push_str("    strcat(buffer, \"]\");\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static char* ve_array_string_to_str(ve_Array* arr) {\n");
        self.header
            .push_str("    if (!arr) return \"[null array]\";\n");
        self.header.push_str("    size_t len = arr->length;\n");
        self.header.push_str("    if (len == 0) return \"[]\";\n");
        self.header
            .push_str("    size_t buffer_size = 16 + len * 64;\n");
        self.header
            .push_str("    char* buffer = ve_arena_alloc(buffer_size);\n");
        self.header.push_str("    strcpy(buffer, \"[\");\n");
        self.header
            .push_str("    for (size_t i = 0; i < len; i++) {\n");
        self.header
            .push_str("        if (i > 0) strcat(buffer, \", \");\n");
        self.header.push_str("        strcat(buffer, \"\\\"\");\n");
        self.header
            .push_str("        strcat(buffer, ((const char**)ve_array_data(arr))[i]);\n");
        self.header.push_str("        strcat(buffer, \"\\\"\");\n");
        self.header.push_str("    }\n");
        self.header.push_str("    strcat(buffer, \"]\");\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static char* ve_array_bool_to_str(ve_Array* arr) {\n");
        self.header
            .push_str("    if (!arr) return \"[null array]\";\n");
        self.header.push_str("    size_t len = arr->length;\n");
        self.header.push_str("    if (len == 0) return \"[]\";\n");
        self.header
            .push_str("    size_t buffer_size = 16 + len * 8;\n");
        self.header
            .push_str("    char* buffer = ve_arena_alloc(buffer_size);\n");
        self.header.push_str("    strcpy(buffer, \"[\");\n");
        self.header
            .push_str("    for (size_t i = 0; i < len; i++) {\n");
        self.header
            .push_str("        if (i > 0) strcat(buffer, \", \");\n");
        self.header.push_str(
            "        strcat(buffer, ((bool*)ve_array_data(arr))[i] ? \"true\" : \"false\");\n",
        );
        self.header.push_str("    }\n");
        self.header.push_str("    strcat(buffer, \"]\");\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        let (strcpy_fn, strcat_fn) = if cfg!(target_os = "windows") && cfg!(target_env = "msvc") {
            (
                "strcpy_s(result, len1 + len2 + 1, s1)",
                "strcat_s(result, len1 + len2 + 1, s2)",
            )
        } else {
            ("strcpy(result, s1)", "strcat(result, s2)")
        };

        self.header
            .push_str("static char* ve_concat(const char* s1, const char* s2) {\n");
        self.header.push_str("    size_t len1 = strlen(s1);\n");
        self.header.push_str("    size_t len2 = strlen(s2);\n");
        self.header
            .push_str("    char* result = ve_arena_alloc(len1 + len2 + 1);\n");
        self.header.push_str("    if (result) {\n");
        self.header.push_str(&format!("        {};\n", strcpy_fn));
        self.header.push_str(&format!("        {};\n", strcat_fn));
        self.header.push_str("    }\n");
        self.header.push_str("    return result;\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static i32 ve_string_at(const char* s, i32 index) {\n");
        self.header.push_str("    if (!s) return -1;\n");
        self.header.push_str("    size_t len = strlen(s);\n");
        self.header
            .push_str("    if (index < 0 || (size_t)index >= len) return -1;\n");
        self.header
            .push_str("    return (i32)(unsigned char)s[index];\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static char* ve_string_slice(const char* s, i32 start, i32 end) {\n");
        self.header.push_str("    if (!s) return \"\";\n");
        self.header.push_str("    size_t len = strlen(s);\n");
        self.header.push_str("    if (start < 0) start = 0;\n");
        self.header
            .push_str("    if (end > (i32)len) end = (i32)len;\n");
        self.header.push_str("    if (start >= end) return \"\";\n");
        self.header.push_str("    \n");
        self.header
            .push_str("    size_t slice_len = end - start;\n");
        self.header
            .push_str("    char* result = ve_arena_alloc(slice_len + 1);\n");
        self.header.push_str("    if (result) {\n");
        self.header
            .push_str("        memcpy(result, s + start, slice_len);\n");
        self.header.push_str("        result[slice_len] = '\\0';\n");
        self.header.push_str("    }\n");
        self.header.push_str("    return result ? result : \"\";\n");
        self.header.push_str("}\n\n");

        #[cfg(debug_assertions)]
        {
            self.header.push_str("#ifdef VE_DEBUG_MEMORY\n");
            self.header.push_str("static void ve_arena_stats() {\n");
            self.header
                .push_str("    printf(\"Arena Statistics:\\n\");\n");
            self.header
                .push_str("    printf(\"  Capacity: %zu bytes\\n\", ve_temp_arena.capacity);\n");
            self.header
                .push_str("    printf(\"  Used: %zu bytes\\n\", ve_temp_arena.used);\n");
            self.header.push_str("    printf(\"  Free: %zu bytes\\n\", ve_temp_arena.capacity - ve_temp_arena.used);\n");
            self.header
                .push_str("    printf(\"  Utilization: %.1f%%\\n\", \n");
            self.header.push_str("           ve_temp_arena.capacity > 0 ? (100.0 * ve_temp_arena.used / ve_temp_arena.capacity) : 0.0);\n");
            self.header
                .push_str("    printf(\"  Malloc fallbacks: %zu\\n\", ve_malloc_count);\n");
            self.header.push_str("}\n");
            self.header.push_str("#else\n");
            self.header.push_str("static void ve_arena_stats() {}\n");
            self.header.push_str("#endif\n\n");
        }

        #[cfg(not(debug_assertions))]
        {
            self.header.push_str("#ifdef VE_DEBUG_MEMORY\n");
            self.header.push_str("static void ve_arena_stats() {\n");
            self.header
                .push_str("    printf(\"Arena Statistics:\\n\");\n");
            self.header
                .push_str("    printf(\"  Capacity: %zu bytes\\n\", ve_temp_arena.capacity);\n");
            self.header
                .push_str("    printf(\"  Used: %zu bytes\\n\", ve_temp_arena.used);\n");
            self.header.push_str("    printf(\"  Free: %zu bytes\\n\", ve_temp_arena.capacity - ve_temp_arena.used);\n");
            self.header
                .push_str("    printf(\"  Utilization: %.1f%%\\n\", \n");
            self.header.push_str("           ve_temp_arena.capacity > 0 ? (100.0 * ve_temp_arena.used / ve_temp_arena.capacity) : 0.0);\n");
            self.header
                .push_str("    printf(\"  Malloc fallbacks: %zu\\n\", ve_malloc_count);\n");
            self.header.push_str("}\n");
            self.header.push_str("#else\n");
            self.header.push_str("static void ve_arena_stats() {}\n");
            self.header.push_str("#endif\n\n");
        }
    }

    fn ensure_optional_type(&mut self, inner_type: &Type) {
        if let Type::Optional(nested_inner) = inner_type {
            self.ensure_optional_type(nested_inner);
        }

        let type_name = self.type_to_c_name(inner_type);

        if !self.generated_optional_types.insert(type_name.clone()) {
            return;
        }

        let c_type = self.type_to_c(inner_type);
        let type_def = format!("ve_optional_{}", type_name);

        self.header.push_str(&format!(
            "typedef struct {{\n    bool has_value;\n    {} value;\n}} {};\n\n",
            c_type, type_def
        ));

        self.header.push_str(&format!(
            "static {} ve_some_{}({} value) {{\n    return ({}){{\n        .has_value = true,\n        .value = value\n    }};\n}}\n\n",
            type_def, type_name, c_type, type_def
        ));

        self.header.push_str(&format!(
            "static {} ve_none_{}() {{\n    return ({}){{\n        .has_value = false,\n        .value = {{0}}\n    }};\n}}\n\n",
            type_def, type_name, type_def
        ));
    }

    fn is_simple_enum(&self, enum_name: &str) -> bool {
        self.enum_defs
            .get(enum_name)
            .map(|enum_def| {
                enum_def
                    .variants
                    .iter()
                    .all(|variant| variant.data.is_none())
            })
            .unwrap_or_else(|| false)
    }

    fn emit_globals(&mut self, program: &ast::Program) -> Result<(), CompileError> {
        for stmt in &program.stmts {
            if let ast::Stmt::Let(name, ty, expr, _, _) = stmt {
                if self.is_constant_expr(expr) {
                    let c_ty = self.type_to_c(ty.as_ref().unwrap_or(&Type::I32));
                    let value = self.emit_expr(expr)?;
                    self.body
                        .push_str(&format!("{} {} = {};\n", c_ty, name, value));
                } else {
                    return Err(CompileError::CodegenError {
                        message: format!(
                            "Non-constant initializer for global '{}'. Globals must have constant initializers. Move this declaration into a function (e.g., initialize in main) or make the initializer compile-time constant.",
                            name
                        ),
                        span: Some(expr.span()),
                        file_id: self.file_id,
                    });
                }
            }
        }
        Ok(())
    }

    fn is_constant_expr(&self, expr: &ast::Expr) -> bool {
        matches!(
            expr,
            ast::Expr::Int(..)
                | ast::Expr::Str(..)
                | ast::Expr::Bool(..)
                | ast::Expr::F32(..)
                | ast::Expr::F64(..)
        )
    }

    fn emit_main_if_missing(&mut self, program: &ast::Program) -> Result<(), CompileError> {
        if !program.functions.iter().any(|f| f.name == "main") {
            self.body.push_str("\nint main(int argc, char* argv[]) {\n");
            self.body.push_str("ve_arena_enter();\n");

            if self.is_test_mode {
                self.body
                    .push_str("    const char* test_to_run = argc > 1 ? argv[1] : NULL;\n");
                self.body.push_str("    \n");

                for test in &program.tests {
                    let func_name = format!("ve_test_{}", test.name);
                    self.body.push_str(&format!(
                        "    if (test_to_run == NULL || strcmp(test_to_run, \"{}\") == 0) {{\n",
                        test.name
                    ));
                    self.body.push_str(&format!("        {}();\n", func_name));
                    self.body.push_str("    }\n");
                }

                self.body.push_str("    printf(\"\\n\");\n");
                self.body.push_str("    if (ve_test_failed == 0) {\n");
                self.body.push_str("        printf(\"✓ %d test%s passed\\n\", ve_test_passed, ve_test_passed == 1 ? \"\" : \"s\");\n");
                self.body.push_str("    } else {\n");
                self.body.push_str("        printf(\"✗ %d passed, %d failed\\n\", ve_test_passed, ve_test_failed);\n");
                self.body.push_str("    }\n");
                self.body.push_str("    \n");
                self.body.push_str("    if (ve_test_failed > 0) {\n");
                self.body.push_str("        return 1;\n");
                self.body.push_str("    }\n");
            } else {
                for stmt in &program.stmts {
                    if !matches!(stmt, ast::Stmt::Let(..)) {
                        self.emit_stmt(stmt)?;
                    }
                }
            }

            self.body.push_str("ve_arena_exit();\n");
            self.body
                .push_str("#ifdef VE_DEBUG_MEMORY\n    ve_arena_stats();\n#endif\n");
            self.body.push_str("    ve_arena_cleanup();\n");
            self.body.push_str("    return 0;\n}\n");
        }
        Ok(())
    }

    fn write_output(&self, c_file_path: &Path) -> Result<(), CompileError> {
        std::fs::write(c_file_path, format!("{}{}", self.header, self.body))
            .map_err(CompileError::IOError)?;
        Ok(())
    }

    fn emit_match_switch_with_result(
        &mut self,
        matched_var: &str,
        result_var: &str,
        arms: &[ast::MatchArm],
        code: &mut String,
    ) -> Result<(), CompileError> {
        let matched_type = self.variables.borrow().get(matched_var).cloned();
        let (_enum_name, is_generic, tag_prefix) =
            if let Some(Type::GenericInstance(name, _args)) = &matched_type {
                (
                    name.clone(),
                    true,
                    format!("ve_{}", self.type_to_c_name(matched_type.as_ref().unwrap())),
                )
            } else {
                ("".to_string(), false, "".to_string())
            };

        let switch_expr = match &matched_type {
            Some(Type::Enum(enum_name)) => {
                if self.is_simple_enum(enum_name) {
                    matched_var.to_string()
                } else {
                    format!("{}.tag", matched_var)
                }
            }
            Some(Type::GenericInstance(_, _)) => format!("{}.tag", matched_var),
            _ => matched_var.to_string(),
        };

        let has_guards = arms.iter().any(|arm| arm.guard.is_some());

        if has_guards {
            use std::collections::HashMap;
            let mut variants_map: HashMap<String, Vec<&ast::MatchArm>> = HashMap::new();

            for arm in arms {
                if let ast::Pattern::EnumVariant(_, variant_name, _, _) = &arm.pattern {
                    variants_map
                        .entry(variant_name.clone())
                        .or_default()
                        .push(arm);
                }
            }

            let mut first_arm = true;
            for arm in arms {
                match &arm.pattern {
                    ast::Pattern::EnumVariant(enum_name_arm, variant_name, patterns, _) => {
                        let case_value = if is_generic {
                            format!("{}_{}", tag_prefix, variant_name)
                        } else {
                            format!("ve_{}_{}", enum_name_arm, variant_name)
                        };

                        let arms_for_variant = variants_map.get(variant_name).unwrap();
                        let is_first_for_variant = arms_for_variant
                            .first()
                            .map(|a| std::ptr::eq(*a, arm))
                            .unwrap_or(false);

                        if is_first_for_variant {
                            let if_keyword = if first_arm { "if" } else { "else if" };
                            first_arm = false;
                            code.push_str(&format!(
                                "{} ({} == {}) {{\n",
                                if_keyword, switch_expr, case_value
                            ));

                            for (i, pattern) in patterns.iter().enumerate() {
                                if let ast::Pattern::Variable(var_name, _) = pattern {
                                    let mut field_type = "int".to_string();
                                    if let Some(Type::GenericInstance(enum_name, args)) =
                                        &matched_type
                                        && let Some(enum_def) = self.enum_defs.get(enum_name)
                                    {
                                        enum_def
                                            .variants
                                            .iter()
                                            .find(|v| v.name == *variant_name)
                                            .map(|variant| {
                                                if let Some(data_types) = &variant.data {
                                                    match data_types {
                                                        crate::ast::EnumVariantData::Tuple(
                                                            types,
                                                        ) => {
                                                            if let Some(ty) = types.get(i) {
                                                                field_type = self.type_to_c(
                                                                    if let Some(idx) = enum_def
                                                                        .generic_params
                                                                        .iter()
                                                                        .position(|gp| {
                                                                            gp == &ty.to_string()
                                                                        })
                                                                    {
                                                                        &args[idx]
                                                                    } else {
                                                                        ty
                                                                    },
                                                                );
                                                            }
                                                        }
                                                        crate::ast::EnumVariantData::Struct(
                                                            fields,
                                                        ) => {
                                                            if let Some(f) = fields.get(i) {
                                                                let ty = &f.ty;
                                                                field_type = self.type_to_c(
                                                                    if let Some(idx) = enum_def
                                                                        .generic_params
                                                                        .iter()
                                                                        .position(|gp| {
                                                                            gp == &ty.to_string()
                                                                        })
                                                                    {
                                                                        &args[idx]
                                                                    } else {
                                                                        ty
                                                                    },
                                                                );
                                                            }
                                                        }
                                                    }
                                                }
                                            });
                                    }
                                    code.push_str(&format!(
                                        "        {} {} = {}.data.{}.field{};\n",
                                        field_type,
                                        var_name,
                                        matched_var,
                                        variant_name.to_lowercase(),
                                        i
                                    ));
                                }
                            }

                            for variant_arm in arms_for_variant {
                                if let Some(guard) = &variant_arm.guard {
                                    let guard_code = self.emit_expr(guard)?;
                                    code.push_str(&format!("    if ({}) {{\n", guard_code));
                                } else {
                                    code.push_str("    {\n");
                                }

                                let body_code = match &variant_arm.body {
                                    ast::MatchArmBody::Expr(expr) => self.emit_expr(expr)?,
                                    ast::MatchArmBody::Block(stmts) => {
                                        let mut block_code = String::new();
                                        for stmt in stmts {
                                            block_code.push_str(&self.emit_stmt_to_string(stmt)?);
                                        }
                                        block_code
                                    }
                                };

                                code.push_str(&format!(
                                    "        {} = {};\n",
                                    result_var, body_code
                                ));
                                code.push_str("    }\n");

                                if variant_arm.guard.is_some() {
                                    code.push_str("    else ");
                                } else {
                                    break;
                                }
                            }
                            code.push_str("}\n");
                        }
                    }
                    ast::Pattern::Wildcard(_) => {
                        let else_keyword = if first_arm { "" } else { "else " };
                        first_arm = false;
                        code.push_str(&format!("{}{{ \n", else_keyword));
                        let body_code = match &arm.body {
                            ast::MatchArmBody::Expr(expr) => self.emit_expr(expr)?,
                            ast::MatchArmBody::Block(stmts) => {
                                let mut block_code = String::new();
                                for stmt in stmts {
                                    block_code.push_str(&self.emit_stmt_to_string(stmt)?);
                                }
                                block_code
                            }
                        };
                        code.push_str(&format!("    {} = {};\n", result_var, body_code));
                        code.push_str("}\n");
                    }
                    ast::Pattern::Variable(var_name, _) => {
                        let else_keyword = if first_arm { "" } else { "else " };
                        first_arm = false;
                        code.push_str(&format!("{}{{ \n", else_keyword));
                        let expr_type = Type::Unknown;
                        let c_type = self.type_to_c(&expr_type);
                        code.push_str(&format!("    {} {} = {};\n", c_type, var_name, matched_var));
                        let body_code = match &arm.body {
                            ast::MatchArmBody::Expr(expr) => self.emit_expr(expr)?,
                            ast::MatchArmBody::Block(stmts) => {
                                let mut block_code = String::new();
                                for stmt in stmts {
                                    block_code.push_str(&self.emit_stmt_to_string(stmt)?);
                                }
                                block_code
                            }
                        };
                        code.push_str(&format!("    {} = {};\n", result_var, body_code));
                        code.push_str("}\n");
                    }
                    ast::Pattern::Literal(_expr, _) => {
                        return Err(CompileError::CodegenError {
                            message: "Literal patterns in enum match not yet supported".to_string(),
                            span: Some(arm.span),
                            file_id: self.file_id,
                        });
                    }
                }
            }
        } else {
            code.push_str(&format!("switch ({}) {{\n", switch_expr));

            for arm in arms {
                match &arm.pattern {
                    ast::Pattern::EnumVariant(enum_name_arm, variant_name, patterns, _) => {
                        let case_value = if is_generic {
                            format!("{}_{}", tag_prefix, variant_name)
                        } else {
                            format!("ve_{}_{}", enum_name_arm, variant_name)
                        };
                        code.push_str(&format!("    case {}: {{\n", case_value));

                        for (i, pattern) in patterns.iter().enumerate() {
                            if let ast::Pattern::Variable(var_name, _) = pattern {
                                let mut field_type = "int".to_string();
                                if let Some(Type::GenericInstance(enum_name, args)) = &matched_type
                                    && let Some(enum_def) = self.enum_defs.get(enum_name)
                                {
                                    enum_def
                                        .variants
                                        .iter()
                                        .find(|v| v.name == *variant_name)
                                        .map(|variant| {
                                            if let Some(data_types) = &variant.data {
                                                match data_types {
                                                    crate::ast::EnumVariantData::Tuple(types) => {
                                                        if let Some(ty) = types.get(i) {
                                                            field_type = self.type_to_c(
                                                                if let Some(idx) = enum_def
                                                                    .generic_params
                                                                    .iter()
                                                                    .position(|gp| {
                                                                        gp == &ty.to_string()
                                                                    })
                                                                {
                                                                    &args[idx]
                                                                } else {
                                                                    ty
                                                                },
                                                            );
                                                        }
                                                    }
                                                    crate::ast::EnumVariantData::Struct(fields) => {
                                                        if let Some(f) = fields.get(i) {
                                                            let ty = &f.ty;
                                                            field_type = self.type_to_c(
                                                                if let Some(idx) = enum_def
                                                                    .generic_params
                                                                    .iter()
                                                                    .position(|gp| {
                                                                        gp == &ty.to_string()
                                                                    })
                                                                {
                                                                    &args[idx]
                                                                } else {
                                                                    ty
                                                                },
                                                            );
                                                        }
                                                    }
                                                }
                                            }
                                        });
                                }
                                code.push_str(&format!(
                                    "    {} {} = {}.data.{}.field{};\n",
                                    field_type,
                                    var_name,
                                    matched_var,
                                    variant_name.to_lowercase(),
                                    i
                                ));
                            }
                        }

                        let body_code = match &arm.body {
                            ast::MatchArmBody::Expr(expr) => self.emit_expr(expr)?,
                            ast::MatchArmBody::Block(stmts) => {
                                let mut block_code = String::new();
                                for stmt in stmts {
                                    block_code.push_str(&self.emit_stmt_to_string(stmt)?);
                                }
                                block_code
                            }
                        };
                        code.push_str(&format!("    {} = {};\n", result_var, body_code));
                        code.push_str("    break;\n");
                        code.push_str("}\n");
                    }
                    ast::Pattern::Wildcard(_) => {
                        code.push_str("    default: {\n");
                        let body_code = match &arm.body {
                            ast::MatchArmBody::Expr(expr) => self.emit_expr(expr)?,
                            ast::MatchArmBody::Block(stmts) => {
                                let mut block_code = String::new();
                                for stmt in stmts {
                                    block_code.push_str(&self.emit_stmt_to_string(stmt)?);
                                }
                                block_code
                            }
                        };
                        code.push_str(&format!("        {} = {};\n", result_var, body_code));
                        code.push_str("        break;\n");
                        code.push_str("    }\n");
                    }
                    ast::Pattern::Variable(var_name, _) => {
                        code.push_str("    default: {\n");
                        let expr_type = Type::Unknown;
                        let c_type = self.type_to_c(&expr_type);
                        code.push_str(&format!(
                            "        {} {} = {};\n",
                            c_type, var_name, matched_var
                        ));
                        let body_code = match &arm.body {
                            ast::MatchArmBody::Expr(expr) => self.emit_expr(expr)?,
                            ast::MatchArmBody::Block(stmts) => {
                                let mut block_code = String::new();
                                for stmt in stmts {
                                    block_code.push_str(&self.emit_stmt_to_string(stmt)?);
                                }
                                block_code
                            }
                        };
                        code.push_str(&format!("        {} = {};\n", result_var, body_code));
                        code.push_str("        break;\n");
                        code.push_str("    }\n");
                    }
                    ast::Pattern::Literal(_expr, _) => {
                        return Err(CompileError::CodegenError {
                            message: "Literal patterns in enum match not yet supported".to_string(),
                            span: Some(arm.span),
                            file_id: self.file_id,
                        });
                    }
                }
            }

            code.push_str("}\n");
        }
        Ok(())
    }
}
