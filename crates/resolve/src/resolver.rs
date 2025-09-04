//! Main resolver implementation for the Veil compiler
//!
//! This module provides the main resolver that orchestrates symbol table building,
//! name resolution, module graph construction, and visibility checking.

use codespan::{FileId, Span};
use std::collections::HashMap;
use veil_hir::{
    HirBlock, HirEnum, HirExpr, HirExprKind, HirFfiFunction, HirFfiVariable, HirFunction, HirImpl,
    HirImport, HirItem, HirItemKind, HirParam, HirPattern, HirPatternKind, HirProgram, HirStmt,
    HirStmtKind, HirStruct, HirTest, HirType, ModuleId, NodeId, SymbolId,
};

use crate::errors::{ResolveError, ResolveErrorKind, ResolveResult};
use crate::module_graph::{ImportKind, ModuleGraph, ModuleImport, ModuleNode};
use crate::scope::{Scope, ScopeKind, ScopeStack};
use crate::symbol_table::{Symbol, SymbolKind, SymbolTable};
use crate::visibility::VisibilityChecker;

/// Main resolver context containing all resolution state
#[derive(Debug)]
pub struct ResolverContext {
    /// Symbol table for all resolved symbols
    pub symbol_table: SymbolTable,
    /// Module dependency graph
    pub module_graph: ModuleGraph,
    /// Visibility checker for access control
    pub visibility_checker: VisibilityChecker,
    /// Current scope stack
    pub scope_stack: ScopeStack,
    /// Current module being processed
    pub current_module: Option<ModuleId>,
    /// Current file being processed
    pub current_file: Option<FileId>,
    /// Map from unresolved names to resolved symbol IDs
    pub name_resolutions: HashMap<String, SymbolId>,
    /// Map from HIR nodes to resolved symbols
    pub node_to_symbol: HashMap<NodeId, SymbolId>,
    /// Map from module paths to module IDs
    pub module_path_map: HashMap<String, ModuleId>,
    /// Next available module ID
    next_module_id: u32,
    /// Accumulated errors during resolution
    pub errors: Vec<ResolveError>,
}

impl ResolverContext {
    pub fn new() -> Self {
        let crate_root = ModuleId::new(0);
        let mut ctx = Self {
            symbol_table: SymbolTable::new(),
            module_graph: ModuleGraph::new(),
            visibility_checker: VisibilityChecker::new(crate_root),
            scope_stack: ScopeStack::new(),
            current_module: Some(crate_root),
            current_file: None,
            name_resolutions: HashMap::new(),
            node_to_symbol: HashMap::new(),
            module_path_map: HashMap::new(),
            next_module_id: 1, // 0 is reserved for crate root
            errors: Vec::new(),
        };
        let root_scope = Scope::new(
            ctx.scope_stack.next_scope_id(),
            ScopeKind::Module,
            crate_root,
        );
        ctx.scope_stack.push_scope(root_scope);

        ctx
    }

    /// Generate a new unique module ID
    pub fn next_module_id(&mut self) -> ModuleId {
        let id = ModuleId::new(self.next_module_id);
        self.next_module_id += 1;
        id
    }

    /// Add an error to the context
    pub fn add_error(&mut self, error: ResolveError) {
        self.errors.push(error);
    }

    /// Add an error with span information
    pub fn add_error_at(&mut self, error: ResolveError, span: Span) {
        let error = if let Some(file_id) = self.current_file {
            error.with_span(span).with_file(file_id)
        } else {
            error.with_span(span)
        };
        self.add_error(error);
    }

    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Take all accumulated errors
    pub fn take_errors(&mut self) -> Vec<ResolveError> {
        std::mem::take(&mut self.errors)
    }

    /// Get the current module
    pub fn current_module(&self) -> ModuleId {
        self.current_module.unwrap_or(ModuleId::new(0))
    }

    /// Set the current module
    pub fn set_current_module(&mut self, module_id: ModuleId) {
        self.current_module = Some(module_id);
    }

    /// Set the current file
    pub fn set_current_file(&mut self, file_id: FileId) {
        self.current_file = Some(file_id);
    }

    /// Define a symbol in the current scope
    pub fn define_symbol(&mut self, symbol: Symbol) -> Result<SymbolId, ResolveError> {
        let symbol_id = symbol.id;
        let name = symbol.name.clone();

        // Check if symbol already exists in current scope
        if let Some(scope) = self.scope_stack.current_scope()
            && scope.shadows_symbol(&name)
        {
            return Err(ResolveError::duplicate_symbol(&name));
        }

        // Add to symbol table
        self.symbol_table.insert(symbol);

        // Add to current scope
        if let Err(msg) = self.scope_stack.define_symbol(name.clone(), symbol_id) {
            return Err(ResolveError::duplicate_symbol(msg));
        }

        // Add to name resolutions
        self.name_resolutions.insert(name, symbol_id);

        Ok(symbol_id)
    }

    /// Resolve a name to a symbol
    pub fn resolve_name(&self, name: &str) -> Option<SymbolId> {
        // First check scope stack
        if let Some((symbol_id, _scope_id)) = self.scope_stack.lookup_symbol(name) {
            return Some(symbol_id);
        }

        // Then check name resolutions cache
        if let Some(&symbol_id) = self.name_resolutions.get(name) {
            return Some(symbol_id);
        }

        // Finally check symbol table directly
        let current_module = self.current_module();
        let accessible_symbols = self
            .symbol_table
            .accessible_symbols_from(current_module, ModuleId::new(0));

        for symbol in accessible_symbols {
            if symbol.name == name {
                return Some(symbol.id);
            }
        }

        None
    }

    /// Resolve a type name to a symbol
    pub fn resolve_type(&mut self, name: &str) -> Result<SymbolId, ResolveError> {
        // Check type parameters first
        if let Some((symbol_id, _scope_id)) = self.scope_stack.lookup_type_param(name) {
            return Ok(symbol_id);
        }

        // Then check for type symbols
        if let Some(symbol_id) = self.resolve_name(name)
            && let Some(symbol) = self.symbol_table.get(symbol_id)
            && symbol.is_type()
        {
            return Ok(symbol_id);
        }

        // Find candidates for better error messages
        let candidates = self.symbol_table.find_candidates(name, 2);
        Err(ResolveError::undefined_symbol_with_candidates(
            name.to_string(),
            candidates,
        ))
    }

    /// Check visibility and access control
    pub fn check_access(
        &mut self,
        symbol_id: SymbolId,
        span: Option<Span>,
    ) -> Result<(), ResolveError> {
        if let Some(symbol) = self.symbol_table.get(symbol_id) {
            let current_module = self.current_module();
            self.visibility_checker
                .check_access(symbol, current_module, span)
        } else {
            Ok(()) // Symbol doesn't exist, will be caught elsewhere
        }
    }
}

impl Default for ResolverContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Main resolver for HIR programs
#[derive(Debug)]
pub struct Resolver {
    /// Resolver context
    context: ResolverContext,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            context: ResolverContext::new(),
        }
    }

    /// Resolve a HIR program
    pub fn resolve_program(&mut self, program: &mut HirProgram) -> ResolveResult<()> {
        // Set up root module
        let root_module = ModuleNode::new(program.module_id, "crate".to_string());
        self.context.module_graph.add_module(root_module);
        self.context.set_current_module(program.module_id);

        // Create root scope
        let root_scope = Scope::new(
            self.context.scope_stack.next_scope_id(),
            ScopeKind::Module,
            program.module_id,
        );
        self.context.scope_stack.push_scope(root_scope);

        // Resolve all items
        for item in &mut program.items {
            if let Err(mut errors) = self.resolve_item(item) {
                self.context.errors.append(&mut errors);
            }
        }

        // Check for circular dependencies
        if let Err(error) = self.context.module_graph.check_cycles() {
            self.context.add_error(error);
        }

        // Validate module graph
        let validation_errors = self.context.module_graph.validate();
        for error in validation_errors {
            self.context.add_error(error);
        }

        // Pop root scope
        self.context.scope_stack.pop_scope();

        // Return errors if any
        if self.context.has_errors() {
            Err(self.context.take_errors())
        } else {
            Ok(())
        }
    }

    /// Resolve a top-level item
    fn resolve_item(&mut self, item: &mut HirItem) -> ResolveResult<()> {
        match &mut item.kind {
            HirItemKind::Function(func) => self.resolve_function(func),
            HirItemKind::Struct(struct_def) => self.resolve_struct(struct_def),
            HirItemKind::Enum(enum_def) => self.resolve_enum(enum_def),
            HirItemKind::Impl(impl_block) => self.resolve_impl(impl_block),
            HirItemKind::FfiFunction(ffi_func) => self.resolve_ffi_function(ffi_func),
            HirItemKind::FfiVariable(ffi_var) => self.resolve_ffi_variable(ffi_var),
            HirItemKind::Test(test) => self.resolve_test(test),
            HirItemKind::Import(import) => self.resolve_import(import),
        }
    }

    /// Resolve a function definition
    fn resolve_function(&mut self, func: &mut HirFunction) -> ResolveResult<()> {
        // Create symbol for function
        let symbol = Symbol::new(
            self.context.symbol_table.next_symbol_id(),
            func.name.clone(),
            SymbolKind::Function,
            self.context.current_module(),
            func.id,
        )
        .with_generics(func.generic_params.clone());

        // Define function symbol
        let function_symbol_id = self.context.define_symbol(symbol).map_err(|e| vec![e])?;

        // Map node to symbol
        self.context
            .node_to_symbol
            .insert(func.id, function_symbol_id);

        // Create function scope
        let func_scope = Scope::new(
            self.context.scope_stack.next_scope_id(),
            ScopeKind::Function,
            self.context.current_module(),
        )
        .with_node(func.id);

        self.context.scope_stack.push_scope(func_scope);

        // Define generic parameters
        for generic_param in &func.generic_params {
            let param_symbol = Symbol::new(
                self.context.symbol_table.next_symbol_id(),
                generic_param.clone(),
                SymbolKind::GenericParameter,
                self.context.current_module(),
                func.id,
            );

            if let Err(e) = self.context.define_symbol(param_symbol) {
                self.context.add_error(e);
            }
        }

        // Resolve parameters
        for param in &mut func.params {
            self.resolve_parameter(param)?;
        }

        // Resolve return type
        self.resolve_type_annotation(&mut func.return_type)?;

        // Resolve function body
        self.resolve_block(&mut func.body)?;

        // Pop function scope
        self.context.scope_stack.pop_scope();

        Ok(())
    }

    /// Resolve a function parameter
    fn resolve_parameter(&mut self, param: &mut HirParam) -> ResolveResult<()> {
        // Resolve parameter type
        self.resolve_type_annotation(&mut param.ty)?;

        // Create symbol for parameter
        let symbol = Symbol::new(
            self.context.symbol_table.next_symbol_id(),
            param.name.clone(),
            SymbolKind::Parameter,
            self.context.current_module(),
            param.id,
        )
        .with_type(param.ty.clone());

        // Define parameter symbol
        let param_symbol_id = self.context.define_symbol(symbol).map_err(|e| vec![e])?;

        // Map node to symbol
        self.context
            .node_to_symbol
            .insert(param.id, param_symbol_id);

        Ok(())
    }

    /// Resolve a struct definition
    fn resolve_struct(&mut self, struct_def: &mut HirStruct) -> ResolveResult<()> {
        // Create symbol for struct
        let symbol = Symbol::new(
            self.context.symbol_table.next_symbol_id(),
            struct_def.name.clone(),
            SymbolKind::Struct,
            self.context.current_module(),
            struct_def.id,
        )
        .with_generics(struct_def.generic_params.clone());

        // Define struct symbol
        let struct_symbol_id = self.context.define_symbol(symbol).map_err(|e| vec![e])?;

        // Map node to symbol
        self.context
            .node_to_symbol
            .insert(struct_def.id, struct_symbol_id);

        // Create type scope for generics
        let type_scope = Scope::new(
            self.context.scope_stack.next_scope_id(),
            ScopeKind::Type,
            self.context.current_module(),
        )
        .with_node(struct_def.id);

        self.context.scope_stack.push_scope(type_scope);

        // Define generic parameters
        for generic_param in &struct_def.generic_params {
            let param_symbol = Symbol::new(
                self.context.symbol_table.next_symbol_id(),
                generic_param.clone(),
                SymbolKind::GenericParameter,
                self.context.current_module(),
                struct_def.id,
            );

            if let Err(e) = self.context.define_symbol(param_symbol) {
                self.context.add_error(e);
            }
        }

        // Resolve struct fields
        for field in &mut struct_def.fields {
            self.resolve_struct_field(field)?;
        }

        // Pop type scope
        self.context.scope_stack.pop_scope();

        Ok(())
    }

    /// Resolve a struct field
    fn resolve_struct_field(&mut self, field: &mut veil_hir::HirStructField) -> ResolveResult<()> {
        // Resolve field type
        self.resolve_type_annotation(&mut field.ty)?;

        // Create symbol for field
        let symbol = Symbol::new(
            self.context.symbol_table.next_symbol_id(),
            field.name.clone(),
            SymbolKind::Field,
            self.context.current_module(),
            field.id,
        )
        .with_type(field.ty.clone())
        .with_visibility(field.visibility.clone());

        // Define field symbol
        let field_symbol_id = self.context.define_symbol(symbol).map_err(|e| vec![e])?;

        // Map node to symbol
        self.context
            .node_to_symbol
            .insert(field.id, field_symbol_id);

        Ok(())
    }

    /// Resolve an enum definition
    fn resolve_enum(&mut self, enum_def: &mut HirEnum) -> ResolveResult<()> {
        // Create symbol for enum
        let symbol = Symbol::new(
            self.context.symbol_table.next_symbol_id(),
            enum_def.name.clone(),
            SymbolKind::Enum,
            self.context.current_module(),
            enum_def.id,
        )
        .with_generics(enum_def.generic_params.clone());

        // Define enum symbol
        let enum_symbol_id = self.context.define_symbol(symbol).map_err(|e| vec![e])?;

        // Map node to symbol
        self.context
            .node_to_symbol
            .insert(enum_def.id, enum_symbol_id);

        // Create type scope for generics
        let type_scope = Scope::new(
            self.context.scope_stack.next_scope_id(),
            ScopeKind::Type,
            self.context.current_module(),
        )
        .with_node(enum_def.id);

        self.context.scope_stack.push_scope(type_scope);

        // Define generic parameters
        for generic_param in &enum_def.generic_params {
            let param_symbol = Symbol::new(
                self.context.symbol_table.next_symbol_id(),
                generic_param.clone(),
                SymbolKind::GenericParameter,
                self.context.current_module(),
                enum_def.id,
            );

            if let Err(e) = self.context.define_symbol(param_symbol) {
                self.context.add_error(e);
            }
        }

        // Resolve enum variants
        for variant in &mut enum_def.variants {
            self.resolve_enum_variant(variant)?;
        }

        // Pop type scope
        self.context.scope_stack.pop_scope();

        Ok(())
    }

    /// Resolve an enum variant
    fn resolve_enum_variant(
        &mut self,
        variant: &mut veil_hir::HirEnumVariant,
    ) -> ResolveResult<()> {
        // Create symbol for variant
        let symbol = Symbol::new(
            self.context.symbol_table.next_symbol_id(),
            variant.name.clone(),
            SymbolKind::EnumVariant,
            self.context.current_module(),
            variant.id,
        );

        // Define variant symbol
        let variant_symbol_id = self.context.define_symbol(symbol).map_err(|e| vec![e])?;

        // Map node to symbol
        self.context
            .node_to_symbol
            .insert(variant.id, variant_symbol_id);

        // Resolve variant data if present
        if let Some(data) = &mut variant.data {
            match data {
                veil_hir::HirEnumVariantData::Tuple(types) => {
                    for ty in types {
                        self.resolve_type_annotation(ty)?;
                    }
                }
                veil_hir::HirEnumVariantData::Struct(fields) => {
                    for field in fields {
                        self.resolve_struct_field(field)?;
                    }
                }
            }
        }

        // Resolve discriminant if present
        if let Some(expr) = &mut variant.discriminant {
            self.resolve_expression(expr)?;
        }

        Ok(())
    }

    /// Resolve an impl block
    fn resolve_impl(&mut self, impl_block: &mut HirImpl) -> ResolveResult<()> {
        // Resolve target type
        self.resolve_type_annotation(&mut impl_block.target_type)?;

        // Create impl scope
        let impl_scope = Scope::new(
            self.context.scope_stack.next_scope_id(),
            ScopeKind::Impl,
            self.context.current_module(),
        )
        .with_node(impl_block.id);

        self.context.scope_stack.push_scope(impl_scope);

        // Resolve methods
        for method in &mut impl_block.methods {
            // Mark as method instead of function
            let method_symbol = Symbol::new(
                self.context.symbol_table.next_symbol_id(),
                method.name.clone(),
                SymbolKind::Method,
                self.context.current_module(),
                method.id,
            )
            .with_generics(method.generic_params.clone());

            let method_symbol_id = self
                .context
                .define_symbol(method_symbol)
                .map_err(|e| vec![e])?;

            self.context
                .node_to_symbol
                .insert(method.id, method_symbol_id);

            // Resolve the method like a function
            self.resolve_function(method)?;
        }

        // Pop impl scope
        self.context.scope_stack.pop_scope();

        Ok(())
    }

    /// Resolve an FFI function
    fn resolve_ffi_function(&mut self, ffi_func: &mut HirFfiFunction) -> ResolveResult<()> {
        // Create symbol for FFI function
        let symbol = Symbol::new(
            self.context.symbol_table.next_symbol_id(),
            ffi_func.name.clone(),
            SymbolKind::FfiFunction,
            self.context.current_module(),
            ffi_func.id,
        );

        // Define FFI function symbol
        let ffi_symbol_id = self.context.define_symbol(symbol).map_err(|e| vec![e])?;

        // Map node to symbol
        self.context
            .node_to_symbol
            .insert(ffi_func.id, ffi_symbol_id);

        // Resolve parameter types
        for param in &mut ffi_func.params {
            self.resolve_parameter(param)?;
        }

        // Resolve return type
        self.resolve_type_annotation(&mut ffi_func.return_type)?;

        Ok(())
    }

    /// Resolve an FFI variable
    fn resolve_ffi_variable(&mut self, ffi_var: &mut HirFfiVariable) -> ResolveResult<()> {
        // Create symbol for FFI variable
        let symbol = Symbol::new(
            self.context.symbol_table.next_symbol_id(),
            ffi_var.name.clone(),
            SymbolKind::FfiVariable,
            self.context.current_module(),
            ffi_var.id,
        )
        .with_type(ffi_var.ty.clone());

        // Define FFI variable symbol
        let ffi_symbol_id = self.context.define_symbol(symbol).map_err(|e| vec![e])?;

        // Map node to symbol
        self.context
            .node_to_symbol
            .insert(ffi_var.id, ffi_symbol_id);

        // Resolve variable type
        self.resolve_type_annotation(&mut ffi_var.ty)?;

        Ok(())
    }

    /// Resolve a test function
    fn resolve_test(&mut self, test: &mut HirTest) -> ResolveResult<()> {
        // Create symbol for test
        let symbol = Symbol::new(
            self.context.symbol_table.next_symbol_id(),
            test.name.clone(),
            SymbolKind::Test,
            self.context.current_module(),
            test.id,
        );

        // Define test symbol
        let test_symbol_id = self.context.define_symbol(symbol).map_err(|e| vec![e])?;

        // Map node to symbol
        self.context.node_to_symbol.insert(test.id, test_symbol_id);

        // Create function scope for test
        let test_scope = Scope::new(
            self.context.scope_stack.next_scope_id(),
            ScopeKind::Function,
            self.context.current_module(),
        )
        .with_node(test.id);

        self.context.scope_stack.push_scope(test_scope);

        // Resolve test body
        self.resolve_block(&mut test.body)?;

        // Pop test scope
        self.context.scope_stack.pop_scope();

        Ok(())
    }

    /// Resolve an import declaration
    fn resolve_import(&mut self, import: &mut HirImport) -> ResolveResult<()> {
        // Accept both "::" and "/" syntax for now, canonicalize "/" to "::"
        if import.path.contains("/") && !import.path.contains("::") {
            // Canonicalize to "::" syntax
            import.path = import.path.replace("/", "::");
        }

        // Resolve the module path
        match self.context.module_graph.resolve_module_path(&import.path) {
            Ok(target_module_id) => {
                // Create import relationship
                let import_kind = if let Some(ref items) = import.items {
                    if items.is_empty() {
                        ImportKind::All
                    } else {
                        ImportKind::Specific(items.clone())
                    }
                } else {
                    ImportKind::Single(import.alias.clone().unwrap_or_else(|| {
                        import
                            .path
                            .split("::")
                            .last()
                            .unwrap_or("unknown")
                            .to_string()
                    }))
                };

                let module_import = ModuleImport {
                    source_module: import.path.clone(),
                    kind: import_kind,
                    alias: import.alias.clone(),
                    resolved_module_id: Some(target_module_id),
                };

                let current_module = self.context.current_module();
                if let Err(error) = self.context.module_graph.add_import(
                    current_module,
                    target_module_id,
                    module_import,
                ) {
                    self.context.add_error(error);
                }
            }
            Err(error) => {
                self.context.add_error(error);
            }
        }

        Ok(())
    }

    /// Resolve a type annotation
    fn resolve_type_annotation(&mut self, ty: &mut HirType) -> ResolveResult<()> {
        match ty {
            HirType::Unresolved(name) => {
                // Try to resolve the type name
                match self.context.resolve_type(name) {
                    Ok(symbol_id) => {
                        if let Some(_symbol) = self.context.symbol_table.get(symbol_id)
                            && let Err(error) = self.context.check_access(symbol_id, None)
                        {
                            self.context.add_error(error);
                        }
                        // Keep as unresolved for now - full resolution happens in type checker
                    }
                    Err(error) => {
                        self.context.add_error(error);
                    }
                }
            }
            HirType::Pointer(inner) => {
                self.resolve_type_annotation(inner)?;
            }
            HirType::Array(inner) => {
                self.resolve_type_annotation(inner)?;
            }
            HirType::SizedArray(inner, _size) => {
                self.resolve_type_annotation(inner)?;
            }
            HirType::Optional(inner) => {
                self.resolve_type_annotation(inner)?;
            }
            HirType::Function(params, return_type) => {
                for param_type in params {
                    self.resolve_type_annotation(param_type)?;
                }
                self.resolve_type_annotation(return_type)?;
            }
            HirType::GenericInstance(_name, type_args) => {
                for type_arg in type_args {
                    self.resolve_type_annotation(type_arg)?;
                }
            }
            HirType::Union(types) => {
                for union_type in types {
                    self.resolve_type_annotation(union_type)?;
                }
            }
            HirType::Intersection(types) => {
                for intersection_type in types {
                    self.resolve_type_annotation(intersection_type)?;
                }
            }
            _ => {
                // Primitive and other resolved types don't need resolution
            }
        }
        Ok(())
    }

    /// Resolve a block of statements
    fn resolve_block(&mut self, block: &mut HirBlock) -> ResolveResult<()> {
        // Create block scope
        let block_scope = Scope::new(
            self.context.scope_stack.next_scope_id(),
            ScopeKind::Block,
            self.context.current_module(),
        )
        .with_node(block.id);

        self.context.scope_stack.push_scope(block_scope);

        // Resolve statements
        for stmt in &mut block.stmts {
            self.resolve_statement(stmt)?;
        }

        // Resolve expression if present
        if let Some(expr) = &mut block.expr {
            self.resolve_expression(expr)?;
        }

        // Pop block scope
        self.context.scope_stack.pop_scope();

        Ok(())
    }

    /// Resolve a statement
    fn resolve_statement(&mut self, stmt: &mut HirStmt) -> ResolveResult<()> {
        match &mut stmt.kind {
            HirStmtKind::Expr(expr) => {
                self.resolve_expression(expr)?;
            }
            HirStmtKind::Let { pattern, ty, init } => {
                // Resolve type annotation if present
                if let Some(type_annotation) = ty {
                    self.resolve_type_annotation(type_annotation)?;
                }

                // Resolve initializer if present
                if let Some(init_expr) = init {
                    self.resolve_expression(init_expr)?;
                }

                // Resolve pattern (defines new variables)
                self.resolve_pattern(pattern)?;
            }
            HirStmtKind::Assign { lhs, rhs } => {
                self.resolve_expression(lhs)?;
                self.resolve_expression(rhs)?;
            }
            HirStmtKind::Return(expr) => {
                if let Some(return_expr) = expr {
                    self.resolve_expression(return_expr)?;
                }
            }
            HirStmtKind::Break(expr) => {
                // Check if we're in a loop context
                if !self.context.scope_stack.can_break_or_continue() {
                    let error = ResolveError::new(ResolveErrorKind::InvalidTypeParameterUsage {
                        param_name: "break".to_string(),
                        context: "outside of loop".to_string(),
                    });
                    self.context.add_error(error);
                }

                if let Some(break_expr) = expr {
                    self.resolve_expression(break_expr)?;
                }
            }
            HirStmtKind::Continue => {
                // Check if we're in a loop context
                if !self.context.scope_stack.can_break_or_continue() {
                    let error = ResolveError::new(ResolveErrorKind::InvalidTypeParameterUsage {
                        param_name: "continue".to_string(),
                        context: "outside of loop".to_string(),
                    });
                    self.context.add_error(error);
                }
            }
        }
        Ok(())
    }

    /// Resolve an expression
    fn resolve_expression(&mut self, expr: &mut HirExpr) -> ResolveResult<()> {
        match expr.kind.as_mut() {
            HirExprKind::Variable(name) => {
                // Try to resolve the variable name
                if let Some(symbol_id) = self.context.resolve_name(name) {
                    self.context.node_to_symbol.insert(expr.id, symbol_id);
                    if let Err(error) = self.context.check_access(symbol_id, None) {
                        self.context.add_error(error);
                    }
                } else {
                    let candidates = self.context.symbol_table.find_candidates(name, 2);
                    let error =
                        ResolveError::undefined_symbol_with_candidates(name.clone(), candidates);
                    self.context.add_error(error);
                }
            }
            HirExprKind::FieldAccess { base, field: _ } => {
                self.resolve_expression(base)?;
                // Field resolution happens in type checker
            }
            HirExprKind::Index { base, index } => {
                self.resolve_expression(base)?;
                self.resolve_expression(index)?;
            }
            HirExprKind::Call { func, args } => {
                self.resolve_expression(func)?;
                for arg in args {
                    self.resolve_expression(arg)?;
                }
            }
            HirExprKind::MethodCall {
                receiver,
                method: _,
                args,
            } => {
                self.resolve_expression(receiver)?;
                for arg in args {
                    self.resolve_expression(arg)?;
                }
                // Method resolution happens in type checker
            }
            HirExprKind::Unary { expr, .. } => {
                self.resolve_expression(expr)?;
            }
            HirExprKind::Binary { lhs, rhs, .. } => {
                self.resolve_expression(lhs)?;
                self.resolve_expression(rhs)?;
            }
            HirExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expression(condition)?;
                self.resolve_block(then_branch)?;
                if let Some(else_block) = else_branch {
                    self.resolve_block(else_block)?;
                }
            }
            HirExprKind::Match { expr, arms } => {
                self.resolve_expression(expr)?;
                for arm in arms {
                    // Create match arm scope
                    let arm_scope = Scope::new(
                        self.context.scope_stack.next_scope_id(),
                        ScopeKind::MatchArm,
                        self.context.current_module(),
                    )
                    .with_node(arm.id);

                    self.context.scope_stack.push_scope(arm_scope);

                    self.resolve_pattern(&mut arm.pattern)?;

                    if let Some(guard) = &mut arm.guard {
                        self.resolve_expression(guard)?;
                    }

                    match &mut arm.body {
                        veil_hir::HirMatchArmBody::Expr(body_expr) => {
                            self.resolve_expression(body_expr)?;
                        }
                        veil_hir::HirMatchArmBody::Block(body_block) => {
                            self.resolve_block(body_block)?;
                        }
                    }

                    self.context.scope_stack.pop_scope();
                }
            }
            HirExprKind::Loop { body } => {
                let loop_scope = Scope::new(
                    self.context.scope_stack.next_scope_id(),
                    ScopeKind::Loop,
                    self.context.current_module(),
                )
                .with_loop_control();

                self.context.scope_stack.push_scope(loop_scope);
                self.resolve_block(body)?;
                self.context.scope_stack.pop_scope();
            }
            HirExprKind::While { condition, body } => {
                self.resolve_expression(condition)?;

                let loop_scope = Scope::new(
                    self.context.scope_stack.next_scope_id(),
                    ScopeKind::Loop,
                    self.context.current_module(),
                )
                .with_loop_control();

                self.context.scope_stack.push_scope(loop_scope);
                self.resolve_block(body)?;
                self.context.scope_stack.pop_scope();
            }
            HirExprKind::For {
                pattern,
                iter,
                body,
            } => {
                self.resolve_expression(iter)?;

                let loop_scope = Scope::new(
                    self.context.scope_stack.next_scope_id(),
                    ScopeKind::Loop,
                    self.context.current_module(),
                )
                .with_loop_control();

                self.context.scope_stack.push_scope(loop_scope);
                self.resolve_pattern(pattern)?;
                self.resolve_block(body)?;
                self.context.scope_stack.pop_scope();
            }
            HirExprKind::Ref(expr) => {
                self.resolve_expression(expr)?;
            }
            HirExprKind::Deref(expr) => {
                self.resolve_expression(expr)?;
                // Check if we need unsafe context for raw pointer deref
                // This is simplified - full safety checking happens in type checker
            }
            HirExprKind::Cast { expr, ty } => {
                self.resolve_expression(expr)?;
                self.resolve_type_annotation(ty)?;
            }
            HirExprKind::StructLiteral { name: _, fields } => {
                // Struct name resolution happens in type checker
                for (_field_name, field_expr) in fields {
                    self.resolve_expression(field_expr)?;
                }
            }
            HirExprKind::ArrayLiteral(elements) => {
                for element in elements {
                    self.resolve_expression(element)?;
                }
            }
            HirExprKind::TupleLiteral(elements) => {
                for element in elements {
                    self.resolve_expression(element)?;
                }
            }
            HirExprKind::Block(block) => {
                self.resolve_block(block)?;
            }
            HirExprKind::UnsafeBlock(block) => {
                let unsafe_scope = Scope::new(
                    self.context.scope_stack.next_scope_id(),
                    ScopeKind::Unsafe,
                    self.context.current_module(),
                )
                .with_unsafe();

                self.context.scope_stack.push_scope(unsafe_scope);
                self.resolve_block(block)?;
                self.context.scope_stack.pop_scope();
            }
            HirExprKind::Await(expr) => {
                // Check if we're in an async context
                if !self.context.scope_stack.is_in_async_context() {
                    let error = ResolveError::new(ResolveErrorKind::InvalidTypeParameterUsage {
                        param_name: "await".to_string(),
                        context: "outside of async context".to_string(),
                    });
                    self.context.add_error(error);
                }
                self.resolve_expression(expr)?;
            }
            HirExprKind::Spawn(block) => {
                let async_scope = Scope::new(
                    self.context.scope_stack.next_scope_id(),
                    ScopeKind::Async,
                    self.context.current_module(),
                )
                .with_async();

                self.context.scope_stack.push_scope(async_scope);
                self.resolve_block(block)?;
                self.context.scope_stack.pop_scope();
            }
            HirExprKind::Pipeline { expr, func } => {
                self.resolve_expression(expr)?;
                self.resolve_expression(func)?;
            }
            HirExprKind::PostfixQuestion(expr) => {
                self.resolve_expression(expr)?;
            }
            HirExprKind::PostfixIncrement(expr) => {
                self.resolve_expression(expr)?;
            }
            HirExprKind::PostfixDecrement(expr) => {
                self.resolve_expression(expr)?;
            }
            HirExprKind::Template { parts } => {
                for part in parts {
                    if let veil_hir::HirTemplateStringPart::Expr(template_expr) = part {
                        self.resolve_expression(template_expr)?;
                    }
                }
            }
            _ => {
                // Literals and other leaf expressions don't need resolution
            }
        }
        Ok(())
    }

    /// Resolve a pattern (for destructuring and variable binding)
    fn resolve_pattern(&mut self, pattern: &mut HirPattern) -> ResolveResult<()> {
        match pattern.kind.as_mut() {
            HirPatternKind::Variable(name) => {
                // Define a new variable symbol for this pattern
                let symbol = Symbol::new(
                    self.context.symbol_table.next_symbol_id(),
                    name.clone(),
                    SymbolKind::Variable,
                    self.context.current_module(),
                    pattern.id,
                );

                let symbol_id = self.context.define_symbol(symbol).map_err(|e| vec![e])?;

                self.context.node_to_symbol.insert(pattern.id, symbol_id);
            }
            HirPatternKind::Literal(expr) => {
                self.resolve_expression(expr)?;
            }
            HirPatternKind::EnumVariant { patterns, .. } => {
                // Enum variant resolution happens in type checker
                for sub_pattern in patterns {
                    self.resolve_pattern(sub_pattern)?;
                }
            }
            HirPatternKind::Struct { fields, .. } => {
                // Struct resolution happens in type checker
                for (_field_name, field_pattern) in fields {
                    self.resolve_pattern(field_pattern)?;
                }
            }
            HirPatternKind::Tuple(patterns) => {
                for sub_pattern in patterns {
                    self.resolve_pattern(sub_pattern)?;
                }
            }
            HirPatternKind::Array(patterns) => {
                for sub_pattern in patterns {
                    self.resolve_pattern(sub_pattern)?;
                }
            }
            HirPatternKind::Range { start, end, .. } => {
                if let Some(start_expr) = start {
                    self.resolve_expression(start_expr)?;
                }
                if let Some(end_expr) = end {
                    self.resolve_expression(end_expr)?;
                }
            }
            HirPatternKind::Wildcard => {
                // Wildcard patterns don't need resolution
            }
        }
        Ok(())
    }

    /// Get the resolver context
    pub fn context(&self) -> &ResolverContext {
        &self.context
    }

    /// Get a mutable reference to the resolver context
    pub fn context_mut(&mut self) -> &mut ResolverContext {
        &mut self.context
    }

    /// Take the resolver context
    pub fn into_context(self) -> ResolverContext {
        self.context
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function to resolve a HIR program
pub fn resolve_program(program: &mut HirProgram) -> ResolveResult<ResolverContext> {
    let mut resolver = Resolver::new();
    resolver.resolve_program(program)?;
    Ok(resolver.into_context())
}

#[cfg(test)]
mod tests {
    use super::*;
    use veil_hir::{HirBlock, HirFunction, HirParam};

    fn create_test_function() -> HirFunction {
        HirFunction {
            id: NodeId::new(1),
            name: "test_function".to_string(),
            symbol_id: None,
            generic_params: Vec::new(),
            params: vec![HirParam {
                id: NodeId::new(2),
                name: "param".to_string(),
                symbol_id: None,
                ty: HirType::I32,
            }],
            return_type: HirType::Void,
            body: HirBlock {
                id: NodeId::new(3),
                stmts: Vec::new(),
                expr: None,
            },
        }
    }

    #[test]
    fn test_resolver_creation() {
        let resolver = Resolver::new();
        assert_eq!(resolver.context.symbol_table.len(), 0);
        assert!(resolver.context.module_graph.is_empty());
    }

    #[test]
    fn test_function_resolution() {
        let mut resolver = Resolver::new();
        let mut function = create_test_function();

        assert!(resolver.resolve_function(&mut function).is_ok());
        assert!(!resolver.context.symbol_table.is_empty());

        // Check that function symbol was created
        let symbols = resolver.context.symbol_table.find_by_name("test_function");
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].kind, SymbolKind::Function);
    }

    #[test]
    fn test_duplicate_symbol_error() {
        let mut resolver = Resolver::new();

        // Define the same symbol twice
        let symbol1 = Symbol::new(
            SymbolId::new(0),
            "duplicate".to_string(),
            SymbolKind::Function,
            ModuleId::new(0),
            NodeId::new(0),
        );

        let symbol2 = Symbol::new(
            SymbolId::new(1),
            "duplicate".to_string(),
            SymbolKind::Variable,
            ModuleId::new(0),
            NodeId::new(1),
        );

        // First definition should succeed
        assert!(resolver.context.define_symbol(symbol1).is_ok());

        // Second definition should fail
        assert!(resolver.context.define_symbol(symbol2).is_err());
    }

    #[test]
    fn test_scope_management() {
        let mut resolver = Resolver::new();

        // Push a function scope
        let func_scope = Scope::new(
            resolver.context.scope_stack.next_scope_id(),
            ScopeKind::Function,
            ModuleId::new(0),
        );
        resolver.context.scope_stack.push_scope(func_scope);

        assert_eq!(resolver.context.scope_stack.depth(), 2);

        // Pop the scope
        resolver.context.scope_stack.pop_scope();
        assert_eq!(resolver.context.scope_stack.depth(), 1);
    }

    #[test]
    fn test_name_resolution() {
        let mut resolver = Resolver::new();

        // Define a symbol
        let symbol = Symbol::new(
            SymbolId::new(0),
            "test_symbol".to_string(),
            SymbolKind::Function,
            ModuleId::new(0),
            NodeId::new(0),
        );

        let symbol_id = resolver.context.define_symbol(symbol).unwrap();

        // Resolve the name
        let resolved = resolver.context.resolve_name("test_symbol");
        assert_eq!(resolved, Some(symbol_id));

        // Try to resolve non-existent name
        let not_found = resolver.context.resolve_name("nonexistent");
        assert_eq!(not_found, None);
    }
}
