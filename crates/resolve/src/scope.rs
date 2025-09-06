//! Scope management for the Veil resolver
//!
//! This module provides data structures for managing nested scopes
//! and implementing lexical scoping rules during name resolution.

use codespan::Span;
use indexmap::IndexMap;
use std::collections::HashMap;
use veil_hir::{ModuleId, NodeId, SymbolId};

use crate::symbol_table::SymbolKind;

/// A single scope in the scope stack
#[derive(Debug, Clone)]
pub struct Scope {
    /// Unique identifier for this scope
    pub id: ScopeId,
    /// Type of scope (function, block, module, etc.)
    pub kind: ScopeKind,
    /// Parent scope (None for root scope)
    pub parent: Option<ScopeId>,
    /// Module this scope belongs to
    pub module_id: ModuleId,
    /// Node that created this scope
    pub node_id: Option<NodeId>,
    /// Span of the scope in source code
    pub span: Option<Span>,
    /// Symbols defined directly in this scope
    symbols: IndexMap<String, SymbolId>,
    /// Type parameters introduced in this scope
    type_params: HashMap<String, SymbolId>,
    /// Whether this scope can break/continue (for loops)
    pub can_break: bool,
    pub can_continue: bool,
    /// Whether this scope is inside an unsafe block
    pub is_unsafe: bool,
    /// Whether this scope is inside an async context
    pub is_async: bool,
}

impl Scope {
    pub fn new(id: ScopeId, kind: ScopeKind, module_id: ModuleId) -> Self {
        Self {
            id,
            kind,
            parent: None,
            module_id,
            node_id: None,
            span: None,
            symbols: IndexMap::new(),
            type_params: HashMap::new(),
            can_break: false,
            can_continue: false,
            is_unsafe: false,
            is_async: false,
        }
    }

    pub fn with_parent(mut self, parent: ScopeId) -> Self {
        self.parent = Some(parent);
        self
    }

    pub fn with_node(mut self, node_id: NodeId) -> Self {
        self.node_id = Some(node_id);
        self
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_loop_control(mut self) -> Self {
        self.can_break = true;
        self.can_continue = true;
        self
    }

    pub fn with_unsafe(mut self) -> Self {
        self.is_unsafe = true;
        self
    }

    pub fn with_async(mut self) -> Self {
        self.is_async = true;
        self
    }

    /// Define a symbol in this scope
    pub fn define_symbol(&mut self, name: String, symbol_id: SymbolId) -> Result<(), String> {
        if self.symbols.contains_key(&name) {
            Err(format!(
                "Symbol '{}' is already defined in this scope",
                name
            ))
        } else {
            self.symbols.insert(name, symbol_id);
            Ok(())
        }
    }

    /// Define a type parameter in this scope
    pub fn define_type_param(&mut self, name: String, symbol_id: SymbolId) -> Result<(), String> {
        match self.type_params.entry(name) {
            std::collections::hash_map::Entry::Occupied(entry) => Err(format!(
                "Type parameter '{}' is already defined in this scope",
                entry.key()
            )),
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(symbol_id);
                Ok(())
            }
        }
    }

    /// Look up a symbol in this scope (not recursive)
    pub fn lookup_local(&self, name: &str) -> Option<SymbolId> {
        self.symbols.get(name).copied()
    }

    /// Look up a type parameter in this scope (not recursive)
    pub fn lookup_local_type_param(&self, name: &str) -> Option<SymbolId> {
        self.type_params.get(name).copied()
    }

    /// Get all symbols defined in this scope
    pub fn local_symbols(&self) -> impl Iterator<Item = (&String, &SymbolId)> {
        self.symbols.iter()
    }

    /// Get all type parameters defined in this scope
    pub fn local_type_params(&self) -> impl Iterator<Item = (&String, &SymbolId)> {
        self.type_params.iter()
    }

    /// Check if this scope shadows a symbol from parent scopes
    pub fn shadows_symbol(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }

    /// Check if this scope can define a given symbol kind
    pub fn can_define(&self, kind: SymbolKind) -> bool {
        match (&self.kind, &kind) {
            // Module scope can define most things
            (ScopeKind::Module, _) => !matches!(kind, SymbolKind::Parameter),

            // Function scope can define parameters and local variables
            (ScopeKind::Function, SymbolKind::Parameter) => true,
            (ScopeKind::Function, SymbolKind::Variable) => true,
            (ScopeKind::Function, SymbolKind::Constant) => true,

            // Block scope can define local variables
            (ScopeKind::Block, SymbolKind::Variable) => true,
            (ScopeKind::Block, SymbolKind::Constant) => true,

            // Loop scope inherits from block
            (ScopeKind::Loop, SymbolKind::Variable) => true,
            (ScopeKind::Loop, SymbolKind::Constant) => true,

            // Match arm scope for pattern variables
            (ScopeKind::MatchArm, SymbolKind::Variable) => true,

            // Type scope for generic parameters
            (ScopeKind::Type, SymbolKind::GenericParameter) => true,

            _ => false,
        }
    }

    /// Check if this scope requires unsafe context for certain operations
    pub fn requires_unsafe_for(&self, operation: UnsafeOperation) -> bool {
        match operation {
            UnsafeOperation::RawPointerDeref => !self.is_unsafe,
            UnsafeOperation::FfiCall => !self.is_unsafe,
            UnsafeOperation::UnionFieldAccess => !self.is_unsafe,
            UnsafeOperation::TransmuteCall => !self.is_unsafe,
        }
    }
}

/// Different kinds of scopes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    /// Module-level scope
    Module,
    /// Function scope (including parameters)
    Function,
    /// Generic block scope
    Block,
    /// Loop scope (for, while, loop)
    Loop,
    /// Match arm scope
    MatchArm,
    /// Type definition scope (for generic parameters)
    Type,
    /// Impl block scope
    Impl,
    /// Unsafe block scope
    Unsafe,
    /// Async block scope
    Async,
}

impl ScopeKind {
    /// Get a human-readable description of this scope kind
    pub fn description(&self) -> &'static str {
        match self {
            Self::Module => "module",
            Self::Function => "function",
            Self::Block => "block",
            Self::Loop => "loop",
            Self::MatchArm => "match arm",
            Self::Type => "type definition",
            Self::Impl => "impl block",
            Self::Unsafe => "unsafe block",
            Self::Async => "async block",
        }
    }

    /// Check if this scope kind creates a new namespace
    pub fn creates_namespace(&self) -> bool {
        matches!(self, Self::Module | Self::Type | Self::Impl)
    }

    /// Check if this scope kind allows break/continue
    pub fn allows_loop_control(&self) -> bool {
        matches!(self, Self::Loop)
    }
}

/// Unique identifier for scopes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub u32);

impl ScopeId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    pub fn as_u32(self) -> u32 {
        self.0
    }
}

/// Operations that require unsafe context
#[derive(Debug, Clone, Copy)]
pub enum UnsafeOperation {
    /// Dereferencing raw pointers
    RawPointerDeref,
    /// Calling FFI functions
    FfiCall,
    /// Accessing union fields
    UnionFieldAccess,
    /// Calling transmute
    TransmuteCall,
}

/// Stack of scopes for managing nested lexical scoping
#[derive(Debug, Clone)]
pub struct ScopeStack {
    /// All scopes indexed by ID
    scopes: IndexMap<ScopeId, Scope>,
    /// Current scope stack (most recent scope is last)
    stack: Vec<ScopeId>,
    /// Next available scope ID
    next_scope_id: u32,
}

impl ScopeStack {
    pub fn new() -> Self {
        Self {
            scopes: IndexMap::new(),
            stack: Vec::new(),
            next_scope_id: 0,
        }
    }

    /// Generate a new unique scope ID
    pub fn next_scope_id(&mut self) -> ScopeId {
        let id = ScopeId::new(self.next_scope_id);
        self.next_scope_id += 1;
        id
    }

    /// Push a new scope onto the stack
    pub fn push_scope(&mut self, mut scope: Scope) -> ScopeId {
        // Set parent to current scope if we have one
        if let Some(&current_id) = self.stack.last() {
            scope.parent = Some(current_id);
        }

        let scope_id = scope.id;
        self.scopes.insert(scope_id, scope);
        self.stack.push(scope_id);
        scope_id
    }

    /// Pop the current scope from the stack
    pub fn pop_scope(&mut self) -> Option<Scope> {
        if let Some(scope_id) = self.stack.pop() {
            self.scopes.shift_remove(&scope_id)
        } else {
            None
        }
    }

    /// Get the current (top) scope
    pub fn current_scope(&self) -> Option<&Scope> {
        self.stack.last().and_then(|&id| self.scopes.get(&id))
    }

    /// Get a mutable reference to the current scope
    pub fn current_scope_mut(&mut self) -> Option<&mut Scope> {
        if let Some(&scope_id) = self.stack.last() {
            self.scopes.get_mut(&scope_id)
        } else {
            None
        }
    }

    /// Get a scope by ID
    pub fn get_scope(&self, scope_id: ScopeId) -> Option<&Scope> {
        self.scopes.get(&scope_id)
    }

    /// Get a mutable reference to a scope by ID
    pub fn get_scope_mut(&mut self, scope_id: ScopeId) -> Option<&mut Scope> {
        self.scopes.get_mut(&scope_id)
    }

    /// Define a symbol in the current scope
    pub fn define_symbol(&mut self, name: String, symbol_id: SymbolId) -> Result<(), String> {
        if let Some(scope) = self.current_scope_mut() {
            scope.define_symbol(name, symbol_id)
        } else {
            Err("No current scope to define symbol in".to_string())
        }
    }

    /// Define a type parameter in the current scope
    pub fn define_type_param(&mut self, name: String, symbol_id: SymbolId) -> Result<(), String> {
        if let Some(scope) = self.current_scope_mut() {
            scope.define_type_param(name, symbol_id)
        } else {
            Err("No current scope to define type parameter in".to_string())
        }
    }

    /// Look up a symbol starting from the current scope and walking up the stack
    pub fn lookup_symbol(&self, name: &str) -> Option<(SymbolId, ScopeId)> {
        for &scope_id in self.stack.iter().rev() {
            if let Some(scope) = self.scopes.get(&scope_id)
                && let Some(symbol_id) = scope.lookup_local(name)
            {
                return Some((symbol_id, scope_id));
            }
        }
        None
    }

    /// Look up a type parameter starting from the current scope and walking up the stack
    pub fn lookup_type_param(&self, name: &str) -> Option<(SymbolId, ScopeId)> {
        for &scope_id in self.stack.iter().rev() {
            if let Some(scope) = self.scopes.get(&scope_id)
                && let Some(symbol_id) = scope.lookup_local_type_param(name)
            {
                return Some((symbol_id, scope_id));
            }
        }
        None
    }

    /// Check if we're currently in an unsafe context
    pub fn is_in_unsafe_context(&self) -> bool {
        self.stack.iter().rev().any(|&scope_id| {
            self.scopes
                .get(&scope_id)
                .map(|scope| scope.is_unsafe)
                .unwrap_or(false)
        })
    }

    /// Check if we're currently in an async context
    pub fn is_in_async_context(&self) -> bool {
        self.stack.iter().rev().any(|&scope_id| {
            self.scopes
                .get(&scope_id)
                .map(|scope| scope.is_async)
                .unwrap_or(false)
        })
    }

    /// Check if we're in a context where break/continue is allowed
    pub fn can_break_or_continue(&self) -> bool {
        self.stack.iter().rev().any(|&scope_id| {
            self.scopes
                .get(&scope_id)
                .map(|scope| scope.can_break)
                .unwrap_or(false)
        })
    }

    /// Find the nearest scope of a given kind
    pub fn find_scope_of_kind(&self, kind: ScopeKind) -> Option<ScopeId> {
        self.stack
            .iter()
            .rev()
            .find(|&&scope_id| {
                self.scopes
                    .get(&scope_id)
                    .map(|scope| scope.kind == kind)
                    .unwrap_or(false)
            })
            .copied()
    }

    /// Get the current module scope
    pub fn current_module_scope(&self) -> Option<ScopeId> {
        self.find_scope_of_kind(ScopeKind::Module)
    }

    /// Get the current function scope
    pub fn current_function_scope(&self) -> Option<ScopeId> {
        self.find_scope_of_kind(ScopeKind::Function)
    }

    /// Get the depth of the scope stack
    pub fn depth(&self) -> usize {
        self.stack.len()
    }

    /// Check if the stack is empty
    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    /// Clear all scopes
    pub fn clear(&mut self) {
        self.scopes.clear();
        self.stack.clear();
    }

    /// Get all symbols visible from the current scope
    pub fn visible_symbols(&self) -> Vec<(String, SymbolId, ScopeId)> {
        let mut symbols = Vec::new();
        let mut seen_names = std::collections::HashSet::new();

        // Walk up the scope stack, collecting symbols
        for &scope_id in self.stack.iter().rev() {
            if let Some(scope) = self.scopes.get(&scope_id) {
                for (name, &symbol_id) in scope.local_symbols() {
                    // Only include if we haven't seen this name before (shadowing)
                    if seen_names.insert(name.clone()) {
                        symbols.push((name.clone(), symbol_id, scope_id));
                    }
                }
            }
        }

        symbols
    }

    /// Get all type parameters visible from the current scope
    pub fn visible_type_params(&self) -> Vec<(String, SymbolId, ScopeId)> {
        let mut params = Vec::new();
        let mut seen_names = std::collections::HashSet::new();

        // Walk up the scope stack, collecting type parameters
        for &scope_id in self.stack.iter().rev() {
            if let Some(scope) = self.scopes.get(&scope_id) {
                for (name, &symbol_id) in scope.local_type_params() {
                    // Only include if we haven't seen this name before (shadowing)
                    if seen_names.insert(name.clone()) {
                        params.push((name.clone(), symbol_id, scope_id));
                    }
                }
            }
        }

        params
    }
}

impl Default for ScopeStack {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use veil_hir::ModuleId;

    #[test]
    fn test_scope_creation() {
        let scope = Scope::new(ScopeId::new(0), ScopeKind::Function, ModuleId::new(0))
            .with_loop_control()
            .with_unsafe();

        assert_eq!(scope.kind, ScopeKind::Function);
        assert!(scope.can_break);
        assert!(scope.can_continue);
        assert!(scope.is_unsafe);
    }

    #[test]
    fn test_scope_stack_operations() {
        let mut stack = ScopeStack::new();

        // Push module scope
        let module_scope = Scope::new(stack.next_scope_id(), ScopeKind::Module, ModuleId::new(0));
        let module_id = stack.push_scope(module_scope);

        // Push function scope
        let function_scope =
            Scope::new(stack.next_scope_id(), ScopeKind::Function, ModuleId::new(0));
        let function_id = stack.push_scope(function_scope);

        assert_eq!(stack.depth(), 2);
        assert_eq!(stack.current_module_scope(), Some(module_id));
        assert_eq!(stack.current_function_scope(), Some(function_id));

        // Pop function scope
        let popped = stack.pop_scope();
        assert!(popped.is_some());
        assert_eq!(popped.unwrap().kind, ScopeKind::Function);
        assert_eq!(stack.depth(), 1);
    }

    #[test]
    fn test_symbol_definition_and_lookup() {
        let mut stack = ScopeStack::new();

        // Create and push a scope
        let scope = Scope::new(stack.next_scope_id(), ScopeKind::Function, ModuleId::new(0));
        stack.push_scope(scope);

        // Define a symbol
        let symbol_id = SymbolId::new(42);
        stack
            .define_symbol("test_var".to_string(), symbol_id)
            .unwrap();

        // Look up the symbol
        let result = stack.lookup_symbol("test_var");
        assert!(result.is_some());
        let (found_symbol_id, _) = result.unwrap();
        assert_eq!(found_symbol_id, symbol_id);

        // Look up non-existent symbol
        let not_found = stack.lookup_symbol("not_exists");
        assert!(not_found.is_none());
    }

    #[test]
    fn test_shadowing() {
        let mut stack = ScopeStack::new();

        // Push outer scope and define a symbol
        let outer_scope = Scope::new(stack.next_scope_id(), ScopeKind::Block, ModuleId::new(0));
        stack.push_scope(outer_scope);
        stack
            .define_symbol("x".to_string(), SymbolId::new(1))
            .unwrap();

        // Push inner scope and shadow the symbol
        let inner_scope = Scope::new(stack.next_scope_id(), ScopeKind::Block, ModuleId::new(0));
        stack.push_scope(inner_scope);
        stack
            .define_symbol("x".to_string(), SymbolId::new(2))
            .unwrap();

        // Lookup should find the shadowed symbol
        let result = stack.lookup_symbol("x");
        assert!(result.is_some());
        let (symbol_id, _) = result.unwrap();
        assert_eq!(symbol_id, SymbolId::new(2));

        // Pop inner scope
        stack.pop_scope();

        // Lookup should now find the original symbol
        let result = stack.lookup_symbol("x");
        assert!(result.is_some());
        let (symbol_id, _) = result.unwrap();
        assert_eq!(symbol_id, SymbolId::new(1));
    }

    #[test]
    fn test_context_checking() {
        let mut stack = ScopeStack::new();

        // Normal scope
        let normal_scope = Scope::new(stack.next_scope_id(), ScopeKind::Block, ModuleId::new(0));
        stack.push_scope(normal_scope);
        assert!(!stack.is_in_unsafe_context());
        assert!(!stack.is_in_async_context());

        // Unsafe scope
        let unsafe_scope =
            Scope::new(stack.next_scope_id(), ScopeKind::Unsafe, ModuleId::new(0)).with_unsafe();
        stack.push_scope(unsafe_scope);
        assert!(stack.is_in_unsafe_context());

        // Async scope
        let async_scope =
            Scope::new(stack.next_scope_id(), ScopeKind::Async, ModuleId::new(0)).with_async();
        stack.push_scope(async_scope);
        assert!(stack.is_in_async_context());
    }
}
