//! Symbol table implementation for the Veil resolver
//!
//! This module provides data structures for tracking symbol definitions,
//! their types, scopes, and visibility throughout the compilation process.

use codespan::Span;
use indexmap::IndexMap;
use std::collections::HashMap;
use veil_hir::{HirType, HirVisibility, ModuleId, NodeId, SymbolId};

/// A symbol in the symbol table
#[derive(Debug, Clone)]
pub struct Symbol {
    pub id: SymbolId,
    pub name: String,
    pub kind: SymbolKind,
    pub ty: Option<HirType>,
    pub visibility: HirVisibility,
    pub module_id: ModuleId,
    pub node_id: NodeId,
    pub span: Option<Span>,
    pub is_mutable: bool,
    pub is_generic: bool,
    pub generic_params: Vec<String>,
}

impl Symbol {
    pub fn new(
        id: SymbolId,
        name: String,
        kind: SymbolKind,
        module_id: ModuleId,
        node_id: NodeId,
    ) -> Self {
        Self {
            id,
            name,
            kind,
            ty: None,
            visibility: HirVisibility::Private,
            module_id,
            node_id,
            span: None,
            is_mutable: false,
            is_generic: false,
            generic_params: Vec::new(),
        }
    }

    pub fn with_type(mut self, ty: HirType) -> Self {
        self.ty = Some(ty);
        self
    }

    pub fn with_visibility(mut self, visibility: HirVisibility) -> Self {
        self.visibility = visibility;
        self
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_mutability(mut self, is_mutable: bool) -> Self {
        self.is_mutable = is_mutable;
        self
    }

    pub fn with_generics(mut self, generic_params: Vec<String>) -> Self {
        self.is_generic = !generic_params.is_empty();
        self.generic_params = generic_params;
        self
    }

    /// Check if this symbol is accessible from the given module with the given visibility rules
    pub fn is_accessible_from(&self, from_module: ModuleId, _current_crate: ModuleId) -> bool {
        match &self.visibility {
            HirVisibility::Private => self.module_id == from_module,
            HirVisibility::Public => true,
            HirVisibility::PublicCrate => {
                // Same crate (simplified - in reality we'd need proper crate tracking)
                true
            }
            HirVisibility::PublicSuper => {
                // Parent module or same module (simplified)
                self.module_id == from_module
            }
            HirVisibility::PublicIn(_path) => {
                // Specific path visibility (simplified)
                self.module_id == from_module
            }
            HirVisibility::PublicInResolved(target_module) => {
                // Resolved path visibility (simplified)
                // Allow if accessed from the resolved module or within the same defining module
                from_module == *target_module
                    || self.module_id == *target_module
                    || self.module_id == from_module
            }
        }
    }

    /// Check if this symbol is a type (struct, enum, trait, etc.)
    pub fn is_type(&self) -> bool {
        matches!(
            self.kind,
            SymbolKind::Struct | SymbolKind::Enum | SymbolKind::Trait | SymbolKind::TypeAlias
        )
    }

    /// Check if this symbol is a value (function, variable, constant)
    pub fn is_value(&self) -> bool {
        matches!(
            self.kind,
            SymbolKind::Function
                | SymbolKind::Variable
                | SymbolKind::Constant
                | SymbolKind::Parameter
                | SymbolKind::EnumVariant
        )
    }

    /// Check if this symbol can be called (function or method)
    pub fn is_callable(&self) -> bool {
        matches!(self.kind, SymbolKind::Function | SymbolKind::Method)
    }
}

/// Different kinds of symbols that can exist in the symbol table
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    /// Function definition
    Function,
    /// Method in an impl block
    Method,
    /// Struct type definition
    Struct,
    /// Enum type definition
    Enum,
    /// Enum variant
    EnumVariant,
    /// Trait definition
    Trait,
    /// Type alias
    TypeAlias,
    /// Variable (let binding)
    Variable,
    /// Constant (const binding)
    Constant,
    /// Function parameter
    Parameter,
    /// Generic type parameter
    GenericParameter,
    /// Module
    Module,
    /// FFI function
    FfiFunction,
    /// FFI variable
    FfiVariable,
    /// Struct field
    Field,
    /// Test function
    Test,
}

impl SymbolKind {
    /// Get a human-readable description of this symbol kind
    pub fn description(&self) -> &'static str {
        match self {
            Self::Function => "function",
            Self::Method => "method",
            Self::Struct => "struct",
            Self::Enum => "enum",
            Self::EnumVariant => "enum variant",
            Self::Trait => "trait",
            Self::TypeAlias => "type alias",
            Self::Variable => "variable",
            Self::Constant => "constant",
            Self::Parameter => "parameter",
            Self::GenericParameter => "generic parameter",
            Self::Module => "module",
            Self::FfiFunction => "FFI function",
            Self::FfiVariable => "FFI variable",
            Self::Field => "field",
            Self::Test => "test function",
        }
    }

    /// Check if this symbol kind can have generic parameters
    pub fn can_have_generics(&self) -> bool {
        matches!(
            self,
            Self::Function
                | Self::Method
                | Self::Struct
                | Self::Enum
                | Self::Trait
                | Self::TypeAlias
        )
    }

    /// Check if this symbol kind represents a namespace
    pub fn is_namespace(&self) -> bool {
        matches!(self, Self::Module | Self::Struct | Self::Enum | Self::Trait)
    }
}

/// Symbol table for tracking all symbols in a compilation unit
#[derive(Debug, Clone)]
pub struct SymbolTable {
    /// All symbols indexed by their ID
    symbols: IndexMap<SymbolId, Symbol>,
    /// Name to symbol ID mapping for quick lookup
    name_to_symbol: HashMap<String, Vec<SymbolId>>,
    /// Module to symbols mapping
    module_symbols: HashMap<ModuleId, Vec<SymbolId>>,
    /// Type name resolution cache
    type_cache: HashMap<String, SymbolId>,
    /// Next available symbol ID
    next_symbol_id: u32,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: IndexMap::new(),
            name_to_symbol: HashMap::new(),
            module_symbols: HashMap::new(),
            type_cache: HashMap::new(),
            next_symbol_id: 0,
        }
    }

    /// Generate a new unique symbol ID
    pub fn next_symbol_id(&mut self) -> SymbolId {
        let id = SymbolId::new(self.next_symbol_id);
        self.next_symbol_id += 1;
        id
    }

    /// Insert a symbol into the table
    pub fn insert(&mut self, symbol: Symbol) -> SymbolId {
        let symbol_id = symbol.id;
        let name = symbol.name.clone();
        let module_id = symbol.module_id;

        // Update name mapping
        self.name_to_symbol.entry(name).or_default().push(symbol_id);

        // Update module mapping
        self.module_symbols
            .entry(module_id)
            .or_default()
            .push(symbol_id);

        // Update type cache if this is a type
        if symbol.is_type() {
            self.type_cache.insert(symbol.name.clone(), symbol_id);
        }

        // Insert the symbol
        self.symbols.insert(symbol_id, symbol);
        symbol_id
    }

    /// Get a symbol by ID
    pub fn get(&self, symbol_id: SymbolId) -> Option<&Symbol> {
        self.symbols.get(&symbol_id)
    }

    /// Get a mutable reference to a symbol by ID
    pub fn get_mut(&mut self, symbol_id: SymbolId) -> Option<&mut Symbol> {
        self.symbols.get_mut(&symbol_id)
    }

    /// Find symbols by name
    pub fn find_by_name(&self, name: &str) -> Vec<&Symbol> {
        self.name_to_symbol
            .get(name)
            .map(|ids| ids.iter().filter_map(|id| self.symbols.get(id)).collect())
            .unwrap_or_default()
    }

    /// Find a specific symbol by name and kind
    pub fn find_by_name_and_kind(&self, name: &str, kind: SymbolKind) -> Option<&Symbol> {
        self.find_by_name(name)
            .into_iter()
            .find(|symbol| symbol.kind == kind)
    }

    /// Find symbols in a specific module
    pub fn symbols_in_module(&self, module_id: ModuleId) -> Vec<&Symbol> {
        self.module_symbols
            .get(&module_id)
            .map(|ids| ids.iter().filter_map(|id| self.symbols.get(id)).collect())
            .unwrap_or_default()
    }

    /// Find accessible symbols from a given module
    pub fn accessible_symbols_from(
        &self,
        from_module: ModuleId,
        current_crate: ModuleId,
    ) -> Vec<&Symbol> {
        self.symbols
            .values()
            .filter(|symbol| symbol.is_accessible_from(from_module, current_crate))
            .collect()
    }

    /// Find type symbols accessible from a given module
    pub fn accessible_types_from(
        &self,
        from_module: ModuleId,
        current_crate: ModuleId,
    ) -> Vec<&Symbol> {
        self.accessible_symbols_from(from_module, current_crate)
            .into_iter()
            .filter(|symbol| symbol.is_type())
            .collect()
    }

    /// Find value symbols accessible from a given module
    pub fn accessible_values_from(
        &self,
        from_module: ModuleId,
        current_crate: ModuleId,
    ) -> Vec<&Symbol> {
        self.accessible_symbols_from(from_module, current_crate)
            .into_iter()
            .filter(|symbol| symbol.is_value())
            .collect()
    }

    /// Get all symbols
    pub fn all_symbols(&self) -> impl Iterator<Item = &Symbol> {
        self.symbols.values()
    }

    /// Get all symbol IDs
    pub fn all_symbol_ids(&self) -> impl Iterator<Item = SymbolId> + '_ {
        self.symbols.keys().copied()
    }

    /// Check if a symbol exists with the given name
    pub fn contains_name(&self, name: &str) -> bool {
        self.name_to_symbol.contains_key(name)
    }

    /// Get the number of symbols in the table
    pub fn len(&self) -> usize {
        self.symbols.len()
    }

    /// Check if the symbol table is empty
    pub fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }

    /// Clear the symbol table
    pub fn clear(&mut self) {
        self.symbols.clear();
        self.name_to_symbol.clear();
        self.module_symbols.clear();
        self.type_cache.clear();
    }

    /// Find candidates for a misspelled symbol name (for better error messages)
    pub fn find_candidates(&self, name: &str, max_distance: usize) -> Vec<String> {
        let mut candidates = Vec::new();

        for symbol_name in self.name_to_symbol.keys() {
            if levenshtein_distance(name, symbol_name) <= max_distance {
                candidates.push(symbol_name.clone());
            }
        }

        candidates.sort();
        candidates.truncate(5); // Limit to 5 suggestions
        candidates
    }

    /// Update the type of a symbol
    pub fn update_symbol_type(&mut self, symbol_id: SymbolId, ty: HirType) -> bool {
        if let Some(symbol) = self.symbols.get_mut(&symbol_id) {
            symbol.ty = Some(ty);
            true
        } else {
            false
        }
    }

    /// Remove a symbol from the table
    pub fn remove(&mut self, symbol_id: SymbolId) -> Option<Symbol> {
        if let Some(symbol) = self.symbols.shift_remove(&symbol_id) {
            // Clean up name mapping
            if let Some(ids) = self.name_to_symbol.get_mut(&symbol.name) {
                ids.retain(|&id| id != symbol_id);
                if ids.is_empty() {
                    self.name_to_symbol.remove(&symbol.name);
                }
            }

            // Clean up module mapping
            if let Some(ids) = self.module_symbols.get_mut(&symbol.module_id) {
                ids.retain(|&id| id != symbol_id);
                if ids.is_empty() {
                    self.module_symbols.remove(&symbol.module_id);
                }
            }

            // Clean up type cache
            if symbol.is_type() {
                self.type_cache.remove(&symbol.name);
            }

            Some(symbol)
        } else {
            None
        }
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

/// Calculate Levenshtein distance for finding symbol name candidates
fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let len1 = s1.len();
    let len2 = s2.len();
    let mut matrix = vec![vec![0; len2 + 1]; len1 + 1];

    for i in 0..=len1 {
        matrix[i][0] = i;
    }
    for j in 0..=len2 {
        matrix[0][j] = j;
    }

    for (i, c1) in s1.chars().enumerate() {
        for (j, c2) in s2.chars().enumerate() {
            let cost = if c1 == c2 { 0 } else { 1 };
            matrix[i + 1][j + 1] = (matrix[i][j + 1] + 1)
                .min(matrix[i + 1][j] + 1)
                .min(matrix[i][j] + cost);
        }
    }

    matrix[len1][len2]
}

#[cfg(test)]
mod tests {
    use super::*;
    use veil_hir::{ModuleId, NodeId};

    #[test]
    fn test_symbol_creation() {
        let symbol = Symbol::new(
            SymbolId::new(0),
            "test_function".to_string(),
            SymbolKind::Function,
            ModuleId::new(0),
            NodeId::new(0),
        )
        .with_type(HirType::I32)
        .with_visibility(HirVisibility::Public);

        assert_eq!(symbol.name, "test_function");
        assert_eq!(symbol.kind, SymbolKind::Function);
        assert_eq!(symbol.ty, Some(HirType::I32));
        assert_eq!(symbol.visibility, HirVisibility::Public);
    }

    #[test]
    fn test_symbol_table_operations() {
        let mut table = SymbolTable::new();
        let symbol_id = table.next_symbol_id();

        let symbol = Symbol::new(
            symbol_id,
            "test".to_string(),
            SymbolKind::Function,
            ModuleId::new(0),
            NodeId::new(0),
        );

        table.insert(symbol);

        assert!(table.contains_name("test"));
        assert_eq!(table.len(), 1);

        let found = table.find_by_name("test");
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].name, "test");
    }

    #[test]
    fn test_symbol_accessibility() {
        let module1 = ModuleId::new(1);
        let module2 = ModuleId::new(2);
        let crate_root = ModuleId::new(0);

        let private_symbol = Symbol::new(
            SymbolId::new(0),
            "private".to_string(),
            SymbolKind::Function,
            module1,
            NodeId::new(0),
        )
        .with_visibility(HirVisibility::Private);

        let public_symbol = Symbol::new(
            SymbolId::new(1),
            "public".to_string(),
            SymbolKind::Function,
            module1,
            NodeId::new(1),
        )
        .with_visibility(HirVisibility::Public);

        // Private symbol accessible only from same module
        assert!(private_symbol.is_accessible_from(module1, crate_root));
        assert!(!private_symbol.is_accessible_from(module2, crate_root));

        // Public symbol accessible from anywhere
        assert!(public_symbol.is_accessible_from(module1, crate_root));
        assert!(public_symbol.is_accessible_from(module2, crate_root));
    }

    #[test]
    fn test_symbol_kind_predicates() {
        assert!(SymbolKind::Function.can_have_generics());
        assert!(!SymbolKind::Variable.can_have_generics());

        assert!(SymbolKind::Module.is_namespace());
        assert!(!SymbolKind::Variable.is_namespace());
    }

    #[test]
    fn test_levenshtein_distance() {
        assert_eq!(levenshtein_distance("hello", "hello"), 0);
        assert_eq!(levenshtein_distance("hello", "hallo"), 1);
        assert_eq!(levenshtein_distance("hello", "world"), 4);
    }

    #[test]
    fn test_find_candidates() {
        let mut table = SymbolTable::new();

        let symbols = vec!["hello", "hallo", "help", "world", "word"];
        for (i, name) in symbols.iter().enumerate() {
            let symbol = Symbol::new(
                SymbolId::new(i as u32),
                name.to_string(),
                SymbolKind::Function,
                ModuleId::new(0),
                NodeId::new(i as u32),
            );
            table.insert(symbol);
        }

        let candidates = table.find_candidates("helo", 2);
        assert!(candidates.contains(&"hello".to_string()));
        assert!(candidates.contains(&"help".to_string()));
    }
}
