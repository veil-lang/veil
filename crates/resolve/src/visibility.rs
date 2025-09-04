//! Visibility checking for the Veil resolver
//!
//! This module implements visibility and access control rules for symbols,
//! ensuring that private, public, and crate-local items are accessed correctly.

use codespan::Span;
use std::collections::HashMap;
use veil_hir::{HirVisibility, ModuleId, SymbolId};

use crate::errors::{ResolveError, ResolveErrorKind};
use crate::symbol_table::{Symbol, SymbolTable};

/// Visibility levels in the Veil language
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VisibilityLevel {
    /// Private to the defining module
    Private,
    /// Public within the current crate
    PublicCrate,
    /// Public to parent module
    PublicSuper,
    /// Public to a specific path
    PublicPath,
    /// Fully public (accessible from anywhere)
    Public,
}

impl From<&HirVisibility> for VisibilityLevel {
    fn from(hir_vis: &HirVisibility) -> Self {
        match hir_vis {
            HirVisibility::Private => VisibilityLevel::Private,
            HirVisibility::Public => VisibilityLevel::Public,
            HirVisibility::PublicCrate => VisibilityLevel::PublicCrate,
            HirVisibility::PublicSuper => VisibilityLevel::PublicSuper,
            HirVisibility::PublicIn(_) => VisibilityLevel::PublicPath,
            HirVisibility::PublicInResolved(_) => VisibilityLevel::PublicPath,
        }
    }
}

/// Module hierarchy for visibility checking
#[derive(Debug, Clone)]
pub struct ModuleHierarchy {
    /// Parent relationships: child_module -> parent_module
    parents: HashMap<ModuleId, ModuleId>,
    /// Module paths for debugging
    module_paths: HashMap<ModuleId, String>,
    /// Root module of the current crate
    _crate_root: ModuleId,
}

impl ModuleHierarchy {
    pub fn new(crate_root: ModuleId) -> Self {
        Self {
            parents: HashMap::new(),
            module_paths: HashMap::new(),
            _crate_root: crate_root,
        }
    }

    /// Add a parent-child relationship
    pub fn add_parent(&mut self, child: ModuleId, parent: ModuleId) {
        self.parents.insert(child, parent);
    }

    /// Set the path for a module (for debugging)
    pub fn set_module_path(&mut self, module: ModuleId, path: String) {
        self.module_paths.insert(module, path);
    }

    /// Get the parent of a module
    pub fn parent(&self, module: ModuleId) -> Option<ModuleId> {
        self.parents.get(&module).copied()
    }

    /// Check if one module is an ancestor of another
    pub fn is_ancestor(&self, ancestor: ModuleId, descendant: ModuleId) -> bool {
        let mut current = descendant;
        while let Some(parent) = self.parent(current) {
            if parent == ancestor {
                return true;
            }
            current = parent;
        }
        false
    }

    /// Check if two modules are in the same crate
    pub fn in_same_crate(&self, module1: ModuleId, module2: ModuleId) -> bool {
        self.get_crate_root(module1) == self.get_crate_root(module2)
    }

    /// Get the crate root for a module
    pub fn get_crate_root(&self, module: ModuleId) -> ModuleId {
        let mut current = module;
        while let Some(parent) = self.parent(current) {
            current = parent;
        }
        current
    }

    /// Get all ancestor modules (including self)
    pub fn ancestors(&self, module: ModuleId) -> Vec<ModuleId> {
        let mut ancestors = vec![module];
        let mut current = module;
        while let Some(parent) = self.parent(current) {
            ancestors.push(parent);
            current = parent;
        }
        ancestors
    }

    /// Get the module path for debugging
    pub fn module_path(&self, module: ModuleId) -> String {
        self.module_paths
            .get(&module)
            .cloned()
            .unwrap_or_else(|| format!("mod_{}", module.as_u32()))
    }

    /// Find a module by its path string
    pub fn find_module_by_path(&self, path: &str) -> Option<ModuleId> {
        self.module_paths
            .iter()
            .find_map(|(id, p)| if p == path { Some(*id) } else { None })
    }
}

/// Visibility checker for enforcing access control
#[derive(Debug)]
pub struct VisibilityChecker {
    /// Module hierarchy for path-based visibility checks
    hierarchy: ModuleHierarchy,
    /// Cached visibility decisions for performance
    visibility_cache: HashMap<(SymbolId, ModuleId), bool>,
}

impl VisibilityChecker {
    pub fn new(crate_root: ModuleId) -> Self {
        Self {
            hierarchy: ModuleHierarchy::new(crate_root),
            visibility_cache: HashMap::new(),
        }
    }

    /// Update the module hierarchy
    pub fn set_hierarchy(&mut self, hierarchy: ModuleHierarchy) {
        self.hierarchy = hierarchy;
        // Clear cache when hierarchy changes
        self.visibility_cache.clear();
    }

    /// Add a parent-child module relationship
    pub fn add_module_parent(&mut self, child: ModuleId, parent: ModuleId) {
        self.hierarchy.add_parent(child, parent);
        self.visibility_cache.clear();
    }

    /// Set a module's path for debugging
    pub fn set_module_path(&mut self, module: ModuleId, path: String) {
        self.hierarchy.set_module_path(module, path);
    }

    /// Check if a symbol is accessible from a given module
    pub fn is_accessible(
        &mut self,
        symbol: &Symbol,
        from_module: ModuleId,
    ) -> Result<bool, ResolveError> {
        // Check cache first
        let cache_key = (symbol.id, from_module);
        if let Some(&cached) = self.visibility_cache.get(&cache_key) {
            return Ok(cached);
        }

        let accessible = self.check_accessibility_impl(symbol, from_module)?;

        // Cache the result
        self.visibility_cache.insert(cache_key, accessible);

        Ok(accessible)
    }

    /// Internal implementation of accessibility checking
    fn check_accessibility_impl(
        &self,
        symbol: &Symbol,
        from_module: ModuleId,
    ) -> Result<bool, ResolveError> {
        let symbol_module = symbol.module_id;
        let visibility_level = VisibilityLevel::from(&symbol.visibility);

        match visibility_level {
            VisibilityLevel::Private => {
                // Private symbols are only accessible from the same module
                Ok(symbol_module == from_module)
            }

            VisibilityLevel::Public => {
                // Public symbols are accessible from anywhere
                Ok(true)
            }

            VisibilityLevel::PublicCrate => {
                // Crate-public symbols are accessible within the same crate
                Ok(self.hierarchy.in_same_crate(symbol_module, from_module))
            }

            VisibilityLevel::PublicSuper => {
                // Super-public symbols are accessible from parent modules and the same module
                Ok(symbol_module == from_module
                    || self.hierarchy.is_ancestor(from_module, symbol_module))
            }

            VisibilityLevel::PublicPath => {
                match &symbol.visibility {
                    HirVisibility::PublicInResolved(target_module) => {
                        // Accessible from the specified module and its descendants
                        Ok(*target_module == from_module
                            || self.hierarchy.is_ancestor(*target_module, from_module))
                    }
                    HirVisibility::PublicIn(path) => {
                        if let Some(target) = self.hierarchy.find_module_by_path(path) {
                            Ok(target == from_module
                                || self.hierarchy.is_ancestor(target, from_module))
                        } else {
                            Err(ResolveError::new(ResolveErrorKind::InvalidVisibility {
                                visibility: format!("pub(in {})", path),
                                context: symbol.kind.description().to_string(),
                            }))
                        }
                    }
                    _ => Ok(false),
                }
            }
        }
    }

    /// Check visibility and return an error if access is denied
    pub fn check_access(
        &mut self,
        symbol: &Symbol,
        from_module: ModuleId,
        access_span: Option<Span>,
    ) -> Result<(), ResolveError> {
        if self.is_accessible(symbol, from_module)? {
            Ok(())
        } else {
            let error = ResolveError::new(ResolveErrorKind::PrivateAccess {
                symbol_name: symbol.name.clone(),
                symbol_module: self.hierarchy.module_path(symbol.module_id),
                access_module: self.hierarchy.module_path(from_module),
            });

            if let Some(span) = access_span {
                Err(error.with_span(span))
            } else {
                Err(error)
            }
        }
    }

    /// Check if a symbol can be imported into a module
    pub fn can_import(
        &mut self,
        symbol: &Symbol,
        into_module: ModuleId,
    ) -> Result<bool, ResolveError> {
        // For imports, we need to check if the symbol is accessible
        // and also if it's marked as exportable
        self.is_accessible(symbol, into_module)
    }

    /// Check if a module can re-export a symbol
    pub fn can_reexport(
        &mut self,
        symbol: &Symbol,
        reexport_module: ModuleId,
        target_module: ModuleId,
    ) -> Result<bool, ResolveError> {
        // To re-export, the symbol must be accessible from the re-export module
        // and the re-export must not violate visibility rules
        if !self.is_accessible(symbol, reexport_module)? {
            return Ok(false);
        }

        // Additional check: can't re-export with higher visibility than original
        match VisibilityLevel::from(&symbol.visibility) {
            VisibilityLevel::Private => {
                // Can't re-export private symbols
                Ok(false)
            }
            VisibilityLevel::PublicCrate => {
                // Can re-export within the same crate
                Ok(self.hierarchy.in_same_crate(reexport_module, target_module))
            }
            _ => {
                // Other visibility levels can generally be re-exported
                Ok(true)
            }
        }
    }

    /// Get all accessible symbols from a module
    pub fn accessible_symbols_from(
        &mut self,
        symbol_table: &SymbolTable,
        from_module: ModuleId,
    ) -> Result<Vec<SymbolId>, Vec<ResolveError>> {
        let mut accessible = Vec::new();
        let mut errors = Vec::new();

        for symbol in symbol_table.all_symbols() {
            match self.is_accessible(symbol, from_module) {
                Ok(true) => accessible.push(symbol.id),
                Ok(false) => {
                    // Not accessible, but not an error
                }
                Err(error) => errors.push(error),
            }
        }

        if errors.is_empty() {
            Ok(accessible)
        } else {
            Err(errors)
        }
    }

    /// Validate visibility of all symbols in a module
    pub fn validate_module_visibility(
        &mut self,
        symbol_table: &SymbolTable,
        module_id: ModuleId,
    ) -> Vec<ResolveError> {
        let mut errors = Vec::new();

        for symbol in symbol_table.symbols_in_module(module_id) {
            // Check if the visibility declaration is valid for this symbol
            if let Err(error) = self.validate_visibility_declaration(symbol) {
                errors.push(error);
            }
        }

        errors
    }

    /// Validate a single visibility declaration
    fn validate_visibility_declaration(&self, symbol: &Symbol) -> Result<(), ResolveError> {
        match &symbol.visibility {
            HirVisibility::PublicIn(path) => {
                // Validate that the path is non-empty and resolves to a known module
                if path.is_empty() {
                    return Err(ResolveError::new(ResolveErrorKind::InvalidVisibility {
                        visibility: format!("pub(in {})", path),
                        context: symbol.kind.description().to_string(),
                    }));
                }
                if self.hierarchy.find_module_by_path(path).is_none() {
                    return Err(ResolveError::new(ResolveErrorKind::InvalidVisibility {
                        visibility: format!("pub(in {})", path),
                        context: format!("unknown module path '{}'", path),
                    }));
                }
            }
            HirVisibility::PublicSuper => {
                // Check if there actually is a parent module
                if self.hierarchy.parent(symbol.module_id).is_none() {
                    return Err(ResolveError::new(ResolveErrorKind::InvalidVisibility {
                        visibility: "pub(super)".to_string(),
                        context: "root module".to_string(),
                    }));
                }
            }
            _ => {
                // Other visibility levels are always valid
            }
        }

        Ok(())
    }

    /// Clear the visibility cache (useful when module structure changes)
    pub fn clear_cache(&mut self) {
        self.visibility_cache.clear();
    }

    /// Get statistics about the visibility checker
    pub fn stats(&self) -> VisibilityStats {
        VisibilityStats {
            total_modules: self.hierarchy.parents.len() + 1, // +1 for root
            cache_entries: self.visibility_cache.len(),
        }
    }
}

/// Statistics about the visibility checker
#[derive(Debug, Clone)]
pub struct VisibilityStats {
    pub total_modules: usize,
    pub cache_entries: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbol_table::{Symbol, SymbolKind};
    use veil_hir::{NodeId, SymbolId};

    fn create_test_symbol(
        id: u32,
        name: &str,
        visibility: HirVisibility,
        module: ModuleId,
    ) -> Symbol {
        Symbol::new(
            SymbolId::new(id),
            name.to_string(),
            SymbolKind::Function,
            module,
            NodeId::new(id),
        )
        .with_visibility(visibility)
    }

    #[test]
    fn test_private_visibility() {
        let mut checker = VisibilityChecker::new(ModuleId::new(0));
        let module1 = ModuleId::new(1);
        let module2 = ModuleId::new(2);

        let private_symbol = create_test_symbol(0, "private_fn", HirVisibility::Private, module1);

        // Private symbol accessible from same module
        assert!(checker.is_accessible(&private_symbol, module1).unwrap());

        // Private symbol not accessible from different module
        assert!(!checker.is_accessible(&private_symbol, module2).unwrap());
    }

    #[test]
    fn test_public_visibility() {
        let mut checker = VisibilityChecker::new(ModuleId::new(0));
        let module1 = ModuleId::new(1);
        let module2 = ModuleId::new(2);

        let public_symbol = create_test_symbol(0, "public_fn", HirVisibility::Public, module1);

        // Public symbol accessible from anywhere
        assert!(checker.is_accessible(&public_symbol, module1).unwrap());
        assert!(checker.is_accessible(&public_symbol, module2).unwrap());
    }

    #[test]
    fn test_crate_visibility() {
        let mut checker = VisibilityChecker::new(ModuleId::new(0));
        let module1 = ModuleId::new(1);
        let module2 = ModuleId::new(2);

        // Set up hierarchy (both modules in same crate)
        checker.add_module_parent(module1, ModuleId::new(0));
        checker.add_module_parent(module2, ModuleId::new(0));

        let crate_symbol = create_test_symbol(0, "crate_fn", HirVisibility::PublicCrate, module1);

        // Crate-public symbol accessible within crate
        assert!(checker.is_accessible(&crate_symbol, module1).unwrap());
        assert!(checker.is_accessible(&crate_symbol, module2).unwrap());
    }

    #[test]
    fn test_super_visibility() {
        let mut checker = VisibilityChecker::new(ModuleId::new(0));
        let parent_module = ModuleId::new(1);
        let child_module = ModuleId::new(2);
        let sibling_module = ModuleId::new(3);

        // Set up hierarchy
        checker.add_module_parent(parent_module, ModuleId::new(0));
        checker.add_module_parent(child_module, parent_module);
        checker.add_module_parent(sibling_module, ModuleId::new(0));

        let super_symbol =
            create_test_symbol(0, "super_fn", HirVisibility::PublicSuper, child_module);

        // Super-public symbol accessible from same module and ancestors
        assert!(checker.is_accessible(&super_symbol, child_module).unwrap());
        assert!(checker.is_accessible(&super_symbol, parent_module).unwrap());

        // Not accessible from sibling
        assert!(
            !checker
                .is_accessible(&super_symbol, sibling_module)
                .unwrap()
        );
    }

    #[test]
    fn test_module_hierarchy() {
        let mut hierarchy = ModuleHierarchy::new(ModuleId::new(0));
        let module1 = ModuleId::new(1);
        let module2 = ModuleId::new(2);
        let module3 = ModuleId::new(3);

        // Build hierarchy: 0 -> 1 -> 2 -> 3
        hierarchy.add_parent(module1, ModuleId::new(0));
        hierarchy.add_parent(module2, module1);
        hierarchy.add_parent(module3, module2);

        assert_eq!(hierarchy.parent(module1), Some(ModuleId::new(0)));
        assert_eq!(hierarchy.parent(module2), Some(module1));
        assert_eq!(hierarchy.parent(module3), Some(module2));

        assert!(hierarchy.is_ancestor(ModuleId::new(0), module3));
        assert!(hierarchy.is_ancestor(module1, module3));
        assert!(hierarchy.is_ancestor(module2, module3));
        assert!(!hierarchy.is_ancestor(module3, module1));

        let ancestors = hierarchy.ancestors(module3);
        assert_eq!(ancestors, vec![module3, module2, module1, ModuleId::new(0)]);
    }

    #[test]
    fn test_access_checking_with_errors() {
        let mut checker = VisibilityChecker::new(ModuleId::new(0));
        let module1 = ModuleId::new(1);
        let module2 = ModuleId::new(2);

        let private_symbol = create_test_symbol(0, "private_fn", HirVisibility::Private, module1);

        // Should succeed for same module
        assert!(checker.check_access(&private_symbol, module1, None).is_ok());

        // Should fail for different module
        let result = checker.check_access(&private_symbol, module2, None);
        assert!(result.is_err());

        let error = result.unwrap_err();
        assert!(matches!(error.kind, ResolveErrorKind::PrivateAccess { .. }));
    }

    #[test]
    fn test_visibility_cache() {
        let mut checker = VisibilityChecker::new(ModuleId::new(0));
        let module1 = ModuleId::new(1);

        let symbol = create_test_symbol(0, "test_fn", HirVisibility::Public, module1);

        // First access should compute result
        assert!(checker.is_accessible(&symbol, module1).unwrap());
        assert_eq!(checker.visibility_cache.len(), 1);

        // Second access should use cache
        assert!(checker.is_accessible(&symbol, module1).unwrap());
        assert_eq!(checker.visibility_cache.len(), 1);

        // Clear cache
        checker.clear_cache();
        assert_eq!(checker.visibility_cache.len(), 0);
    }
}
