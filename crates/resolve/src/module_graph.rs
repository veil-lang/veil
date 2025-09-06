//! Module graph implementation for the Veil resolver
//!
//! This module provides data structures for tracking module import dependencies,
//! detecting circular imports, and managing the module system hierarchy.

use indexmap::IndexMap;

use petgraph::algo::is_cyclic_directed;
use petgraph::graph::{DiGraph, NodeIndex};
use std::collections::{HashMap, HashSet};
use veil_hir::ModuleId;

use crate::errors::ResolveError;

/// A node in the module graph representing a single module
#[derive(Debug, Clone)]
pub struct ModuleNode {
    /// Unique identifier for this module
    pub module_id: ModuleId,
    /// Module path (e.g., "std/io", "my_crate/utils")
    pub path: String,
    /// Whether this module has been resolved
    pub is_resolved: bool,
    /// Whether this module is external (from another crate)
    pub is_external: bool,
    /// Direct imports from this module
    pub imports: Vec<ModuleImport>,
    /// Direct exports from this module
    pub exports: Vec<ModuleExport>,
    /// Symbols defined in this module
    pub defined_symbols: HashSet<String>,
}

impl ModuleNode {
    pub fn new(module_id: ModuleId, path: String) -> Self {
        Self {
            module_id,
            path,
            is_resolved: false,
            is_external: false,
            imports: Vec::new(),
            exports: Vec::new(),
            defined_symbols: HashSet::new(),
        }
    }

    pub fn with_external(mut self, is_external: bool) -> Self {
        self.is_external = is_external;
        self
    }

    /// Add an import to this module
    pub fn add_import(&mut self, import: ModuleImport) {
        self.imports.push(import);
    }

    /// Add an export from this module
    pub fn add_export(&mut self, export: ModuleExport) {
        self.exports.push(export);
    }

    /// Define a symbol in this module
    pub fn define_symbol(&mut self, symbol_name: String) {
        self.defined_symbols.insert(symbol_name);
    }

    /// Check if this module defines a symbol
    pub fn defines_symbol(&self, symbol_name: &str) -> bool {
        self.defined_symbols.contains(symbol_name)
    }

    /// Get all imported symbols
    pub fn imported_symbols(&self) -> Vec<&str> {
        let mut symbols = Vec::new();
        for import in &self.imports {
            match &import.kind {
                ImportKind::All => {
                    // Would need to resolve the target module to get all symbols
                    // For now, we'll skip this case
                }
                ImportKind::Specific(items) => {
                    for item in items {
                        symbols.push(item.as_str());
                    }
                }
                ImportKind::Single(name) => {
                    symbols.push(name.as_str());
                }
            }
        }
        symbols
    }

    /// Get all exported symbols
    pub fn exported_symbols(&self) -> Vec<&str> {
        let mut symbols = Vec::new();
        for export in &self.exports {
            match &export.kind {
                ExportKind::All => {
                    // Export all defined symbols
                    symbols.extend(self.defined_symbols.iter().map(|s| s.as_str()));
                }
                ExportKind::Specific(items) => {
                    for item in items {
                        symbols.push(item.as_str());
                    }
                }
                ExportKind::Single(name) => {
                    symbols.push(name.as_str());
                }
                ExportKind::Reexport { items, .. } => {
                    for item in items {
                        symbols.push(item.as_str());
                    }
                }
            }
        }
        symbols
    }
}

/// An import declaration in a module
#[derive(Debug, Clone)]
pub struct ModuleImport {
    /// The module being imported from
    pub source_module: String,
    /// What is being imported
    pub kind: ImportKind,
    /// Optional alias for the import
    pub alias: Option<String>,
    /// Resolved module ID (filled during resolution)
    pub resolved_module_id: Option<ModuleId>,
}

/// Different kinds of imports
#[derive(Debug, Clone)]
pub enum ImportKind {
    /// Import everything (import module/*)
    All,
    /// Import specific items (import module/{item1, item2})
    Specific(Vec<String>),
    /// Import the module itself (import module)
    Single(String),
}

/// An export declaration in a module
#[derive(Debug, Clone)]
pub struct ModuleExport {
    /// What is being exported
    pub kind: ExportKind,
    /// Whether the export is public
    pub is_public: bool,
}

/// Different kinds of exports
#[derive(Debug, Clone)]
pub enum ExportKind {
    /// Export everything defined in this module
    All,
    /// Export specific items
    Specific(Vec<String>),
    /// Export a single item
    Single(String),
    /// Re-export from another module
    Reexport {
        source_module: String,
        items: Vec<String>,
    },
}

/// Module dependency graph for tracking imports and detecting cycles
#[derive(Debug)]
pub struct ModuleGraph {
    /// Underlying directed graph for dependency analysis
    graph: DiGraph<ModuleId, ImportRelation>,
    /// Module information indexed by ModuleId
    modules: IndexMap<ModuleId, ModuleNode>,
    /// Module path to ModuleId mapping
    path_to_module: HashMap<String, ModuleId>,
    /// Graph node indices for each module
    module_to_node: HashMap<ModuleId, NodeIndex>,
    /// Import search paths
    search_paths: Vec<String>,
}

/// Information about an import relationship between modules
#[derive(Debug, Clone)]
pub struct ImportRelation {
    /// The import declaration that created this edge
    pub import: ModuleImport,
    /// Whether this is a direct or transitive dependency
    pub is_direct: bool,
}

impl ModuleGraph {
    pub fn new() -> Self {
        Self {
            graph: DiGraph::new(),
            modules: IndexMap::new(),
            path_to_module: HashMap::new(),
            module_to_node: HashMap::new(),
            search_paths: vec!["lib/std/src".to_string()], // Default std library path
        }
    }

    /// Add a search path for module resolution
    pub fn add_search_path(&mut self, path: String) {
        self.search_paths.push(path);
    }

    /// Add a module to the graph
    pub fn add_module(&mut self, module: ModuleNode) -> ModuleId {
        let module_id = module.module_id;
        let path = module.path.clone();

        // Add to graph
        let node_index = self.graph.add_node(module_id);
        self.module_to_node.insert(module_id, node_index);

        // Add to our mappings
        self.modules.insert(module_id, module);
        self.path_to_module.insert(path, module_id);

        module_id
    }

    /// Get a module by ID
    pub fn get_module(&self, module_id: ModuleId) -> Option<&ModuleNode> {
        self.modules.get(&module_id)
    }

    /// Get a mutable reference to a module
    pub fn get_module_mut(&mut self, module_id: ModuleId) -> Option<&mut ModuleNode> {
        self.modules.get_mut(&module_id)
    }

    /// Find a module by path
    pub fn find_module_by_path(&self, path: &str) -> Option<ModuleId> {
        self.path_to_module.get(path).copied()
    }

    /// Add an import relationship between modules
    pub fn add_import(
        &mut self,
        from_module: ModuleId,
        to_module: ModuleId,
        import: ModuleImport,
    ) -> Result<(), ResolveError> {
        // Check if both modules exist
        if !self.modules.contains_key(&from_module) || !self.modules.contains_key(&to_module) {
            return Err(ResolveError::module_not_found(import.source_module));
        }

        // Get graph node indices
        let from_node = self.module_to_node[&from_module];
        let to_node = self.module_to_node[&to_module];

        // Create the import relation
        let relation = ImportRelation {
            import,
            is_direct: true,
        };

        // Add edge to graph (dependency -> dependent)
        self.graph.add_edge(to_node, from_node, relation);

        Ok(())
    }

    /// Check for circular import dependencies
    pub fn check_cycles(&self) -> Result<(), ResolveError> {
        if is_cyclic_directed(&self.graph) {
            // Find the actual cycle for better error reporting
            if let Some(cycle) = self.find_cycle() {
                return Err(ResolveError::circular_import(cycle));
            } else {
                return Err(ResolveError::circular_import(vec![
                    "unknown cycle".to_string(),
                ]));
            }
        }
        Ok(())
    }

    /// Find a specific cycle in the import graph
    fn find_cycle(&self) -> Option<Vec<String>> {
        use petgraph::algo::tarjan_scc;

        // Use Tarjan's algorithm to find strongly connected components
        let sccs = tarjan_scc(&self.graph);

        // Find the first non-trivial SCC (more than one node)
        for scc in sccs {
            if scc.len() > 1 {
                // Convert node indices back to module paths
                let cycle: Vec<String> = scc
                    .into_iter()
                    .filter_map(|node_idx| {
                        self.graph
                            .node_weight(node_idx)
                            .and_then(|&module_id| self.modules.get(&module_id))
                            .map(|module| module.path.clone())
                    })
                    .collect();

                if !cycle.is_empty() {
                    return Some(cycle);
                }
            }
        }

        None
    }

    /// Get all modules that directly depend on a given module
    pub fn get_dependents(&self, module_id: ModuleId) -> Vec<ModuleId> {
        if let Some(&node_idx) = self.module_to_node.get(&module_id) {
            self.graph
                .neighbors_directed(node_idx, petgraph::Direction::Outgoing)
                .filter_map(|neighbor_idx| self.graph.node_weight(neighbor_idx))
                .copied()
                .collect()
        } else {
            Vec::new()
        }
    }

    /// Get all modules that a given module directly depends on
    pub fn get_dependencies(&self, module_id: ModuleId) -> Vec<ModuleId> {
        if let Some(&node_idx) = self.module_to_node.get(&module_id) {
            self.graph
                .neighbors_directed(node_idx, petgraph::Direction::Incoming)
                .filter_map(|neighbor_idx| self.graph.node_weight(neighbor_idx))
                .copied()
                .collect()
        } else {
            Vec::new()
        }
    }

    /// Get all modules in topological order (dependencies before dependents)
    pub fn topological_sort(&self) -> Result<Vec<ModuleId>, ResolveError> {
        use petgraph::algo::toposort;

        match toposort(&self.graph, None) {
            Ok(sorted_nodes) => {
                let sorted_modules = sorted_nodes
                    .into_iter()
                    .filter_map(|node_idx| self.graph.node_weight(node_idx))
                    .copied()
                    .collect();
                Ok(sorted_modules)
            }
            Err(_) => {
                // Graph has cycles
                self.check_cycles()?;
                unreachable!("check_cycles should have returned an error");
            }
        }
    }

    /// Resolve a module path to a ModuleId
    pub fn resolve_module_path(&mut self, path: &str) -> Result<ModuleId, ResolveError> {
        // Check if already resolved
        if let Some(module_id) = self.find_module_by_path(path) {
            return Ok(module_id);
        }

        // Try to find the module in search paths
        for search_path in &self.search_paths.clone() {
            let full_path = if path.starts_with("std/") {
                // Standard library module
                format!("{}/{}", search_path, path)
            } else {
                // Local module - path is relative to current location
                path.to_string()
            };

            // In a real implementation, we would check if the file exists
            // For now, we'll create a placeholder module
            if self.should_create_module(&full_path) {
                let module_id = ModuleId::new(self.modules.len() as u32);
                let module = ModuleNode::new(module_id, path.to_string())
                    .with_external(path.starts_with("std/"));

                self.add_module(module);
                return Ok(module_id);
            }
        }

        Err(ResolveError::module_not_found(path))
    }

    /// Check if we should create a module for the given path (simplified logic)
    fn should_create_module(&self, _path: &str) -> bool {
        // In a real implementation, this would check if the file exists
        // For now, we'll always return true for testing
        true
    }

    /// Get all modules
    pub fn all_modules(&self) -> impl Iterator<Item = &ModuleNode> {
        self.modules.values()
    }

    /// Get the number of modules
    pub fn module_count(&self) -> usize {
        self.modules.len()
    }

    /// Check if the graph is empty
    pub fn is_empty(&self) -> bool {
        self.modules.is_empty()
    }

    /// Clear all modules and dependencies
    pub fn clear(&mut self) {
        self.graph.clear();
        self.modules.clear();
        self.path_to_module.clear();
        self.module_to_node.clear();
    }

    /// Validate the entire module graph
    pub fn validate(&self) -> Vec<ResolveError> {
        let mut errors = Vec::new();

        // Check for cycles
        if let Err(error) = self.check_cycles() {
            errors.push(error);
        }

        // Check for unresolved imports
        for module in self.modules.values() {
            for import in &module.imports {
                if import.resolved_module_id.is_none() {
                    errors.push(ResolveError::module_not_found(&import.source_module));
                }
            }
        }

        // Check for invalid exports
        for module in self.modules.values() {
            for export in &module.exports {
                if let ExportKind::Reexport { source_module, .. } = &export.kind
                    && self.find_module_by_path(source_module).is_none()
                {
                    errors.push(ResolveError::module_not_found(source_module));
                }
            }
        }

        errors
    }

    /// Get statistics about the module graph
    pub fn stats(&self) -> ModuleGraphStats {
        let mut external_modules = 0;
        let mut total_imports = 0;
        let mut total_exports = 0;

        for module in self.modules.values() {
            if module.is_external {
                external_modules += 1;
            }
            total_imports += module.imports.len();
            total_exports += module.exports.len();
        }

        ModuleGraphStats {
            total_modules: self.modules.len(),
            external_modules,
            total_imports,
            total_exports,
            graph_edges: self.graph.edge_count(),
        }
    }
}

impl Default for ModuleGraph {
    fn default() -> Self {
        Self::new()
    }
}

/// Statistics about the module graph
#[derive(Debug, Clone)]
pub struct ModuleGraphStats {
    pub total_modules: usize,
    pub external_modules: usize,
    pub total_imports: usize,
    pub total_exports: usize,
    pub graph_edges: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_node_creation() {
        let module =
            ModuleNode::new(ModuleId::new(0), "test/module".to_string()).with_external(true);

        assert_eq!(module.module_id, ModuleId::new(0));
        assert_eq!(module.path, "test/module");
        assert!(module.is_external);
        assert!(!module.is_resolved);
    }

    #[test]
    fn test_module_graph_basic_operations() {
        let mut graph = ModuleGraph::new();

        // Add modules
        let module1 = ModuleNode::new(ModuleId::new(1), "module1".to_string());
        let module2 = ModuleNode::new(ModuleId::new(2), "module2".to_string());

        let id1 = graph.add_module(module1);
        let id2 = graph.add_module(module2);

        assert_eq!(graph.module_count(), 2);
        assert_eq!(id1, ModuleId::new(1));
        assert_eq!(id2, ModuleId::new(2));

        // Find module by path
        assert_eq!(graph.find_module_by_path("module1"), Some(id1));
        assert_eq!(graph.find_module_by_path("module2"), Some(id2));
        assert_eq!(graph.find_module_by_path("nonexistent"), None);
    }

    #[test]
    fn test_import_relationship() {
        let mut graph = ModuleGraph::new();

        let module1 = ModuleNode::new(ModuleId::new(1), "module1".to_string());
        let module2 = ModuleNode::new(ModuleId::new(2), "module2".to_string());

        let id1 = graph.add_module(module1);
        let id2 = graph.add_module(module2);

        let import = ModuleImport {
            source_module: "module2".to_string(),
            kind: ImportKind::Single("some_function".to_string()),
            alias: None,
            resolved_module_id: Some(id2),
        };

        // Add import relationship
        graph.add_import(id1, id2, import).unwrap();

        // Check dependencies
        let deps = graph.get_dependencies(id1);
        assert_eq!(deps, vec![id2]);

        let dependents = graph.get_dependents(id2);
        assert_eq!(dependents, vec![id1]);
    }

    #[test]
    fn test_cycle_detection() {
        let mut graph = ModuleGraph::new();

        // Create three modules
        let module1 = ModuleNode::new(ModuleId::new(1), "module1".to_string());
        let module2 = ModuleNode::new(ModuleId::new(2), "module2".to_string());
        let module3 = ModuleNode::new(ModuleId::new(3), "module3".to_string());

        let id1 = graph.add_module(module1);
        let id2 = graph.add_module(module2);
        let id3 = graph.add_module(module3);

        // Create imports: 1 -> 2 -> 3 -> 1 (cycle)
        let import1 = ModuleImport {
            source_module: "module2".to_string(),
            kind: ImportKind::All,
            alias: None,
            resolved_module_id: Some(id2),
        };

        let import2 = ModuleImport {
            source_module: "module3".to_string(),
            kind: ImportKind::All,
            alias: None,
            resolved_module_id: Some(id3),
        };

        let import3 = ModuleImport {
            source_module: "module1".to_string(),
            kind: ImportKind::All,
            alias: None,
            resolved_module_id: Some(id1),
        };

        graph.add_import(id1, id2, import1).unwrap();
        graph.add_import(id2, id3, import2).unwrap();
        graph.add_import(id3, id1, import3).unwrap();

        // Should detect cycle
        assert!(graph.check_cycles().is_err());
    }

    #[test]
    fn test_topological_sort() {
        let mut graph = ModuleGraph::new();

        // Create modules without cycles: 1 -> 2 -> 3
        let module1 = ModuleNode::new(ModuleId::new(1), "module1".to_string());
        let module2 = ModuleNode::new(ModuleId::new(2), "module2".to_string());
        let module3 = ModuleNode::new(ModuleId::new(3), "module3".to_string());

        let id1 = graph.add_module(module1);
        let id2 = graph.add_module(module2);
        let id3 = graph.add_module(module3);

        let import1 = ModuleImport {
            source_module: "module2".to_string(),
            kind: ImportKind::All,
            alias: None,
            resolved_module_id: Some(id2),
        };

        let import2 = ModuleImport {
            source_module: "module3".to_string(),
            kind: ImportKind::All,
            alias: None,
            resolved_module_id: Some(id3),
        };

        graph.add_import(id1, id2, import1).unwrap();
        graph.add_import(id2, id3, import2).unwrap();

        // Should succeed and return modules in dependency order
        let sorted = graph.topological_sort().unwrap();

        // Module 3 should come before module 2, which should come before module 1
        let pos1 = sorted.iter().position(|&id| id == id1).unwrap();
        let pos2 = sorted.iter().position(|&id| id == id2).unwrap();
        let pos3 = sorted.iter().position(|&id| id == id3).unwrap();

        assert!(pos3 < pos2);
        assert!(pos2 < pos1);
    }

    #[test]
    fn test_module_symbols() {
        let mut module = ModuleNode::new(ModuleId::new(1), "test".to_string());

        module.define_symbol("function1".to_string());
        module.define_symbol("struct1".to_string());

        assert!(module.defines_symbol("function1"));
        assert!(module.defines_symbol("struct1"));
        assert!(!module.defines_symbol("nonexistent"));

        // Add import
        let import = ModuleImport {
            source_module: "other_module".to_string(),
            kind: ImportKind::Specific(vec!["imported_fn".to_string()]),
            alias: None,
            resolved_module_id: None,
        };
        module.add_import(import);

        let imported = module.imported_symbols();
        assert_eq!(imported, vec!["imported_fn"]);
    }

    #[test]
    fn test_graph_validation() {
        let mut graph = ModuleGraph::new();

        // Add a module with an unresolved import
        let mut module = ModuleNode::new(ModuleId::new(1), "test".to_string());
        module.add_import(ModuleImport {
            source_module: "nonexistent".to_string(),
            kind: ImportKind::All,
            alias: None,
            resolved_module_id: None, // Unresolved
        });

        graph.add_module(module);

        let errors = graph.validate();
        assert!(!errors.is_empty());
        assert!(matches!(
            errors[0].kind,
            crate::errors::ResolveErrorKind::ModuleNotFound { .. }
        ));
    }
}
