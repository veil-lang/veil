//! Stable identifier types for HIR nodes
//!
//! HIR uses stable IDs to refer to symbols, nodes, and types instead of strings.
//! This enables efficient lookups and prevents name resolution issues.

use std::fmt;

/// Unique identifier for HIR nodes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeId(pub u32);

impl NodeId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    pub fn as_u32(self) -> u32 {
        self.0
    }
}

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "node_{}", self.0)
    }
}

/// Unique identifier for symbols (functions, types, variables, etc.)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SymbolId(pub u32);

impl SymbolId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    pub fn as_u32(self) -> u32 {
        self.0
    }
}

impl fmt::Display for SymbolId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "sym_{}", self.0)
    }
}

/// Unique identifier for types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeId(pub u32);

impl TypeId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    pub fn as_u32(self) -> u32 {
        self.0
    }
}

impl fmt::Display for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ty_{}", self.0)
    }
}

/// Unique identifier for modules
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleId(pub u32);

impl ModuleId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    pub fn as_u32(self) -> u32 {
        self.0
    }
}

impl fmt::Display for ModuleId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "mod_{}", self.0)
    }
}

/// Generator for creating unique IDs
#[derive(Debug, Default)]
pub struct IdGenerator {
    next_node_id: u32,
    next_symbol_id: u32,
    next_type_id: u32,
    next_module_id: u32,
}

impl IdGenerator {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn next_node_id(&mut self) -> NodeId {
        let id = NodeId::new(self.next_node_id);
        self.next_node_id += 1;
        id
    }

    pub fn next_symbol_id(&mut self) -> SymbolId {
        let id = SymbolId::new(self.next_symbol_id);
        self.next_symbol_id += 1;
        id
    }

    pub fn next_type_id(&mut self) -> TypeId {
        let id = TypeId::new(self.next_type_id);
        self.next_type_id += 1;
        id
    }

    pub fn next_module_id(&mut self) -> ModuleId {
        let id = ModuleId::new(self.next_module_id);
        self.next_module_id += 1;
        id
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_id_generation() {
        let mut id_gen = IdGenerator::new();

        let node1 = id_gen.next_node_id();
        let node2 = id_gen.next_node_id();
        assert_ne!(node1, node2);
        assert_eq!(node1.as_u32(), 0);
        assert_eq!(node2.as_u32(), 1);

        let sym1 = id_gen.next_symbol_id();
        let sym2 = id_gen.next_symbol_id();
        assert_ne!(sym1, sym2);
        assert_eq!(sym1.as_u32(), 0);
        assert_eq!(sym2.as_u32(), 1);
    }

    #[test]
    fn test_id_display() {
        let node = NodeId::new(42);
        let sym = SymbolId::new(123);
        let ty = TypeId::new(456);
        let module = ModuleId::new(789);

        assert_eq!(format!("{}", node), "node_42");
        assert_eq!(format!("{}", sym), "sym_123");
        assert_eq!(format!("{}", ty), "ty_456");
        assert_eq!(format!("{}", module), "mod_789");
    }
}
