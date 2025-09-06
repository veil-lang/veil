#![forbid(unsafe_code)]
#![allow(rust_2018_idioms)]
#![allow(elided_lifetimes_in_paths)]
#![deny(unused_must_use)]

//! Veil Name Resolution and Symbol Management
//!
//! This crate provides:
//! - Symbol tables for tracking names and their definitions
//! - Module graph construction and cycle detection
//! - Import resolution (std/local/external)
//! - Visibility enforcement (private/internal/public)
//! - Scoping rules and name lookup
//!
//! The resolver operates on HIR (High-level Intermediate Representation)
//! and produces a fully resolved symbol table that can be used by
//! subsequent compiler passes.

pub mod errors;
pub mod module_graph;
pub mod resolver;
pub mod scope;
pub mod symbol_table;
pub mod visibility;

// Re-export main types
pub use errors::{ResolveError, ResolveResult};
pub use module_graph::{ModuleGraph, ModuleNode};
pub use resolver::{Resolver, ResolverContext, resolve_program};
pub use scope::{Scope, ScopeStack};
pub use symbol_table::{Symbol, SymbolKind, SymbolTable};
pub use visibility::{VisibilityChecker, VisibilityLevel};

/// Resolver version for compatibility checking
pub const RESOLVER_VERSION: u32 = 1;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolver_version() {
        assert_eq!(RESOLVER_VERSION, 1);
    }
}
