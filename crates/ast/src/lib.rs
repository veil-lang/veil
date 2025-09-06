#![forbid(unsafe_code)]
#![allow(rust_2018_idioms)]
#![allow(elided_lifetimes_in_paths)]
#![deny(unused_must_use)]

//! Veil AST crate
//!
//! This crate provides the core Abstract Syntax Tree (AST) definitions for the Veil language.
//! It contains all the data structures representing parsed Veil code, including:
//!
//! - Type definitions (primitives, structs, enums, generics, etc.)
//! - Expression nodes (literals, binary ops, function calls, etc.)
//! - Statement nodes (variable declarations, control flow, etc.)
//! - Declaration nodes (functions, structs, traits, etc.)
//! - Program structure and metadata
//!
//! This crate is used by the parser to build AST from source code, and by all
//! subsequent compiler passes for analysis and transformation.
//!
/// Veil AST module containing all core language constructs.
pub mod ast;
pub use ast::*;

/// Basic compile-time tests for the AST module.
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ast_types_available() {
        // Verify core AST types are accessible
        let _type_example = Type::I32;
        let _visibility_example = Visibility::Private;
    }
}
