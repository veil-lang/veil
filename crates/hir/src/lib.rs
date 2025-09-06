#![forbid(unsafe_code)]
#![allow(rust_2018_idioms)]
#![allow(elided_lifetimes_in_paths)]
#![deny(unused_must_use)]

//! Veil HIR (High-level Intermediate Representation)
//!
//! This crate provides:
//! - HIR node definitions with stable IDs and symbol references
//! - AST→HIR lowering pass
//! - Span mapping table for diagnostics
//! - Visitor patterns for HIR traversal
//!
//! The HIR is designed to be more type-checker friendly than the AST,
//! while preserving all necessary information for accurate diagnostics.

pub mod ids;
pub mod lower;
pub mod nodes;
pub mod visitors;

// Re-export main types
pub use ids::*;
pub use lower::{LoweringContext, lower_program};
pub use nodes::*;
pub use visitors::*;

/// HIR version for compatibility checking
pub const HIR_VERSION: u32 = 1;
