#![forbid(unsafe_code)]
#![deny(unused_must_use, rust_2018_idioms)]
#![cfg_attr(not(debug_assertions), deny(warnings))]

//! Veil Lexer (stub)
//!
//! This crate is a minimal placeholder to establish the workspace structure and
//! public surface for token- and trivia-related types. The actual tokenization
//! is performed by the grammar-driven frontend (Pest) in the syntax layer.
//!
//! Goals of this stub:
//! - Provide stable, reusable types for tokens, trivia, and spans
//! - Allow downstream crates to depend on `veil_lexer` without circular deps
//! - Make it trivial to replace internals later without breaking import paths
//!
//! Non-goals:
//! - Implement a real lexer here (the default parser is Pest-driven)

use core::fmt;

/// A byte span into a source file.
///
/// Start is inclusive, end is exclusive, both are byte offsets into the file.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    #[inline]
    pub const fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    #[inline]
    pub const fn len(self) -> u32 {
        self.end.saturating_sub(self.start)
    }

    #[inline]
    pub const fn is_empty(self) -> bool {
        self.start >= self.end
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Span")
            .field("start", &self.start)
            .field("end", &self.end)
            .finish()
    }
}

/// The kind of trivia (non-semantic tokens) encountered.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
#[non_exhaustive]
pub enum TriviaKind {
    Whitespace,
    Comment,
}

/// A trivia item (e.g., whitespace, comment) with its span.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Trivia {
    pub kind: TriviaKind,
    pub span: Span,
}

impl Trivia {
    #[inline]
    pub const fn new(kind: TriviaKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// The kind of a token. This is intentionally minimal while the real parser
/// is Pest-driven. Extend as needed when/if a self-hosted lexer is introduced.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
#[non_exhaustive]
pub enum TokenKind {
    /// A placeholder for any token that is not modeled in the stub.
    Unknown,
    /// Identifier (when needed by helpers or tests).
    Ident,
    /// String literal (when needed by helpers or tests).
    String,
    /// Integer literal (when needed by helpers or tests).
    Int,
    /// Punctuation (catch-all).
    Punct,
}

/// A token with its kind and span.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    #[inline]
    pub const fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// Result of a (future) lexing operation.
///
/// Kept for compatibility with potential helper tooling or tests that want to
/// reason about leading/trailing trivia separately.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct LexResult {
    pub tokens: Vec<Token>,
    pub leading_trivia: Vec<Trivia>,
    pub trailing_trivia: Vec<Trivia>,
}

/// Stub entry point. Returns an empty result.
///
/// The real parser is grammar-driven (Pest) and should be used for production
/// parsing. This function exists to keep a stable API and unblock dependencies.
#[inline]
pub fn lex_stub(_source: &str) -> LexResult {
    LexResult::default()
}

// Re-exports for downstream convenience.
pub use Span as LexSpan;
pub use Token as LexToken;
pub use TokenKind as LexTokenKind;
pub use Trivia as LexTrivia;
pub use TriviaKind as LexTriviaKind;
