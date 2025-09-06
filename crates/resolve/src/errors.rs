//! Error types for the Veil resolver
//!
//! This module defines error types for name resolution, import resolution,
//! visibility checking, and module system validation.

use codespan::{FileId, Span};
use std::fmt;
use veil_diagnostics::{Diagnostic, Label};

/// Result type for resolver operations
pub type ResolveResult<T> = Result<T, Vec<ResolveError>>;

/// Error types that can occur during name resolution
#[derive(Debug, Clone)]
pub struct ResolveError {
    pub kind: ResolveErrorKind,
    pub span: Option<Span>,
    pub file_id: Option<FileId>,
}

impl ResolveError {
    pub fn new(kind: ResolveErrorKind) -> Self {
        Self {
            kind,
            span: None,
            file_id: None,
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_file(mut self, file_id: FileId) -> Self {
        self.file_id = Some(file_id);
        self
    }

    /// Convert to a diagnostic for reporting
    pub fn to_diagnostic(&self) -> Diagnostic<FileId> {
        let message = self.kind.message();
        let code = self.kind.code();

        let mut diagnostic = Diagnostic::error().with_message(message).with_code(code);

        if let (Some(file_id), Some(span)) = (self.file_id, self.span) {
            diagnostic = diagnostic.with_labels(vec![
                Label::primary(file_id, span).with_message(self.kind.label_message()),
            ]);
        }

        // Add notes/suggestions where applicable
        if let Some(note) = self.kind.note() {
            diagnostic = diagnostic.with_notes(vec![note]);
        }

        diagnostic
    }
}

/// Specific kinds of resolution errors
#[derive(Debug, Clone)]
pub enum ResolveErrorKind {
    /// Undefined symbol
    UndefinedSymbol {
        name: String,
        candidates: Vec<String>,
    },

    /// Symbol already defined in current scope
    DuplicateSymbol {
        name: String,
        original_location: Option<Span>,
    },

    /// Module not found during import
    ModuleNotFound {
        module_path: String,
        searched_paths: Vec<String>,
    },

    /// Circular import dependency
    CircularImport { cycle: Vec<String> },

    /// Invalid visibility access
    PrivateAccess {
        symbol_name: String,
        symbol_module: String,
        access_module: String,
    },

    /// Import item not found in module
    ImportItemNotFound {
        item_name: String,
        module_path: String,
        available_items: Vec<String>,
    },

    /// Invalid module path syntax
    InvalidModulePath { path: String, reason: String },

    /// Ambiguous import (multiple items with same name)
    AmbiguousImport { name: String, sources: Vec<String> },

    /// Self-import (module importing itself)
    SelfImport { module_path: String },

    /// Invalid visibility modifier
    InvalidVisibility { visibility: String, context: String },

    /// Generic parameter conflict
    GenericParameterConflict { name: String, location: String },

    /// Type parameter used in invalid context
    InvalidTypeParameterUsage { param_name: String, context: String },

    /// FFI symbol conflict
    FfiSymbolConflict {
        symbol_name: String,
        conflicting_type: String,
    },
}

impl ResolveErrorKind {
    /// Error code for categorization
    pub fn code(&self) -> String {
        match self {
            Self::UndefinedSymbol { .. } => "E0001".to_string(),
            Self::DuplicateSymbol { .. } => "E0002".to_string(),
            Self::ModuleNotFound { .. } => "E0003".to_string(),
            Self::CircularImport { .. } => "E0004".to_string(),
            Self::PrivateAccess { .. } => "E0005".to_string(),
            Self::ImportItemNotFound { .. } => "E0006".to_string(),
            Self::InvalidModulePath { .. } => "E0007".to_string(),
            Self::AmbiguousImport { .. } => "E0008".to_string(),
            Self::SelfImport { .. } => "E0009".to_string(),
            Self::InvalidVisibility { .. } => "E0010".to_string(),
            Self::GenericParameterConflict { .. } => "E0011".to_string(),
            Self::InvalidTypeParameterUsage { .. } => "E0012".to_string(),
            Self::FfiSymbolConflict { .. } => "E0013".to_string(),
        }
    }

    /// Primary error message
    pub fn message(&self) -> String {
        match self {
            Self::UndefinedSymbol { name, .. } => {
                format!("Cannot find symbol `{}` in this scope", name)
            }
            Self::DuplicateSymbol { name, .. } => {
                format!("Symbol `{}` is already defined in this scope", name)
            }
            Self::ModuleNotFound { module_path, .. } => {
                format!("Module `{}` not found", module_path)
            }
            Self::CircularImport { .. } => "Circular import dependency detected".to_string(),
            Self::PrivateAccess {
                symbol_name,
                symbol_module,
                ..
            } => {
                format!(
                    "Symbol `{}` from module `{}` is private",
                    symbol_name, symbol_module
                )
            }
            Self::ImportItemNotFound {
                item_name,
                module_path,
                ..
            } => {
                format!("Item `{}` not found in module `{}`", item_name, module_path)
            }
            Self::InvalidModulePath { path, .. } => {
                format!("Invalid module path `{}`", path)
            }
            Self::AmbiguousImport { name, .. } => {
                format!("Ambiguous import of `{}`", name)
            }
            Self::SelfImport { module_path } => {
                format!("Module `{}` cannot import itself", module_path)
            }
            Self::InvalidVisibility {
                visibility,
                context,
            } => {
                format!("Invalid visibility `{}` in {}", visibility, context)
            }
            Self::GenericParameterConflict { name, .. } => {
                format!(
                    "Generic parameter `{}` conflicts with existing declaration",
                    name
                )
            }
            Self::InvalidTypeParameterUsage {
                param_name,
                context,
            } => {
                format!(
                    "Type parameter `{}` cannot be used in {}",
                    param_name, context
                )
            }
            Self::FfiSymbolConflict {
                symbol_name,
                conflicting_type,
            } => {
                format!(
                    "FFI symbol `{}` conflicts with existing {}",
                    symbol_name, conflicting_type
                )
            }
        }
    }

    /// Label message for the primary span
    pub fn label_message(&self) -> String {
        match self {
            Self::UndefinedSymbol { name, .. } => {
                format!("`{}` not found", name)
            }
            Self::DuplicateSymbol { name, .. } => {
                format!("`{}` redefined here", name)
            }
            Self::ModuleNotFound { .. } => "module not found".to_string(),
            Self::CircularImport { .. } => "circular import starts here".to_string(),
            Self::PrivateAccess { .. } => "private symbol accessed here".to_string(),
            Self::ImportItemNotFound { item_name, .. } => {
                format!("`{}` not found in module", item_name)
            }
            Self::InvalidModulePath { .. } => "invalid path syntax".to_string(),
            Self::AmbiguousImport { .. } => "ambiguous import".to_string(),
            Self::SelfImport { .. } => "self-import".to_string(),
            Self::InvalidVisibility { .. } => "invalid visibility".to_string(),
            Self::GenericParameterConflict { .. } => "conflicting parameter".to_string(),
            Self::InvalidTypeParameterUsage { .. } => "invalid usage".to_string(),
            Self::FfiSymbolConflict { .. } => "conflicting symbol".to_string(),
        }
    }

    /// Additional note/suggestion for the error
    pub fn note(&self) -> Option<String> {
        match self {
            Self::UndefinedSymbol { candidates, .. } if !candidates.is_empty() => {
                Some(format!("Did you mean one of: {}", candidates.join(", ")))
            }
            Self::ModuleNotFound { searched_paths, .. } if !searched_paths.is_empty() => {
                Some(format!("Searched in: {}", searched_paths.join(", ")))
            }
            Self::CircularImport { cycle } => Some(format!("Import cycle: {}", cycle.join(" -> "))),
            Self::ImportItemNotFound {
                available_items, ..
            } if !available_items.is_empty() => {
                Some(format!("Available items: {}", available_items.join(", ")))
            }
            Self::AmbiguousImport { sources, .. } => {
                Some(format!("Found in: {}", sources.join(", ")))
            }
            Self::InvalidModulePath { reason, .. } => Some(reason.clone()),
            _ => None,
        }
    }
}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.kind.code(), self.kind.message())
    }
}

impl std::error::Error for ResolveError {}

/// Helper functions for common error creation
impl ResolveError {
    pub fn undefined_symbol(name: impl Into<String>) -> Self {
        Self::new(ResolveErrorKind::UndefinedSymbol {
            name: name.into(),
            candidates: Vec::new(),
        })
    }

    pub fn undefined_symbol_with_candidates(
        name: impl Into<String>,
        candidates: Vec<String>,
    ) -> Self {
        Self::new(ResolveErrorKind::UndefinedSymbol {
            name: name.into(),
            candidates,
        })
    }

    pub fn duplicate_symbol(name: impl Into<String>) -> Self {
        Self::new(ResolveErrorKind::DuplicateSymbol {
            name: name.into(),
            original_location: None,
        })
    }

    pub fn module_not_found(module_path: impl Into<String>) -> Self {
        Self::new(ResolveErrorKind::ModuleNotFound {
            module_path: module_path.into(),
            searched_paths: Vec::new(),
        })
    }

    pub fn circular_import(cycle: Vec<String>) -> Self {
        Self::new(ResolveErrorKind::CircularImport { cycle })
    }

    pub fn private_access(
        symbol_name: impl Into<String>,
        symbol_module: impl Into<String>,
        access_module: impl Into<String>,
    ) -> Self {
        Self::new(ResolveErrorKind::PrivateAccess {
            symbol_name: symbol_name.into(),
            symbol_module: symbol_module.into(),
            access_module: access_module.into(),
        })
    }

    pub fn import_item_not_found(
        item_name: impl Into<String>,
        module_path: impl Into<String>,
    ) -> Self {
        Self::new(ResolveErrorKind::ImportItemNotFound {
            item_name: item_name.into(),
            module_path: module_path.into(),
            available_items: Vec::new(),
        })
    }

    pub fn invalid_module_path(path: impl Into<String>, reason: impl Into<String>) -> Self {
        Self::new(ResolveErrorKind::InvalidModulePath {
            path: path.into(),
            reason: reason.into(),
        })
    }

    pub fn ambiguous_import(name: impl Into<String>, sources: Vec<String>) -> Self {
        Self::new(ResolveErrorKind::AmbiguousImport {
            name: name.into(),
            sources,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_creation() {
        let error = ResolveError::undefined_symbol("test_symbol");
        assert!(matches!(
            error.kind,
            ResolveErrorKind::UndefinedSymbol { .. }
        ));
        assert_eq!(error.kind.code(), "E0001");
    }

    #[test]
    fn test_error_with_span() {
        let span = Span::new(codespan::ByteIndex(0), codespan::ByteIndex(10));
        let error = ResolveError::undefined_symbol("test").with_span(span);
        assert_eq!(error.span, Some(span));
    }

    #[test]
    fn test_diagnostic_conversion() {
        let error = ResolveError::undefined_symbol("test_symbol");
        let diagnostic = error.to_diagnostic();
        assert_eq!(diagnostic.severity, veil_diagnostics::Severity::Error);
    }

    #[test]
    fn test_error_messages() {
        let error = ResolveError::duplicate_symbol("test");
        let message = error.kind.message();
        assert!(message.contains("test"));
        assert!(message.contains("already defined"));
    }
}
