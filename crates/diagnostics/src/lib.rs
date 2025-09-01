#![forbid(unsafe_code)]
#![deny(rust_2018_idioms)]
#![deny(unused_must_use)]

//! Veil Diagnostics (minimal API)
//!
//! Thin, ergonomic wrappers over `codespan` and `codespan-reporting` to keep
//! diagnostics consistent across the compiler passes.
//!
//! Goals:
//! - Central place to create errors/warnings/notes with spans
//! - Consistent emission helpers for terminal output
//! - Re-export commonly used types (FileId, Span, Diagnostic, Label, Severity)
//!
//! Non-goals (for now):
//! - Advanced formatting policies
//! - Rich fix-it application (we expose helpers as notes)

pub use codespan::{FileId, Files, Span};
pub use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
pub use codespan_reporting::term;
pub use codespan_reporting::term::termcolor;

/// Type aliases to reduce repetition at call sites.
pub type Diag = Diagnostic<FileId>;
pub type Lbl = Label<FileId>;

/// Convenience prelude for downstream crates.
pub mod prelude {
    pub use super::{
        Diag, FileId, Files, Lbl, Reporter, Severity, Span, error, error_with, label_primary,
        label_secondary, note, warning,
    };
}

// -------------------------------------------------------------------------------------------------
// Builders & helpers
// -------------------------------------------------------------------------------------------------

/// Create an error diagnostic with a single primary label at `span`.
///
/// Example:
/// let d = error("Expected identifier", file_id, span);
pub fn error(message: impl Into<String>, file_id: FileId, span: Span) -> Diag {
    Diagnostic::error()
        .with_message(message)
        .with_labels(vec![Label::primary(file_id, span)])
}

/// Create an error diagnostic with a code and a single primary label.
///
/// Example:
/// let d = error_with("VE0001", "Expected identifier", file, span);
pub fn error_with(
    code: impl Into<String>,
    message: impl Into<String>,
    file_id: FileId,
    span: Span,
) -> Diag {
    Diagnostic::error()
        .with_code(code)
        .with_message(message)
        .with_labels(vec![Label::primary(file_id, span)])
}

/// Create a warning diagnostic with a single primary label.
pub fn warning(message: impl Into<String>, file_id: FileId, span: Span) -> Diag {
    Diagnostic::warning()
        .with_message(message)
        .with_labels(vec![Label::primary(file_id, span)])
}

/// Create a note diagnostic with a single primary label.
pub fn note(message: impl Into<String>, file_id: FileId, span: Span) -> Diag {
    Diagnostic::note()
        .with_message(message)
        .with_labels(vec![Label::primary(file_id, span)])
}

/// Primary label helper with message.
pub fn label_primary(file_id: FileId, span: Span, message: impl Into<String>) -> Lbl {
    Label::primary(file_id, span).with_message(message)
}

/// Secondary label helper with message.
pub fn label_secondary(file_id: FileId, span: Span, message: impl Into<String>) -> Lbl {
    Label::secondary(file_id, span).with_message(message)
}

/// Attach an additional note (help/fix-it) to a diagnostic.
///
/// Example:
/// let d = error("...", fid, sp).with_notes(vec![help("use `var` instead of `let`")]);
pub fn help(msg: impl Into<String>) -> String {
    let m: String = msg.into();
    if m.starts_with("help:") {
        m
    } else {
        format!("help: {}", m)
    }
}

// -------------------------------------------------------------------------------------------------
// Reporter
// -------------------------------------------------------------------------------------------------

/// Terminal reporter for diagnostics.
///
/// Wraps `codespan-reporting::term::emit` with a standard stream and config.
pub struct Reporter {
    writer: termcolor::StandardStream,
    config: term::Config,
}

impl Reporter {
    /// Create a reporter with the given color choice (Auto recommended).
    pub fn new(color: termcolor::ColorChoice) -> Self {
        Self {
            writer: termcolor::StandardStream::stderr(color),
            config: term::Config::default(),
        }
    }

    /// Create a reporter with a custom `term::Config`.
    pub fn with_config(color: termcolor::ColorChoice, config: term::Config) -> Self {
        Self {
            writer: termcolor::StandardStream::stderr(color),
            config,
        }
    }

    /// Emit a single diagnostic.
    pub fn emit(
        &mut self,
        files: &Files<String>,
        diag: &Diag,
    ) -> Result<(), codespan_reporting::files::Error> {
        term::emit(&mut self.writer.lock(), &self.config, files, diag)
    }

    /// Emit multiple diagnostics.
    pub fn emit_all<'a, I>(
        &mut self,
        files: &Files<String>,
        diagnostics: I,
    ) -> Result<(), codespan_reporting::files::Error>
    where
        I: IntoIterator<Item = &'a Diag>,
    {
        let mut lock = self.writer.lock();
        for d in diagnostics {
            term::emit(&mut lock, &self.config, files, d)?;
        }
        Ok(())
    }

    /// Emit diagnostics and return the first error if any were errors.
    ///
    /// This is useful for "fail-fast" flows that still want to print everything.
    pub fn emit_and_fail_on_error<'a, I>(
        &mut self,
        files: &Files<String>,
        diagnostics: I,
    ) -> Result<Option<Diag>, codespan_reporting::files::Error>
    where
        I: IntoIterator<Item = &'a Diag>,
    {
        let mut lock = self.writer.lock();
        let mut first_error: Option<Diag> = None;

        for d in diagnostics {
            if matches!(d.severity, Severity::Error | Severity::Bug) && first_error.is_none() {
                first_error = Some(d.clone());
            }
            term::emit(&mut lock, &self.config, files, d)?;
        }
        Ok(first_error)
    }
}

// -------------------------------------------------------------------------------------------------
// Tests (crate-local)
// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_error_with_code_and_help() {
        let mut files = Files::<String>::new();
        let fid = files.add("test.veil".to_string(), "test content".to_string());
        let span = Span::new(10, 20);
        let diag = error_with("VE0001", "Expected identifier", fid, span)
            .with_labels(vec![
                label_primary(fid, span, "here"),
                label_secondary(fid, Span::new(0, 5), "context"),
            ])
            .with_notes(vec![help("declare a name: `var name = ...`")]);

        assert!(matches!(diag.severity, Severity::Error));
        assert!(diag.code.is_some());
        assert_eq!(diag.labels.len(), 2);
        assert_eq!(diag.notes.len(), 1);
    }
}
