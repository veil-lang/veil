#![forbid(unsafe_code)]
#![deny(rust_2018_idioms)]
#![deny(unused_must_use)]

/*!
Veil Syntax (Pest frontend skeleton)

This crate provides a minimal, production-safe skeleton for the Pest-based
frontend. It intentionally does not build a real AST yet; instead, it validates
that the input can be recognized by a trivial grammar and returns a placeholder
structure. Subsequent milestones will replace the placeholder with a real AST
builder and a complete grammar.

Key points:
- Pest is the default parser (no feature flag).
- Grammar is intentionally minimal here; it will be expanded in M2.
- parse() returns a placeholder to be wired to the AST crate later.

API stability notes:
- The parse() signature takes codespan Files and a FileId so diagnostics remain
  consistent across passes.
- Diagnostics are returned as a Vec of codespan-reporting diagnostics.
*/

use pest::Parser;
use veil_diagnostics::prelude::*;

/// Minimal Pest grammar for the skeleton phase.
///
/// Notes:
/// - WHITESPACE is silent (`_`) to avoid producing pairs for trivia.
/// - program accepts any input between SOI and EOI to keep the stub permissive.
///   This will be replaced with the real grammar rules in the next milestone.
#[derive(Parser)]
#[grammar_inline = r#"
WHITESPACE = _{ " " | "\t" | "\r" | "\n" }

program = { SOI ~ ANY* ~ EOI }
"#]
struct VeilParser;

/// Placeholder type returned by parse(), to be replaced by the real AST type.
///
/// This holds the file identifier and whether the file was non-empty. It is
/// deliberately small and forward-compatible with future wiring to the AST crate.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AstPlaceholder {
    pub file_id: FileId,
    pub non_empty: bool,
}

/// Parse the given file and return a placeholder AST or diagnostics.
///
/// Future work (M2+):
/// - Replace AstPlaceholder with the real AST type (e.g., ast::Program)
/// - Expand grammar and implement an adapter that lowers pairs into AST nodes
/// - Map pest spans to codespan::Span for precise diagnostics
pub fn parse(files: &Files<String>, file_id: FileId) -> Result<AstPlaceholder, Vec<Diag>> {
    let source = files.source(file_id);

    match VeilParser::parse(Rule::program, source) {
        Ok(mut pairs) => {
            // For now, the stub only checks if there's any non-whitespace content.
            let non_empty = pairs
                .next()
                .map(|p| {
                    let span = p.as_span();
                    let s = &source[span.start()..span.end()];
                    s.chars().any(|c| !c.is_whitespace())
                })
                .unwrap_or(false);

            Ok(AstPlaceholder { file_id, non_empty })
        }
        Err(e) => {
            // Convert pest error into a single diagnostic. We keep it simple here.
            let mut diags = Vec::with_capacity(1);
            let message = format!("Parse error: {}", e);
            let span = match e.location {
                pest::error::InputLocation::Pos(pos) => {
                    // Highlight a single byte when only a position is given.
                    let start = pos as u32;
                    Span::new(start, start.saturating_add(1))
                }
                pest::error::InputLocation::Span((start, end)) => {
                    Span::new(start as u32, end as u32)
                }
            };
            diags.push(error(message, file_id, span));
            Err(diags)
        }
    }
}

/// Optional grammar validation hook (no-op for now).
///
/// When we introduce a standalone `.pest` file and full grammar, this function
/// can leverage pest_meta to validate the grammar during CI or dev checks.
#[cfg(feature = "grammar-validation")]
pub fn validate_grammar() -> Result<(), String> {
    // Placeholder: the inline grammar is trivially valid for the skeleton.
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use veil_diagnostics::Files;

    #[test]
    fn parse_minimal_file_succeeds() {
        let mut files = Files::<String>::new();
        let fid = files.add("test.veil".into(), "fn main {}".into());
        let result = parse(&files, fid);
        assert!(result.is_ok());
        let p = result.unwrap();
        assert_eq!(p.file_id, fid);
        assert!(p.non_empty);
    }

    #[test]
    fn parse_empty_file_succeeds_and_is_empty() {
        let mut files = Files::<String>::new();
        let fid = files.add("empty.veil".into(), "".into());
        let result = parse(&files, fid);
        assert!(result.is_ok());
        let p = result.unwrap();
        assert_eq!(p.file_id, fid);
        assert!(!p.non_empty);
    }
}
