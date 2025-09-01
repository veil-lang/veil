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
use pest_derive::Parser;
use veil_diagnostics::prelude::*;

/// Pest grammar for skeleton parsing and raw tokenization.
///
/// Notes:
/// - WHITESPACE and COMMENT are silent to avoid trivia pairs.
/// - tokens rule is used by `lex_raw`; program remains permissive for the stub.
#[derive(Parser)]
#[grammar_inline = r##"
WHITESPACE = _{ " " | "\t" | "\r" | "\n" }
COMMENT    = _{ LINE_COMMENT | BLOCK_COMMENT }
LINE_COMMENT  = @{ "//" ~ (!"\n" ~ ANY)* ~ ("\n" | EOI) }
BLOCK_COMMENT = @{ "/*" ~ (!"*/" ~ ANY)* ~ "*/" }

ident_continue = _{ ASCII_ALPHANUMERIC | "_" }
ident = @{ (ASCII_ALPHA | "_") ~ ident_continue* }

hex_prefix = @{ "0x" }
bin_prefix = @{ "0b" }
int = @{
    (hex_prefix ~ ASCII_HEX_DIGIT+)
  | (bin_prefix ~ ("0" | "1")+)
  | ASCII_DIGIT+
}
float = @{ ASCII_DIGIT+ ~ "." ~ ASCII_DIGIT+ }

dq_string_inner = { "\\\"" | !"\"" ~ ANY }
str_lit = @{ "\"" ~ dq_string_inner* ~ "\"" }

bt_string_inner = { "\\`" | !"`" ~ ANY }
template_str = @{ "`" ~ bt_string_inner* ~ "`" }

ELLIPSIS = { "..." }
DOUBLE_STAR = { "**" }
ARROW  = { "->" }
FATARROW = { "=>" }
EQEQ   = { "==" }
NOTEQ  = { "!=" }
GTE    = { ">=" }
LTE    = { "<=" }
RANGE_EQ = { "..=" }
RANGE_GT = { "..>" }
RANGE_LT = { "..<" }
RANGE   = { ".." }
ANDAND  = { "&&" }
OROR    = { "||" }

DOT = { "." }
LPAREN = { "(" }
RPAREN = { ")" }
LBRACE = { "{" }
RBRACE = { "}" }
LBRACK = { "[" }
RBRACK = { "]" }
EMPTY_ARR = { "[]" }
COMMA = { "," }
EQ = { "=" }
SEMI = { ";" }
PLUS = { "+" }
MINUS = { "-" }
STAR = { "*" }
SLASH = { "/" }
CARET = { "^" }
PERCENT = { "%" }
GT = { ">" }
LT = { "<" }
PIPE = { "|" }
HASH = { "#" }
BANG = { "!" }
QUESTION = { "?" }
COLON = { ":" }

KW_FN = @{ "fn" ~ !ident_continue }
KW_TEST = @{ "test" ~ !ident_continue }
KW_LET = @{ "let" ~ !ident_continue }
KW_VAR = @{ "var" ~ !ident_continue }
KW_IF = @{ "if" ~ !ident_continue }
KW_ELSE = @{ "else" ~ !ident_continue }
KW_RETURN = @{ "return" ~ !ident_continue }
KW_RAWPTR = @{ "rawptr" ~ !ident_continue }
KW_DEFER = @{ "defer" ~ !ident_continue }
KW_SAFE = @{ "safe" ~ !ident_continue }
KW_AS = @{ "as" ~ !ident_continue }
KW_WHILE = @{ "while" ~ !ident_continue }
KW_FOR = @{ "for" ~ !ident_continue }
KW_STEP = @{ "step" ~ !ident_continue }
KW_LOOP = @{ "loop" ~ !ident_continue }
KW_IMPORT = @{ "import" ~ !ident_continue }
KW_FROM = @{ "from" ~ !ident_continue }
KW_EXPORT = @{ "export" ~ !ident_continue }
KW_STRUCT = @{ "struct" ~ !ident_continue }
KW_IMPL = @{ "impl" ~ !ident_continue }
KW_NEW = @{ "new" ~ !ident_continue }
KW_CONSTRUCTOR = @{ "constructor" ~ !ident_continue }
KW_ENUM = @{ "enum" ~ !ident_continue }
KW_MATCH = @{ "match" ~ !ident_continue }
KW_TRUE = @{ "true" ~ !ident_continue }
KW_FALSE = @{ "false" ~ !ident_continue }
KW_NONE1 = @{ "None" ~ !ident_continue }
KW_NONE2 = @{ "none" ~ !ident_continue }
KW_IN = @{ "in" ~ !ident_continue }
KW_FOREIGN = @{ "foreign" ~ !ident_continue }

TY_BOOL = @{ "bool" ~ !ident_continue }
TY_STRING = @{ "string" ~ !ident_continue }
TY_VOID = @{ "void" ~ !ident_continue }
TY_ANY = @{ "any" ~ !ident_continue }

TY_U8A = @{ "byte" ~ !ident_continue }
TY_U8B = @{ "u8" ~ !ident_continue }
TY_U16A = @{ "ushort" ~ !ident_continue }
TY_U16B = @{ "u16" ~ !ident_continue }
TY_U32A = @{ "uint" ~ !ident_continue }
TY_U32B = @{ "u32" ~ !ident_continue }
TY_U64A = @{ "ulong" ~ !ident_continue }
TY_U64B = @{ "u64" ~ !ident_continue }
TY_I8A = @{ "sbyte" ~ !ident_continue }
TY_I8B = @{ "i8" ~ !ident_continue }
TY_I16A = @{ "short" ~ !ident_continue }
TY_I16B = @{ "i16" ~ !ident_continue }
TY_I32A = @{ "int" ~ !ident_continue }
TY_I32B = @{ "i32" ~ !ident_continue }
TY_I64A = @{ "long" ~ !ident_continue }
TY_I64B = @{ "i64" ~ !ident_continue }
TY_F32A = @{ "float" ~ !ident_continue }
TY_F32B = @{ "f32" ~ !ident_continue }
TY_F64A = @{ "double" ~ !ident_continue }
TY_F64B = @{ "f64" ~ !ident_continue }

token = {
      COMMENT
    | KW_FN | KW_TEST | KW_LET | KW_VAR | KW_IF | KW_ELSE | KW_RETURN | KW_RAWPTR | KW_DEFER
    | KW_SAFE | KW_AS | KW_WHILE | KW_FOR | KW_STEP | KW_LOOP | KW_IMPORT | KW_FROM | KW_EXPORT
    | KW_STRUCT | KW_IMPL | KW_NEW | KW_CONSTRUCTOR | KW_ENUM | KW_MATCH | KW_TRUE | KW_FALSE
    | KW_NONE1 | KW_NONE2 | KW_IN | KW_FOREIGN
    | TY_BOOL | TY_STRING | TY_VOID | TY_ANY
    | TY_U8A | TY_U8B | TY_U16A | TY_U16B | TY_U32A | TY_U32B | TY_U64A | TY_U64B
    | TY_I8A | TY_I8B | TY_I16A | TY_I16B | TY_I32A | TY_I32B | TY_I64A | TY_I64B
    | TY_F32A | TY_F32B | TY_F64A | TY_F64B
    | ELLIPSIS | DOUBLE_STAR | ARROW | FATARROW | EQEQ | NOTEQ | GTE | LTE
    | RANGE_EQ | RANGE_GT | RANGE_LT | RANGE
    | ANDAND | OROR
    | EMPTY_ARR
    | DOT | LPAREN | RPAREN | LBRACE | RBRACE | LBRACK | RBRACK | COMMA | EQ | SEMI
    | PLUS | MINUS | STAR | SLASH | CARET | PERCENT | GT | LT | PIPE | HASH | BANG | QUESTION | COLON
    | float | int | template_str | str_lit | ident
}

tokens = { SOI ~ (WHITESPACE | token)* ~ EOI }

program = { SOI ~ ANY* ~ EOI }
"##]
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

/// Raw token kind exposed by the syntax crate for M2a.
/// This is intentionally coarse; the RD parser remains in use and will be fed later.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RawToken {
    Ident(String),
    Int(String),
    Float(String),
    Str(String),
    TemplateStr(String),
    Keyword(String),
    TypeKeyword(String),
    Symbol(String),
}

/// A raw token with its source span.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RawSpanned {
    pub token: RawToken,
    pub span: Span,
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

/// Tokenize the file into RawSpanned tokens using the Pest grammar.
/// Whitespace and comments are omitted.
pub fn lex_raw(files: &Files<String>, file_id: FileId) -> Result<Vec<RawSpanned>, Vec<Diag>> {
    let source = files.source(file_id);
    let mut out = Vec::new();

    let pairs = match VeilParser::parse(Rule::tokens, source) {
        Ok(p) => p,
        Err(e) => {
            let message = format!("Lex error: {}", e);
            let span = match e.location {
                pest::error::InputLocation::Pos(pos) => {
                    let start = pos as u32;
                    Span::new(start, start.saturating_add(1))
                }
                pest::error::InputLocation::Span((start, end)) => {
                    Span::new(start as u32, end as u32)
                }
            };
            return Err(vec![error(message, file_id, span)]);
        }
    };

    for pair in pairs {
        if pair.as_rule() != Rule::tokens {
            continue;
        }
        for inner in pair.into_inner() {
            // Skip WHITESPACE because it is silent; COMMENT also silent
            let rule = inner.as_rule();
            if matches!(
                rule,
                Rule::WHITESPACE | Rule::COMMENT | Rule::LINE_COMMENT | Rule::BLOCK_COMMENT
            ) {
                continue;
            }
            let span = inner.as_span();
            let start = span.start() as u32;
            let end = span.end() as u32;
            let text = inner.as_str();

            let token = match rule {
                Rule::ident => RawToken::Ident(text.to_string()),
                Rule::int => RawToken::Int(text.to_string()),
                Rule::float => RawToken::Float(text.to_string()),
                Rule::str_lit => RawToken::Str(text.to_string()),
                Rule::template_str => RawToken::TemplateStr(text.to_string()),

                // Keywords
                Rule::KW_FN
                | Rule::KW_TEST
                | Rule::KW_LET
                | Rule::KW_VAR
                | Rule::KW_IF
                | Rule::KW_ELSE
                | Rule::KW_RETURN
                | Rule::KW_RAWPTR
                | Rule::KW_DEFER
                | Rule::KW_SAFE
                | Rule::KW_AS
                | Rule::KW_WHILE
                | Rule::KW_FOR
                | Rule::KW_STEP
                | Rule::KW_LOOP
                | Rule::KW_IMPORT
                | Rule::KW_FROM
                | Rule::KW_EXPORT
                | Rule::KW_STRUCT
                | Rule::KW_IMPL
                | Rule::KW_NEW
                | Rule::KW_CONSTRUCTOR
                | Rule::KW_ENUM
                | Rule::KW_MATCH
                | Rule::KW_TRUE
                | Rule::KW_FALSE
                | Rule::KW_NONE1
                | Rule::KW_NONE2
                | Rule::KW_IN
                | Rule::KW_FOREIGN => RawToken::Keyword(text.to_string()),

                // Type keywords
                Rule::TY_BOOL
                | Rule::TY_STRING
                | Rule::TY_VOID
                | Rule::TY_ANY
                | Rule::TY_U8A
                | Rule::TY_U8B
                | Rule::TY_U16A
                | Rule::TY_U16B
                | Rule::TY_U32A
                | Rule::TY_U32B
                | Rule::TY_U64A
                | Rule::TY_U64B
                | Rule::TY_I8A
                | Rule::TY_I8B
                | Rule::TY_I16A
                | Rule::TY_I16B
                | Rule::TY_I32A
                | Rule::TY_I32B
                | Rule::TY_I64A
                | Rule::TY_I64B
                | Rule::TY_F32A
                | Rule::TY_F32B
                | Rule::TY_F64A
                | Rule::TY_F64B => RawToken::TypeKeyword(text.to_string()),

                // Symbols and operators
                Rule::ELLIPSIS
                | Rule::DOUBLE_STAR
                | Rule::ARROW
                | Rule::FATARROW
                | Rule::EQEQ
                | Rule::NOTEQ
                | Rule::GTE
                | Rule::LTE
                | Rule::RANGE_EQ
                | Rule::RANGE_GT
                | Rule::RANGE_LT
                | Rule::RANGE
                | Rule::ANDAND
                | Rule::OROR
                | Rule::EMPTY_ARR
                | Rule::DOT
                | Rule::LPAREN
                | Rule::RPAREN
                | Rule::LBRACE
                | Rule::RBRACE
                | Rule::LBRACK
                | Rule::RBRACK
                | Rule::COMMA
                | Rule::EQ
                | Rule::SEMI
                | Rule::PLUS
                | Rule::MINUS
                | Rule::STAR
                | Rule::SLASH
                | Rule::CARET
                | Rule::PERCENT
                | Rule::GT
                | Rule::LT
                | Rule::PIPE
                | Rule::HASH
                | Rule::BANG
                | Rule::QUESTION
                | Rule::COLON => RawToken::Symbol(text.to_string()),

                // Container rules, should be skipped above
                Rule::tokens | Rule::program => continue,

                // Fallback: treat any unknown as symbol to avoid panic in early skeleton
                _ => RawToken::Symbol(text.to_string()),
            };

            out.push(RawSpanned {
                token,
                span: Span::new(start, end),
            });
        }
    }

    Ok(out)
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

    #[test]
    fn lex_raw_basic_tokens() {
        let mut files = Files::<String>::new();
        let fid = files.add("t.veil".into(), "fn test() { return 1+2; }".into());
        let toks = lex_raw(&files, fid).expect("lex ok");
        assert!(
            toks.iter()
                .any(|t| matches!(t.token, RawToken::Keyword(ref k) if k == "fn"))
        );
        assert!(
            toks.iter()
                .any(|t| matches!(t.token, RawToken::Ident(ref s) if s == "test"))
        );
        assert!(
            toks.iter()
                .any(|t| matches!(t.token, RawToken::Int(ref s) if s == "1"))
        );
    }
}
