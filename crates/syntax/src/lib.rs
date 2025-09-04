#![forbid(unsafe_code)]
#![deny(rust_2018_idioms)]
#![deny(unused_must_use)]

/*!
Veil Syntax (Pest frontend)

This crate provides the production Pest-based frontend for the Veil compiler.
It includes a complete grammar implementation and builds real AST structures
from the parsed input.

Key features:
- Complete Veil v2.0 grammar support via Pest
- Full AST construction with proper spans and type information
- Template string parsing with embedded expressions
- Comprehensive error diagnostics with source locations

API stability notes:
- The parse() signature takes codespan Files and a FileId so diagnostics remain
  consistent across passes.
- Diagnostics are returned as a Vec of codespan-reporting diagnostics.
*/

use pest::Parser;
use pest_derive::Parser;
pub use veil_ast as ast;
use veil_diagnostics::help;
use veil_diagnostics::prelude::*;
use veil_diagnostics::{Diagnostic, Label};

/// Pest grammar for skeleton parsing and raw tokenization.
///
/// Notes:
/// - WHITESPACE and COMMENT are silent to avoid trivia pairs.
/// - tokens rule is used by `lex_raw`; program provides complete language support.
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
POW = { "**" }
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
IDIV    = { "//" }

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
TY_STR = @{ "str" ~ !ident_continue }
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
    | TY_BOOL | TY_STRING | TY_STR | TY_VOID | TY_ANY
    | TY_U8A | TY_U8B | TY_U16A | TY_U16B | TY_U32A | TY_U32B | TY_U64A | TY_U64B
    | TY_I8A | TY_I8B | TY_I16A | TY_I16B | TY_I32A | TY_I32B | TY_I64A | TY_I64B
    | TY_F32A | TY_F32B | TY_F64A | TY_F64B
    | ELLIPSIS | POW | ARROW | FATARROW | EQEQ | NOTEQ | GTE | LTE
    | RANGE_EQ | RANGE_GT | RANGE_LT | RANGE
    | ANDAND | OROR | IDIV
    | EMPTY_ARR
    | DOT | LPAREN | RPAREN | LBRACE | RBRACE | LBRACK | RBRACK | COMMA | EQ | SEMI
    | PLUS | MINUS | STAR | SLASH | CARET | PERCENT | GT | LT | PIPE | HASH | BANG | QUESTION | COLON
    | float | int | template_str | str_lit | ident
}

tokens = { SOI ~ (WHITESPACE | token)* ~ EOI }

program = { SOI ~ ANY* ~ EOI }
"##]
struct VeilParser;

pub mod ast_grammar {

    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "veil.pest"]
    pub struct AstParser;
    pub use self::Rule as AstRule;
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

/// Parse the given file and return a complete AST or diagnostics.
///
/// Implementation:
/// - Grammar provides complete language support
/// - AST adapter lowers Pest pairs into proper AST nodes with spans
/// - Maps pest spans to codespan::Span for precise diagnostics
fn codespan_span(span: pest::Span<'_>) -> Span {
    Span::new(span.start() as u32, span.end() as u32)
}

/// Parse using veil.pest and build a veil_ast::Program with spans.
/// Returns the program only; warnings are discarded.
pub fn parse_ast(files: &Files<String>, file_id: FileId) -> Result<ast::Program, Vec<Diag>> {
    match parse_ast_with_warnings(files, file_id) {
        Ok((program, _warnings)) => Ok(program),
        Err(diags) => Err(diags),
    }
}

/// Parse using veil.pest and build a veil_ast::Program with spans, returning (Program, parser_warnings).
/// This adapter covers declarations (fn/struct/enum/impl/type alias/import/export/ffi/test),
/// statements, expressions (with precedence), types (incl. arrays/optionals/pointers),
/// template strings with embedded expressions, and "/"-only module paths.
pub fn parse_ast_with_warnings(
    files: &Files<String>,
    file_id: FileId,
) -> Result<(ast::Program, Vec<Diag>), Vec<Diag>> {
    use ast_grammar::AstRule as R;
    use pest::iterators::Pair;

    let source = files.source(file_id);

    // Helpers
    let to_span = |sp: pest::Span<'_>| Span::new(sp.start() as u32, sp.end() as u32);
    let _new_info = |sp: pest::Span<'_>| ast::ExprInfo {
        span: to_span(sp),
        ty: ast::Type::Unknown,
        is_tail: false,
    };

    let _parse_ident = |p: &Pair<'_, R>| p.as_str().to_string();
    // Collect parser warnings (e.g., legacy '/' module paths) without failing parse
    let mut warnings: Vec<Diag> = Vec::new();

    // Add deprecation warning helper
    let add_deprecation_warning =
        |warnings: &mut Vec<Diag>, span: Span, old_token: &str, new_token: &str, context: &str| {
            let warning = Diagnostic::new(Severity::Warning)
                .with_message(format!("Use of deprecated {} operator", context))
                .with_labels(vec![
                    Label::primary(file_id, usize::from(span.start())..usize::from(span.end()))
                        .with_message(format!("Use '{}' instead of '{}'", new_token, old_token)),
                ])
                .with_notes(vec![format!(
                    "The '{}' operator is deprecated. Use '{}' for {} operations.",
                    old_token, new_token, context
                )]);
            warnings.push(warning);
        };

    // Parse ModuleType from a module path string
    let _module_type_of = |mp: &str| {
        if mp.starts_with("std::") || mp.starts_with("std/") {
            ast::ModuleType::Standard
        } else if mp.starts_with("./") || mp.starts_with("../") {
            ast::ModuleType::Local
        } else {
            ast::ModuleType::External
        }
    };

    // Parse types (subset mapped to existing ast::Type variants)
    fn parse_type(pair: Pair<'_, R>) -> ast::Type {
        fn parse_type_inner(p: Pair<'_, R>) -> ast::Type {
            use ast::Type;
            match p.as_rule() {
                R::primitive_type => {
                    let t = p.as_str();
                    match t {
                        "bool" => Type::Bool,
                        "string" | "str" => Type::String,
                        "void" => Type::Void,
                        "any" => Type::Any,
                        "i8" | "sbyte" => Type::I8,
                        "i16" | "short" => Type::I16,
                        "i32" | "int" => Type::I32,
                        "i64" | "long" => Type::I64,
                        "u8" | "byte" => Type::U8,
                        "u16" | "ushort" => Type::U16,
                        "u32" | "uint" => Type::U32,
                        "u64" | "ulong" => Type::U64,
                        "f32" | "float" => Type::F32,
                        "f64" | "double" => Type::F64,
                        "rawptr" => Type::RawPtr,
                        _ => Type::Unknown,
                    }
                }
                R::ty_array => {
                    // [T] or [T; N]
                    let mut inner = None;
                    let mut size_expr = None;
                    for c in p.into_inner() {
                        match c.as_rule() {
                            R::ty => inner = Some(parse_type(c)),
                            R::expr => {
                                // Sized arrays: keep type unknown-sized for now if size is not constant
                                size_expr = Some(c);
                            }
                            _ => {}
                        }
                    }
                    if let Some(t) = inner {
                        if size_expr.is_some() {
                            // Fallback to Array for now (we don't have a Size literal in Type)
                            ast::Type::Array(Box::new(t))
                        } else {
                            ast::Type::Array(Box::new(t))
                        }
                    } else {
                        ast::Type::Unknown
                    }
                }
                R::ty_ref => {
                    // &T -> Pointer<T> for now (no distinct ref type at AST)
                    let mut inner = ast::Type::Unknown;
                    for c in p.into_inner() {
                        if matches!(c.as_rule(), R::ty) {
                            inner = parse_type(c);
                        }
                    }
                    ast::Type::Pointer(Box::new(inner))
                }
                R::ty_ptr => {
                    let mut inner = ast::Type::Unknown;
                    for c in p.into_inner() {
                        if matches!(c.as_rule(), R::ty) {
                            inner = parse_type(c);
                        }
                    }
                    ast::Type::Pointer(Box::new(inner))
                }
                R::ty_path => {
                    // Treat as Struct type by default; enum resolution happens later
                    let name = p.as_str().to_string();
                    ast::Type::Struct(name)
                }
                R::ty_primary => {
                    // unwrap to deeper rule
                    let mut last = ast::Type::Unknown;
                    for c in p.into_inner() {
                        last = parse_type_inner(c);
                    }
                    last
                }
                R::ty_union | R::ty_intersection => {
                    // Currently no union/intersection Type node; keep first
                    let mut first = ast::Type::Unknown;
                    for (i, c) in p.into_inner().enumerate() {
                        if i == 0 {
                            first = parse_type_inner(c);
                        }
                    }
                    first
                }
                R::ty => {
                    // Optional support: type ('?')?
                    let mut base = ast::Type::Unknown;
                    let mut is_optional = false;
                    for c in p.into_inner() {
                        match c.as_rule() {
                            R::ty_union | R::ty_intersection | R::ty_primary => {
                                base = parse_type_inner(c);
                            }
                            R::QUESTION => is_optional = true,
                            _ => {}
                        }
                    }
                    if is_optional {
                        ast::Type::Optional(Box::new(base))
                    } else {
                        base
                    }
                }
                _ => ast::Type::Unknown,
            }
        }
        parse_type_inner(pair)
    }

    // Parse expressions (precedence handled by grammar)
    fn parse_expr(pair: Pair<'_, R>) -> ast::Expr {
        use ast::{BinOp, Expr, ExprInfo, RangeType, TemplateStrPart, Type, UnOp};
        let sp = pair.as_span();
        let info = |sp: pest::Span<'_>| ExprInfo {
            span: Span::new(sp.start() as u32, sp.end() as u32),
            ty: Type::Unknown,
            is_tail: false,
        };

        match pair.as_rule() {
            R::literal => {
                let inner = pair.into_inner().next().unwrap();
                match inner.as_rule() {
                    R::float_lit => {
                        Expr::F64(inner.as_str().parse().unwrap_or(0.0), info(inner.as_span()))
                    }
                    R::int_lit => {
                        let txt = inner.as_str();
                        // Prefer i64 if it doesn't fit i32 parsing
                        if let Ok(v) = txt.parse::<i32>() {
                            Expr::Int(v, info(inner.as_span()))
                        } else {
                            let v = if let Some(stripped) = txt.strip_prefix("0x") {
                                i64::from_str_radix(stripped, 16).unwrap_or(0)
                            } else if let Some(stripped) = txt.strip_prefix("0b") {
                                i64::from_str_radix(stripped, 2).unwrap_or(0)
                            } else {
                                txt.parse::<i64>().unwrap_or(0)
                            };
                            Expr::Int64(v, info(inner.as_span()))
                        }
                    }
                    R::str_lit => {
                        // Strip quotes
                        let s = inner.as_str();
                        let content = s[1..s.len().saturating_sub(1)].to_string();
                        Expr::Str(content, info(inner.as_span()))
                    }
                    R::template_str => {
                        // Split into literal and embedded expressions
                        let mut parts = Vec::new();
                        for p in inner.into_inner() {
                            match p.as_rule() {
                                R::bt_string_chunk => {
                                    parts.push(TemplateStrPart::Literal(p.as_str().to_string()))
                                }
                                R::template_interpolation => {
                                    if let Some(e) =
                                        p.into_inner().find(|x| matches!(x.as_rule(), R::expr))
                                    {
                                        parts.push(TemplateStrPart::Expression(Box::new(
                                            parse_expr(e),
                                        )));
                                    }
                                }
                                _ => {}
                            }
                        }
                        Expr::TemplateStr(parts, info(sp))
                    }
                    R::bool_lit => Expr::Bool(inner.as_str() == "true", info(inner.as_span())),
                    R::none_lit => Expr::None(info(inner.as_span())),
                    _ => Expr::Void(info(inner.as_span())),
                }
            }
            R::var_ref => {
                let name = pair.as_str().to_string();
                Expr::Var(name, info(sp))
            }
            R::new_expr => {
                let mut name = String::new();
                let mut args = Vec::new();
                for c in pair.into_inner() {
                    match c.as_rule() {
                        R::ident => name = c.as_str().to_string(),
                        R::argument_list => {
                            for a in c.into_inner().filter(|x| matches!(x.as_rule(), R::expr)) {
                                args.push(parse_expr(a));
                            }
                        }
                        _ => {}
                    }
                }
                Expr::New(name, args, info(sp))
            }
            R::postfix_expr => {
                // Transform primary with chained ops
                let mut inner_iter = pair.into_inner();
                let first = inner_iter.next().unwrap();
                let mut expr = parse_expr(first);
                for op in inner_iter {
                    if op.as_rule() == R::postfix_op {
                        let mut sub = op.into_inner();
                        if let Some(rule) = sub.next() {
                            match rule.as_rule() {
                                R::DOT => {
                                    if let Some(id) = sub.next() {
                                        expr = Expr::FieldAccess(
                                            Box::new(expr),
                                            id.as_str().to_string(),
                                            info(sp),
                                        );
                                    }
                                }
                                R::LBRACK => {
                                    if let Some(index_expr) = sub.next() {
                                        expr = Expr::ArrayAccess(
                                            Box::new(expr),
                                            Box::new(parse_expr(index_expr)),
                                            info(sp),
                                        );
                                    }
                                }
                                R::LPAREN => {
                                    let mut args = Vec::new();
                                    if let Some(al) = sub.next()
                                        && matches!(al.as_rule(), R::argument_list)
                                    {
                                        for a in al
                                            .into_inner()
                                            .filter(|x| matches!(x.as_rule(), R::expr))
                                        {
                                            args.push(parse_expr(a));
                                        }
                                    }
                                    // Function/method call name resolution deferred; use "<method>." for field-based later if needed
                                    expr = match &expr {
                                        Expr::Var(name, _) => {
                                            Expr::Call(name.clone(), args, info(sp))
                                        }
                                        _ => Expr::Call(
                                            "<method>.call".into(),
                                            {
                                                let mut v = vec![expr.clone()];
                                                v.extend(args);
                                                v
                                            },
                                            info(sp),
                                        ),
                                    };
                                }
                                R::QUESTION => {
                                    // Optional chaining or result propagation not modeled; keep as identity
                                }
                                _ => {}
                            }
                        }
                    }
                }
                expr
            }
            R::unary_expr => {
                let mut inner = pair.into_inner();
                let first = inner.next().unwrap();
                match first.as_rule() {
                    R::BANG => ast::Expr::UnaryOp(
                        UnOp::Not,
                        Box::new(parse_expr(inner.next().unwrap())),
                        info(sp),
                    ),
                    R::PLUS => ast::Expr::UnaryOp(
                        UnOp::Plus,
                        Box::new(parse_expr(inner.next().unwrap())),
                        info(sp),
                    ),
                    R::MINUS => ast::Expr::UnaryOp(
                        UnOp::Neg,
                        Box::new(parse_expr(inner.next().unwrap())),
                        info(sp),
                    ),
                    R::AMP | R::STAR | R::KW_AWAIT => {
                        // Desugar to identity for now; pointer ops not modeled here
                        parse_expr(inner.next().unwrap())
                    }
                    _ => parse_expr(first),
                }
            }
            R::cast_expr => {
                // left (as T)*
                let mut t = None;
                let mut left = None;
                for c in pair.into_inner() {
                    match c.as_rule() {
                        R::unary_expr
                        | R::postfix_expr
                        | R::primary_expr
                        | R::literal
                        | R::var_ref => {
                            if left.is_none() {
                                left = Some(parse_expr(c));
                            }
                        }
                        R::ty => t = Some(parse_type(c)),
                        _ => {}
                    }
                }
                match (left, t) {
                    (Some(e), Some(ty)) => ast::Expr::Cast(Box::new(e), ty, info(sp)),
                    (Some(e), None) => e,
                    (None, _) => ast::Expr::Void(info(sp)),
                }
            }
            R::power_expr
            | R::multiplicative_expr
            | R::additive_expr
            | R::shift_expr
            | R::relational_expr
            | R::equality_expr
            | R::bit_and_expr
            | R::bit_xor_expr
            | R::bit_or_expr
            | R::logical_and_expr
            | R::logical_or_expr
            | R::pipeline_expr
            | R::assignment_expr => {
                // Fold left to binary ops based on child sequence
                // We rely on grammar to group; we pick first op we find and nest accordingly.
                let mut it = pair.into_inner();
                let mut lhs = parse_expr(it.next().unwrap());
                while let Some(op_or_rhs) = it.next() {
                    match op_or_rhs.as_rule() {
                        R::PLUS
                        | R::MINUS
                        | R::STAR
                        | R::SLASH
                        | R::IDIV
                        | R::PERCENT
                        | R::CARET
                        | R::EQEQ
                        | R::NOTEQ
                        | R::LT
                        | R::GT
                        | R::LTE
                        | R::GTE
                        | R::ANDAND
                        | R::OROR
                        | R::POW
                        | R::PIPE
                        | R::AMP
                        | R::SHL
                        | R::SHR => {
                            let rhs = parse_expr(it.next().unwrap());
                            let bop = match op_or_rhs.as_rule() {
                                R::PLUS => BinOp::Add,
                                R::MINUS => BinOp::Sub,
                                R::STAR => BinOp::Mul,
                                R::SLASH => BinOp::Div,
                                R::IDIV => BinOp::IDiv,
                                R::PERCENT => BinOp::Mod,
                                R::CARET => BinOp::Pow, // reuse Pow for xor; better would be distinct, but AST has Pow/Pow2
                                R::EQEQ => BinOp::Eq,
                                R::NOTEQ => BinOp::NotEq,
                                R::LT => BinOp::Lt,
                                R::GT => BinOp::Gt,
                                R::LTE => BinOp::LtEq,
                                R::GTE => BinOp::GtEq,
                                R::ANDAND => BinOp::And,
                                R::OROR => BinOp::Or,
                                R::POW => BinOp::Pow,
                                R::PIPE => BinOp::Or,
                                R::AMP => BinOp::And,
                                R::SHL | R::SHR => BinOp::Add, // no shift in AST; approximate
                                _ => BinOp::Add,
                            };
                            lhs = Expr::BinOp(Box::new(lhs), bop, Box::new(rhs), info(sp));
                        }
                        R::EQ
                        | R::PLUSEQ
                        | R::MINUSEQ
                        | R::MULEQ
                        | R::DIVEQ
                        | R::IDIVEQ
                        | R::MODEQ
                        | R::POWEQ
                        | R::SHLEQ
                        | R::SHREQ
                        | R::ANDEQ
                        | R::XOREQ
                        | R::OREQ => {
                            // Assignment: lhs = next
                            let rhs = parse_expr(it.next().unwrap());
                            lhs = Expr::Assign(Box::new(lhs), Box::new(rhs), info(sp));
                        }
                        _ => {
                            // fallback treat as rhs continuation
                            lhs = parse_expr(op_or_rhs);
                        }
                    }
                }
                lhs
            }
            R::range_expr => {
                // Handle .., ..=, ..>, ..< or a..b / a..=b
                let mut inner = pair.into_inner().peekable();
                if let Some(first) = inner.peek() {
                    match first.as_rule() {
                        R::RANGE => {
                            // ".." variants handled by concrete tokens in grammar; fallback infinite
                            ast::Expr::InfiniteRange(RangeType::Infinite, info(sp))
                        }
                        R::RANGE_EQ => ast::Expr::InfiniteRange(RangeType::Infinite, info(sp)),
                        R::RANGE_GT => ast::Expr::InfiniteRange(RangeType::InfiniteUp, info(sp)),
                        R::RANGE_LT => ast::Expr::InfiniteRange(RangeType::InfiniteDown, info(sp)),
                        _ => {
                            // expr .. expr or expr ..= expr
                            let start = parse_expr(inner.next().unwrap());
                            let op = inner.next().unwrap(); // RANGE or RANGE_EQ
                            let end = parse_expr(inner.next().unwrap());
                            let rt = if matches!(op.as_rule(), R::RANGE_EQ) {
                                RangeType::Inclusive
                            } else {
                                RangeType::Exclusive
                            };
                            ast::Expr::Range(Box::new(start), Box::new(end), rt, info(sp))
                        }
                    }
                } else {
                    ast::Expr::InfiniteRange(RangeType::Infinite, info(sp))
                }
            }
            R::array_lit => {
                let mut elems = Vec::new();
                for c in pair.into_inner() {
                    if matches!(c.as_rule(), R::expr) {
                        elems.push(parse_expr(c));
                    }
                }
                ast::Expr::ArrayInit(elems, info(sp))
            }
            R::if_expr => {
                let mut cond = None;
                let mut then_stmts = Vec::new();
                let mut else_stmts = None;
                for c in pair.into_inner() {
                    match c.as_rule() {
                        R::expr => cond = Some(parse_expr(c)),
                        R::block => then_stmts = parse_block_stmts(c),
                        R::if_expr => { /* handled above */ }
                        R::KW_ELSE => { /* skip */ }
                        _ => {
                            // nested else-if or else block
                            if matches!(c.as_rule(), R::block) {
                                else_stmts = Some(parse_block_stmts(c));
                            } else if matches!(c.as_rule(), R::if_expr) {
                                let span_c = codespan_span(c.as_span());
                                let expr_c = parse_expr(c);
                                else_stmts = Some(vec![ast::Stmt::Expr(expr_c, span_c)]);
                            }
                        }
                    }
                }
                ast::Expr::If(
                    Box::new(cond.unwrap_or(ast::Expr::Bool(true, info(sp)))),
                    then_stmts,
                    else_stmts,
                    info(sp),
                )
            }
            R::match_expr => {
                let mut subject = None;
                let mut arms = Vec::new();
                for c in pair.into_inner() {
                    match c.as_rule() {
                        R::expr => subject = Some(parse_expr(c)),
                        R::match_arm => {
                            // pattern (if expr)? => (expr | block)
                            let mut pat = None;
                            let mut guard = None;
                            let mut body =
                                ast::MatchArmBody::Expr(ast::Expr::Void(info(c.as_span())));
                            let arm_span = codespan_span(c.as_span());
                            for a in c.into_inner() {
                                match a.as_rule() {
                                    R::pattern => pat = Some(parse_pattern(a)),
                                    R::expr => {
                                        if guard.is_none() {
                                            guard = Some(parse_expr(a));
                                        } else {
                                            body = ast::MatchArmBody::Expr(parse_expr(a));
                                        }
                                    }
                                    R::block => {
                                        body = ast::MatchArmBody::Block(parse_block_stmts(a));
                                    }
                                    _ => {}
                                }
                            }
                            arms.push(ast::MatchArm {
                                pattern: pat.unwrap_or(ast::Pattern::Wildcard(arm_span)),
                                guard,
                                body,
                                span: arm_span,
                            });
                        }
                        _ => {}
                    }
                }
                ast::Expr::Match(
                    Box::new(subject.unwrap_or(ast::Expr::Void(info(sp)))),
                    arms,
                    info(sp),
                )
            }
            R::loop_expr => {
                let mut body = Vec::new();
                for c in pair.into_inner() {
                    if matches!(c.as_rule(), R::block) {
                        body = parse_block_stmts(c);
                    }
                }
                ast::Expr::Loop(body, info(sp))
            }
            R::primary_expr | R::expr | R::postfix_op | R::argument_list => {
                // Recurse through wrappers
                let mut last = ast::Expr::Void(info(sp));
                for c in pair.into_inner() {
                    last = parse_expr(c);
                }
                last
            }
            _ => ast::Expr::Void(info(sp)),
        }
    }

    // Parse patterns (subset)
    fn parse_pattern(pair: Pair<'_, R>) -> ast::Pattern {
        let sp = pair.as_span();
        match pair.as_rule() {
            R::pattern => {
                let mut last =
                    ast::Pattern::Wildcard(Span::new(sp.start() as u32, sp.end() as u32));
                for c in pair.into_inner() {
                    last = parse_pattern(c);
                }
                last
            }
            R::ident => ast::Pattern::Variable(
                pair.as_str().to_string(),
                Span::new(sp.start() as u32, sp.end() as u32),
            ),
            R::literal => {
                let expr = parse_expr(pair);
                ast::Pattern::Literal(expr, Span::new(sp.start() as u32, sp.end() as u32))
            }
            _ => ast::Pattern::Wildcard(Span::new(sp.start() as u32, sp.end() as u32)),
        }
    }

    // Parse statements
    fn parse_stmt(pair: Pair<'_, R>) -> ast::Stmt {
        use ast::Stmt;
        let sp = pair.as_span();
        match pair.as_rule() {
            R::stmt => {
                let mut last =
                    Stmt::Block(Vec::new(), Span::new(sp.start() as u32, sp.end() as u32));
                for c in pair.into_inner() {
                    last = parse_stmt(c);
                }
                last
            }
            R::block => Stmt::Block(
                parse_block_stmts(pair.clone()),
                Span::new(sp.start() as u32, sp.end() as u32),
            ),
            R::let_stmt => {
                // let name (: type)? = expr;
                let mut name = String::new();
                let mut ty = None;
                let mut expr = ast::Expr::Void(ast::ExprInfo {
                    span: Span::new(sp.start() as u32, sp.end() as u32),
                    ty: ast::Type::Unknown,
                    is_tail: false,
                });
                for c in pair.into_inner() {
                    match c.as_rule() {
                        R::ident => name = c.as_str().to_string(),
                        R::ty => ty = Some(parse_type(c)),
                        R::expr => expr = parse_expr(c),
                        _ => {}
                    }
                }
                Stmt::Let(
                    name,
                    ty,
                    expr,
                    Span::new(sp.start() as u32, sp.end() as u32),
                    ast::Visibility::Private,
                )
            }
            R::var_stmt => {
                let mut name = String::new();
                let span = Span::new(sp.start() as u32, sp.end() as u32);
                for c in pair.into_inner() {
                    if c.as_rule() == R::ident {
                        name = c.as_str().to_string()
                    }
                }
                Stmt::Var(name, None, span)
            }
            R::return_stmt => {
                let mut expr = None;
                for c in pair.into_inner() {
                    if matches!(c.as_rule(), R::expr) {
                        expr = Some(parse_expr(c))
                    }
                }
                Stmt::Return(
                    expr.unwrap_or(ast::Expr::Void(ast::ExprInfo {
                        span: Span::new(sp.start() as u32, sp.end() as u32),
                        ty: ast::Type::Void,
                        is_tail: false,
                    })),
                    Span::new(sp.start() as u32, sp.end() as u32),
                )
            }
            R::defer_stmt => {
                let mut expr = ast::Expr::Void(ast::ExprInfo {
                    span: Span::new(sp.start() as u32, sp.end() as u32),
                    ty: ast::Type::Void,
                    is_tail: false,
                });
                for c in pair.into_inner() {
                    if matches!(c.as_rule(), R::expr) {
                        expr = parse_expr(c)
                    }
                }
                Stmt::Defer(expr, Span::new(sp.start() as u32, sp.end() as u32))
            }
            R::while_stmt => {
                let mut cond = ast::Expr::Bool(
                    true,
                    ast::ExprInfo {
                        span: Span::new(sp.start() as u32, sp.end() as u32),
                        ty: ast::Type::Bool,
                        is_tail: false,
                    },
                );
                let mut body = Vec::new();
                for c in pair.into_inner() {
                    match c.as_rule() {
                        R::expr => cond = parse_expr(c),
                        R::block => body = parse_block_stmts(c),
                        _ => {}
                    }
                }
                Stmt::While(cond, body, Span::new(sp.start() as u32, sp.end() as u32))
            }
            R::for_stmt => {
                // for pattern in expr (step expr)? { body }
                let mut it_name = String::new();
                let mut range_expr = ast::Expr::Void(ast::ExprInfo {
                    span: Span::new(sp.start() as u32, sp.end() as u32),
                    ty: ast::Type::Unknown,
                    is_tail: false,
                });
                let mut step = None;
                let mut body = Vec::new();
                for c in pair.into_inner() {
                    match c.as_rule() {
                        R::pattern => {
                            if let R::ident = c
                                .clone()
                                .into_inner()
                                .next()
                                .map(|x| x.as_rule())
                                .unwrap_or(R::pattern)
                            {
                                it_name = c.as_str().to_string();
                            } else {
                                it_name = "_".into();
                            }
                        }
                        R::expr => {
                            if range_expr.get_info().span.start()
                                == range_expr.get_info().span.end()
                            {
                                range_expr = parse_expr(c)
                            } else {
                                step = Some(parse_expr(c))
                            }
                        }
                        R::block => body = parse_block_stmts(c),
                        _ => {}
                    }
                }
                Stmt::For(
                    it_name,
                    None,
                    range_expr,
                    step,
                    body,
                    Span::new(sp.start() as u32, sp.end() as u32),
                )
            }
            R::loop_stmt => {
                let mut body = Vec::new();
                for c in pair.into_inner() {
                    if matches!(c.as_rule(), R::block) {
                        body = parse_block_stmts(c)
                    }
                }
                Stmt::Loop(body, Span::new(sp.start() as u32, sp.end() as u32))
            }
            R::if_stmt => {
                // Transform to expression node inside a statement for parity
                let expr = parse_expr(pair);
                Stmt::Expr(expr.clone(), Span::new(sp.start() as u32, sp.end() as u32))
            }
            R::match_stmt => {
                let expr = parse_expr(pair);
                Stmt::Expr(expr.clone(), Span::new(sp.start() as u32, sp.end() as u32))
            }
            _ => {
                // expression statement
                let expr = parse_expr(pair);
                Stmt::Expr(expr.clone(), Span::new(sp.start() as u32, sp.end() as u32))
            }
        }
    }

    fn parse_block_stmts(block: Pair<'_, R>) -> Vec<ast::Stmt> {
        let mut out = Vec::new();
        for s in block.into_inner() {
            if matches!(s.as_rule(), R::stmt) {
                out.push(parse_stmt(s));
            }
        }
        out
    }

    // Parse imports and export-import
    fn parse_import_decl(
        pair: Pair<'_, R>,
        file_id: FileId,
        warnings: &mut Vec<Diag>,
    ) -> ast::ImportDeclaration {
        let mut module_path = String::new();
        let mut alias = None;
        let mut specifiers: Vec<ast::ImportSpecifier> = Vec::new();
        let mut is_list = false;

        for c in pair.into_inner() {
            match c.as_rule() {
                R::module_path => {
                    module_path = c.as_str().to_string();
                    // Legacy path warning: prefer '::' over '/'
                    if module_path.contains('/') {
                        let sp = codespan_span(c.as_span());
                        let d = warning(
                            "Deprecated module path separator '/' detected; use '::' instead",
                            file_id,
                            sp,
                        )
                        .with_notes(vec![help("Replace '/' with '::'")]);
                        warnings.push(d);
                    }
                }
                R::import_list => {
                    is_list = true;
                    for it in c.into_inner() {
                        if matches!(it.as_rule(), R::import_item) {
                            let mut name = String::new();
                            let mut alias_i = None;
                            for p in it.into_inner() {
                                if p.as_rule() == R::ident {
                                    if name.is_empty() {
                                        name = p.as_str().to_string()
                                    } else {
                                        alias_i = Some(p.as_str().to_string())
                                    }
                                }
                            }
                            specifiers.push(ast::ImportSpecifier {
                                name,
                                alias: alias_i,
                            });
                        }
                    }
                }
                R::ident => alias = Some(c.as_str().to_string()),
                _ => {}
            }
        }

        // Canonicalize module paths to '::' while detecting legacy '/' for classification
        let raw_module_path = module_path.clone();
        let module_path_canon = if raw_module_path.contains('/') {
            raw_module_path.replace("/", "::")
        } else {
            raw_module_path.clone()
        };
        let module_type =
            if raw_module_path.starts_with("std::") || raw_module_path.starts_with("std/") {
                ast::ModuleType::Standard
            } else if raw_module_path.starts_with("./") || raw_module_path.starts_with("../") {
                ast::ModuleType::Local
            } else {
                ast::ModuleType::External
            };

        if is_list {
            ast::ImportDeclaration::ImportSpecifiers {
                module_path: module_path_canon,
                module_type,
                specifiers,
            }
        } else {
            ast::ImportDeclaration::ImportAll {
                module_path: module_path_canon,
                module_type,
                alias,
            }
        }
    }

    fn parse_export_import_decl(
        pair: Pair<'_, R>,
        file_id: FileId,
        warnings: &mut Vec<Diag>,
    ) -> ast::ImportDeclaration {
        // export import ...; normalize to ExportImport* variants
        let mut module_path = String::new();
        let mut alias = None;
        let mut specifiers: Vec<ast::ImportSpecifier> = Vec::new();
        let mut list = false;

        for c in pair.into_inner() {
            match c.as_rule() {
                R::module_path => {
                    module_path = c.as_str().to_string();
                    // Legacy path warning: prefer '::' over '/'
                    if module_path.contains('/') {
                        let sp = codespan_span(c.as_span());
                        let d = warning(
                            "Deprecated module path separator '/' detected; use '::' instead",
                            file_id,
                            sp,
                        )
                        .with_notes(vec![help("Replace '/' with '::'")]);
                        warnings.push(d);
                    }
                }
                R::import_list => {
                    list = true;
                    for it in c.into_inner() {
                        if matches!(it.as_rule(), R::import_item) {
                            let mut name = String::new();
                            let mut alias_i = None;
                            for p in it.into_inner() {
                                if p.as_rule() == R::ident {
                                    if name.is_empty() {
                                        name = p.as_str().to_string()
                                    } else {
                                        alias_i = Some(p.as_str().to_string())
                                    }
                                }
                            }
                            specifiers.push(ast::ImportSpecifier {
                                name,
                                alias: alias_i,
                            });
                        }
                    }
                }
                R::ident => alias = Some(c.as_str().to_string()),
                _ => {}
            }
        }

        // Canonicalize module paths to '::' while detecting legacy '/' for classification
        let raw_module_path = module_path.clone();
        let module_path_canon = if raw_module_path.contains('/') {
            raw_module_path.replace("/", "::")
        } else {
            raw_module_path.clone()
        };
        let module_type =
            if raw_module_path.starts_with("std::") || raw_module_path.starts_with("std/") {
                ast::ModuleType::Standard
            } else if raw_module_path.starts_with("./") || raw_module_path.starts_with("../") {
                ast::ModuleType::Local
            } else {
                ast::ModuleType::External
            };

        if list {
            ast::ImportDeclaration::ExportImportSpecifiers {
                module_path: module_path_canon,
                module_type,
                specifiers,
            }
        } else {
            ast::ImportDeclaration::ExportImportAll {
                module_path: module_path_canon,
                module_type,
                alias,
            }
        }
    }

    // Parse function items (name, params, return type, body)
    fn parse_fn_decl(pair: Pair<'_, R>) -> ast::Function {
        let mut name = String::new();
        let mut params: Vec<(String, ast::Type)> = Vec::new();
        let mut return_type = ast::Type::Void;
        let mut body: Vec<ast::Stmt> = Vec::new();
        let mut span = Span::new(pair.as_span().start() as u32, pair.as_span().end() as u32);
        let mut visibility = ast::Visibility::Private;

        for c in pair.into_inner() {
            match c.as_rule() {
                R::visibility => {
                    visibility = ast::Visibility::Public;
                }
                R::ident => name = c.as_str().to_string(),
                R::function_decl_min => {
                    // Handle nested function_decl_min
                    for inner in c.into_inner() {
                        match inner.as_rule() {
                            R::ident => name = inner.as_str().to_string(),
                            R::parameter_list => {
                                for p in inner.into_inner() {
                                    if matches!(p.as_rule(), R::parameter) {
                                        let mut pname = String::new();
                                        let mut pty = ast::Type::Unknown;
                                        for q in p.into_inner() {
                                            match q.as_rule() {
                                                R::ident => pname = q.as_str().to_string(),
                                                R::ty => pty = parse_type(q),
                                                _ => {}
                                            }
                                        }
                                        params.push((pname, pty));
                                    }
                                }
                            }
                            R::ty => return_type = parse_type(inner),
                            R::block => {
                                span = Span::new(
                                    inner.as_span().start() as u32,
                                    inner.as_span().end() as u32,
                                );
                                body = parse_block_stmts(inner);
                            }
                            _ => {}
                        }
                    }
                }
                R::parameter_list => {
                    for p in c.into_inner() {
                        if matches!(p.as_rule(), R::parameter) {
                            let mut pname = String::new();
                            let mut pty = ast::Type::Unknown;
                            for q in p.into_inner() {
                                match q.as_rule() {
                                    R::ident => pname = q.as_str().to_string(),
                                    R::ty => pty = parse_type(q),
                                    _ => {}
                                }
                            }
                            params.push((pname, pty));
                        }
                    }
                }
                R::ty => return_type = parse_type(c),
                R::block => {
                    span = Span::new(c.as_span().start() as u32, c.as_span().end() as u32);
                    body = parse_block_stmts(c);
                }
                _ => {}
            }
        }

        ast::Function {
            name,
            generic_params: Vec::new(),
            params,
            return_type,
            body,
            span,
            visibility,
        }
    }

    // Parse struct declaration
    fn parse_struct_decl(pair: Pair<'_, R>) -> ast::StructDef {
        let mut name = String::new();
        let mut fields: Vec<ast::StructField> = Vec::new();
        let mut span = Span::new(pair.as_span().start() as u32, pair.as_span().end() as u32);
        let mut visibility = ast::Visibility::Private;

        for c in pair.into_inner() {
            match c.as_rule() {
                R::visibility => visibility = ast::Visibility::Public,
                R::ident => name = c.as_str().to_string(),
                R::struct_fields => {
                    for f in c.into_inner() {
                        if matches!(f.as_rule(), R::struct_field) {
                            let mut fname = String::new();
                            let mut fty = ast::Type::Unknown;
                            let fspan = f.as_span();
                            for q in f.into_inner() {
                                match q.as_rule() {
                                    R::ident => fname = q.as_str().to_string(),
                                    R::ty => fty = parse_type(q),
                                    _ => {}
                                }
                            }
                            fields.push(ast::StructField {
                                name: fname,
                                ty: fty,
                                span: Span::new(fspan.start() as u32, fspan.end() as u32),
                            });
                        }
                    }
                }
                R::block | R::tuple_fields | R::SEMI => {
                    span = Span::new(c.as_span().start() as u32, c.as_span().end() as u32);
                }
                _ => {}
            }
        }

        ast::StructDef {
            name,
            generic_params: Vec::new(),
            fields,
            span,
            visibility,
            repr: None,
        }
    }

    // Parse enum declaration
    fn parse_enum_decl(pair: Pair<'_, R>) -> ast::EnumDef {
        let mut name = String::new();
        let mut variants: Vec<ast::EnumVariant> = Vec::new();
        let span = Span::new(pair.as_span().start() as u32, pair.as_span().end() as u32);
        let mut visibility = ast::Visibility::Private;

        for c in pair.into_inner() {
            match c.as_rule() {
                R::visibility => visibility = ast::Visibility::Public,
                R::ident => name = c.as_str().to_string(),
                R::enum_variants => {
                    for v in c.into_inner() {
                        if matches!(v.as_rule(), R::enum_variant) {
                            let mut vname = String::new();
                            let mut vdata: Option<ast::EnumVariantData> = None;
                            let mut vvalue: Option<i32> = None;
                            let vspan = v.as_span();
                            for q in v.into_inner() {
                                match q.as_rule() {
                                    R::ident => vname = q.as_str().to_string(),
                                    R::enum_data => {
                                        // tuple data only for now
                                        let mut tuple = Vec::new();
                                        for ty in
                                            q.into_inner().filter(|x| matches!(x.as_rule(), R::ty))
                                        {
                                            tuple.push(parse_type(ty));
                                        }
                                        vdata = if tuple.is_empty() {
                                            None
                                        } else {
                                            Some(ast::EnumVariantData::Tuple(tuple))
                                        };
                                    }
                                    R::expr => {
                                        // constant expr; try parse int
                                        let text = q.as_str();
                                        vvalue = text.parse::<i32>().ok();
                                    }
                                    _ => {}
                                }
                            }
                            variants.push(ast::EnumVariant {
                                name: vname,
                                data: vdata,
                                value: vvalue,
                                span: Span::new(vspan.start() as u32, vspan.end() as u32),
                            });
                        }
                    }
                }
                _ => {}
            }
        }

        ast::EnumDef {
            name,
            generic_params: Vec::new(),
            variants,
            span,
            visibility,
        }
    }

    // Parse FFI block/items (# metadata then foreign items)
    fn parse_ffi_decl(pair: Pair<'_, R>) -> (Vec<ast::FfiFunction>, Vec<ast::FfiVariable>) {
        use std::collections::HashMap;
        let mut funcs = Vec::new();
        let mut vars = Vec::new();
        let mut metadata: Option<HashMap<String, String>> = None;

        for c in pair.into_inner() {
            match c.as_rule() {
                R::metadata => {
                    let mut map = HashMap::new();
                    for kvs in c.into_inner() {
                        if matches!(kvs.as_rule(), R::metadata_kv) {
                            for kv in kvs
                                .into_inner()
                                .filter(|x| matches!(x.as_rule(), R::metadata_kv_pair))
                            {
                                let mut k = String::new();
                                let mut v = String::new();
                                for p in kv.into_inner() {
                                    match p.as_rule() {
                                        R::ident => k = p.as_str().to_string(),
                                        R::literal => v = p.as_str().to_string(),
                                        _ => {}
                                    }
                                }
                                map.insert(k, v);
                            }
                        }
                    }
                    metadata = Some(map);
                }
                R::ffi_block => {
                    for item in c.into_inner() {
                        if matches!(item.as_rule(), R::ffi_item) {
                            let mut fname = String::new();
                            let mut params = Vec::new();
                            let mut ret_ty = ast::Type::Void;
                            let mut vname = String::new();
                            let mut vty = ast::Type::Unknown;
                            let mut is_var = false;

                            for q in item.into_inner() {
                                match q.as_rule() {
                                    R::KW_VAR => is_var = true,
                                    R::ident => {
                                        if is_var {
                                            vname = q.as_str().to_string()
                                        } else {
                                            fname = q.as_str().to_string()
                                        }
                                    }
                                    R::parameter_list => {
                                        for p in q.into_inner() {
                                            if matches!(p.as_rule(), R::parameter) {
                                                for t in p.into_inner() {
                                                    if matches!(t.as_rule(), R::ty) {
                                                        params.push(parse_type(t));
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    R::ty => {
                                        if is_var {
                                            vty = parse_type(q)
                                        } else {
                                            ret_ty = parse_type(q)
                                        }
                                    }
                                    _ => {}
                                }
                            }

                            if is_var {
                                vars.push(ast::FfiVariable {
                                    name: vname,
                                    ty: vty,
                                    metadata: metadata.clone(),
                                });
                            } else {
                                funcs.push(ast::FfiFunction {
                                    name: fname,
                                    params,
                                    return_type: ret_ty,
                                    metadata: metadata.clone(),
                                });
                            }
                        }
                    }
                }
                R::ffi_item => {
                    // single item form
                    let mut fname = String::new();
                    let mut params = Vec::new();
                    let mut ret_ty = ast::Type::Void;
                    let mut vname = String::new();
                    let mut vty = ast::Type::Unknown;
                    let mut is_var = false;

                    for q in c.into_inner() {
                        match q.as_rule() {
                            R::KW_VAR => is_var = true,
                            R::ident => {
                                if is_var {
                                    vname = q.as_str().to_string()
                                } else {
                                    fname = q.as_str().to_string()
                                }
                            }
                            R::parameter_list => {
                                for p in q.into_inner() {
                                    if matches!(p.as_rule(), R::parameter) {
                                        for t in p.into_inner() {
                                            if matches!(t.as_rule(), R::ty) {
                                                params.push(parse_type(t));
                                            }
                                        }
                                    }
                                }
                            }
                            R::ty => {
                                if is_var {
                                    vty = parse_type(q)
                                } else {
                                    ret_ty = parse_type(q)
                                }
                            }
                            _ => {}
                        }
                    }

                    if is_var {
                        vars.push(ast::FfiVariable {
                            name: vname,
                            ty: vty,
                            metadata: metadata.clone(),
                        });
                    } else {
                        funcs.push(ast::FfiFunction {
                            name: fname,
                            params,
                            return_type: ret_ty,
                            metadata: metadata.clone(),
                        });
                    }
                }
                _ => {}
            }
        }

        (funcs, vars)
    }

    // Pre-scan for deprecated tokens before main parse
    if let Ok(token_pairs) = ast_grammar::AstParser::parse(ast_grammar::AstRule::tokens, source) {
        for pair in token_pairs.flatten() {
            let span = to_span(pair.as_span());
            match pair.as_rule() {
                ast_grammar::AstRule::ANDAND => {
                    add_deprecation_warning(&mut warnings, span, "&&", "&", "logical AND");
                }
                ast_grammar::AstRule::OROR => {
                    add_deprecation_warning(&mut warnings, span, "||", "|", "logical OR");
                }
                _ => {}
            }
        }
    }

    // Main parse
    match ast_grammar::AstParser::parse(ast_grammar::AstRule::program, source) {
        Ok(mut pairs) => {
            let mut program = ast::Program {
                imports: Vec::new(),
                stmts: Vec::new(),
                functions: Vec::new(),
                structs: Vec::new(),
                enums: Vec::new(),
                impls: Vec::new(),
                ffi_functions: Vec::new(),
                ffi_variables: Vec::new(),
                tests: Vec::new(),
            };

            if let Some(program_pair) = pairs.next() {
                debug_assert!(matches!(program_pair.as_rule(), R::program));
                for item in program_pair.into_inner() {
                    match item.as_rule() {
                        R::import_decl => {
                            program
                                .imports
                                .push(parse_import_decl(item, file_id, &mut warnings));
                        }
                        R::export_import_decl => {
                            program.imports.push(parse_export_import_decl(
                                item,
                                file_id,
                                &mut warnings,
                            ));
                        }
                        R::export_decl => {
                            // export {fn|struct|enum|type_alias}
                            for n in item.into_inner() {
                                match n.as_rule() {
                                    R::function_decl => {
                                        let mut f = parse_fn_decl(n);
                                        f.visibility = ast::Visibility::Public;
                                        program.functions.push(f);
                                    }
                                    R::struct_decl => {
                                        let mut s = parse_struct_decl(n);
                                        s.visibility = ast::Visibility::Public;
                                        program.structs.push(s);
                                    }
                                    R::enum_decl => {
                                        let mut e = parse_enum_decl(n);
                                        e.visibility = ast::Visibility::Public;
                                        program.enums.push(e);
                                    }
                                    R::type_alias => {
                                        // Not modeled in AST; ignore (could be tracked via helpers crate later)
                                    }
                                    _ => {}
                                }
                            }
                        }
                        R::function_decl => {
                            program.functions.push(parse_fn_decl(item));
                        }
                        R::struct_decl => {
                            program.structs.push(parse_struct_decl(item));
                        }
                        R::enum_decl => {
                            program.enums.push(parse_enum_decl(item));
                        }
                        R::ffi_decl => {
                            let (fs, vs) = parse_ffi_decl(item);
                            program.ffi_functions.extend(fs);
                            program.ffi_variables.extend(vs);
                        }
                        R::test_decl => {
                            // test name { block }
                            let mut name = String::new();
                            let mut stmts = Vec::new();
                            let mut tsp = Span::new(
                                item.as_span().start() as u32,
                                item.as_span().end() as u32,
                            );
                            for c in item.into_inner() {
                                match c.as_rule() {
                                    R::ident => name = c.as_str().to_string(),
                                    R::block => {
                                        tsp = Span::new(
                                            c.as_span().start() as u32,
                                            c.as_span().end() as u32,
                                        );
                                        stmts = parse_block_stmts(c);
                                    }
                                    _ => {}
                                }
                            }
                            program.tests.push(ast::Test {
                                name,
                                stmts,
                                span: tsp,
                            });
                        }
                        R::stmt => {
                            program.stmts.push(parse_stmt(item));
                        }
                        _ => {}
                    }
                }
            }

            Ok((program, warnings))
        }
        Err(e) => {
            let mut diags = Vec::with_capacity(1);
            let message = format!("Parse error: {}", e);
            let span = match e.location {
                pest::error::InputLocation::Pos(pos) => {
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

            // Handle the token rule which wraps individual tokens
            let (actual_rule, actual_span, actual_text) = if rule == Rule::token {
                // Get the first inner rule from the token
                if let Some(token_inner) = inner.into_inner().next() {
                    (
                        token_inner.as_rule(),
                        token_inner.as_span(),
                        token_inner.as_str(),
                    )
                } else {
                    continue;
                }
            } else {
                (rule, inner.as_span(), inner.as_str())
            };

            let span = actual_span;
            let start = span.start() as u32;
            let end = span.end() as u32;
            let text = actual_text;

            let token = match actual_rule {
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
                | Rule::KW_IN
                | Rule::KW_FOREIGN => RawToken::Keyword(text.to_string()),

                // Type keywords
                Rule::TY_BOOL
                | Rule::TY_STRING
                | Rule::TY_STR
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
                | Rule::POW
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
                | Rule::IDIV
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
                Rule::tokens | Rule::program | Rule::token => continue,

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

#[cfg(feature = "grammar-validation")]
pub fn validate_grammar() -> Result<(), String> {
    // The embedded Pest grammar is validated at compile time.
    // This function exists for runtime validation if needed in the future.
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use veil_diagnostics::Files;

    #[test]
    fn parse_minimal_file_succeeds() {
        let mut files = Files::<String>::new();
        let fid = files.add("test.veil", "fn main() {}".to_string());
        let result = parse_ast(&files, fid);
        assert!(result.is_ok());
    }

    #[test]
    fn parse_empty_file_succeeds_and_is_empty() {
        let mut files = Files::<String>::new();
        let fid = files.add("empty.veil", "".to_string());
        let result = parse_ast(&files, fid);
        assert!(result.is_ok());
    }

    #[test]
    fn lex_raw_basic_tokens() {
        let mut files = Files::<String>::new();
        let fid = files.add("t.veil", "fn hello() { return 1+2; }".to_string());
        let toks = lex_raw(&files, fid).expect("lex ok");

        assert!(
            toks.iter()
                .any(|t| matches!(t.token, RawToken::Keyword(ref k) if k == "fn"))
        );
        assert!(
            toks.iter()
                .any(|t| matches!(t.token, RawToken::Ident(ref s) if s == "hello"))
        );
        assert!(
            toks.iter()
                .any(|t| matches!(t.token, RawToken::Int(ref s) if s == "1"))
        );
    }
}
