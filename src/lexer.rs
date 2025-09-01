use codespan::{FileId, Files, Span};
use logos::Logos;
use veil_syntax::{RawToken, lex_raw};

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    #[token("fn")]
    KwFn,

    #[token("test")]
    KwTest,

    #[token("let")]
    KwLet,
    #[token("var")]
    KwVar,
    #[token("if")]
    KwIf,
    #[token("else")]
    KwElse,
    #[token("return")]
    KwReturn,
    #[token("rawptr")]
    KwRawPtr,
    #[token("defer")]
    KwDefer,
    #[token("safe")]
    KwSafe,
    #[token("as")]
    KwAs,
    #[token("while")]
    KwWhile,
    #[token("for")]
    KwFor,
    #[token("step")]
    KwStep,
    #[token("loop")]
    KwLoop,
    #[token("import")]
    KwImport,
    #[token("from")]
    KwFrom,
    #[token("export")]
    KwExport,
    #[token("struct")]
    KwStruct,
    #[token("impl")]
    KwImpl,
    #[token("new")]
    KwNew,
    #[token("constructor")]
    KwConstructor,
    #[token("enum")]
    KwEnum,
    #[token("match")]
    KwMatch,
    #[token("true")]
    KwTrue,
    #[token("false")]
    KwFalse,
    #[token("None")]
    #[token("none")]
    KwNone,
    #[token(".")]
    Dot,
    #[token("...")]
    Ellipsis,
    #[token("foreign")]
    Foreign,
    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice()[1..lex.slice().len()-1].to_string())]
    Str(String),
    #[regex(r#"`([^`\\]|\\.)*`"#, |lex| lex.slice()[1..lex.slice().len()-1].to_string())]
    TemplateStr(String),

    #[token("bool")]
    TyBool,
    #[token("string")]
    TyString,
    #[token("void")]
    TyVoid,
    #[token("any")]
    TyAny,

    #[token("byte")]
    #[token("u8")]
    TyU8,

    #[token("ushort")]
    #[token("u16")]
    TyU16,

    #[token("uint")]
    #[token("u32")]
    TyU32,

    #[token("ulong")]
    #[token("u64")]
    TyU64,

    #[token("sbyte")]
    #[token("i8")]
    TyI8,

    #[token("short")]
    #[token("i16")]
    TyI16,

    #[token("int")]
    #[token("i32")]
    TyI32,

    #[token("long")]
    #[token("i64")]
    TyI64,

    #[token("float")]
    #[token("f32")]
    TyF32,

    #[token("double")]
    #[token("f64")]
    TyF64,

    #[token("->")]
    Arrow,
    #[token(":")]
    Colon,
    #[token("==")]
    EqEq,
    #[token("!=")]
    NotEq,
    #[token(">=")]
    GtEq,
    #[token("<=")]
    LtEq,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("[]")]
    EmptyArray,
    #[token(",")]
    Comma,
    #[token("=")]
    Eq,
    #[token(";")]
    Semi,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("**")]
    DoubleStar,
    #[token("^")]
    Caret,
    #[token("%")]
    Percent,
    #[token(">")]
    Gt,
    #[token("<")]
    Lt,
    #[token("in")]
    KwIn,
    #[token("..=")]
    DotDotEq,
    #[token("..>")]
    DotDotGt,
    #[token("..<")]
    DotDotLt,
    #[token("..")]
    DotDot,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    OrOr,
    #[token("|")]
    Pipe,
    #[token("=>")]
    Arrow2,
    #[token("#")]
    Hash,
    #[token("!")]
    Bang,
    #[token("?")]
    Question,

    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse().ok())]
    F64(f64),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),

    #[regex(r"[ \t\n]+", logos::skip)]
    Whitespace,

    #[regex(r"//[^\n]*", logos::skip)]
    SingleLineComment,

    #[regex(r"/\*[^*]*\*+(?:[^/*][^*]*\*+)*/", logos::skip)]
    MultiLineComment,

    #[regex(r"0x[0-9a-fA-F]+", |lex| {
        let s = lex.slice();
        match i64::from_str_radix(&s[2..], 16) {
            Ok(n) => n.to_string(),
            Err(_) => "0".to_string()
        }
    })]
    #[regex(r"0b[01]+", |lex| {
        let s = lex.slice();
        match i64::from_str_radix(&s[2..], 2) {
            Ok(n) => n.to_string(),
            Err(_) => "0".to_string()
        }
    })]
    #[regex(r"[0-9]+", |lex| lex.slice().to_string())]
    Int(String),

    #[token("break")]
    KwBreak,
    #[token("continue")]
    KwContinue,
}

pub struct Lexer<'a> {
    pub(crate) files: &'a Files<String>,
    pub(crate) file_id: FileId,
}

impl<'a> Lexer<'a> {
    pub fn new(files: &'a Files<String>, file_id: FileId) -> Self {
        Self { files, file_id }
    }

    pub fn tokens(&self) -> Vec<(Token, Span)> {
        let mut out: Vec<(Token, Span)> = Vec::new();

        match lex_raw(self.files, self.file_id) {
            Ok(raws) => {
                for r in raws {
                    let mapped: Option<Token> = match &r.token {
                        RawToken::Ident(s) => {
                            // Map reserved words not explicitly tokenized by grammar
                            match s.as_str() {
                                "break" => Some(Token::KwBreak),
                                "continue" => Some(Token::KwContinue),
                                _ => Some(Token::Ident(s.clone())),
                            }
                        }
                        RawToken::Int(s) => {
                            // Normalize hex/bin to decimal string like the previous logos lexer
                            if let Some(rest) = s.strip_prefix("0x") {
                                match i64::from_str_radix(rest, 16) {
                                    Ok(n) => Some(Token::Int(n.to_string())),
                                    Err(_) => Some(Token::Int("0".to_string())),
                                }
                            } else if let Some(rest) = s.strip_prefix("0b") {
                                match i64::from_str_radix(rest, 2) {
                                    Ok(n) => Some(Token::Int(n.to_string())),
                                    Err(_) => Some(Token::Int("0".to_string())),
                                }
                            } else {
                                Some(Token::Int(s.clone()))
                            }
                        }
                        RawToken::Float(s) => match s.parse::<f64>() {
                            Ok(v) => Some(Token::F64(v)),
                            Err(_) => None,
                        },
                        RawToken::Str(s) => {
                            // Strip surrounding quotes
                            if s.len() >= 2 {
                                Some(Token::Str(s[1..s.len().saturating_sub(1)].to_string()))
                            } else {
                                Some(Token::Str(String::new()))
                            }
                        }
                        RawToken::TemplateStr(s) => {
                            // Strip surrounding backticks
                            if s.len() >= 2 {
                                Some(Token::TemplateStr(
                                    s[1..s.len().saturating_sub(1)].to_string(),
                                ))
                            } else {
                                Some(Token::TemplateStr(String::new()))
                            }
                        }
                        RawToken::Keyword(k) => {
                            let t = match k.as_str() {
                                "fn" => Token::KwFn,
                                "test" => Token::KwTest,
                                "let" => Token::KwLet,
                                "var" => Token::KwVar,
                                "if" => Token::KwIf,
                                "else" => Token::KwElse,
                                "return" => Token::KwReturn,
                                "rawptr" => Token::KwRawPtr,
                                "defer" => Token::KwDefer,
                                "safe" => Token::KwSafe,
                                "as" => Token::KwAs,
                                "while" => Token::KwWhile,
                                "for" => Token::KwFor,
                                "step" => Token::KwStep,
                                "loop" => Token::KwLoop,
                                "import" => Token::KwImport,
                                "from" => Token::KwFrom,
                                "export" => Token::KwExport,
                                "struct" => Token::KwStruct,
                                "impl" => Token::KwImpl,
                                "new" => Token::KwNew,
                                "constructor" => Token::KwConstructor,
                                "enum" => Token::KwEnum,
                                "match" => Token::KwMatch,
                                "true" => Token::KwTrue,
                                "false" => Token::KwFalse,
                                "None" | "none" => Token::KwNone,
                                "in" => Token::KwIn,
                                "foreign" => Token::Foreign,
                                // Fallback to identifier if something slipped through
                                other => Token::Ident(other.to_string()),
                            };
                            Some(t)
                        }
                        RawToken::TypeKeyword(tk) => {
                            let t = match tk.as_str() {
                                "bool" => Token::TyBool,
                                "string" => Token::TyString,
                                "void" => Token::TyVoid,
                                "any" => Token::TyAny,
                                "byte" | "u8" => Token::TyU8,
                                "ushort" | "u16" => Token::TyU16,
                                "uint" | "u32" => Token::TyU32,
                                "ulong" | "u64" => Token::TyU64,
                                "sbyte" | "i8" => Token::TyI8,
                                "short" | "i16" => Token::TyI16,
                                "int" | "i32" => Token::TyI32,
                                "long" | "i64" => Token::TyI64,
                                "float" | "f32" => Token::TyF32,
                                "double" | "f64" => Token::TyF64,
                                other => Token::Ident(other.to_string()),
                            };
                            Some(t)
                        }
                        RawToken::Symbol(sym) => {
                            let t = match sym.as_str() {
                                "..." => Token::Ellipsis,
                                "**" => Token::DoubleStar,
                                "->" => Token::Arrow,
                                "=>" => Token::Arrow2,
                                "==" => Token::EqEq,
                                "!=" => Token::NotEq,
                                ">=" => Token::GtEq,
                                "<=" => Token::LtEq,
                                "..=" => Token::DotDotEq,
                                "..>" => Token::DotDotGt,
                                "..<" => Token::DotDotLt,
                                ".." => Token::DotDot,
                                "." => Token::Dot,
                                "(" => Token::LParen,
                                ")" => Token::RParen,
                                "{" => Token::LBrace,
                                "}" => Token::RBrace,
                                "[" => Token::LBracket,
                                "]" => Token::RBracket,
                                "[]" => Token::EmptyArray,
                                "," => Token::Comma,
                                "=" => Token::Eq,
                                ";" => Token::Semi,
                                "+" => Token::Plus,
                                "-" => Token::Minus,
                                "*" => Token::Star,
                                "/" => Token::Slash,
                                "^" => Token::Caret,
                                "%" => Token::Percent,
                                ">" => Token::Gt,
                                "<" => Token::Lt,
                                "|" => Token::Pipe,
                                "#" => Token::Hash,
                                "!" => Token::Bang,
                                "?" => Token::Question,
                                ":" => Token::Colon,
                                _ => {
                                    // Unknown punctuation: drop it to mimic previous logos behavior on invalid tokens
                                    // (logos would return None on invalid matches)
                                    // You can add more mappings here as grammar expands.
                                    continue;
                                }
                            };
                            Some(t)
                        }
                    };

                    if let Some(tok) = mapped {
                        out.push((tok, r.span));
                    }
                }
            }
            Err(_diags) => {
                // On lexing error, return empty token stream to trigger parser diagnostics later.
            }
        }

        out
    }
}
