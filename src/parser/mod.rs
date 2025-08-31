mod decl;
mod expr;
mod ffi;
mod import_export;
mod precedence;
mod stmt;
mod test;
mod types;

use super::{
    ast,
    lexer::{Lexer, Token},
};
use codespan::{FileId, Files, Span};
use codespan_reporting::diagnostic::Diagnostic;

use crate::parser::ffi::ForeignItem;
pub(super) use precedence::Precedence;

pub struct Parser<'a> {
    tokens: Vec<(Token, Span)>,
    pos: usize,
    #[allow(dead_code)]
    files: &'a Files<String>,
    file_id: FileId,
    previous_token: Option<(Token, Span)>,
    enums: Vec<ast::EnumDef>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            tokens: lexer.tokens(),
            pos: 0,
            files: lexer.files,
            file_id: lexer.file_id,
            previous_token: None,
            enums: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> Result<ast::Program, Diagnostic<FileId>> {
        let mut program = ast::Program {
            imports: Vec::new(),
            stmts: Vec::new(),
            functions: Vec::new(),
            structs: Vec::new(),
            enums: Vec::new(),
            ffi_functions: Vec::new(),
            ffi_variables: Vec::new(),
            tests: Vec::new(),
            impls: Vec::new(),
        };

        while !self.is_at_end() {
            if self.check(Token::KwImport) {
                let import = self.parse_import()?;
                program.imports.push(import);
            } else if self.check(Token::KwExport) {
                self.advance();
                if self.check(Token::KwImport) {
                    let import = self.parse_export_import()?;
                    program.imports.push(import);
                } else if self.check(Token::KwStruct) {
                    let mut struct_def = self.parse_struct()?;
                    struct_def.visibility = ast::Visibility::Public;
                    program.structs.push(struct_def);
                } else if self.check(Token::KwEnum) {
                    let mut enum_def = self.parse_enum()?;
                    enum_def.visibility = ast::Visibility::Public;
                    self.enums.push(enum_def.clone());
                    program.enums.push(enum_def);
                } else if self.check(Token::KwFn) {
                    let mut func = self.parse_function()?;
                    func.visibility = ast::Visibility::Public;
                    program.functions.push(func);
                } else if self.check(Token::LBrace) {
                    self.parse_export_block(&mut program)?;
                } else {
                    let span = self.peek_span();
                    return self.error(
                        "Expected 'fn', 'struct', 'import', or '{' after 'export'",
                        span,
                    );
                }
            } else if self.check(Token::KwFn) {
                program.functions.push(self.parse_function()?);
            } else if self.check(Token::KwStruct) {
                program.structs.push(self.parse_struct()?);
            } else if self.check(Token::KwEnum) {
                let enum_def = self.parse_enum()?;
                self.enums.push(enum_def.clone());
                program.enums.push(enum_def);
            } else if self.check(Token::KwImpl) {
                program.impls.push(self.parse_impl()?);
            } else if self.check(Token::Hash) {
                self.advance();
                let metadata = self.parse_metadata()?;
                self.expect(Token::Foreign)?;
                match self.parse_ffi(metadata)? {
                    ForeignItem::Function(f) => program.ffi_functions.push(f),
                    ForeignItem::Variable(v) => program.ffi_variables.push(v),
                }
            } else if self.check(Token::Foreign) {
                self.advance();
                match self.parse_ffi(None)? {
                    ForeignItem::Function(f) => program.ffi_functions.push(f),
                    ForeignItem::Variable(v) => program.ffi_variables.push(v),
                }
            } else if self.check(Token::KwTest) {
                program.tests.push(self.parse_test()?);
            } else {
                program.stmts.push(self.parse_stmt()?);
            }
        }

        Ok(program)
    }

    fn peek(&self) -> Option<&(Token, Span)> {
        self.tokens.get(self.pos)
    }
    fn advance(&mut self) -> Option<&(Token, Span)> {
        let t = self.tokens.get(self.pos);
        if let Some(ts) = t {
            self.previous_token = Some(ts.clone());
            self.pos += 1;
        }
        t
    }
    fn is_at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn peek_token(&self) -> Token {
        self.peek().map(|(t, _)| t.clone()).unwrap()
    }

    fn peek_span(&self) -> Span {
        self.peek().map(|(_, s)| *s).unwrap_or(Span::new(0, 0))
    }

    fn consume(&mut self, expected: Token, err_msg: &str) -> Result<Span, Diagnostic<FileId>> {
        if self.check(expected.clone()) {
            let span = self.peek().map(|(_, s)| *s).unwrap();
            self.advance();
            Ok(span)
        } else {
            let span = self.peek().map(|(_, s)| *s).unwrap_or(Span::new(0, 0));
            self.error(err_msg, span)
        }
    }
    fn consume_ident(&mut self) -> Result<(String, Span), Diagnostic<FileId>> {
        let token = self.advance().cloned();
        match token.as_ref() {
            Some((Token::Ident(name), span)) => Ok((name.clone(), *span)),
            Some((_, span)) => self.error("Expected identifier", *span),
            None => {
                if let Some((_, prev_span)) = self.previous() {
                    use codespan::ByteIndex;
                    let end_pos = prev_span.end().0;
                    let error_span = Span::new(prev_span.end(), ByteIndex::from(end_pos + 1));
                    self.error("Expected identifier", error_span)
                } else {
                    self.error("Expected identifier", Span::new(0, 0))
                }
            }
        }
    }

    fn check(&self, token: Token) -> bool {
        matches!(self.peek(), Some((t, _)) if *t == token)
    }

    fn previous(&self) -> Option<&(Token, Span)> {
        self.previous_token.as_ref()
    }

    fn expect(&mut self, token: Token) -> Result<Span, Diagnostic<FileId>> {
        if self.check(token.clone()) {
            let span = self.peek().map(|(_, s)| *s).unwrap();
            self.advance();
            Ok(span)
        } else {
            if matches!(token, Token::Semi)
                && let Some((_, prev_span)) = self.previous()
            {
                use codespan::ByteIndex;
                let end_pos = prev_span.end().0;
                let error_span = Span::new(prev_span.end(), ByteIndex::from(end_pos + 1));
                return self.error(&format!("Expected {:?}", token), error_span);
            }
            let span = self.peek().map(|(_, s)| *s).unwrap_or(Span::new(0, 0));
            self.error(&format!("Expected {:?}", token), span)
        }
    }

    fn error<T>(&self, message: &str, span: Span) -> Result<T, Diagnostic<FileId>> {
        Err(Diagnostic::error().with_message(message).with_labels(vec![
            codespan_reporting::diagnostic::Label::primary(self.file_id, span),
        ]))
    }
}
