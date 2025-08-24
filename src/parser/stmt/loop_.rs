use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;
use crate::ast;
use crate::lexer::Token;
use crate::parser::Parser;

impl<'a> Parser<'a> {
    pub fn parse_loop(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        self.expect(Token::KwLoop)?;
        let loop_span = self.previous().map(|(_, s)| *s).unwrap();

        let body = self.parse_block()?;

        Ok(ast::Stmt::Loop(
            body,
            Span::new(loop_span.start(), self.previous().unwrap().1.end()),
        ))
    }
}