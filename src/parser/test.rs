use crate::ast;
use crate::lexer::Token;
use crate::parser::Parser;
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;

impl<'a> Parser<'a> {
    pub fn parse_test(&mut self) -> Result<ast::Test, Diagnostic<FileId>> {
        self.consume(Token::KwTest, "Expected 'test'")?;
        let start_span = self.previous().map(|(_, s)| *s).unwrap();

        let (name, _name_span) = self.consume_ident()?;

        let body = self.parse_block_with_tail(false)?;
        let end_span = match body.last() {
            Some(last_stmt) => last_stmt.span(),
            None => start_span,
        };

        Ok(ast::Test {
            name,
            stmts: body,
            span: Span::new(start_span.start(), end_span.end()),
        })
    }
}
