use crate::ast;
use crate::lexer::Token;
use crate::parser::Parser;
use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic;

impl<'a> Parser<'a> {
    pub fn parse_break(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        let break_span = self.peek_span();
        self.expect(Token::KwBreak)?;

        let expr = if self.check(Token::Semi) || self.check(Token::RBrace) {
            None
        } else {
            Some(self.parse_expr()?)
        };

        if self.check(Token::Semi) {
            self.advance();
        }

        Ok(ast::Stmt::Break(expr, break_span))
    }
    pub fn parse_continue(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        let continue_span = self.peek_span();
        self.expect(Token::KwContinue)?;

        if self.check(Token::Semi) {
            self.advance();
        }

        Ok(ast::Stmt::Continue(continue_span))
    }
}
