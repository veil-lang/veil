use crate::ast;
use crate::lexer::Token;
use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic;

mod block;
mod break_continue;
mod for_;
mod if_;
mod let_;
mod loop_;
mod return_;
mod while_;

impl<'a> super::Parser<'a> {
    pub fn parse_stmt(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        if self.check(Token::KwLet) {
            self.advance();
            self.parse_let(true)
        } else if self.check(Token::KwIf) {
            self.parse_if()
        } else if self.check(Token::KwReturn) {
            self.parse_return()
        } else if self.check(Token::KwWhile) {
            self.parse_while()
        } else if self.check(Token::KwLoop) {
            self.parse_loop()
        } else if self.check(Token::KwFor) {
            self.parse_for()
        } else if self.check(Token::KwBreak) {
            self.parse_break()
        } else if self.check(Token::KwContinue) {
            self.parse_continue()
        } else if self.check(Token::KwDefer) {
            self.parse_defer()
        } else if self.check(Token::KwVar) {
            self.parse_var()
        } else {
            let expr = self.parse_expr()?;
            let span = expr.span();

            if self.check(Token::Semi) {
                self.advance();
            }
            Ok(ast::Stmt::Expr(expr, span))
        }
    }

    pub fn parse_defer(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        let span = self.peek_span();
        self.expect(Token::KwDefer)?;
        let expr = self.parse_expr()?;
        if self.check(Token::Semi) {
            self.advance();
        }
        Ok(ast::Stmt::Defer(expr, span))
    }

    pub fn parse_var(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        let var_span = self.peek_span();
        self.expect(Token::KwVar)?;

        let (name, _) = self.consume_ident()?;

        let ty = if self.check(Token::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        if self.check(Token::Semi) {
            self.advance();
        }

        Ok(ast::Stmt::Var(name, ty, var_span))
    }
}
