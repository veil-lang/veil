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
        } else {
            let expr = self.parse_expr()?;
            let span = expr.span();

            if self.check(Token::Semi) {
                self.advance();
            }
            Ok(ast::Stmt::Expr(expr, span))
        }
    }
}
