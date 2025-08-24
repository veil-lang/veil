use crate::ast;
use crate::lexer::Token;
use crate::parser::Parser;
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;

impl<'a> Parser<'a> {
    pub fn parse_for(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        self.consume(Token::KwFor, "Expected 'for'")?;
        let for_span = self.previous().map(|(_, s)| *s).unwrap();
        let first_expr = self.parse_expr()?;

        let (value_var, index_var, range_expr, step_expr) =
            if let ast::Expr::Var(var_name, _) = first_expr {
                if self.check(Token::Comma) {
                    self.consume(Token::Comma, "Expected ',' after first variable")?;
                    let second_expr = self.parse_expr()?;
                    let second_var = if let ast::Expr::Var(idx_var, _) = second_expr {
                        idx_var
                    } else {
                        let span = self.peek_span();
                        return self.error("Expected variable name after comma", span);
                    };
                    self.consume(Token::KwIn, "Expected 'in' after loop variables")?;
                    let range_expr = self.parse_expr()?;

                    let step_expr = if self.check(Token::KwStep) {
                        self.consume(Token::KwStep, "Expected 'step'")?;
                        Some(self.parse_expr()?)
                    } else {
                        None
                    };

                    (var_name, Some(second_var), range_expr, step_expr)
                } else if self.check(Token::KwIn) {
                    self.consume(Token::KwIn, "Expected 'in' after loop variable")?;
                    let range_expr = self.parse_expr()?;

                    let step_expr = if self.check(Token::KwStep) {
                        self.consume(Token::KwStep, "Expected 'step'")?;
                        Some(self.parse_expr()?)
                    } else {
                        None
                    };

                    (var_name, None, range_expr, step_expr)
                } else {
                    let span = self.peek_span();
                    return self.error("Expected 'in' after loop variable", span);
                }
            } else {
                let span = self.peek_span();
                return self.error("Expected variable name in for loop", span);
            };

        let body = self.parse_block()?;

        Ok(ast::Stmt::For(
            value_var,
            index_var,
            range_expr,
            step_expr,
            body,
            Span::new(for_span.start(), self.previous().unwrap().1.end()),
        ))
    }
}
