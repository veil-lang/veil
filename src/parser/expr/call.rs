use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;
use crate::ast;
use crate::lexer::Token;

impl<'a> super::super::Parser<'a> {
    pub fn parse_call_args(&mut self) -> Result<(Vec<ast::Expr>, Span), Diagnostic<FileId>> {
        self.expect(Token::LParen)?;
        let mut args = Vec::new();
        if !self.check(Token::RParen) {
            while !self.check(Token::RParen) {
                args.push(self.parse_expr()?);
                if !self.check(Token::Comma) {
                    break;
                }
                self.advance();
            }
        }
        let rparen_span = self.expect(Token::RParen)?;
        Ok((args, rparen_span))
    }

    pub fn parse_function_call(
        &mut self,
        name: String,
        span: Span,
    ) -> Result<ast::Expr, Diagnostic<FileId>> {
        self.expect(Token::LParen)?;
        let mut args = Vec::new();
        if !self.check(Token::RParen) {
            while !self.check(Token::RParen) {
                args.push(self.parse_expr()?);
                if !self.check(Token::Comma) {
                    break;
                }
                self.advance();
            }
        }
        let rparen_span = self.expect(Token::RParen)?;

        Ok(ast::Expr::Call(name, args, ast::ExprInfo {
            span: Span::new(span.start(), rparen_span.end()),
            ty: ast::Type::Unknown,
            is_tail: false,
        }))
    }


}