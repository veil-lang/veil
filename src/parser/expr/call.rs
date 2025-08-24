use crate::ast;
use crate::lexer::Token;
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;

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

        if let Some((enum_name, variant_name)) = Self::split_enum_ctor(&name) {
            if self.is_enum_variant(enum_name, variant_name) {
                return Ok(ast::Expr::EnumConstruct(
                    enum_name.to_string(),
                    variant_name.to_string(),
                    args,
                    ast::ExprInfo {
                        span: Span::new(span.start(), rparen_span.end()),
                        ty: ast::Type::Unknown,
                        is_tail: false,
                    },
                ))
            }
        }

        Ok(ast::Expr::Call(
            name,
            args,
            ast::ExprInfo {
                span: Span::new(span.start(), rparen_span.end()),
                ty: ast::Type::Unknown,
                is_tail: false,
            },
        ))
    }

    fn split_enum_ctor(s: &str) -> Option<(&str, &str)> {
        let mut parts = s.splitn(2, '.');
        match (parts.next(), parts.next()) {
            (Some(enum_name), Some(variant_name)) => Some((enum_name, variant_name)),
            _ => None,
        }
    }

    fn is_enum_variant(&self, enum_name: &str, variant_name: &str) -> bool {
        self.enums.iter().any(|e|
            e.name == enum_name && e.variants.iter().any(|v| v.name == variant_name)
        )
    }
}
