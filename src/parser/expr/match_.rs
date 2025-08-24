use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;
use crate::ast;
use crate::lexer::Token;

impl<'a> super::super::Parser<'a> {
    pub fn parse_match(&mut self, start_span: Span) -> Result<ast::Expr, Diagnostic<FileId>> {
        let expr = self.parse_pattern()?;
        self.expect(Token::LBrace)?;

        let mut arms = Vec::new();
        while !self.check(Token::RBrace) {
            arms.push(self.parse_match_arm()?);

            if self.check(Token::Comma) {
                self.advance();
            }
        }

        let end_span = self.expect(Token::RBrace)?;

        Ok(ast::Expr::Match(Box::new(expr), arms, ast::ExprInfo {
            span: Span::new(start_span.start(), end_span.end()),
            ty: ast::Type::Unknown,
            is_tail: false,
        }))
    }

    pub fn parse_match_arm(&mut self) -> Result<ast::MatchArm, Diagnostic<FileId>> {
        let pattern = self.parse_pattern()?;

        let guard = if self.check(Token::KwIf) {
            self.consume(Token::KwIf, "Expected 'if'")?;
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.expect(Token::Arrow2)?;

        let (body, body_span) = if self.check(Token::LBrace) {
            let stmts = self.parse_block()?;
            let span = stmts
                .last()
                .map(|s| s.span())
                .unwrap_or_else(|| self.previous().map(|(_, s)| *s).unwrap());
            (ast::MatchArmBody::Block(stmts), span)
        } else {
            let expr = self.parse_expr()?;
            let span = expr.span();
            (ast::MatchArmBody::Expr(expr), span)
        };

        let pattern_span = pattern.span();

        Ok(ast::MatchArm {
            pattern,
            guard,
            body,
            span: Span::new(pattern_span.start(), body_span.end()),
        })
    }
}