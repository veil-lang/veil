use crate::ast;
use crate::lexer::Token;
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;

impl<'a> super::super::Parser<'a> {
    pub fn parse_if(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        self.consume(Token::KwIf, "Expected 'if'")?;
        let if_span = self.previous().map(|(_, s)| *s).unwrap();
        let condition = self.parse_expr()?;
        let then_branch = self.parse_block()?;
        let mut else_branch = None;

        if self.check(Token::KwElse) {
            self.advance();
            else_branch = Some(if self.check(Token::KwIf) {
                vec![self.parse_if()?]
            } else {
                self.parse_block()?
            });
        }

        let end_span = else_branch
            .as_ref()
            .and_then(|b| b.last())
            .map(|s| s.span().end())
            .unwrap_or_else(|| then_branch.last().unwrap().span().end());

        Ok(ast::Stmt::If(
            condition,
            then_branch,
            else_branch,
            Span::new(if_span.start(), end_span),
        ))
    }
}
