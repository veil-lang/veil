use crate::ast;
use crate::lexer::Token;
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;

impl<'a> super::super::Parser<'a> {
    pub fn parse_if_expr(&mut self, if_span: Span) -> Result<ast::Expr, Diagnostic<FileId>> {
        let condition = self.parse_expr()?;
        let then_branch = self.parse_block()?;
        let mut else_branch = None;

        if self.check(Token::KwElse) {
            self.advance();
            else_branch = Some(if self.check(Token::KwIf) {
                let if_span = self.peek_span();
                self.advance();
                let inner_if = self.parse_if_expr(if_span)?;
                let inner_span = inner_if.span();
                vec![ast::Stmt::Expr(inner_if, inner_span)]
            } else {
                self.parse_block()?
            });
        }

        let end_span = else_branch
            .as_ref()
            .and_then(|b| b.last())
            .map(|s| s.span().end())
            .unwrap_or_else(|| then_branch.last().unwrap().span().end());

        Ok(ast::Expr::If(
            Box::new(condition),
            then_branch,
            else_branch,
            ast::ExprInfo {
                span: Span::new(if_span.start(), end_span),
                ty: ast::Type::Unknown,
                is_tail: false,
            },
        ))
    }
}
