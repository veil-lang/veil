use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;
use crate::ast;

impl<'a> super::super::Parser<'a> {
    pub fn parse_loop_expr(&mut self, loop_span: Span) -> Result<ast::Expr, Diagnostic<FileId>> {
        let body = self.parse_block()?;

        let end_span = body.last()
            .map(|s| s.span().end())
            .unwrap_or(loop_span.end());

        Ok(ast::Expr::Loop(
            body,
            ast::ExprInfo {
                span: Span::new(loop_span.start(), end_span),
                ty: ast::Type::Unknown,
                is_tail: false,
            },
        ))
    }
}