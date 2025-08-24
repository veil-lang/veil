use crate::ast;
use crate::lexer::Token;
use crate::parser::Parser;
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;

impl<'a> Parser<'a> {
    pub fn parse_return(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        self.consume(Token::KwReturn, "Expected 'return'")?;
        let ret_span = self.previous().map(|(_, s)| *s).unwrap();
        let expr = if self.check(Token::Semi) {
            ast::Expr::Void(ast::ExprInfo {
                span: ret_span,
                ty: ast::Type::Void,
                is_tail: false,
            })
        } else {
            self.parse_expr()?
        };
        self.expect(Token::Semi)?;
        let end_span = self.previous().map(|(_, s)| *s).unwrap();
        Ok(ast::Stmt::Return(
            expr,
            Span::new(ret_span.start(), end_span.end()),
        ))
    }
}
