use crate::ast;
use crate::lexer::Token;
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;

impl<'a> super::super::Parser<'a> {
    pub fn parse_impl(&mut self) -> Result<ast::ImplBlock, Diagnostic<FileId>> {
        let start_span = self.consume(Token::KwImpl, "Expected 'impl'")?;

        let target_type_ast = self.parse_type()?;
        let target_type = target_type_ast.to_string();

        self.expect(Token::LBrace)?;

        let mut methods = Vec::new();
        while !self.check(Token::RBrace) {
            if self.check(Token::KwFn) {
                methods.push(self.parse_function()?);
            } else if self.check(Token::KwConstructor) {
                methods.push(self.parse_constructor()?);
            } else {
                let span = self.peek_span();
                return self.error("Expected function or constructor in impl block", span);
            }
        }

        let end_span = self.expect(Token::RBrace)?;

        Ok(ast::ImplBlock {
            target_type,
            methods,
            span: Span::new(start_span.start(), end_span.end()),
        })
    }

    pub fn parse_constructor(&mut self) -> Result<ast::Function, Diagnostic<FileId>> {
        let start_span = self.consume(Token::KwConstructor, "Expected 'constructor'")?;

        self.expect(Token::LParen)?;
        let mut params = Vec::new();

        if !self.check(Token::RParen) {
            loop {
                let (param_name, _) = self.consume_ident()?;
                self.expect(Token::Colon)?;
                let param_type = self.parse_type()?;
                params.push((param_name, param_type));

                if self.check(Token::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        self.expect(Token::RParen)?;
        self.expect(Token::Arrow)?;
        let return_type = self.parse_type()?;
        self.expect(Token::LBrace)?;

        let mut body = Vec::new();

        while !self.check(Token::RBrace) {
            let stmt = self.parse_stmt()?;
            body.push(stmt);
        }

        if let Some(&mut ast::Stmt::Expr(ref mut expr, _)) = body.last_mut() {
            self.mark_as_tail(expr);
        }

        let end_span = self.expect(Token::RBrace)?;

        Ok(ast::Function {
            name: "constructor".to_string(),
            generic_params: Vec::new(),
            params,
            return_type,
            body,
            span: Span::new(start_span.start(), end_span.end()),
            visibility: ast::Visibility::Private,
        })
    }
}
