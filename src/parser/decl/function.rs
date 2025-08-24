use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;
use crate::ast;
use crate::lexer::Token;

impl<'a> super::super::Parser<'a> {
    pub fn parse_function(&mut self) -> Result<ast::Function, Diagnostic<FileId>> {
        self.consume(Token::KwFn, "Expected 'fn'")?;
        let start_span = self.previous().map(|(_, s)| *s).unwrap();

        let (name, _name_span) = self.consume_ident()?;
        let generic_params = self.parse_generic_params()?;
        let params = self.parse_parameters()?;

        let return_type = if self.check(Token::Arrow) {
            self.advance();
            self.parse_type()?
        } else {
            ast::Type::Void
        };

        let has_tail = !matches!(return_type, ast::Type::Void);
        let body = self.parse_block_with_tail(has_tail)?;
        let end_span = match body.last() {
            Some(last_stmt) => last_stmt.span(),
            None => start_span,
        };
        Ok(ast::Function {
            name,
            generic_params,
            params,
            return_type,
            body,
            span: Span::new(start_span.start(), end_span.end()),
            visibility: ast::Visibility::Private,
        })
    }

    pub fn parse_parameters(&mut self) -> Result<Vec<(String, ast::Type)>, Diagnostic<FileId>> {
        self.consume(Token::LParen, "Expected '(' after function name")?;
        let mut params = Vec::new();
        while !self.check(Token::RParen) {
            if self.check(Token::Ellipsis) {
                self.advance();
                if matches!(self.peek_token(), Token::Ident(_)) {
                    let (name, _) = self.consume_ident()?;
                    self.consume(Token::Colon, "Expected ':' after parameter name")?;
                    let param_type = self.parse_type()?;
                    params.push((format!("...{}", name), param_type));
                } else {
                    params.push(("...".to_string(), ast::Type::Ellipsis));
                }
                break;
            } else if self.check(Token::Dot) {
                self.advance();
                if self.check(Token::Dot) {
                    self.advance();
                    if self.check(Token::Dot) {
                        self.advance();
                        if matches!(self.peek_token(), Token::Ident(_)) {
                            let (name, _) = self.consume_ident()?;
                            self.consume(Token::Colon, "Expected ':' after parameter name")?;
                            let param_type = self.parse_type()?;
                            params.push((format!("...{}", name), param_type));
                        } else {
                            params.push(("...".to_string(), ast::Type::Any));
                        }
                        break;
                    } else {
                        let span = self.peek_span();
                        return self.error("Expected third dot for variadic parameter", span);
                    }
                } else {
                    let span = self.peek_span();
                    return self.error("Expected second dot for variadic parameter", span);
                }
            } else {
                let (name, _) = self.consume_ident()?;


                if name == "self" {
                    params.push((name, ast::Type::Unknown));
                } else {
                    self.consume(Token::Colon, "Expected ':' after parameter name")?;
                    let param_type = self.parse_type()?;
                    params.push((name, param_type));
                }
            }
            if !self.check(Token::Comma) {
                break;
            }
            self.advance();
        }
        self.consume(Token::RParen, "Expected ')' after parameters")?;
        Ok(params)
    }

    pub fn parse_generic_params(&mut self) -> Result<Vec<String>, Diagnostic<FileId>> {
        if !self.check(Token::Lt) {
            return Ok(Vec::new());
        }

        self.advance();
        let mut generic_params = Vec::new();

        if !self.check(Token::Gt) {
            loop {
                let (param_name, _) = self.consume_ident()?;
                generic_params.push(param_name);

                if !self.check(Token::Comma) {
                    break;
                }
                self.advance();
            }
        }

        self.expect(Token::Gt)?;
        Ok(generic_params)
    }
}