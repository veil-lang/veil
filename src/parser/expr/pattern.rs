use crate::ast;
use crate::lexer::Token;
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;

impl<'a> super::super::Parser<'a> {
    pub fn parse_pattern(&mut self) -> Result<ast::Pattern, Diagnostic<FileId>> {
        let current = self.peek().cloned();
        match current {
            Some((Token::Ident(name), span)) if name == "_" => {
                self.advance();
                Ok(ast::Pattern::Wildcard(span))
            }
            Some((Token::Ident(name), span)) => {
                self.advance();
                if self.check(Token::Dot) {
                    self.advance();
                    let (variant_name, variant_span) = self.consume_ident()?;

                    if self.check(Token::LParen) {
                        self.advance();
                        let mut patterns = Vec::new();

                        if !self.check(Token::RParen) {
                            loop {
                                patterns.push(self.parse_pattern()?);
                                if !self.check(Token::Comma) {
                                    break;
                                }
                                self.advance();
                            }
                        }

                        let end_span = self.expect(Token::RParen)?;
                        Ok(ast::Pattern::EnumVariant(
                            name,
                            variant_name,
                            patterns,
                            Span::new(span.start(), end_span.end()),
                        ))
                    } else {
                        Ok(ast::Pattern::EnumVariant(
                            name,
                            variant_name,
                            Vec::new(),
                            Span::new(span.start(), variant_span.end()),
                        ))
                    }
                } else {
                    Ok(ast::Pattern::Variable(name, span))
                }
            }
            Some((Token::Int(n), span)) => {
                self.advance();
                match n.parse::<i32>() {
                    Ok(val) => Ok(ast::Pattern::Literal(
                        ast::Expr::Int(
                            val,
                            ast::ExprInfo {
                                span,
                                ty: ast::Type::I32,
                                is_tail: false,
                            },
                        ),
                        span,
                    )),
                    Err(_) => self.error("Integer literal out of range for pattern", span),
                }
            }
            Some((Token::Str(s), span)) => {
                self.advance();
                Ok(ast::Pattern::Literal(
                    ast::Expr::Str(
                        s,
                        ast::ExprInfo {
                            span,
                            ty: ast::Type::String,
                            is_tail: false,
                        },
                    ),
                    span,
                ))
            }
            Some((Token::KwTrue, span)) => {
                self.advance();
                Ok(ast::Pattern::Literal(
                    ast::Expr::Bool(
                        true,
                        ast::ExprInfo {
                            span,
                            ty: ast::Type::Bool,
                            is_tail: false,
                        },
                    ),
                    span,
                ))
            }
            Some((Token::KwFalse, span)) => {
                self.advance();
                Ok(ast::Pattern::Literal(
                    ast::Expr::Bool(
                        false,
                        ast::ExprInfo {
                            span,
                            ty: ast::Type::Bool,
                            is_tail: false,
                        },
                    ),
                    span,
                ))
            }
            Some((Token::KwNone, span)) => Ok(ast::Pattern::Literal(
                ast::Expr::None(ast::ExprInfo {
                    span,
                    ty: ast::Type::NoneType,
                    is_tail: false,
                }),
                span,
            )),
            _ => {
                let (_, span) = self.advance().unwrap();
                let span = *span;
                self.error("Expected pattern", span)
            }
        }
    }
}
