use crate::ast;
use crate::lexer::Token;
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;

impl<'a> super::super::Parser<'a> {
    pub fn parse_atom(&mut self) -> Result<ast::Expr, Diagnostic<FileId>> {
        let current = self.advance().cloned();
        match current {
            Some((Token::KwNew, span)) => {
                let (struct_name, _) = self.consume_ident()?;
                let args = if self.check(Token::LParen) {
                    let (args, _) = self.parse_call_args()?;
                    args
                } else {
                    Vec::new()
                };
                Ok(ast::Expr::New(
                    struct_name.clone(),
                    args,
                    ast::ExprInfo {
                        span,
                        ty: ast::Type::Struct(struct_name),
                        is_tail: false,
                    },
                ))
            }
            Some((Token::Int(s), span)) => match s.parse::<i32>() {
                Ok(val) => Ok(ast::Expr::Int(
                    val,
                    ast::ExprInfo {
                        span,
                        ty: ast::Type::I32,
                        is_tail: false,
                    },
                )),
                Err(_) => match s.parse::<i64>() {
                    Ok(val) => Ok(ast::Expr::Int64(
                        val,
                        ast::ExprInfo {
                            span,
                            ty: ast::Type::I64,
                            is_tail: false,
                        },
                    )),
                    Err(_) => self.error(&format!("Integer literal '{}' is out of range", s), span),
                },
            },
            Some((Token::Str(s), span)) => Ok(ast::Expr::Str(
                s,
                ast::ExprInfo {
                    span,
                    ty: ast::Type::String,
                    is_tail: false,
                },
            )),
            Some((Token::TemplateStr(s), span)) => self.parse_template_string(s, span),
            Some((Token::KwTrue, span)) => Ok(ast::Expr::Bool(
                true,
                ast::ExprInfo {
                    span,
                    ty: ast::Type::Bool,
                    is_tail: false,
                },
            )),
            Some((Token::KwFalse, span)) => Ok(ast::Expr::Bool(
                false,
                ast::ExprInfo {
                    span,
                    ty: ast::Type::Bool,
                    is_tail: false,
                },
            )),
            Some((Token::F32(val), span)) => Ok(ast::Expr::F32(
                val,
                ast::ExprInfo {
                    span,
                    ty: ast::Type::F32,
                    is_tail: false,
                },
            )),
            Some((Token::KwNone, span)) => Ok(ast::Expr::None(ast::ExprInfo {
                span,
                ty: ast::Type::NoneType,
                is_tail: false,
            })),
            Some((Token::DotDot, span)) => {
                if let Some((next_token, _)) = self.peek() {
                    if matches!(next_token, Token::Int(_) | Token::Ident(_)) {
                        let rhs = self.parse_expr()?;
                        let end_span = rhs.span();
                        Ok(ast::Expr::Range(
                            Box::new(ast::Expr::InfiniteRange(
                                ast::RangeType::Infinite,
                                ast::ExprInfo {
                                    span: Span::new(span.start(), span.start()),
                                    ty: ast::Type::Unknown,
                                    is_tail: false,
                                },
                            )),
                            Box::new(rhs),
                            ast::RangeType::Exclusive,
                            ast::ExprInfo {
                                span: Span::new(span.start(), end_span.end()),
                                ty: ast::Type::Unknown,
                                is_tail: false,
                            },
                        ))
                    } else {
                        Ok(ast::Expr::InfiniteRange(
                            ast::RangeType::Infinite,
                            ast::ExprInfo {
                                span,
                                ty: ast::Type::Unknown,
                                is_tail: false,
                            },
                        ))
                    }
                } else {
                    Ok(ast::Expr::InfiniteRange(
                        ast::RangeType::Infinite,
                        ast::ExprInfo {
                            span,
                            ty: ast::Type::Unknown,
                            is_tail: false,
                        },
                    ))
                }
            }
            Some((Token::DotDotGt, span)) => Ok(ast::Expr::InfiniteRange(
                ast::RangeType::InfiniteUp,
                ast::ExprInfo {
                    span,
                    ty: ast::Type::Unknown,
                    is_tail: false,
                },
            )),
            Some((Token::DotDotLt, span)) => Ok(ast::Expr::InfiniteRange(
                ast::RangeType::InfiniteDown,
                ast::ExprInfo {
                    span,
                    ty: ast::Type::Unknown,
                    is_tail: false,
                },
            )),
            Some((Token::LParen, span_start)) => {
                let expr = self.parse_expr()?;
                let span_end = self.expect(Token::RParen)?;
                let _span = Span::new(span_start.start(), span_end.end());
                Ok(expr)
            }
            Some((Token::Ident(name), span)) => {
                if self.check(Token::Dot) {
                    let look1 = self.peek().cloned();
                    let look2 = self.tokens.get(self.pos + 1).cloned();
                    let look3 = self.tokens.get(self.pos + 2).cloned();
                    
                    // Check if this is an enum construction pattern: identifier.variant { ... }
                    // Only trigger if the variant and brace are immediately adjacent
                    if let (Some((Token::Dot, _)), Some((Token::Ident(variant_name), variant_span)), Some((Token::LBrace, brace_span))) = (look1, look2, look3) {
                        // Check if the variant and brace are adjacent (no other tokens in between)
                        if variant_span.end() == brace_span.start() {
                        self.advance();
                        let variant = variant_name.clone();
                        self.advance();
                        self.advance();
                        let mut fields = Vec::new();
                        let enum_name = name.clone();
                        while !self.check(Token::RBrace) {
                            let (field_name, _) = self.consume_ident()?;
                            self.expect(Token::Colon)?;
                            let expr = self.parse_expr()?;
                            fields.push((field_name, expr));
                            if !self.check(Token::RBrace) {
                                self.expect(Token::Comma)?;
                            }
                        }
                        let end_span = self.expect(Token::RBrace)?;
                        return Ok(ast::Expr::EnumConstruct(
                            enum_name.clone(),
                            variant.clone(),
                            fields.into_iter().map(|(_, e)| e).collect(),
                            ast::ExprInfo {
                                span: Span::new(span.start(), end_span.end()),
                                ty: ast::Type::Unknown,
                                is_tail: false,
                            },
                        ));
                        }
                    }
                }

                if self.check(Token::LBrace) && self.can_start_struct_init() {
                    self.advance();
                    let mut fields = Vec::new();
                    let struct_name = name.clone();
                    while !self.check(Token::RBrace) {
                        let (field_name, _) = self.consume_ident()?;
                        self.expect(Token::Colon)?;
                        let expr = self.parse_expr()?;
                        fields.push((field_name, expr));
                        if !self.check(Token::RBrace) {
                            self.expect(Token::Comma)?;
                        }
                    }
                    let end_span = self.expect(Token::RBrace)?;
                    Ok(ast::Expr::StructInit(
                        struct_name.clone(),
                        fields,
                        ast::ExprInfo {
                            span: Span::new(span.start(), end_span.end()),
                            ty: ast::Type::Struct(struct_name),
                            is_tail: false,
                        },
                    ))
                } else if self.check(Token::LParen) {
                    self.parse_function_call(name, span)
                } else {
                    Ok(ast::Expr::Var(
                        name,
                        ast::ExprInfo {
                            span,
                            ty: ast::Type::Unknown,
                            is_tail: false,
                        },
                    ))
                }
            }
            Some((Token::LBracket, span)) => {
                let mut elements = Vec::new();
                if !self.check(Token::RBracket) {
                    loop {
                        elements.push(self.parse_expr()?);
                        if !self.check(Token::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                let end_span = self.expect(Token::RBracket)?;
                Ok(ast::Expr::ArrayInit(
                    elements,
                    ast::ExprInfo {
                        span: Span::new(span.start(), end_span.end()),
                        ty: ast::Type::Unknown,
                        is_tail: false,
                    },
                ))
            }
            Some((Token::EmptyArray, span)) => Ok(ast::Expr::ArrayInit(
                Vec::new(),
                ast::ExprInfo {
                    span,
                    ty: ast::Type::Unknown,
                    is_tail: false,
                },
            )),
            Some((Token::KwMatch, span)) => self.parse_match(span),
            Some((Token::KwIf, span)) => self.parse_if_expr(span),
            Some((Token::KwLoop, span)) => self.parse_loop_expr(span),
            _ => {
                let token = self.previous().map(|(t, _)| t.clone()).unwrap();
                let span = self.previous().map(|(_, s)| *s).unwrap();
                self.error(
                    &format!("Unexpected token in expression: {:?}", token),
                    span,
                )
            }
        }
    }

    pub fn can_start_struct_init(&self) -> bool {
        let mut i = self.pos;
        if let Some((Token::LBrace, _)) = self.tokens.get(i) {
            i += 1;
            if let Some((Token::Ident(_), _)) = self.tokens.get(i) {
                i += 1;
                if let Some((Token::Colon, _)) = self.tokens.get(i) {
                    return true;
                }
            }
        }
        false
    }
}
