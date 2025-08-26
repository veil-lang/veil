use crate::ast;
use crate::lexer::Token;
use crate::parser::Precedence;
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;

impl<'a> super::super::Parser<'a> {
    pub fn parse_infix(
        &mut self,
        op: &Token,
        lhs: ast::Expr,
        _lbp: Precedence,
        rbp: Precedence,
    ) -> Result<ast::Expr, Diagnostic<FileId>> {
        match op {
            Token::Eq => {
                let rhs = self.parse_expr_bp(rbp)?;
                let span = Span::new(lhs.span().start(), rhs.span().end());
                Ok(ast::Expr::Assign(
                    Box::new(lhs),
                    Box::new(rhs),
                    ast::ExprInfo {
                        span,
                        ty: ast::Type::Unknown,
                        is_tail: false,
                    },
                ))
            }
            Token::Plus
            | Token::Minus
            | Token::Star
            | Token::Slash
            | Token::DoubleStar
            | Token::Caret
            | Token::Percent
            | Token::EqEq
            | Token::NotEq
            | Token::Gt
            | Token::Lt
            | Token::GtEq
            | Token::LtEq
            | Token::AndAnd
            | Token::OrOr => {
                let bin_op = match op {
                    Token::Plus => ast::BinOp::Add,
                    Token::Minus => ast::BinOp::Sub,
                    Token::Star => ast::BinOp::Mul,
                    Token::Slash => ast::BinOp::Div,
                    Token::DoubleStar => ast::BinOp::Pow,
                    Token::Caret => ast::BinOp::Pow2,
                    Token::Percent => ast::BinOp::Mod,
                    Token::EqEq => ast::BinOp::Eq,
                    Token::NotEq => ast::BinOp::NotEq,
                    Token::Gt => ast::BinOp::Gt,
                    Token::Lt => ast::BinOp::Lt,
                    Token::GtEq => ast::BinOp::GtEq,
                    Token::LtEq => ast::BinOp::LtEq,
                    Token::AndAnd => ast::BinOp::And,
                    Token::OrOr => ast::BinOp::Or,
                    _ => unreachable!(),
                };
                let rhs = self.parse_expr_bp(rbp)?;
                let span = Span::new(lhs.span().start(), rhs.span().end());
                Ok(ast::Expr::BinOp(
                    Box::new(lhs),
                    bin_op,
                    Box::new(rhs),
                    ast::ExprInfo {
                        span,
                        ty: ast::Type::Unknown,
                        is_tail: false,
                    },
                ))
            }
            Token::LBracket => {
                let index = self.parse_expr()?;
                let end_span = self.expect(Token::RBracket)?;
                let span = Span::new(lhs.span().start(), end_span.end());

                Ok(ast::Expr::ArrayAccess(
                    Box::new(lhs),
                    Box::new(index),
                    ast::ExprInfo {
                        span,
                        ty: ast::Type::Unknown,
                        is_tail: false,
                    },
                ))
            }
            Token::Dot => {
                let (field, field_span) = self.consume_ident()?;
                if self.check(Token::LParen) {
                    let is_enum_variant = if let ast::Expr::Var(enum_name, _) = &lhs {
                        self.enums.iter().any(|e| e.name == *enum_name && e.variants.iter().any(|v| v.name == field))
                    } else { false };

                    if is_enum_variant {
                        let args = self.parse_call_args()?;
                        let end_span = args.1;
                        Ok(ast::Expr::EnumConstruct(
                            if let ast::Expr::Var(enum_name, _) = lhs { enum_name } else { unreachable!() },
                            field,
                            args.0,
                            ast::ExprInfo {
                                span: Span::new(field_span.start(), end_span.end()),
                                ty: ast::Type::Unknown,
                                is_tail: false,
                            },
                        ))
                    } else {
                        let method_span = Span::new(lhs.span().start(), field_span.end());
                        let args = self.parse_call_args()?;
                        let end_span = args.1;
                        Ok(ast::Expr::Call(
                            format!("<method>.{}", field),
                            {
                                let mut v = vec![lhs];
                                v.extend(args.0);
                                v
                            },
                            ast::ExprInfo {
                                span: Span::new(method_span.start(), end_span.end()),
                                ty: ast::Type::Unknown,
                                is_tail: false,
                            },
                        ))
                    }
                } else {
                    if let ast::Expr::Var(_e_num_name, _) = &lhs {
                        let span = Span::new(lhs.span().start(), field_span.end());
                        Ok(ast::Expr::FieldAccess(
                            Box::new(lhs),
                            field,
                            ast::ExprInfo {
                                span,
                                ty: ast::Type::Unknown,
                                is_tail: false,
                            },
                        ))
                    } else {
                        let span = Span::new(lhs.span().start(), field_span.end());
                        Ok(ast::Expr::FieldAccess(
                            Box::new(lhs),
                            field,
                            ast::ExprInfo {
                                span,
                                ty: ast::Type::Unknown,
                                is_tail: false,
                            },
                        ))
                    }
                }
            }
            Token::DotDot | Token::DotDotEq | Token::DotDotGt | Token::DotDotLt => {
                let range_type = match *op {
                    Token::DotDot => ast::RangeType::Exclusive,
                    Token::DotDotEq => ast::RangeType::Inclusive,
                    Token::DotDotGt => ast::RangeType::InfiniteUp,
                    Token::DotDotLt => ast::RangeType::InfiniteDown,
                    _ => unreachable!(),
                };

                if matches!(*op, Token::DotDotGt | Token::DotDotLt) {
                    let span = Span::new(lhs.span().start(), lhs.span().end());
                    Ok(ast::Expr::InfiniteRange(
                        range_type,
                        ast::ExprInfo {
                            span,
                            ty: ast::Type::Unknown,
                            is_tail: false,
                        },
                    ))
                } else {
                    if self.check(Token::LBrace)
                        || self.check(Token::RParen)
                        || self.check(Token::Comma)
                        || self.is_at_end()
                    {
                        let infinite_range = ast::Expr::InfiniteRange(
                            ast::RangeType::Infinite,
                            ast::ExprInfo {
                                span: Span::new(lhs.span().end(), lhs.span().end()),
                                ty: ast::Type::Unknown,
                                is_tail: false,
                            },
                        );
                        let span = Span::new(lhs.span().start(), lhs.span().end());
                        Ok(ast::Expr::Range(
                            Box::new(lhs),
                            Box::new(infinite_range),
                            range_type,
                            ast::ExprInfo {
                                span,
                                ty: ast::Type::Unknown,
                                is_tail: false,
                            },
                        ))
                    } else {
                        let rhs = self.parse_expr_bp(rbp)?;
                        let span = Span::new(lhs.span().start(), rhs.span().end());
                        Ok(ast::Expr::Range(
                            Box::new(lhs),
                            Box::new(rhs),
                            range_type,
                            ast::ExprInfo {
                                span,
                                ty: ast::Type::Unknown,
                                is_tail: false,
                            },
                        ))
                    }
                }
            }
            Token::KwAs => {
                let cast_type = self.parse_type()?;
                let end_span = self.previous().map(|(_, s)| *s).unwrap();
                let span = Span::new(lhs.span().start(), end_span.end());

                Ok(ast::Expr::Cast(
                    Box::new(lhs),
                    cast_type,
                    ast::ExprInfo {
                        span,
                        ty: ast::Type::Unknown,
                        is_tail: false,
                    },
                ))
            }
            _ => {
                let (token, span) = self.peek().unwrap();
                let token = token.clone();
                let span = *span;
                self.error(
                    &format!("Unexpected token in infix position: {:?}", token),
                    span,
                )
            }
        }
    }
}
