use crate::ast;
use crate::lexer::Token;
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;

impl<'a> super::super::Parser<'a> {
    pub fn parse_prefix(&mut self) -> Result<ast::Expr, Diagnostic<FileId>> {
        let token = self.peek_token();
        match token {
            Token::Star => {
                let op_span = self.peek_span();
                self.advance();
                let prefix_bp = self.get_prefix_bp(&token);
                let expr = self.parse_expr_bp(prefix_bp)?;
                Ok(ast::Expr::Deref(
                    Box::new(expr),
                    ast::ExprInfo {
                        span: op_span,
                        ty: ast::Type::Unknown,
                        is_tail: false,
                    },
                ))
            }
            Token::Minus | Token::Plus => {
                let (op_token, op_span) = self.advance().unwrap();
                let op_token = op_token.clone();
                let op_span = *op_span;
                let prefix_bp = self.get_prefix_bp(&op_token);
                let expr = self.parse_expr_bp(prefix_bp)?;

                match (&op_token, &expr) {
                    (Token::Minus, ast::Expr::Int(val, _)) => {
                        let negated = -(*val as i64);
                        if (i32::MIN as i64..=i32::MAX as i64).contains(&negated) {
                            Ok(ast::Expr::Int(
                                negated as i32,
                                ast::ExprInfo {
                                    span: op_span,
                                    ty: ast::Type::I32,
                                    is_tail: false,
                                },
                            ))
                        } else {
                            Ok(ast::Expr::Int64(
                                negated,
                                ast::ExprInfo {
                                    span: op_span,
                                    ty: ast::Type::I64,
                                    is_tail: false,
                                },
                            ))
                        }
                    }
                    (Token::Minus, ast::Expr::Int64(val, _)) => {
                        if let Some(neg) = val.checked_neg() {
                            Ok(ast::Expr::Int64(
                                neg,
                                ast::ExprInfo {
                                    span: op_span,
                                    ty: ast::Type::I64,
                                    is_tail: false,
                                },
                            ))
                        } else {
                            self.error(
                                &format!("Cannot negate {val} as it would overflow i64"),
                                op_span,
                            )
                        }
                    }
                    _ => {
                        let span = expr.span();
                        Ok(ast::Expr::UnaryOp(
                            match op_token {
                                Token::Minus => ast::UnOp::Neg,
                                Token::Plus => ast::UnOp::Plus,
                                _ => unreachable!(),
                            },
                            Box::new(expr),
                            ast::ExprInfo {
                                span,
                                ty: ast::Type::Unknown,
                                is_tail: false,
                            },
                        ))
                    }
                }
            }

            Token::Bang => {
                let _op_span = self.peek_span();
                self.advance();
                let prefix_bp = self.get_prefix_bp(&token);
                let expr = self.parse_expr_bp(prefix_bp)?;
                let span = expr.span();
                Ok(ast::Expr::UnaryOp(
                    ast::UnOp::Not,
                    Box::new(expr),
                    ast::ExprInfo {
                        span,
                        ty: ast::Type::Unknown,
                        is_tail: false,
                    },
                ))
            }
            Token::Ellipsis => {
                let op_span = self.peek_span();
                self.advance();
                let expr = self.parse_expr_bp(8)?;
                let expr_span = expr.span();
                Ok(ast::Expr::Spread(
                    Box::new(expr),
                    ast::ExprInfo {
                        span: Span::new(op_span.start(), expr_span.end()),
                        ty: ast::Type::Unknown,
                        is_tail: false,
                    },
                ))
            }
            Token::KwNew => {
                let new_span = self.peek_span();
                self.advance();
                let (struct_name, _name_span) = self.consume_ident()?;
                let (args, args_span) = self.parse_call_args()?;
                Ok(ast::Expr::New(
                    struct_name,
                    args,
                    ast::ExprInfo {
                        span: Span::new(new_span.start(), args_span.end()),
                        ty: ast::Type::Unknown,
                        is_tail: false,
                    },
                ))
            }
            _ => self.parse_atom(),
        }
    }
}
