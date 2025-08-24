mod types;
mod precedence;
mod stmt;
mod ffi;
mod import_export;
mod decl;

use super::{
    ast,
    lexer::{Lexer, Token},
};
use codespan::{FileId, Files, Span};
use codespan_reporting::diagnostic::Diagnostic;
use std::{collections::HashMap};
use std::iter::Peekable;
use std::slice::Iter;

pub(super) use precedence::Precedence;
use crate::parser::ffi::ForeignItem;

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, (Token, Span)>>,
    #[allow(dead_code)]
    files: &'a Files<String>,
    file_id: FileId,
    previous_token: Option<(Token, Span)>,
}



impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let tokens_vec = lexer.tokens();
        let leaked_tokens = Box::leak(tokens_vec.into_boxed_slice());
        Self {
            tokens: leaked_tokens.iter().peekable(),
            files: lexer.files,
            file_id: lexer.file_id,
            previous_token: None,
        }
    }

    pub fn parse(&mut self) -> Result<ast::Program, Diagnostic<FileId>> {
        let mut program = ast::Program {
            imports: Vec::new(),
            stmts: Vec::new(),
            functions: Vec::new(),
            structs: Vec::new(),
            enums: Vec::new(),
            ffi_functions: Vec::new(),
            ffi_variables: Vec::new(),
            tests: Vec::new(),
            impls: Vec::new(),
        };

        while !self.is_at_end() {
            if self.check(Token::KwImport) {
                let import = self.parse_import()?;
                program.imports.push(import);
            } else if self.check(Token::KwExport) {
                self.advance();
                if self.check(Token::KwImport) {
                    let import = self.parse_export_import()?;
                    program.imports.push(import);
                } else if self.check(Token::KwStruct) {
                    let mut struct_def = self.parse_struct()?;
                    struct_def.visibility = ast::Visibility::Public;
                    program.structs.push(struct_def);
                } else if self.check(Token::KwEnum) {
                    let mut enum_def = self.parse_enum()?;
                    enum_def.visibility = ast::Visibility::Public;
                    program.enums.push(enum_def);
                } else if self.check(Token::KwFn) {
                    let mut func = self.parse_function()?;
                    func.visibility = ast::Visibility::Public;
                    program.functions.push(func);
                }
                else if self.check(Token::LBrace) {
                    self.parse_export_block(&mut program)?;
                } else {
                    let span = self.peek_span();
                    return self.error("Expected 'fn', 'struct', 'import', or '{' after 'export'", span);
                }
            } else if self.check(Token::KwFn) {
                program.functions.push(self.parse_function()?);
            } else if self.check(Token::KwStruct) {
                program.structs.push(self.parse_struct()?);
            } else if self.check(Token::KwEnum) {
                program.enums.push(self.parse_enum()?);
            } else if self.check(Token::KwImpl) {
                program.impls.push(self.parse_impl()?);
            } else if self.check(Token::Hash) {
                self.advance();
                let metadata = self.parse_metadata()?;
                self.expect(Token::Foreign)?;
                match self.parse_ffi(metadata)? {
                    ForeignItem::Function(f) => program.ffi_functions.push(f),
                    ForeignItem::Variable(v) => program.ffi_variables.push(v),
                }
            } else if self.check(Token::Foreign) {
                self.advance();
                match self.parse_ffi(None)? {
                    ForeignItem::Function(f) => program.ffi_functions.push(f),
                    ForeignItem::Variable(v) => program.ffi_variables.push(v),
                }
            } else if self.check(Token::KwTest) {
                program.tests.push(self.parse_test()?);
            }

            else {
                program.stmts.push(self.parse_stmt()?);
            }
        }

        Ok(program)
    }





    fn parse_expr(&mut self) -> Result<ast::Expr, Diagnostic<FileId>> {
        self.parse_expr_bp(0)
    }
    fn parse_expr_bp(&mut self, min_bp: Precedence) -> Result<ast::Expr, Diagnostic<FileId>> {
        let mut lhs = self.parse_prefix()?;

        while let Some((op, _)) = self.peek() {
            let op = op.clone();
            self.peek_span();
            let Some((lbp, rbp)) = self.get_infix_bp(&op) else {
                break;
            };

            if lbp < min_bp {
                break;
            }
            self.advance();
            lhs = self.parse_infix(&op, lhs, lbp, rbp)?;
        }

        Ok(lhs)
    }
    fn peek_token(&mut self) -> Token {
        self.tokens
            .peek()
            .map(|(t, _)| (*t).clone())
            .unwrap()
    }
    fn peek_span(&mut self) -> Span {
        self.tokens
            .peek()
            .map(|(_, s)| *s)
            .unwrap_or(Span::new(0, 0))
    }
    fn parse_prefix(&mut self) -> Result<ast::Expr, Diagnostic<FileId>> {
        let token = self.peek_token();
        match token {
            Token::Star => {
                let op_span = self.peek_span();
                self.advance();
                let prefix_bp = self.get_prefix_bp(&token);
                let expr = self.parse_expr_bp(prefix_bp)?;
                Ok(ast::Expr::Deref(Box::new(expr), ast::ExprInfo {
                    span: op_span,
                    ty: ast::Type::Unknown,
                    is_tail: false,
                }))
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
                        if negated >= i32::MIN as i64 && negated <= i32::MAX as i64 {
                            Ok(ast::Expr::Int(negated as i32, ast::ExprInfo {
                                span: op_span,
                                ty: ast::Type::I32,
                                is_tail: false,
                            }))
                        } else if negated >= i64::MIN && negated <= i64::MAX {
                            Ok(ast::Expr::Int64(negated, ast::ExprInfo {
                                span: op_span,
                                ty: ast::Type::I64,
                                is_tail: false,
                            }))
                        } else {
                            self.error(&format!("Negated integer {} is out of range", negated), op_span)
                        }
                    }
                    (Token::Minus, ast::Expr::Int64(val, _)) => {
                        if let Some(negated) = val.checked_neg() {
                            Ok(ast::Expr::Int64(negated, ast::ExprInfo {
                                span: op_span,
                                ty: ast::Type::I64,
                                is_tail: false,
                            }))
                        } else {
                            self.error(&format!("Cannot negate {} as it would overflow i64", val), op_span)
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
                Ok(ast::Expr::Spread(Box::new(expr), ast::ExprInfo {
                    span: Span::new(op_span.start(), expr_span.end()),
                    ty: ast::Type::Unknown,
                    is_tail: false,
                }))
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
    fn parse_infix(
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
                    Ok(ast::Expr::InfiniteRange(range_type, ast::ExprInfo {
                        span,
                        ty: ast::Type::Unknown,
                        is_tail: false,
                    }))
                } else {
                    if self.check(Token::LBrace) || self.check(Token::RParen) || self.check(Token::Comma) || self.is_at_end() {
                        let infinite_range = ast::Expr::InfiniteRange(ast::RangeType::Infinite, ast::ExprInfo {
                            span: Span::new(lhs.span().end(), lhs.span().end()),
                            ty: ast::Type::Unknown,
                            is_tail: false,
                        });
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

                Ok(ast::Expr::Cast(Box::new(lhs), cast_type, ast::ExprInfo {
                    span,
                    ty: ast::Type::Unknown,
                    is_tail: false,
                }))
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

    fn parse_call_args(&mut self) -> Result<(Vec<ast::Expr>, Span), Diagnostic<FileId>> {
        self.expect(Token::LParen)?;
        let mut args = Vec::new();
        if !self.check(Token::RParen) {
            while !self.check(Token::RParen) {
                args.push(self.parse_expr()?);
                if !self.check(Token::Comma) {
                    break;
                }
                self.advance();
            }
        }
        let rparen_span = self.expect(Token::RParen)?;
        Ok((args, rparen_span))
    }





    fn consume(&mut self, expected: Token, err_msg: &str) -> Result<Span, Diagnostic<FileId>> {
        if self.check(expected.clone()) {
            let span = self.tokens.peek().map(|(_, s)| *s).unwrap();
            self.advance();
            Ok(span)
        } else {
            let span = self
                .tokens
                .peek()
                .map(|(_, s)| *s)
                .unwrap_or(Span::new(0, 0));
            self.error(err_msg, span)
        }
    }
    fn consume_ident(&mut self) -> Result<(String, Span), Diagnostic<FileId>> {
        let token = self.advance().cloned();
        match token.as_ref() {
            Some((Token::Ident(name), span)) => Ok((name.clone(), *span)),
            Some((_, span)) => self.error("Expected identifier", *span),
            None => {
                if let Some((_, prev_span)) = self.previous() {
                    use codespan::ByteIndex;
                    let end_pos = prev_span.end().0 as u32;
                    let error_span = Span::new(prev_span.end(), ByteIndex::from(end_pos + 1));
                    self.error("Expected identifier", error_span)
                } else {
                    self.error("Expected identifier", Span::new(0, 0))
                }
            },
        }
    }



    fn parse_return(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
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

    fn parse_generic_params(&mut self) -> Result<Vec<String>, Diagnostic<FileId>> {
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

    fn parse_test(&mut self) -> Result<ast::Test, Diagnostic<FileId>> {
        self.consume(Token::KwTest, "Expected 'test'")?;
        let start_span = self.previous().map(|(_, s)| *s).unwrap();

        let (name, _name_span) = self.consume_ident()?;

        let body = self.parse_block_with_tail(false)?;
        let end_span = match body.last() {
            Some(last_stmt) => last_stmt.span(),
            None => start_span,
        };

        Ok(ast::Test {
            name,
            stmts: body,
            span: Span::new(start_span.start(), end_span.end()),
        })
    }


    fn parse_function(&mut self) -> Result<ast::Function, Diagnostic<FileId>> {
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

    fn parse_parameters(&mut self) -> Result<Vec<(String, ast::Type)>, Diagnostic<FileId>> {
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
    fn parse_stmt(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        if self.check(Token::KwLet) {
            self.advance();
            self.parse_let(true)
        } else if self.check(Token::KwIf) {
            self.parse_if()
        } else if self.check(Token::KwReturn) {
            self.parse_return()
        } else if self.check(Token::KwWhile) {
            self.parse_while()
        } else if self.check(Token::KwLoop) {
            self.parse_loop()
        } else if self.check(Token::KwFor) {
            self.parse_for()
        } else if self.check(Token::KwBreak) {
            self.parse_break()
        } else if self.check(Token::KwContinue) {
            self.parse_continue()
        } else {
            let expr = self.parse_expr()?;
            let span = expr.span();

            if self.check(Token::Semi) {
                self.advance();
            }
            Ok(ast::Stmt::Expr(expr, span))
        }
    }

    fn parse_loop(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        self.expect(Token::KwLoop)?;
        let loop_span = self.previous().map(|(_, s)| *s).unwrap();

        let body = self.parse_block()?;

        Ok(ast::Stmt::Loop(
            body,
            Span::new(loop_span.start(), self.previous().unwrap().1.end()),
        ))
    }

    fn parse_while(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        self.expect(Token::KwWhile)?;
        let while_span = self.previous().map(|(_, s)| *s).unwrap();

        let condition = self.parse_expr()?;

        let body = self.parse_block()?;

        Ok(ast::Stmt::While(
            condition,
            body,
            Span::new(while_span.start(), self.previous().unwrap().1.end()),
        ))
    }

    fn parse_for(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        self.consume(Token::KwFor, "Expected 'for'")?;
        let for_span = self.previous().map(|(_, s)| *s).unwrap();
        let first_expr = self.parse_expr()?;

        let (value_var, index_var, range_expr, step_expr) = if let ast::Expr::Var(var_name, _) = first_expr {
            if self.check(Token::Comma) {
                self.consume(Token::Comma, "Expected ',' after first variable")?;
                let second_expr = self.parse_expr()?;
                let second_var = if let ast::Expr::Var(idx_var, _) = second_expr {
                    idx_var
                } else {
                    let span = self.peek_span();
                    return self.error("Expected variable name after comma", span);
                };
                self.consume(Token::KwIn, "Expected 'in' after loop variables")?;
                let range_expr = self.parse_expr()?;

                let step_expr = if self.check(Token::KwStep) {
                    self.consume(Token::KwStep, "Expected 'step'")?;
                    Some(self.parse_expr()?)
                } else {
                    None
                };

                (var_name, Some(second_var), range_expr, step_expr)
            } else if self.check(Token::KwIn) {
                self.consume(Token::KwIn, "Expected 'in' after loop variable")?;
                let range_expr = self.parse_expr()?;

                let step_expr = if self.check(Token::KwStep) {
                    self.consume(Token::KwStep, "Expected 'step'")?;
                    Some(self.parse_expr()?)
                } else {
                    None
                };

                (var_name, None, range_expr, step_expr)
            } else {
                let span = self.peek_span();
                return self.error("Expected 'in' after loop variable", span);
            }
        } else {
            let span = self.peek_span();
            return self.error("Expected variable name in for loop", span);
        };

        let body = self.parse_block()?;

        Ok(ast::Stmt::For(
            value_var,
            index_var,
            range_expr,
            step_expr,
            body,
            Span::new(for_span.start(), self.previous().unwrap().1.end()),
        ))
    }

    fn parse_break(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        let break_span = self.peek_span();
        self.expect(Token::KwBreak)?;

        let expr = if self.check(Token::Semi) || self.check(Token::RBrace) {
            None
        } else {
            Some(self.parse_expr()?)
        };

        if self.check(Token::Semi) {
            self.advance();
        }

        Ok(ast::Stmt::Break(expr, break_span))
    }

    fn parse_continue(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        let continue_span = self.peek_span();
        self.expect(Token::KwContinue)?;

        if self.check(Token::Semi) {
            self.advance();
        }

        Ok(ast::Stmt::Continue(continue_span))
    }

    fn parse_if(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
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

    fn parse_if_expr(&mut self, if_span: Span) -> Result<ast::Expr, Diagnostic<FileId>> {
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

    fn parse_let(&mut self, expect_semi: bool) -> Result<ast::Stmt, Diagnostic<FileId>> {
        let let_span = self.previous().map(|(_, s)| *s).unwrap();

        let mut idents = vec![];
        loop {
            let token = self.advance().cloned();
            let (ident, span) = match token.as_ref() {
                Some((Token::Ident(name), sp)) => (name.clone(), *sp),
                Some((_, sp)) => return self.error("Expected identifier", *sp),
                None => return self.error("Expected identifier", Span::new(0, 0)),
            };
            idents.push((ident, span));

            if !self.check(Token::Comma) {
                break;
            }
            self.advance();
        }

        let type_annot = if self.check(Token::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(Token::Eq)?;

        let mut exprs = vec![];
        loop {
            exprs.push(self.parse_expr()?);
            if !self.check(Token::Comma) {
                break;
            }
            self.advance();
        }

        if idents.len() != exprs.len() {
            return self.error(
                &format!("Expected {} expressions, got {}", idents.len(), exprs.len()),
                Span::new(idents[0].1.start(), exprs.last().unwrap().span().end()),
            );
        }

        let mut stmts = vec![];
        for ((ident, span), expr) in idents.into_iter().zip(exprs) {
            match &expr {
                ast::Expr::Int(..) => ast::Type::I32,
                ast::Expr::Str(..) => ast::Type::String,
                ast::Expr::Bool(..) => ast::Type::Bool,
                _ => ast::Type::Unknown,
            };
            stmts.push(ast::Stmt::Let(
                ident,
                type_annot.clone(),
                expr,
                span,
                ast::Visibility::Private,
            ));
        }

        if expect_semi {
            self.expect(Token::Semi)?;
        }

        let_span.start();
        let end = if expect_semi {
            self.previous()
                .map(|(_, s)| s.end())
                .unwrap_or_else(|| let_span.end())
        } else {
            stmts
                .last()
                .map(|s| s.span().end())
                .unwrap_or_else(|| let_span.end())
        };

        Ok(ast::Stmt::Block(stmts, Span::new(let_span.start(), end)))
    }
    fn parse_atom(&mut self) -> Result<ast::Expr, Diagnostic<FileId>> {
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
                Ok(ast::Expr::New(struct_name.clone(), args, ast::ExprInfo {
                    span,
                    ty: ast::Type::Struct(struct_name),
                    is_tail: false,
                }))
            }
            Some((Token::Int(s), span)) => {
                match s.parse::<i32>() {
                    Ok(val) => Ok(ast::Expr::Int(val, ast::ExprInfo {
                        span,
                        ty: ast::Type::I32,
                        is_tail: false,
                    })),
                    Err(_) => match s.parse::<i64>() {
                        Ok(val) => Ok(ast::Expr::Int64(val, ast::ExprInfo {
                            span,
                            ty: ast::Type::I64,
                            is_tail: false,
                        })),
                        Err(_) => self.error(&format!("Integer literal '{}' is out of range", s), span)
                    }
                }
            },
            Some((Token::Str(s), span)) => Ok(ast::Expr::Str(s, ast::ExprInfo {
                span,
                ty: ast::Type::String,
                is_tail: false,
            })),
            Some((Token::TemplateStr(s), span)) => self.parse_template_string(s, span),
            Some((Token::KwTrue, span)) => Ok(ast::Expr::Bool(true, ast::ExprInfo {
                span,
                ty: ast::Type::Bool,
                is_tail: false,
            })),
            Some((Token::KwFalse, span)) => Ok(ast::Expr::Bool(false, ast::ExprInfo {
                span,
                ty: ast::Type::Bool,
                is_tail: false,
            })),
            Some((Token::F32(val), span)) => Ok(ast::Expr::F32(val, ast::ExprInfo {
                span,
                ty: ast::Type::F32,
                is_tail: false,
            })),
            Some((Token::KwNone, span)) => Ok(ast::Expr::None(ast::ExprInfo {
                span,
                ty: ast::Type::NoneType,
                is_tail: false,
            })),
            Some((Token::DotDot, span)) => {
                if let Some((next_token, _)) = self.tokens.peek() {
                    if matches!(next_token, Token::Int(_) | Token::Ident(_)) {
                        let rhs = self.parse_expr()?;
                        let end_span = rhs.span();
                        Ok(ast::Expr::Range(
                            Box::new(ast::Expr::InfiniteRange(ast::RangeType::Infinite, ast::ExprInfo {
                                span: Span::new(span.start(), span.start()),
                                ty: ast::Type::Unknown,
                                is_tail: false,
                            })),
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
                            }
                        ))
                    }
                } else {
                    Ok(ast::Expr::InfiniteRange(
                        ast::RangeType::Infinite,
                        ast::ExprInfo {
                            span,
                            ty: ast::Type::Unknown,
                            is_tail: false,
                        }
                    ))
                }
            },
            Some((Token::DotDotGt, span)) => Ok(ast::Expr::InfiniteRange(
                ast::RangeType::InfiniteUp,
                ast::ExprInfo {
                    span,
                    ty: ast::Type::Unknown,
                    is_tail: false,
                }
            )),
            Some((Token::DotDotLt, span)) => Ok(ast::Expr::InfiniteRange(
                ast::RangeType::InfiniteDown,
                ast::ExprInfo {
                    span,
                    ty: ast::Type::Unknown,
                    is_tail: false,
                }
            )),
            Some((Token::LParen, span_start)) => {
                let expr = self.parse_expr()?;
                let span_end = self.expect(Token::RParen)?;
                let _span = Span::new(span_start.start(), span_end.end());
                Ok(expr)
            }
            Some((Token::Ident(name), span)) => {
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
                    Ok(ast::Expr::Var(name, ast::ExprInfo {
                        span,
                        ty: ast::Type::Unknown,
                        is_tail: false,
                    }))
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
                Ok(ast::Expr::ArrayInit(elements, ast::ExprInfo {
                    span: Span::new(span.start(), end_span.end()),
                    ty: ast::Type::Unknown,
                    is_tail: false,
                }))
            }
            Some((Token::EmptyArray, span)) => {
                Ok(ast::Expr::ArrayInit(Vec::new(), ast::ExprInfo {
                    span,
                    ty: ast::Type::Unknown,
                    is_tail: false,
                }))
            }
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

    fn parse_function_call(
        &mut self,
        name: String,
        span: Span,
    ) -> Result<ast::Expr, Diagnostic<FileId>> {
        self.expect(Token::LParen)?;
        let mut args = Vec::new();
        if !self.check(Token::RParen) {
            while !self.check(Token::RParen) {
                args.push(self.parse_expr()?);
                if !self.check(Token::Comma) {
                    break;
                }
                self.advance();
            }
        }
        let rparen_span = self.expect(Token::RParen)?;

        Ok(ast::Expr::Call(name, args, ast::ExprInfo {
            span: Span::new(span.start(), rparen_span.end()),
            ty: ast::Type::Unknown,
            is_tail: false,
        }))
    }

    fn parse_match(&mut self, start_span: Span) -> Result<ast::Expr, Diagnostic<FileId>> {
        let expr = self.parse_pattern()?;
        self.expect(Token::LBrace)?;

        let mut arms = Vec::new();
        while !self.check(Token::RBrace) {
            arms.push(self.parse_match_arm()?);

            if self.check(Token::Comma) {
                self.advance();
            }
        }

        let end_span = self.expect(Token::RBrace)?;

        Ok(ast::Expr::Match(Box::new(expr), arms, ast::ExprInfo {
            span: Span::new(start_span.start(), end_span.end()),
            ty: ast::Type::Unknown,
            is_tail: false,
        }))
    }
    fn is_at_end(&mut self) -> bool {
        self.tokens.peek().is_none()
    }
    fn check(&mut self, token: Token) -> bool {
        matches!(self.tokens.peek(), Some((t, _)) if *t == token)
    }
    fn advance(&mut self) -> Option<&(Token, Span)> {
        if let Some(token) = self.tokens.next() {
            self.previous_token = Some(token.clone());
            Some(token)
        } else {
            None
        }
    }

    fn previous(&self) -> Option<&(Token, Span)> {
        self.previous_token.as_ref()
    }

    fn peek(&mut self) -> Option<&(Token, Span)> {
        self.tokens.peek().map(|x| *x)
    }
    fn expect(&mut self, token: Token) -> Result<Span, Diagnostic<FileId>> {
        if self.check(token.clone()) {
            let span = self.tokens.peek().map(|(_, s)| *s).unwrap();
            self.advance();
            Ok(span)
        } else {
            if matches!(token, Token::Semi) {
                if let Some((_, prev_span)) = self.previous() {
                    use codespan::ByteIndex;
                    let end_pos = prev_span.end().0 as u32;
                    let error_span = Span::new(prev_span.end(), ByteIndex::from(end_pos + 1));
                    return self.error(&format!("Expected {:?}", token), error_span);
                }
            }

            let span = self
                .tokens
                .peek()
                .map(|(_, s)| *s)
                .unwrap_or(Span::new(0, 0));
            self.error(&format!("Expected {:?}", token), span)
        }
    }

    fn error<T>(&self, message: &str, span: Span) -> Result<T, Diagnostic<FileId>> {
        Err(Diagnostic::error().with_message(message).with_labels(vec![
            codespan_reporting::diagnostic::Label::primary(self.file_id, span),
        ]))
    }





    fn can_start_struct_init(&mut self) -> bool {
        let mut temp_tokens = self.tokens.clone();
        if let Some((Token::LBrace, _)) = temp_tokens.peek() {
            temp_tokens.next();

            if let Some((Token::Ident(_), _)) = temp_tokens.peek() {
                temp_tokens.next();
                if let Some((Token::Colon, _)) = temp_tokens.peek() {
                    return true;
                }
            }
        }

        false
    }

    fn parse_template_string(
        &mut self,
        content: String,
        span: Span,
    ) -> Result<ast::Expr, Diagnostic<FileId>> {
        let mut parts = Vec::new();
        let mut current = String::new();
        let mut i = 0;
        let chars: Vec<char> = content.chars().collect();

        while i < chars.len() {
            let c = chars[i];
            i += 1;

            if c == '{' {
                if !current.is_empty() {
                    parts.push(ast::TemplateStrPart::Literal(std::mem::take(&mut current)));
                }

                let expr_start = i;
                let mut brace_count = 1;

                while i < chars.len() {
                    let c = chars[i];
                    i += 1;

                    if c == '{' {
                        brace_count += 1;
                    } else if c == '}' {
                        brace_count -= 1;
                        if brace_count == 0 {
                            break;
                        }
                    }
                }

                let expr_end = i - 1;
                let expr_text: String = chars[expr_start..expr_end].iter().collect();

                let expr = self.parse_interpolated_expr(&expr_text, span)?;
                parts.push(ast::TemplateStrPart::Expression(Box::new(expr)));
            } else {
                current.push(c);
            }
        }

        if !current.is_empty() {
            parts.push(ast::TemplateStrPart::Literal(current));
        }

        Ok(ast::Expr::TemplateStr(parts, ast::ExprInfo {
            span,
            ty: ast::Type::String,
            is_tail: false,
        }))
    }
    fn parse_interpolated_expr(
        &mut self,
        expr_text: &str,
        span: Span,
    ) -> Result<ast::Expr, Diagnostic<FileId>> {
        let mut temp_files = codespan::Files::new();
        let temp_file_id = temp_files.add("interpolation".to_string(), expr_text.to_string());
        let temp_lexer = super::lexer::Lexer::new(&temp_files, temp_file_id);

        let mut sub_parser = Parser::new(temp_lexer);

        if sub_parser.is_at_end() {
            return self.error("Empty interpolation expression", span);
        }

        let expr = sub_parser.parse_expr()?;

        if !sub_parser.is_at_end() {
            return self.error("Unexpected tokens after expression in interpolation", span);
        }

        Ok(expr)
    }

    fn parse_expr_tail(&mut self) -> Result<ast::Expr, Diagnostic<FileId>> {
        let mut expr = self.parse_expr()?;
        self.mark_as_tail(&mut expr);
        Ok(expr)
    }




    fn parse_match_arm(&mut self) -> Result<ast::MatchArm, Diagnostic<FileId>> {
        let pattern = self.parse_pattern()?;

        let guard = if self.check(Token::KwIf) {
            self.consume(Token::KwIf, "Expected 'if'")?;
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.expect(Token::Arrow2)?;

        let (body, body_span) = if self.check(Token::LBrace) {
            let stmts = self.parse_block()?;
            let span = stmts
                .last()
                .map(|s| s.span())
                .unwrap_or_else(|| self.previous().map(|(_, s)| *s).unwrap());
            (ast::MatchArmBody::Block(stmts), span)
        } else {
            let expr = self.parse_expr()?;
            let span = expr.span();
            (ast::MatchArmBody::Expr(expr), span)
        };

        let pattern_span = pattern.span();

        Ok(ast::MatchArm {
            pattern,
            guard,
            body,
            span: Span::new(pattern_span.start(), body_span.end()),
        })
    }

    fn parse_pattern(&mut self) -> Result<ast::Pattern, Diagnostic<FileId>> {
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
                        ast::Expr::Int(val, ast::ExprInfo {
                            span,
                            ty: ast::Type::I32,
                            is_tail: false,
                        }),
                        span,
                    )),
                    Err(_) => self.error("Integer literal out of range for pattern", span)
                }
            }
            Some((Token::Str(s), span)) => {
                self.advance();
                Ok(ast::Pattern::Literal(
                    ast::Expr::Str(s, ast::ExprInfo {
                        span,
                        ty: ast::Type::String,
                        is_tail: false,
                    }),
                    span,
                ))
            }
            Some((Token::KwTrue, span)) => {
                self.advance();
                Ok(ast::Pattern::Literal(
                    ast::Expr::Bool(true, ast::ExprInfo {
                        span,
                        ty: ast::Type::Bool,
                        is_tail: false,
                    }),
                    span,
                ))
            }
            Some((Token::KwFalse, span)) => {
                self.advance();
                Ok(ast::Pattern::Literal(
                    ast::Expr::Bool(false, ast::ExprInfo {
                        span,
                        ty: ast::Type::Bool,
                        is_tail: false,
                    }),
                    span,
                ))
            }
            Some((Token::KwNone, span)) => Ok(ast::Pattern::Literal(ast::Expr::None(ast::ExprInfo {
                span,
                ty: ast::Type::NoneType,
                is_tail: false,
            }), span)),
            _ => {
                let (_, span) = self.advance().unwrap();
                let span = *span;
                self.error("Expected pattern", span)
            }
        }
    }

    fn parse_loop_expr(&mut self, loop_span: Span) -> Result<ast::Expr, Diagnostic<FileId>> {
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

    fn parse_impl(&mut self) -> Result<ast::ImplBlock, Diagnostic<FileId>> {
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

    fn parse_constructor(&mut self) -> Result<ast::Function, Diagnostic<FileId>> {
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
