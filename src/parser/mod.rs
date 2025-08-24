mod types;
mod precedence;
mod stmt;
mod ffi;
mod import_export;
mod decl;
mod expr;

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
}
