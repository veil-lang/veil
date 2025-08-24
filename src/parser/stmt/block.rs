use crate::parser::Parser;
use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic;
use crate::ast;
use crate::lexer::Token;

impl<'a> Parser<'a> {
    pub fn parse_block(&mut self) -> Result<Vec<ast::Stmt>, Diagnostic<FileId>> {
        self.expect(Token::LBrace)?;
        let mut stmts = Vec::new();

        while !self.check(Token::RBrace) {
            if self.is_last_expr_in_block() {
                let expr = self.parse_expr_tail()?;
                let span = expr.span();
                if self.check(Token::Semi) {
                    self.advance();
                }
                stmts.push(ast::Stmt::Expr(expr, span));
            } else {
                stmts.push(self.parse_stmt()?);
            }
        }

        self.expect(Token::RBrace)?;
        Ok(stmts)
    }

    pub fn parse_block_with_tail(&mut self, allow_tail: bool) -> Result<Vec<ast::Stmt>, Diagnostic<FileId>> {
        self.expect(Token::LBrace)?;
        let mut stmts = Vec::new();
        while !self.check(Token::RBrace) {
            if self.is_last_expr_in_block() && allow_tail {
                let expr = self.parse_expr_tail()?;
                let span = expr.span();
                if self.check(Token::Semi) {
                    let semi_span = self.peek_span();
                    return self.error("Cannot use ';' after return expression", semi_span);
                }
                stmts.push(ast::Stmt::Expr(expr, span));
            } else {
                stmts.push(self.parse_stmt()?);
            }
        }
        self.expect(Token::RBrace)?;
        Ok(stmts)
    }


    pub fn is_last_expr_in_block(&mut self) -> bool {
        let mut temp_tokens = self.tokens.clone();
        let mut depth = 0;
        let mut found_expr = false;

        while let Some((token, _)) = temp_tokens.peek() {
            match token {
                Token::KwLet | Token::KwReturn | Token::KwWhile | Token::KwFor | Token::KwBreak | Token::KwContinue => {
                    return false;
                }
                Token::LBrace => {
                    depth += 1;
                    temp_tokens.next();
                }
                Token::RBrace => {
                    if depth == 0 {
                        return found_expr;
                    }
                    depth -= 1;
                    temp_tokens.next();
                }
                Token::Semi => {
                    temp_tokens.next();
                    if let Some((Token::RBrace, _)) = temp_tokens.peek() {
                        return found_expr;
                    }
                    return false;
                }
                _ => {
                    found_expr = true;
                    temp_tokens.next();
                }
            }
        }

        found_expr
    }

    pub fn mark_as_tail(&self, expr: &mut ast::Expr) {
        match expr {
            ast::Expr::Int(_, info) => info.is_tail = true,
            ast::Expr::Int64(_, info) => info.is_tail = true,
            ast::Expr::Bool(_, info) => info.is_tail = true,
            ast::Expr::Str(_, info) => info.is_tail = true,
            ast::Expr::Var(_, info) => info.is_tail = true,
            ast::Expr::BinOp(_, _, _, info) => info.is_tail = true,
            ast::Expr::UnaryOp(_, _, info) => info.is_tail = true,
            ast::Expr::Call(_, _, info) => info.is_tail = true,
            ast::Expr::New(_, _, info) => info.is_tail = true,
            ast::Expr::Cast(_, _, info) => info.is_tail = true,
            ast::Expr::SafeBlock(_, info) => info.is_tail = true,
            ast::Expr::Deref(_, info) => info.is_tail = true,
            ast::Expr::Assign(_, _, info) => info.is_tail = true,
            ast::Expr::Range(_, _, _, info) => info.is_tail = true,
            ast::Expr::InfiniteRange(_, info) => info.is_tail = true,
            ast::Expr::StructInit(_, _, info) => info.is_tail = true,
            ast::Expr::FieldAccess(_, _, info) => info.is_tail = true,
            ast::Expr::ArrayInit(_, info) => info.is_tail = true,
            ast::Expr::ArrayAccess(_, _, info) => info.is_tail = true,
            ast::Expr::TemplateStr(_, info) => info.is_tail = true,
            ast::Expr::F32(_, info) => info.is_tail = true,
            ast::Expr::FfiCall(_, _, info) => info.is_tail = true,
            ast::Expr::EnumConstruct(_, _, _, info) => info.is_tail = true,
            ast::Expr::Match(_, _, info) => info.is_tail = true,
            ast::Expr::If(_, _, _, info) => info.is_tail = true,
            ast::Expr::Loop(_, info) => info.is_tail = true,
            ast::Expr::Void(info) => info.is_tail = true,
            ast::Expr::Spread(_, info) => info.is_tail = true,
            ast::Expr::None(info) => info.is_tail = true,
        }
    }
}