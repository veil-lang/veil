use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic;
use crate::ast;
use crate::parser::Precedence;

mod pattern;
mod match_;
mod if_;
mod loop_;
mod atom;
mod prefix;
mod infix;
mod call;
mod template;

impl<'a> super::Parser<'a> {
    pub fn parse_expr(&mut self) -> Result<ast::Expr, Diagnostic<FileId>> {
        self.parse_expr_bp(0)
    }
    pub fn parse_expr_bp(&mut self, min_bp: Precedence) -> Result<ast::Expr, Diagnostic<FileId>> {
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
}