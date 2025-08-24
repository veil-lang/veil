use crate::parser::Parser;
use crate::{ast, lexer};
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;

impl<'a> Parser<'a> {
    pub fn parse_template_string(
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

        Ok(ast::Expr::TemplateStr(
            parts,
            ast::ExprInfo {
                span,
                ty: ast::Type::String,
                is_tail: false,
            },
        ))
    }
    pub fn parse_interpolated_expr(
        &mut self,
        expr_text: &str,
        span: Span,
    ) -> Result<ast::Expr, Diagnostic<FileId>> {
        let mut temp_files = codespan::Files::new();
        let temp_file_id = temp_files.add("interpolation".to_string(), expr_text.to_string());
        let temp_lexer = lexer::Lexer::new(&temp_files, temp_file_id);

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
}
