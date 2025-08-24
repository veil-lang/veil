use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;
use crate::ast;
use crate::lexer::Token;

impl<'a> super::super::Parser<'a> {
    pub fn parse_struct(&mut self) -> Result<ast::StructDef, Diagnostic<FileId>> {
        let start_span = self.consume(Token::KwStruct, "Expected 'struct'")?;
        let (name, _) = self.consume_ident()?;
        let generic_params = self.parse_generic_params()?;

        self.expect(Token::LBrace)?;

        let mut fields = Vec::new();
        while !self.check(Token::RBrace) {
            let (field_name, field_span) = self.consume_ident()?;
            self.expect(Token::Colon)?;
            let field_type = self.parse_type()?;

            fields.push(ast::StructField {
                name: field_name,
                ty: field_type,
                span: field_span,
            });

            if !self.check(Token::RBrace) {
                self.expect(Token::Comma)?;
            }
        }

        let end_span = self.expect(Token::RBrace)?;
        Ok(ast::StructDef {
            name,
            generic_params,
            fields,
            span: Span::new(start_span.start(), end_span.end()),
            visibility: ast::Visibility::Private,
            repr: None,
        })
    }
}