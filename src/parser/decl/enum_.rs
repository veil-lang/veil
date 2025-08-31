use crate::ast;
use crate::lexer::Token;
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;

impl<'a> super::super::Parser<'a> {
    pub fn parse_enum(&mut self) -> Result<ast::EnumDef, Diagnostic<FileId>> {
        let start_span = self.consume(Token::KwEnum, "Expected 'enum'")?;
        let (name, _) = self.consume_ident()?;
        let generic_params = self.parse_generic_params()?;

        self.expect(Token::LBrace)?;

        let mut variants = Vec::new();
        while !self.check(Token::RBrace) {
            let (variant_name, variant_span) = self.consume_ident()?;

            let value = if self.check(Token::Eq) {
                self.advance();
                if let Some((Token::Int(int_str), _)) = self.advance() {
                    match int_str.parse::<i32>() {
                        Ok(val) => Some(val),
                        Err(_) => {
                            return self
                                .error("Invalid integer value for enum variant", variant_span);
                        }
                    }
                } else {
                    return self.error(
                        "Expected integer value after '=' in enum variant",
                        variant_span,
                    );
                }
            } else {
                None
            };

            let data = if self.check(Token::LParen) {
                self.advance();
                let mut types = Vec::new();

                if !self.check(Token::RParen) {
                    loop {
                        types.push(self.parse_type()?);
                        if !self.check(Token::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }

                self.expect(Token::RParen)?;
                Some(crate::ast::EnumVariantData::Tuple(types))
            } else if self.check(Token::LBrace) {
                self.advance();
                let mut fields = Vec::new();
                if !self.check(Token::RBrace) {
                    loop {
                        let (field_name, field_span) = self.consume_ident()?;
                        self.expect(Token::Colon)?;
                        let ty = self.parse_type()?;
                        fields.push(crate::ast::StructField {
                            name: field_name,
                            ty,
                            span: field_span,
                        });

                        if !self.check(Token::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                self.expect(Token::RBrace)?;
                Some(crate::ast::EnumVariantData::Struct(fields))
            } else {
                None
            };

            variants.push(ast::EnumVariant {
                name: variant_name,
                data,
                value,
                span: variant_span,
            });

            if !self.check(Token::RBrace) {
                self.expect(Token::Comma)?;
            }
        }

        let end_span = self.expect(Token::RBrace)?;
        Ok(ast::EnumDef {
            name,
            generic_params,
            variants,
            span: Span::new(start_span.start(), end_span.end()),
            visibility: ast::Visibility::Private,
        })
    }
}
