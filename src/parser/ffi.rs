use std::collections::HashMap;
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;
use crate::ast;
use crate::lexer::Token;


pub enum ForeignItem {
    Function(ast::FfiFunction),
    Variable(ast::FfiVariable),
}



impl<'a> super::Parser<'a> {

    pub fn parse_metadata(&mut self) -> Result<Option<HashMap<String, String>>, Diagnostic<FileId>> {
        self.expect(Token::LBracket)?;
        let mut metadata = HashMap::new();

        loop {
            let (name, _) = self.consume_ident()?;

            if self.check(Token::Eq) {
                self.advance();

                let value = match self.advance().cloned() {
                    Some((Token::Str(s), _)) => s,
                    Some((Token::Ident(s), _)) => s,
                    Some((Token::KwTrue, _)) => "true".to_string(),
                    Some((Token::KwFalse, _)) => "false".to_string(),
                    Some((_, span)) => return self.error("Expected string or bool value", span),
                    None => return self.error("Expected value after '='", Span::new(0, 0)),
                };

                metadata.insert(name, value);
            } else {
                metadata.insert(name, "true".to_string());
            }

            if self.check(Token::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        self.expect(Token::RBracket)?;

        if metadata.is_empty() {
            Ok(None)
        } else {
            Ok(Some(metadata))
        }
    }


    pub fn parse_ffi(
        &mut self,
        metadata: Option<HashMap<String, String>>,
    ) -> Result<ForeignItem, Diagnostic<FileId>> {
        if self.check(Token::Hash) {
            self.advance();
        }

        if self.check(Token::KwFn) {
            self.consume(Token::KwFn, "Expected 'fn'")?;
            let (name, _) = self.consume_ident()?;
            let params = self
                .parse_parameters()?
                .into_iter()
                .map(|(_name, ty)| ty)
                .collect();
            let return_type = if self.check(Token::Arrow) {
                self.advance();
                self.parse_type()?
            } else {
                ast::Type::Void
            };
            self.expect(Token::Semi)?;
            Ok(ForeignItem::Function(ast::FfiFunction {
                name,
                params,
                return_type,
                metadata,
            }))
        } else if self.check(Token::KwVar) {
            self.consume(Token::KwVar, "Expected 'var'")?;
            let (name, _) = self.consume_ident()?;
            self.expect(Token::Colon)?;
            let ty = self.parse_type()?;
            self.expect(Token::Semi)?;
            Ok(ForeignItem::Variable(ast::FfiVariable {
                name,
                ty,
                metadata,
            }))
        } else {
            let span = self.peek_span();
            return self.error("Expected 'fn' or 'var' in foreign block", span);
        }
    }

}