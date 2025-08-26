use crate::ast;
use crate::lexer::Token;
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;

impl<'a> super::Parser<'a> {
    pub fn parse_type(&mut self) -> Result<ast::Type, Diagnostic<FileId>> {
        let mut current_type = self.parse_base_type()?;

        loop {
            if self.check(Token::Question) {
                self.advance();
                current_type = ast::Type::Optional(Box::new(current_type));
            } else if self.check(Token::EmptyArray) {
                self.advance();
                current_type = ast::Type::Array(Box::new(current_type));
            } else if self.check(Token::LBracket) {
                self.advance();
                if self.check(Token::RBracket) {
                    self.advance();
                    current_type = ast::Type::Array(Box::new(current_type));
                } else {
                    let size_token = self.advance().cloned();
                    match size_token {
                        Some((Token::Int(size), span)) => {
                            self.expect(Token::RBracket)?;
                            match size.parse::<usize>() {
                                Ok(size_val) => {
                                    current_type =
                                        ast::Type::SizedArray(Box::new(current_type), size_val);
                                }
                                Err(_) => return self.error("Invalid array size", span),
                            }
                        }
                        _ => {
                            let span = self.peek_span();
                            return self.error("Expected array size (integer) or ']'", span);
                        }
                    }
                }
            } else {
                break;
            }
        }

        Ok(current_type)
    }

    pub fn parse_base_type(&mut self) -> Result<ast::Type, Diagnostic<FileId>> {
        let next = self.advance().map(|(t, s)| (t.clone(), *s));
        match next {
            Some((Token::Ellipsis, _)) => Ok(ast::Type::Ellipsis),
            Some((Token::TyI8, _)) => Ok(ast::Type::I8),
            Some((Token::TyI16, _)) => Ok(ast::Type::I16),
            Some((Token::TyI32, _)) => Ok(ast::Type::I32),
            Some((Token::TyI64, _)) => Ok(ast::Type::I64),
            Some((Token::TyU8, _)) => Ok(ast::Type::U8),
            Some((Token::TyU16, _)) => Ok(ast::Type::U16),
            Some((Token::TyU32, _)) => Ok(ast::Type::U32),
            Some((Token::TyU64, _)) => Ok(ast::Type::U64),
            Some((Token::TyF32, _)) => Ok(ast::Type::F32),
            Some((Token::TyF64, _)) => Ok(ast::Type::F64),
            Some((Token::TyBool, _)) => Ok(ast::Type::Bool),
            Some((Token::TyString, _)) => Ok(ast::Type::String),
            Some((Token::TyVoid, _)) => Ok(ast::Type::Void),
            Some((Token::TyAny, _)) => Ok(ast::Type::Any),
            Some((Token::KwRawPtr, _)) => Ok(ast::Type::RawPtr),
            Some((Token::Star, _)) => {
                let target_type = self.parse_base_type()?;
                Ok(ast::Type::Pointer(Box::new(target_type)))
            }
            Some((Token::EmptyArray, _)) => {
                let element_type = self.parse_base_type()?;
                Ok(ast::Type::Array(Box::new(element_type)))
            }
            Some((Token::LBracket, _)) => {
                let element_type = self.parse_base_type()?;
                if self.check(Token::Semi) {
                    self.advance();

                    let size_token = self.advance().cloned();

                    match size_token {
                        Some((Token::Int(size), span)) => {
                            self.expect(Token::RBracket)?;
                            match size.parse::<usize>() {
                                Ok(size_val) => {
                                    Ok(ast::Type::SizedArray(Box::new(element_type), size_val))
                                }
                                Err(_) => self.error("Invalid array size", span),
                            }
                        }
                        _ => {
                            let span = self.peek_span();
                            self.error("Expected array size (integer)", span)
                        }
                    }
                } else {
                    self.expect(Token::RBracket)?;
                    Ok(ast::Type::Array(Box::new(element_type)))
                }
            }
            Some((Token::Ident(name), _)) => {
                if self.check(Token::Lt) {
                    self.advance();
                    let mut generic_args = Vec::new();

                    if !self.check(Token::Gt) {
                        loop {
                            generic_args.push(self.parse_base_type()?);

                            if !self.check(Token::Comma) {
                                break;
                            }
                            self.advance();
                        }
                    }

                    self.expect(Token::Gt)?;
                    Ok(ast::Type::GenericInstance(name, generic_args))
                } else {
                    // Jeśli identyfikator odpowiada znanemu enumowi, traktuj jako Enum.
                    if self.enums.iter().any(|e| e.name == name) {
                        Ok(ast::Type::Enum(name))
                    } else if name.chars().next().unwrap_or('a').is_uppercase() {
                        // Wielka litera bez dopasowania do enum -> Generic.
                        Ok(ast::Type::Generic(name))
                    } else {
                        Ok(ast::Type::Struct(name))
                    }
                }
            }
            Some((_, span)) => self.error("Expected type annotation", span),
            None => self.error("Expected type annotation", Span::new(0, 0)),
        }
    }
}
