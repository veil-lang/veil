use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic;
use crate::ast;
use crate::lexer::Token;

impl<'a> super::Parser<'a> {
    pub fn parse_import(&mut self) -> Result<ast::ImportDeclaration, Diagnostic<FileId>> {
        self.consume(Token::KwImport, "Expected 'import'")?;

        match self.peek_token() {
            Token::Ident(_) => {
                let module_path = self.parse_module_path()?;

                let module_type = if module_path.starts_with("std/") {
                    ast::ModuleType::Standard
                } else if module_path.starts_with("./") || module_path.starts_with("../") {
                    ast::ModuleType::Local
                } else {
                    ast::ModuleType::External
                };

                let alias = if self.check(Token::KwAs) {
                    self.advance();
                    match self.consume_ident()? {
                        (name, _) => Some(name),
                    }
                } else {
                    None
                };

                self.expect(Token::Semi)?;
                Ok(ast::ImportDeclaration::ImportAll {
                    module_path,
                    module_type,
                    alias,
                })
            }
            Token::LBrace => {
                self.parse_import_specifiers()
            }
            _ => {
                let span = self.peek_span();
                self.error("Invalid import statement", span)
            }
        }
    }

    pub fn parse_export_block(&mut self, program: &mut ast::Program) -> Result<(), Diagnostic<FileId>> {
        self.expect(Token::LBrace)?;
        while !self.check(Token::RBrace) {
            if self.check(Token::KwFn) {
                let mut func = self.parse_function()?;
                func.visibility = ast::Visibility::Public;
                program.functions.push(func);
            } else if self.check(Token::KwStruct) {
                let mut struct_def = self.parse_struct()?;
                struct_def.visibility = ast::Visibility::Public;
                program.structs.push(struct_def);
            } else {
                let span = self.peek_span();
                return self.error("Expected 'fn' or 'struct' in export block", span);
            }

            if self.check(Token::Comma) {
                self.advance();
            }
        }

        self.expect(Token::RBrace)?;
        Ok(())
    }





    pub fn parse_module_path(&mut self) -> Result<String, Diagnostic<FileId>> {
        let mut parts = vec![];

        loop {
            match self.peek_token() {
                Token::Ident(name) => {
                    parts.push(name.clone());
                    self.advance();
                }
                _ => break,
            }

            if self.check(Token::Slash) {
                self.advance();
            } else {
                break;
            }
        }

        if parts.is_empty() {
            let span = self.peek_span();
            return self.error("Expected module path", span);
        }

        Ok(parts.join("/"))
    }


    pub fn parse_import_all(&mut self) -> Result<ast::ImportDeclaration, Diagnostic<FileId>> {
        let module_path = self.parse_module_path()?;

        let module_type = if module_path.starts_with("std/") {
            ast::ModuleType::Standard
        } else if module_path.starts_with("./") || module_path.starts_with("../") {
            ast::ModuleType::Local
        } else {
            ast::ModuleType::External
        };

        let alias = if self.check(Token::KwAs) {
            self.advance();
            match self.consume_ident()? {
                (name, _) => Some(name),
            }
        } else {
            None
        };

        Ok(ast::ImportDeclaration::ImportAll {
            module_path,
            module_type,
            alias,
        })
    }

    pub fn parse_import_specifiers(&mut self) -> Result<ast::ImportDeclaration, Diagnostic<FileId>> {
        self.expect(Token::LBrace)?;
        let mut specifiers = Vec::new();

        loop {
            let (name, alias) = self.parse_import_specifier_item()?;
            specifiers.push(ast::ImportSpecifier { name, alias });

            if !self.check(Token::Comma) {
                break;
            }
            self.advance();
        }
        self.expect(Token::RBrace)?;
        self.expect(Token::KwFrom)?;
        let module_path = self.parse_module_path()?;

        let module_type = if module_path.starts_with("std/") {
            ast::ModuleType::Standard
        } else if module_path.starts_with("./") || module_path.starts_with("../") {
            ast::ModuleType::Local
        } else {
            ast::ModuleType::External
        };

        Ok(ast::ImportDeclaration::ImportSpecifiers {
            module_path,
            module_type,
            specifiers,
        })
    }


    pub fn parse_import_specifier_item(
        &mut self,
    ) -> Result<(String, Option<String>), Diagnostic<FileId>> {
        let name = match self.consume_ident()? {
            (name, _) => name,
        };

        let alias = if self.check(Token::KwAs) {
            self.advance();
            match self.consume_ident()? {
                (alias, _) => Some(alias),
            }
        } else {
            None
        };

        Ok((name, alias))
    }

    pub fn parse_export_import(&mut self) -> Result<ast::ImportDeclaration, Diagnostic<FileId>> {
        self.expect(Token::KwImport)?;

        let result = if self.check(Token::Star) {
            self.advance();

            self.expect(Token::KwFrom)?;
            let import_all = self.parse_import_all()?;
            match import_all {
                ast::ImportDeclaration::ImportAll { module_path, module_type, alias } => {
                    ast::ImportDeclaration::ExportImportAll {
                        module_path,
                        module_type,
                        alias,
                    }
                }
                _ => unreachable!(),
            }
        } else if self.check(Token::LBrace) {
            let import_specifiers = self.parse_import_specifiers()?;
            match import_specifiers {
                ast::ImportDeclaration::ImportSpecifiers { module_path, module_type, specifiers } => {
                    ast::ImportDeclaration::ExportImportSpecifiers {
                        module_path,
                        module_type,
                        specifiers,
                    }
                }
                _ => unreachable!(),
            }
        } else {
            let import_all = self.parse_import_all()?;
            match import_all {
                ast::ImportDeclaration::ImportAll { module_path, module_type, alias } => {
                    ast::ImportDeclaration::ExportImportAll {
                        module_path,
                        module_type,
                        alias,
                    }
                }
                _ => unreachable!(),
            }
        };

        self.expect(Token::Semi)?;
        Ok(result)
    }
}