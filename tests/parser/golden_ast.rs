use codespan::Files;
use veil_ast as ast;
use veil_syntax;

/// Render a stable, human-readable snapshot of the AST that is resilient to span/type detail changes.
/// This focuses on structural elements: imports, functions, tests, structs, enums, FFI.
fn render_program(p: &ast::Program) -> String {
    let mut out = String::new();

    // Functions
    out.push_str("Functions:\n");
    for f in &p.functions {
        let params = f
            .params
            .iter()
            .map(|(name, _ty)| name.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        if params.is_empty() {
            out.push_str(&format!("- {}\n", f.name));
        } else {
            out.push_str(&format!("- {}({})\n", f.name, params));
        }
    }

    // Tests
    out.push_str("Tests:\n");
    for t in &p.tests {
        out.push_str(&format!("- {}\n", t.name));
    }

    // Imports
    out.push_str("Imports:\n");
    for i in &p.imports {
        match i {
            ast::ImportDeclaration::ImportAll {
                module_path,
                module_type: _,
                alias,
            } => {
                if let Some(a) = alias {
                    out.push_str(&format!("- {} (ImportAll as {})\n", module_path, a));
                } else {
                    out.push_str(&format!("- {} (ImportAll)\n", module_path));
                }
            }
            ast::ImportDeclaration::ImportSpecifiers {
                module_path,
                module_type: _,
                specifiers,
            } => {
                let specs = specifiers
                    .iter()
                    .map(|s| {
                        if let Some(a) = &s.alias {
                            format!("{} as {}", s.name, a)
                        } else {
                            s.name.clone()
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                out.push_str(&format!(
                    "- {} (ImportSpecifiers [{}])\n",
                    module_path, specs
                ));
            }
            ast::ImportDeclaration::ExportImportAll {
                module_path,
                module_type: _,
                alias,
            } => {
                if let Some(a) = alias {
                    out.push_str(&format!("- {} (ExportImportAll as {})\n", module_path, a));
                } else {
                    out.push_str(&format!("- {} (ExportImportAll)\n", module_path));
                }
            }
            ast::ImportDeclaration::ExportImportSpecifiers {
                module_path,
                module_type: _,
                specifiers,
            } => {
                let specs = specifiers
                    .iter()
                    .map(|s| {
                        if let Some(a) = &s.alias {
                            format!("{} as {}", s.name, a)
                        } else {
                            s.name.clone()
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                out.push_str(&format!(
                    "- {} (ExportImportSpecifiers [{}])\n",
                    module_path, specs
                ));
            }
        }
    }

    // Structs
    out.push_str("Structs:\n");
    for s in &p.structs {
        out.push_str(&format!("- {}\n", s.name));
    }

    // Enums
    out.push_str("Enums:\n");
    for e in &p.enums {
        out.push_str(&format!("- {} ({} variants)\n", e.name, e.variants.len()));
    }

    // FFI
    out.push_str("FFI Functions:\n");
    for f in &p.ffi_functions {
        out.push_str(&format!("- {}\n", f.name));
    }
    out.push_str("FFI Variables:\n");
    for v in &p.ffi_variables {
        out.push_str(&format!("- {}\n", v.name));
    }

    out
}

#[test]
fn golden_ast_dump_smoke() {
    let mut files = Files::<String>::new();

    // Sample program that exercises: import, test, struct, enum, FFI, functions, params, returns.
    let src = r#"
        import std/prelude;

        test hello_world {
            return;
        }

        struct Point { x: i32, y: i32 }

        enum Color { Red, Green, Blue }

        # { link: "c" } foreign {
            fn puts(s: string);
            var errno: int;
        }

        fn add(a: i32, b: i32) -> i32 {
            return a + b;
        }

        fn main() {
            let x: i32 = 1;
        }
    "#;

    let file_id = files.add("sample.veil".to_string(), src.to_string());
    let program = veil_syntax::parse_ast(&files, file_id).expect("parse ok");

    let got = render_program(&program);

    // Expected stable snapshot. Update only if language structure semantics change.
    let expected = r#"Functions:
- add(a, b)
- main
Tests:
- hello_world
Imports:
- std/prelude (ImportAll)
Structs:
- Point
Enums:
- Color (3 variants)
FFI Functions:
- puts
FFI Variables:
- errno
"#;

    assert_eq!(got, expected, "AST snapshot mismatch:\nGot:\n{}\n", got);
}
