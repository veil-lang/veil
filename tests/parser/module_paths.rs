use codespan::Files;
use veil_ast as ast;
use veil_diagnostics::Severity;
use veil_syntax;

/// Helper to extract the first import's module path as a string.
fn first_import_path(p: &ast::Program) -> Option<&str> {
    p.imports.get(0).map(|imp| match imp {
        ast::ImportDeclaration::ImportAll { module_path, .. } => module_path.as_str(),
        ast::ImportDeclaration::ImportSpecifiers { module_path, .. } => module_path.as_str(),
        ast::ImportDeclaration::ExportImportAll { module_path, .. } => module_path.as_str(),
        ast::ImportDeclaration::ExportImportSpecifiers { module_path, .. } => module_path.as_str(),
    })
}

#[test]
fn canonical_double_colon_produces_no_warning_and_keeps_path() {
    let mut files = Files::<String>::new();
    let src = r#"
        import std::prelude;
        import net::http as http;
    "#;
    let fid = files.add("canonical_ok.veil".to_string(), src.to_string());

    let (program, warnings) =
        veil_syntax::parse_ast_with_warnings(&files, fid).expect("parse should succeed");

    // No warnings expected for canonical '::' module paths
    assert!(
        warnings.is_empty(),
        "expected no warnings, got: {}",
        warnings.len()
    );

    // Validate imports are preserved as '::'
    assert_eq!(
        program.imports.len(),
        2,
        "expected 2 imports, got: {}",
        program.imports.len()
    );

    // 1st import: std::prelude
    match &program.imports[0] {
        ast::ImportDeclaration::ImportAll {
            module_path,
            module_type,
            alias,
        } => {
            assert_eq!(module_path, "std::prelude");
            assert!(matches!(module_type, ast::ModuleType::Standard));
            assert!(alias.is_none());
        }
        other => panic!("unexpected first import variant: {:?}", other),
    }

    // 2nd import: net::http as http
    match &program.imports[1] {
        ast::ImportDeclaration::ImportAll {
            module_path,
            module_type,
            alias,
        } => {
            assert_eq!(module_path, "net::http");
            assert!(matches!(module_type, ast::ModuleType::External));
            assert_eq!(alias.as_deref(), Some("http"));
        }
        other => panic!("unexpected second import variant: {:?}", other),
    }

    // Sanity: helper sees first path correctly
    assert_eq!(first_import_path(&program), Some("std::prelude"));
}

#[test]
fn legacy_slash_paths_warn_and_canonicalize_to_double_colon() {
    let mut files = Files::<String>::new();
    let src = r#"
        import std/prelude;
        import { read, write as w } from io/fs;
    "#;
    let fid = files.add("legacy_slash.veil".to_string(), src.to_string());

    let (program, warnings) =
        veil_syntax::parse_ast_with_warnings(&files, fid).expect("parse should succeed");

    // Expect one warning per module_path using '/'
    assert!(
        warnings.len() >= 2,
        "expected at least 2 warnings (one per legacy module path), got: {}",
        warnings.len()
    );
    for (i, d) in warnings.iter().enumerate() {
        assert!(
            matches!(d.severity, Severity::Warning),
            "warning[{i}] expected Severity::Warning"
        );
        assert!(
            d.message
                .to_lowercase()
                .contains("legacy module path separator '/'"),
            "warning[{i}] message should mention legacy separator: {}",
            d.message
        );
        // Check a 'help' fix-it note is present
        let has_fix = d.notes.iter().any(|n| n.contains("replace '/' with '::'"));
        assert!(has_fix, "warning[{i}] should include fix-it help note");
    }

    // Validate imports are canonicalized to '::'
    assert_eq!(
        program.imports.len(),
        2,
        "expected 2 imports, got: {}",
        program.imports.len()
    );

    // 1st import: std/prelude => std::prelude (ImportAll)
    match &program.imports[0] {
        ast::ImportDeclaration::ImportAll {
            module_path,
            module_type,
            alias,
        } => {
            assert_eq!(
                module_path, "std::prelude",
                "expected canonicalized std::prelude"
            );
            assert!(matches!(module_type, ast::ModuleType::Standard));
            assert!(alias.is_none());
        }
        other => panic!("unexpected first import variant: {:?}", other),
    }

    // 2nd import: {read, write as w} from io/fs => io::fs (ImportSpecifiers)
    match &program.imports[1] {
        ast::ImportDeclaration::ImportSpecifiers {
            module_path,
            module_type,
            specifiers,
        } => {
            assert_eq!(module_path, "io::fs", "expected canonicalized io::fs");
            assert!(
                matches!(module_type, ast::ModuleType::External),
                "io::fs should be External module type"
            );
            let names: Vec<String> = specifiers
                .iter()
                .map(|s| {
                    if let Some(a) = &s.alias {
                        format!("{} as {}", s.name, a)
                    } else {
                        s.name.clone()
                    }
                })
                .collect();
            assert_eq!(names, vec!["read".to_string(), "write as w".to_string()]);
        }
        other => panic!("unexpected second import variant: {:?}", other),
    }

    // Sanity: helper sees canonical path for first import
    assert_eq!(first_import_path(&program), Some("std::prelude"));
}

#[test]
fn export_import_with_slash_warns_and_canonicalizes() {
    let mut files = Files::<String>::new();
    let src = r#"
        export import utils/helpers as helpers;
    "#;
    let fid = files.add("export_import_slash.veil".to_string(), src.to_string());

    let (program, warnings) =
        veil_syntax::parse_ast_with_warnings(&files, fid).expect("parse should succeed");

    assert!(
        !warnings.is_empty(),
        "expected warning for legacy '/' in export import"
    );
    let w = &warnings[0];
    assert!(
        matches!(w.severity, Severity::Warning),
        "expected Severity::Warning for legacy '/' in export import"
    );

    // Validate canonicalization for export import variant
    match &program.imports[0] {
        ast::ImportDeclaration::ExportImportAll {
            module_path,
            module_type,
            alias,
        } => {
            assert_eq!(module_path, "utils::helpers");
            assert!(matches!(module_type, ast::ModuleType::External));
            assert_eq!(alias.as_deref(), Some("helpers"));
        }
        other => panic!("unexpected import variant: {:?}", other),
    }
}
