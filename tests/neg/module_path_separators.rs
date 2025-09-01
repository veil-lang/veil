use codespan::Files;

#[test]
fn rejects_double_colon_in_import_all() {
    let mut files = Files::<String>::new();
    let src = r#"
        import std::prelude;
        fn main() {}
    "#;
    let fid = files.add("neg_module_path_colon_all.veil".into(), src.into());
    let res = veil_syntax::parse_ast(&files, fid);
    assert!(
        res.is_err(),
        "Parser should reject '::' as a module path separator in import-all"
    );
    let diags = res.err().unwrap();
    assert!(
        !diags.is_empty(),
        "Expected diagnostics when using '::' in module paths"
    );
}

#[test]
fn rejects_double_colon_in_import_specifiers() {
    let mut files = Files::<String>::new();
    let src = r#"
        import { prelude } from std::core;
        fn main() {}
    "#;
    let fid = files.add("neg_module_path_colon_specifiers.veil".into(), src.into());
    let res = veil_syntax::parse_ast(&files, fid);
    assert!(
        res.is_err(),
        "Parser should reject '::' as a module path separator in import-specifiers"
    );
    let diags = res.err().unwrap();
    assert!(
        !diags.is_empty(),
        "Expected diagnostics when using '::' in module paths"
    );
}
