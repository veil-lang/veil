use codespan::Files;
use pest::Parser;

#[test]

fn astparser_program_probe_min_fn_print_error() {
    // Directly exercise the Pest parser on a minimal function.
    // This test is ignored by default; run it explicitly when investigating grammar failures.
    let src = "fn main() { }";
    match veil_syntax::ast_grammar::AstParser::parse(
        veil_syntax::ast_grammar::AstRule::program,
        src,
    ) {
        Ok(_) => {
            eprintln!("AstParser successfully parsed minimal fn");
        }
        Err(e) => {
            eprintln!("Pest raw error for program:\n{}", e);
        }
    }

    // Probe the debug-only fn_core rule as well.
    match veil_syntax::ast_grammar::AstParser::parse(
        veil_syntax::ast_grammar::AstRule::fn_core,
        src,
    ) {
        Ok(_) => {
            eprintln!("AstParser successfully parsed fn_core for minimal fn");
        }
        Err(e) => {
            eprintln!("Pest raw error for fn_core:\n{}", e);
        }
    }

    // Adapter-level probe: parse via parse_ast_with_warnings and print function count
    let mut files = Files::<String>::new();
    let fid = files.add("probe_min.veil".to_string(), src.to_string());
    match veil_syntax::parse_ast_with_warnings(&files, fid) {
        Ok((program, warnings)) => {
            eprintln!("Adapter parsed: {} function(s)", program.functions.len());
            if !warnings.is_empty() {
                eprintln!("Adapter warnings: {}", warnings.len());
            }
        }
        Err(diags) => {
            eprintln!("Adapter parse failed with {} error(s)", diags.len());
        }
    }
}
#[test]
fn astparser_function_decl_probe_min_fn() {
    // Targeted probe: parse just a function_decl to isolate grammar failures around top-level fn.
    let src = "fn main() { }";
    match veil_syntax::ast_grammar::AstParser::parse(
        veil_syntax::ast_grammar::AstRule::function_decl,
        src,
    ) {
        Ok(_) => {
            eprintln!("AstParser successfully parsed function_decl for minimal fn");
        }
        Err(e) => {
            eprintln!("Pest raw error for function_decl:\n{}", e);
        }
    }

    // Also probe the minimal declaration shape accepted by the grammar.
    match veil_syntax::ast_grammar::AstParser::parse(
        veil_syntax::ast_grammar::AstRule::function_decl_min,
        src,
    ) {
        Ok(_) => {
            eprintln!("AstParser successfully parsed function_decl_min for minimal fn");
        }
        Err(e) => {
            eprintln!("Pest raw error for function_decl_min:\n{}", e);
        }
    }
}

#[test]
fn astparser_item_probe_min_fn() {
    // Probe the 'item' rule directly to check top-level alternation vs stmt fallback.
    let src = "fn main() { }";
    match veil_syntax::ast_grammar::AstParser::parse(veil_syntax::ast_grammar::AstRule::item, src) {
        Ok(_) => {
            eprintln!("AstParser successfully parsed item for minimal fn");
        }
        Err(e) => {
            eprintln!("Pest raw error for item:\n{}", e);
        }
    }
}

#[test]
fn astparser_kw_fn_token_probe() {
    // Ensure the keyword rule itself is recognized by Pest.
    let src = "fn";
    match veil_syntax::ast_grammar::AstParser::parse(veil_syntax::ast_grammar::AstRule::KW_FN, src)
    {
        Ok(_) => eprintln!("KW_FN token recognized by AstParser"),
        Err(e) => eprintln!("Pest raw error for KW_FN:\n{}", e),
    }
}

#[test]
fn astparser_kw_var_token_probe() {
    let src = "var";
    match veil_syntax::ast_grammar::AstParser::parse(veil_syntax::ast_grammar::AstRule::KW_VAR, src)
    {
        Ok(_) => eprintln!("KW_VAR token recognized by AstParser"),
        Err(e) => eprintln!("Pest raw error for KW_VAR:\n{}", e),
    }
}

#[test]
fn parses_minimal_mvl_program_with_str_and_idiv() {
    // This mirrors the CLI integration test input that currently fails in the pipeline.
    // Goal: reproduce inside the syntax crate and surface precise diagnostics to fix grammar/adapter.
    let src = r#"
fn main() -> void {
    var a: i32 = 6 // 3;     /# integer division operator
    var s: str = "hi";       /# accept `str`
    var t: string = s;       /# `string` still accepted; mapped to String type
    return;
}
"#;

    // Add probe to print intermediate parse results
    let mut files = Files::<String>::new();
    let fid = files.add("probe_mvl.veil".to_string(), src.to_string());
    match veil_syntax::parse_ast_with_warnings(&files, fid) {
        Ok((program, warnings)) => {
            eprintln!(
                "MVL adapter parsed: {} function(s), {} stmt(s)",
                program.functions.len(),
                program.stmts.len()
            );
            eprintln!(
                "Function names: {:?}",
                program
                    .functions
                    .iter()
                    .map(|f| &f.name)
                    .collect::<Vec<_>>()
            );
            if !warnings.is_empty() {
                eprintln!("MVL adapter warnings: {}", warnings.len());
            }
            // Original assertion
            assert!(
                program.functions.iter().any(|f| f.name == "main"),
                "expected a `main` function to be parsed"
            );
        }
        Err(diags) => {
            eprintln!("MVL adapter parse failed with {} error(s)", diags.len());
            panic!("MVL adapter parse failed; see diagnostics");
        }
    }
}

#[test]
fn parses_minimal_addition_program() {
    // Even simpler MVL sample; useful to isolate issues unrelated to `//` or string aliases.
    let src = r#"
fn main() -> void {
    var x = 1 as i32;
    var y = 2 as i32;
    var z = x + y;
    return;
}
"#;

    // Add probe to print intermediate parse results
    let mut files = Files::<String>::new();
    let fid = files.add("probe_add.veil".to_string(), src.to_string());
    match veil_syntax::parse_ast_with_warnings(&files, fid) {
        Ok((program, warnings)) => {
            eprintln!(
                "Addition adapter parsed: {} function(s), {} stmt(s)",
                program.functions.len(),
                program.stmts.len()
            );
            eprintln!(
                "Function names: {:?}",
                program
                    .functions
                    .iter()
                    .map(|f| &f.name)
                    .collect::<Vec<_>>()
            );
            if !warnings.is_empty() {
                eprintln!("Addition adapter warnings: {}", warnings.len());
            }
            assert!(
                program.functions.iter().any(|f| f.name == "main"),
                "expected a `main` function to be parsed"
            );
        }
        Err(diags) => {
            eprintln!(
                "Addition adapter parse failed with {} error(s)",
                diags.len()
            );
            panic!("Addition adapter parse failed; see diagnostics");
        }
    }
}

#[test]
fn parses_fn_without_return_type() {
    // Reduced: no explicit return type; minimal block with a single return.
    let src = r#"
fn main() {
    return;
}
"#;

    // Add probe to print intermediate parse results
    let mut files = Files::<String>::new();
    let fid = files.add("probe_noret.veil".to_string(), src.to_string());
    match veil_syntax::parse_ast_with_warnings(&files, fid) {
        Ok((program, warnings)) => {
            eprintln!(
                "NoRet adapter parsed: {} function(s), {} stmt(s)",
                program.functions.len(),
                program.stmts.len()
            );
            eprintln!(
                "Function names: {:?}",
                program
                    .functions
                    .iter()
                    .map(|f| &f.name)
                    .collect::<Vec<_>>()
            );
            if !warnings.is_empty() {
                eprintln!("NoRet adapter warnings: {}", warnings.len());
            }
            assert!(
                program.functions.iter().any(|f| f.name == "main"),
                "expected a `main` function to be parsed"
            );
        }
        Err(diags) => {
            eprintln!("NoRet adapter parse failed with {} error(s)", diags.len());
            panic!("NoRet adapter parse failed; see diagnostics");
        }
    }
}

#[test]
fn parses_fn_with_empty_block() {
    // Reduced further: empty block body.
    let src = r#"
fn main() { }
"#;

    // Add probe to print intermediate parse results
    let mut files = Files::<String>::new();
    let fid = files.add("probe_empty.veil".to_string(), src.to_string());
    match veil_syntax::parse_ast_with_warnings(&files, fid) {
        Ok((program, warnings)) => {
            eprintln!(
                "Empty adapter parsed: {} function(s), {} stmt(s)",
                program.functions.len(),
                program.stmts.len()
            );
            eprintln!(
                "Function names: {:?}",
                program
                    .functions
                    .iter()
                    .map(|f| &f.name)
                    .collect::<Vec<_>>()
            );
            if !warnings.is_empty() {
                eprintln!("Empty adapter warnings: {}", warnings.len());
            }
            assert!(
                program.functions.iter().any(|f| f.name == "main"),
                "expected a `main` function to be parsed"
            );
        }
        Err(diags) => {
            eprintln!("Empty adapter parse failed with {} error(s)", diags.len());
            panic!("Empty adapter parse failed; see diagnostics");
        }
    }
}

#[test]
fn tokenization_probe_main_with_void_return() {
    use veil_syntax::RawToken;

    let mut files = Files::<String>::new();
    let src = "fn main() -> void { }";
    let fid = files.add("tok_probe.veil".to_string(), src.to_string());

    let toks = veil_syntax::lex_raw(&files, fid).expect("lexing should succeed");

    // Expect the first tokens to identify function header correctly
    // fn main ( ) -> void {
    let want = vec![
        "KW(fn)", "ID(main)", "SYM(()", "SYM())", "SYM(->)", "TY(void)", "SYM({)",
    ];

    let got: Vec<String> = toks
        .iter()
        .take(want.len())
        .map(|t| match &t.token {
            RawToken::Keyword(k) => format!("KW({})", k),
            RawToken::TypeKeyword(k) => format!("TY({})", k),
            RawToken::Ident(s) => format!("ID({})", s),
            RawToken::Symbol(s) => format!("SYM({})", s),
            RawToken::Str(_) => "STR".to_string(),
            RawToken::Int(_) => "INT".to_string(),
            RawToken::Float(_) => "FLOAT".to_string(),
            RawToken::TemplateStr(_) => "TEMPLATE".to_string(),
        })
        .collect();

    assert_eq!(got, want, "unexpected token sequence: {:?}", got);
}

#[test]
fn astparser_parameter_list_probe() {
    use veil_syntax::ast_grammar::{AstParser, AstRule};

    for src in ["()", "(x: i32)", "(x: i32, y: str)"].iter() {
        match AstParser::parse(AstRule::parameter_list, src) {
            Ok(_) => eprintln!("parameter_list parsed: {}", src),
            Err(e) => panic!("parameter_list failed on {}: {}", src, e),
        }
    }
}

#[test]
fn astparser_block_probe() {
    use veil_syntax::ast_grammar::{AstParser, AstRule};

    // First, assert simple blocks parse.
    for src in ["{ }", "{ return; }"].iter() {
        match AstParser::parse(AstRule::block, src) {
            Ok(_) => eprintln!("block parsed: {}", src),
            Err(e) => panic!("block failed on {}: {}", src, e),
        }
    }

    // Direct probes for a minimal var statement as its own rules.
    let var_src = "var x: i32 = 1;";
    match AstParser::parse(AstRule::var_stmt, var_src) {
        Ok(_) => eprintln!("var_stmt parsed: {}", var_src),
        Err(e) => eprintln!("var_stmt failed (diag) on {}: {}", var_src, e),
    }
    match AstParser::parse(AstRule::stmt, var_src) {
        Ok(_) => eprintln!("stmt parsed: {}", var_src),
        Err(e) => eprintln!("stmt failed (diag) on {}: {}", var_src, e),
    }

    // Diagnostic-only probes to isolate the failure with var_stmt in blocks.
    let blk_src = "{ var x: i32 = 1; }";
    match AstParser::parse(AstRule::block, blk_src) {
        Ok(_) => eprintln!("block parsed: {}", blk_src),
        Err(e) => eprintln!("block failed (diag) on {}: {}", blk_src, e),
    }

    let var_src = "var x: i32 = 1;";
    match AstParser::parse(AstRule::var_stmt, var_src) {
        Ok(_) => eprintln!("var_stmt parsed: {}", var_src),
        Err(e) => eprintln!("var_stmt failed (diag) on {}: {}", var_src, e),
    }

    match AstParser::parse(AstRule::stmt, var_src) {
        Ok(_) => eprintln!("stmt parsed: {}", var_src),
        Err(e) => eprintln!("stmt failed (diag) on {}: {}", var_src, e),
    }
}

#[test]
fn astparser_ident_probe() {
    use veil_syntax::ast_grammar::{AstParser, AstRule};

    // Verify plain identifiers parse; print outcome for keyword-like to diagnose KEYWORD vs ident.
    for (src, should_parse) in [
        ("main", true),
        ("_x1", true),
        ("as", false),
        ("await", false),
    ] {
        let res = AstParser::parse(AstRule::ident, src);
        match (res, should_parse) {
            (Ok(_), true) => eprintln!("ident parsed: {}", src),
            (Err(e), true) => panic!("ident should parse but failed on {}: {}", src, e),
            (Ok(_), false) => eprintln!("ident unexpectedly parsed keyword-like '{}'", src),
            (Err(_), false) => eprintln!("ident correctly rejected keyword-like '{}'", src),
        }
    }
}

#[test]
fn astparser_ty_probe() {
    use veil_syntax::ast_grammar::{AstParser, AstRule};

    // Core primitive aliases should parse as types.
    for src in ["i32", "str", "void"].iter() {
        match AstParser::parse(AstRule::ty, src) {
            Ok(_) => eprintln!("ty parsed: {}", src),
            Err(e) => panic!("ty failed on {}: {}", src, e),
        }
    }

    // Diagnostic-only probes; don't fail suite, just print for visibility.
    for src in ["Foo::Bar", "[i32; 4]"].iter() {
        match AstParser::parse(AstRule::ty, src) {
            Ok(_) => eprintln!("ty parsed (diag): {}", src),
            Err(e) => eprintln!("ty failed (diag) on {}: {}", src, e),
        }
    }
}

#[test]
fn astparser_idiv_comment_block_probe() {
    use veil_syntax::ast_grammar::{AstParser, AstRule};

    // Verify '6 // 3; // comment' parses inside a block (IDIV operator followed by line comment).
    let src = r#"
{
    var a: i32 = 6 // 3;     /# integer division operator
}
"#;

    match AstParser::parse(AstRule::block, src.trim()) {
        Ok(_) => eprintln!("block with IDIV and trailing comment parsed"),
        Err(e) => panic!("block with IDIV and trailing comment failed: {}", e),
    }
}
