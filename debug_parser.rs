use codespan::Files;
use std::fs;
use veil_syntax::parse_ast;

fn main() {
    let content = r#"fn main() -> void {
    let x: i32 = 42;
    return;
}"#;

    let mut files = Files::new();
    let file_id = files.add("test.veil".to_string(), content.to_string());

    println!("Attempting to parse:");
    println!("{}", content);
    println!("---");

    match parse_ast(&files, file_id) {
        Ok(program) => {
            println!("Parse successful!");
            println!("Program: {:#?}", program);
        }
        Err(diagnostics) => {
            println!("Parse failed with {} diagnostics:", diagnostics.len());
            for (i, diag) in diagnostics.iter().enumerate() {
                println!("Diagnostic {}: {:?}", i + 1, diag);
            }
        }
    }
}
