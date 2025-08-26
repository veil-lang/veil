use std::env;
use std::fs;
use codespan::Files;
use ve::lexer::Token;
use logos::Logos;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: dump_tokens <file>");
        std::process::exit(1);
    }
    let path = &args[1];
    let src = fs::read_to_string(path).expect("read");
    let mut files = Files::new();
    let file_id = files.add(path.clone(), src.clone());
    for (i, (t, span)) in Token::lexer(&src).spanned().filter_map(|(t, s)| t.ok().map(|tok| (tok, codespan::Span::new(s.start as u32, s.end as u32)))).enumerate() {
        let s = &src[span.start().0 as usize..span.end().0 as usize];
        println!("{}: {:?} -> '{}' @ {:?}", i, t, s, span);
    }
}
