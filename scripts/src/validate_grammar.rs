use veil_syntax::validate_grammar;

fn main() {
    match validate_grammar() {
        Ok(()) => {
            println!("✓ Grammar validation passed");
            std::process::exit(0);
        }
        Err(error) => {
            eprintln!("✗ Grammar validation failed: {}", error);
            std::process::exit(1);
        }
    }
}
