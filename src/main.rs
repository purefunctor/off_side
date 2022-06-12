extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod lexer;

pub const SOURCE: &str = r#"
module Main where

x =
  let
    z = 0
    y = 0
  in
    z + z

y = 0
"#;

fn main() {
    let source = SOURCE.trim();
    let lexer = lexer::Lexer::new(source);
    let mut engine = lexer::LayoutEngine::new();
    let mut tokens = lexer.lex().unwrap();

    let mut current = tokens.next().unwrap();
    loop {
        let future = tokens.next().expect("expected eof");
        engine.insert_layout(current, future.start);
        current = future;
        if current.kind.is_eof() {
            engine.unwind_stack(current.start);
            break;
        }
    }

    println!("{:?}", engine.stack);
    for token in engine.tokens {
        println!("{:?}", token.kind);
    }
}
