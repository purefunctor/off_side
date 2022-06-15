extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod lexer;

pub const SOURCE: &str = r#"
module Test where

test = do
  a
  where
  a = pure 0

test = do
  let
    a = pure 0
  a

test =
  let
    a = pure 0
  in do
    a
"#;

fn main() {
    let lexer = lexer::Lexer::new(SOURCE.trim_start());
    let mut engine = lexer::LayoutEngine::new();
    let mut tokens = lexer.lex().unwrap();

    let mut current = tokens.next().unwrap();
    for future in tokens {
        engine.insert_layout(current, future.start);
        current = future;
    }
    engine.unwind_stack(current.start);

    println!("{:?}", engine.stack);
    for token in engine.tokens {
        println!("{:?}", token.kind);
    }
}
