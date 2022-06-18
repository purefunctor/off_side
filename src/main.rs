use lexer::lex;

extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod lexer;

pub const SOURCE: &str = r#"
module Test where

main =
  case a, z of
    b | c
      , d -> e
    f -> g
"#;

fn main() {
    let source = SOURCE.trim();
    let tokens = lex(source).expect("a valid source file");
    for token in tokens {
        println!("{:?}", token.kind);
    }
}
