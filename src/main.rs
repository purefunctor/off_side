use lexer::lex;

extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod lexer;

pub const SOURCE: &str = r#"
module Test where

main = ado
  let a = let b = c in d
  in a
"#;

fn main() {
    let source = SOURCE.trim();
    let tokens = lex(source).expect("a valid source file");
    for token in tokens {
        println!("{:?}", token.kind);
    }
}
