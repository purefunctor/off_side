use lexer::lex;

extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod lexer;

pub const SOURCE: &str = r#"
module Test where

test = case a, b of
  c, d
   | e ->
     case e of
       f | true -> bar
         | false -> baz
   | f -> g
"#;

fn main() {
    let source = SOURCE.trim();
    let tokens = lex(source).expect("a valid source file");
    for token in tokens {
        println!("{:?}", token.kind);
    }
}
