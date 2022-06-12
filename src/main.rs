extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod lexer;

pub const SOURCE: &str = r#"
let
  x = 1
  y = 2
in
  x + y
"#;

fn main() {
    let source = SOURCE.trim();
    let lexer = lexer::Lexer::new(source);
    let mut engine = lexer::LayoutEngine::new();
    let mut tokens = lexer.lex().unwrap();

    let now = tokens.next().unwrap();
    let then = tokens.next().unwrap();
    engine.insert_layout(now, then.start);

    let mut current = then;
    loop {
        match tokens.next() {
            Some(then) => {
                engine.insert_layout(current, then.start);
                current = then;
            }
            None => {
                engine.insert_layout(current, current.end);
                break;
            }
        }
    }

    println!("{:?}", engine.stack);
    for token in engine.tokens {
        println!("{:?}", token);
    }
}
