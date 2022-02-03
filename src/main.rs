use boolua::lex::Token;

use logos::Logos;

fn main() {
    let source = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    dbg!(Token::lexer(&source).collect::<Vec<_>>());
}
