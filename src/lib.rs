use lex::Token;
use parse::Block;

use chumsky::error::Simple;
use chumsky::{primitive, Parser, Stream};
use logos::Logos;

#[macro_use]
pub mod lex;
pub mod parse;

pub fn parse_source(source: &str) -> Result<Block, Vec<Simple<Token>>> {
    let tokens = Token::lexer(source).spanned();
    let stream = Stream::from_iter(source.len()..source.len() + 1, tokens);

    parse::chunk(source)
        .then_ignore(primitive::end())
        .parse(stream)
}
