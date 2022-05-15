mod combinators;
mod number;
mod pretty_print;
pub mod structs;
#[cfg(test)]
mod tests;

use boolua_lexer::Token;

use chumsky::error::Simple;
use chumsky::{primitive, Parser, Stream};

pub fn parse_source(source: &str) -> Result<structs::Block, Vec<Simple<Token>>> {
    let tokens = boolua_lexer::tokenize(source);
    let stream = Stream::from_iter(source.len()..source.len() + 1, tokens);

    combinators::chunk(source)
        .then_ignore(primitive::end())
        .parse(stream)
}
