use lex::Token;
use parse::Block;

use chumsky::error::Simple;
use chumsky::{primitive, Parser, Stream};
use logos::Logos;

use std::ops::Range;

pub type Span = Range<usize>;

pub struct Spanned<T> {
    pub data: T,
    pub span: Span,
}

pub trait WithSpan {
    fn with_span(self, span: Span) -> Spanned<Self>
    where
        Self: Sized,
    {
        Spanned { data: self, span }
    }
}

impl<T> WithSpan for T {}

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
