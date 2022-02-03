use crate::lex::Token;

use chumsky::prelude::*;

pub enum Stmt {}

pub fn chunk() -> impl Parser<Token, Vec<Stmt>, Error = Simple<Token>> {
    todo()
}
