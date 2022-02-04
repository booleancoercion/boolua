use boolua::{lex::Token, T};

use chumsky::Parser;
use logos::Logos;

use std::fs;

fn load_code() -> String {
    fs::read_to_string("programs/markov.lua").unwrap()
}

#[test]
fn markov_lex() {
    let code = load_code();

    let tokens_lexed: Vec<_> = Token::lexer(&code).collect();

    #[rustfmt::skip]
    let tokens_hardcoded: Vec<Token> = vec![
        T![function], T![ident], T!['('], T![')'],
            T![local], T![ident], T![=], T![ident], T![.], T![ident], T!['('], T![')'],
            T![local], T![ident], T![=], T![numlit],
            T![return], T![function], T!['('], T![')'],
                T![while], T![ident], T![do],
                T![local], T![ident], T![,], T![ident], T![=], T![ident], T![.], T![ident], T!['('], T![ident], T![,], T![shortstring], T![,], T![ident], T![')'],
                T![if], T![ident], T![then],
                    T![ident], T![=], T![ident], T![+], T![numlit],
                    T![return], T![ident], T![.], T![ident], T!['('], T![ident], T![,], T![ident], T![,], T![ident], T![')'],
                T![else],
                    T![ident], T![=], T![ident], T![.], T![ident], T!['('], T![')'],
                    T![ident], T![=], T![numlit],
                T![end],
                T![end],
                T![return], T![nil],
            T![end],
        T![end],

        T![function], T![ident], T!['('], T![ident], T![,], T![ident], T![')'],
            T![return], T![ident], T![..], T![shortstring], T![..], T![ident],
        T![end],

        T![local], T![ident],

        T![local], T![ident], T![=], T![longstring],
        T![local], T![ident], T![=], T![shortstring], T![;],

        T![function], T![ident], T!['('], T![ident], T![,], T![ident], T![')'],
            T![if], T![not], T![ident], T!['['], T![ident], T![']'], T![then],
                T![ident], T!['['], T![ident], T![']'], T![=], T!['{'], T![ident], T![=], T![numlit], T!['}'],
            T![end],
            T![ident], T![.], T![ident], T!['('], T![ident], T!['['], T![ident], T![']'], T![,], T![ident], T![')'],
        T![end],

        T![local], T![ident],  T![=], T![numlit],
        T![local], T![ident], T![=], T![numlit],
        T![local], T![ident], T![=], T![shortstring],

        T![ident], T![=], T!['{'], T!['}'],
        T![local], T![ident], T![,], T![ident], T![=], T![ident], T![,], T![ident],
        T![for], T![ident], T![in], T![ident], T!['('], T![')'], T![do],
            T![ident], T!['('], T![ident], T!['('], T![ident], T![,], T![ident], T![')'], T![,], T![ident], T![')'],
            T![ident], T![=], T![ident], T![;], T![ident], T![=], T![ident], T![;],
        T![end],
        T![ident], T!['('], T![ident], T!['('], T![ident], T![,], T![ident], T![')'], T![,], T![ident], T![')'],
        T![ident], T![=], T![ident], T![;], T![ident], T![=], T![ident],
        T![for], T![ident], T![=], T![numlit], T![,], T![ident], T![do],
            T![local], T![ident], T![=], T![ident], T!['['], T![ident], T!['('], T![ident], T![,], T![ident], T![')'], T![']'],
            T![local], T![ident], T![=], T![ident], T![.], T![ident], T!['('], T![ident], T![.], T![ident], T!['('], T![ident], T![')'], T![')'],
            T![local], T![ident], T![=], T![ident], T!['['], T![ident], T![']'],
            T![if], T![ident], T![==], T![ident], T![then], T![return], T![end],
            T![ident], T![.], T![ident], T!['('], T![ident], T![,], T![shortstring], T![')'],
            T![ident], T![=], T![ident], T![;], T![ident], T![=], T![ident],
        T![end],
    ];

    assert_eq!(tokens_lexed, tokens_hardcoded)
}

#[test]
fn markov_parse() {
    let code = load_code();

    let tokens = Token::lexer(&code).spanned();
    let stream = chumsky::Stream::from_iter(code.len()..code.len() + 1, tokens);

    let parsed = boolua::parse::chunk(&code)
        .then(chumsky::primitive::end())
        .parse(stream);

    let _ = dbg!(parsed);
    panic!()
}
