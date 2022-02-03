use logos::{Filter, Lexer, Logos};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Logos)]
#[rustfmt::skip]
pub enum Token {
    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*")]
    Ident,

    #[regex(r#""([^"]|\\.)*""#)]
    #[regex(r#"'([^']|\\.)*'"#)]
    ShortString,

    #[regex(r"\[=*\[", |x| longstring(x, x.slice().len() -2 ))]
    LongString,

    #[regex(r"\d+(\.\d+)?([eE]\d+)?")]
    #[regex(r"0[xX][[:xdigit:]]+(\.[[:xdigit:]]+)?([pP][[:xdigit:]]+)?")]
    NumLit,

    #[token("and")]       And,
    #[token("break")]     Break,
    #[token("do")]        Do,
    #[token("else")]      Else,
    #[token("elseif")]    Elseif,
    #[token("end")]       End,
    #[token("false")]     False,
    #[token("for")]       For,
    #[token("function")]  Function,
    #[token("goto")]      Goto,
    #[token("if")]        If,
    #[token("in")]        In,
    #[token("local")]     Local,
    #[token("nil")]       Nil,
    #[token("not")]       Not,
    #[token("or")]        Or,
    #[token("repeat")]    Repeat,
    #[token("return")]    Return,
    #[token("then")]      Then,
    #[token("true")]      True,
    #[token("until")]     Until,
    #[token("while")]     While,

    #[token("+")]    Plus,
    #[token("-")]    Minus,
    #[token("*")]    Star,
    #[token("/")]    Slash,
    #[token("%")]    Percent,
    #[token("^")]    Caret,
    #[token("#")]    Pound,
    #[token("&")]    Amp,
    #[token("~")]    Tilde,
    #[token("|")]    Pipe,
    #[token("<<")]   DLAngle,
    #[token(">>")]   DRAngle,
    #[token("//")]   DSlash,
    #[token("==")]   DEq,
    #[token("~=")]   NEq,
    #[token("<=")]   LEq,
    #[token(">=")]   GEq,
    #[token("<")]    LAngle,
    #[token(">")]    RAngle,
    #[token("=")]    Eq,
    #[token("(")]    LParen,
    #[token(")")]    RParen,
    #[token("{")]    LCurly,
    #[token("}")]    RCurly,
    #[token("[")]    LBracket,
    #[token("]")]    RBracket,
    #[token("::")]   DColon,
    #[token(";")]    Semicolon,
    #[token(":")]    Colon,
    #[token(",")]    Comma,
    #[token(".")]    Period,
    #[token("..")]   DPeriod,
    #[token("...")]  TPeriod,

    #[error]
    #[regex(r"[ \f\n\r\t\v]+", logos::skip)]
    #[regex(r"--[^\n]*", logos::skip)]
    #[regex(r"--\[=*\[", longcomment)]
    Error,
}

fn longstring(lex: &mut Lexer<Token>, n: usize) -> bool {
    let mut looking_for = String::from("]");
    looking_for += &"=".repeat(n);
    looking_for.push(']');

    let offset = if let Some(offset) = lex.remainder().find(&looking_for) {
        offset
    } else {
        return false;
    };

    lex.bump(offset + looking_for.len());

    true
}

fn longcomment(lex: &mut Lexer<Token>) -> Filter<()> {
    if longstring(lex, lex.slice().len() - 4) {
        Filter::Skip
    } else {
        Filter::Emit(())
    }
}

#[rustfmt::skip]
macro_rules! T {
    (and) => {$crate::lex::TokenKind::And};
    (break) => {$crate::lex::TokenKind::Break};
    (do) => {$crate::lex::TokenKind::Do};
    (else) => {$crate::lex::TokenKind::Else};
    (elseif) => {$crate::lex::TokenKind::Elseif};
    (end) => {$crate::lex::TokenKind::End};
    (false) => {$crate::lex::TokenKind::False};
    (for) => {$crate::lex::TokenKind::For};
    (function) => {$crate::lex::TokenKind::Function};
    (goto) => {$crate::lex::TokenKind::Goto};
    (if) => {$crate::lex::TokenKind::If};
    (in) => {$crate::lex::TokenKind::In};
    (local) => {$crate::lex::TokenKind::Local};
    (nil) => {$crate::lex::TokenKind::Nil};
    (not) => {$crate::lex::TokenKind::Not};
    (or) => {$crate::lex::TokenKind::Or};
    (repeat) => {$crate::lex::TokenKind::Repeat};
    (return) => {$crate::lex::TokenKind::Return};
    (then) => {$crate::lex::TokenKind::Then};
    (true) => {$crate::lex::TokenKind::True};
    (until) => {$crate::lex::TokenKind::Until};
    (while) => {$crate::lex::TokenKind::While};

    (+) => {$crate::lex::TokenKind::Plus};
    (-) => {$crate::lex::TokenKind::Minus};
    (*) => {$crate::lex::TokenKind::Star};
    (/) => {$crate::lex::TokenKind::Slash};
    (%) => {$crate::lex::TokenKind::Percent};
    (^) => {$crate::lex::TokenKind::Caret};
    (#) => {$crate::lex::TokenKind::Pound};
    (&) => {$crate::lex::TokenKind::Amp};
    (~) => {$crate::lex::TokenKind::Tilde};
    (|) => {$crate::lex::TokenKind::Pipe};
    (<<) => {$crate::lex::TokenKind::DLAngle};
    (>>) => {$crate::lex::TokenKind::DRAngle};
    ("//") => {$crate::lex::TokenKind::DSlash};
    (==) => {$crate::lex::TokenKind::DEq};
    (~=) => {$crate::lex::TokenKind::NEq};
    (<=) => {$crate::lex::TokenKind::LEq};
    (>=) => {$crate::lex::TokenKind::GEq};
    (<) => {$crate::lex::TokenKind::LAngle};
    (>) => {$crate::lex::TokenKind::RAngle};
    (=) => {$crate::lex::TokenKind::Eq};
    (() => {$crate::lex::TokenKind::LParen};
    ()) => {$crate::lex::TokenKind::RParen};
    ('{') => {$crate::lex::TokenKind::LCurly};
    ('}') => {$crate::lex::TokenKind::RCurly};
    ('[') => {$crate::lex::TokenKind::LBracket};
    (']') => {$crate::lex::TokenKind::RBracket};
    (::) => {$crate::lex::TokenKind::DColon};
    (;) => {$crate::lex::TokenKind::Semicolon};
    (:) => {$crate::lex::TokenKind::Colon};
    (,) => {$crate::lex::TokenKind::Comma};
    (.) => {$crate::lex::TokenKind::Period};
    (..) => {$crate::lex::TokenKind::DPeriod};
    (...) => {$crate::lex::TokenKind::TPeriod};
}
