use logos::{Filter, Lexer, Logos};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Logos)]
#[rustfmt::skip]
pub enum Token {
    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*")]
    Ident,

    #[regex(r#"(?s)"([^"]|\\.)*""#)]
    #[regex(r#"(?s)'([^']|\\.)*'"#)]
    ShortString,

    #[regex(r"\[=*\[", |x| longstring(x, x.slice().len() - 2))]
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
    #[regex(r"--\[=*\[", longcomment)]
    #[regex(r"--([^\n(\[=*\[)].*)?", logos::skip)]
    Error,
}

fn longstring(lex: &mut Lexer<Token>, n: usize) -> bool {
    let mut looking_for = String::from("]");
    looking_for.push_str(&"=".repeat(n));
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
#[macro_export]
macro_rules! T {
    (and) => {<T![]>::And};
    (break) => {<T![]>::Break};
    (do) => {<T![]>::Do};
    (else) => {<T![]>::Else};
    (elseif) => {<T![]>::Elseif};
    (end) => {<T![]>::End};
    (false) => {<T![]>::False};
    (for) => {<T![]>::For};
    (function) => {<T![]>::Function};
    (goto) => {<T![]>::Goto};
    (if) => {<T![]>::If};
    (in) => {<T![]>::In};
    (local) => {<T![]>::Local};
    (nil) => {<T![]>::Nil};
    (not) => {<T![]>::Not};
    (or) => {<T![]>::Or};
    (repeat) => {<T![]>::Repeat};
    (return) => {<T![]>::Return};
    (then) => {<T![]>::Then};
    (true) => {<T![]>::True};
    (until) => {<T![]>::Until};
    (while) => {<T![]>::While};

    (+) => {<T![]>::Plus};
    (-) => {<T![]>::Minus};
    (*) => {<T![]>::Star};
    (/) => {<T![]>::Slash};
    (%) => {<T![]>::Percent};
    (^) => {<T![]>::Caret};
    (#) => {<T![]>::Pound};
    (&) => {<T![]>::Amp};
    (~) => {<T![]>::Tilde};
    (|) => {<T![]>::Pipe};
    (<<) => {<T![]>::DLAngle};
    (>>) => {<T![]>::DRAngle};
    ("//") => {<T![]>::DSlash};
    (==) => {<T![]>::DEq};
    (~=) => {<T![]>::NEq};
    (<=) => {<T![]>::LEq};
    (>=) => {<T![]>::GEq};
    (<) => {<T![]>::LAngle};
    (>) => {<T![]>::RAngle};
    (=) => {<T![]>::Eq};
    ('(') => {<T![]>::LParen};
    (')') => {<T![]>::RParen};
    ('{') => {<T![]>::LCurly};
    ('}') => {<T![]>::RCurly};
    ('[') => {<T![]>::LBracket};
    (']') => {<T![]>::RBracket};
    (::) => {<T![]>::DColon};
    (;) => {<T![]>::Semicolon};
    (:) => {<T![]>::Colon};
    (,) => {<T![]>::Comma};
    (.) => {<T![]>::Period};
    (..) => {<T![]>::DPeriod};
    (...) => {<T![]>::TPeriod};

    (ident) => {<T![]>::Ident};
    (shortstring) => {<T![]>::ShortString};
    (longstring) => {<T![]>::LongString};
    (numlit) => {<T![]>::NumLit};

    () => {$crate::lex::Token};
}
