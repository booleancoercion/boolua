use logos::{Lexer, Logos};

pub enum ParseLitError {
    InvalidEscape,
    UnescapedNewline,
}

pub fn parse_short_string(lit: &str) -> Result<Vec<u8>, ParseLitError> {
    todo!()
}

#[derive(Copy, Clone, Debug, Logos)]
enum StringToken {
    #[regex(r"\\.")]
    Escape,

    #[regex(r"\\(\n\r|\r\n|\n)")]
    EscapedLineBreak,

    #[regex(r"\\x[[:xdigit:]][[:xdigit:]]")]
    HexByte,

    #[regex(r"\\\d\d?\d?", priority = 4)]
    DecByte,

    #[regex(r"\\u\{[[:xdigit:]]+\}")]
    Utf8,

    #[error]
    Error,
}
