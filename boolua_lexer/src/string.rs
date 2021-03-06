use boolua_common::{Span, Spanned, WithSpan};

use logos::Logos;

use std::iter::{self, Once};
use std::slice;

// Original code by kestrer! :)
pub fn normalize_newlines(bytes: &mut Vec<u8>) {
    let len = bytes.len();
    let mut del = 0;

    for i in 0..len {
        if bytes[i..].starts_with(b"\r\n") || bytes[i..].starts_with(b"\n\r") {
            del += 1;
            bytes[i] = b'\n';
            bytes[i + 1] = b'\n';
        } else {
            if bytes[i] == b'\r' {
                bytes[i] = b'\n';
            }
            bytes[i - del] = bytes[i];
        }
    }

    if del > 0 {
        bytes.truncate(len - del);
    }
}

pub fn parse_short_string(lit: &str) -> (Vec<u8>, Vec<Spanned<ParseLitError>>) {
    let mut output = vec![];
    let mut errors = vec![];

    for (token, span) in StringToken::lexer(lit).spanned() {
        match token.iter(lit, span.clone()) {
            Ok(iter) => output.extend(iter),
            Err(error) => errors.push(error.with_span(span)),
        }
    }

    (output, errors)
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

    #[regex(".")]
    Char,

    #[regex(r"\\z\s*", logos::skip)]
    #[error]
    Error,
}

impl StringToken {
    fn iter(self, source: &str, span: Span) -> Result<StringTokenIterator, ParseLitError> {
        let slice = &source[span];

        use ParseLitError::*;
        use StringToken::*;
        match self {
            Escape => {
                let slice = &slice[1..];
                if slice.len() != 1 {
                    return Err(InvalidEscape);
                }
                let byte = match slice.bytes().next().unwrap() {
                    b'a' => 0x07,
                    b'b' => 0x08,
                    b'f' => 0x0c,
                    b'n' => b'\n',
                    b'r' => b'\r',
                    b't' => b'\t',
                    b'v' => 0x0b,
                    b'\\' => b'\\',
                    b'"' => b'"',
                    b'\'' => b'\'',
                    _ => return Err(InvalidEscape),
                };

                Ok(byte.into())
            }

            EscapedLineBreak => Ok(b'\n'.into()),

            HexByte => {
                let slice = &slice[2..];
                // unwrapping here is OK because the slice contains exactly two hex digits
                Ok(u8::from_str_radix(slice, 16).unwrap().into())
            }

            DecByte => {
                let slice = &slice[1..];
                if let Ok(byte) = slice.parse::<u8>() {
                    Ok(byte.into())
                } else {
                    Err(ByteLitOverflow)
                }
            }

            Utf8 => {
                let slice = &slice[3..slice.len() - 1]; // we want XXX from \u{XXX}

                u32::from_str_radix(slice, 16)
                    .map_err(|_| Utf8Overflow)
                    .and_then(|x| utf8_rfc2279::encode_u32(x).map_err(|_| Utf8Overflow))
                    .map(|bytes| StringTokenIterator::ArrayVec(bytes.into_iter()))
            }

            Char => Ok(slice.as_bytes().iter().into()),

            Error => Err(UnescapedNewline),
        }
    }
}

enum StringTokenIterator<'a> {
    Once(Once<u8>),
    Slice(slice::Iter<'a, u8>),
    ArrayVec(<utf8_rfc2279::Codepoint as IntoIterator>::IntoIter),
}

impl<'a> Iterator for StringTokenIterator<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Once(once) => once.next(),
            Self::Slice(slice) => slice.next().copied(),
            Self::ArrayVec(iter) => iter.next(),
        }
    }
}

impl<'a> From<Once<u8>> for StringTokenIterator<'a> {
    fn from(once: Once<u8>) -> Self {
        Self::Once(once)
    }
}

impl<'a> From<u8> for StringTokenIterator<'a> {
    fn from(byte: u8) -> Self {
        iter::once(byte).into()
    }
}

impl<'a> From<slice::Iter<'a, u8>> for StringTokenIterator<'a> {
    fn from(iter: slice::Iter<'a, u8>) -> Self {
        Self::Slice(iter)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ParseLitError {
    InvalidEscape,
    ByteLitOverflow,
    Utf8Overflow,
    UnescapedNewline,
}

impl ParseLitError {
    pub fn get_message(self) -> &'static str {
        match self {
            Self::InvalidEscape => "invalid escape character",
            Self::ByteLitOverflow => "overflow in byte literal",
            Self::Utf8Overflow => "overflow in utf-8 literal",
            Self::UnescapedNewline => "newline without escaping `\\` or `\\z`",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn simple() {
        let inputs: Vec<&str> = vec![
            r#"alo\n123""#,
            r#"alo\n123\""#,
            r#"\97lo\10\04923""#,
            r#"\u{61}\x6c\u{6f}\x0a\u{31}\x32\u{33}\x22"#,
        ];
        let output = b"alo\n123\"";

        for input in inputs {
            let (bytes, errors) = parse_short_string(input);
            assert!(errors.is_empty());
            assert_eq!(&bytes, output)
        }
    }

    #[test]
    pub fn normalization() {
        let tests: Vec<(Vec<u8>, &[u8])> = vec![
            (b"\r\n\r\n\r\n\r\n".to_vec(), b"\n\n\n\n"),
            (
                b"hello\nhi\r\nsalut\n\rbye\r".to_vec(),
                b"hello\nhi\nsalut\nbye\n",
            ),
            (
                b"word\r\rword\n\nword\r\n\n\rword\n\r\r\n".to_vec(),
                b"word\n\nword\n\nword\n\nword\n\n",
            ),
            (b"\n\r\n\r\n\r\n\r\n\r".to_vec(), b"\n\n\n\n\n"),
            (b"\r\r\r\r\r".to_vec(), b"\n\n\n\n\n"),
            (b"".to_vec(), b""),
            (b"no newlines here".to_vec(), b"no newlines here"),
        ];

        for (mut input, expected) in tests {
            normalize_newlines(&mut input);
            assert_eq!(&input, expected)
        }
    }
}
