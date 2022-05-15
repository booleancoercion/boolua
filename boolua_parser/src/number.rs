use super::structs::Num;

use lazy_static::lazy_static;
use regex::{Match, Regex};

enum PlusMinus {
    Plus,
    Minus,
}

#[allow(clippy::unnecessary_unwrap)]
pub fn eval_num_lit<const RADIX: u8>(input: &str) -> Num {
    lazy_static! {
        static ref RE10: Regex = Regex::new(r"^(\d+)?\.?(\d+)?(?:[eE]([+-]?)(\d+))?$").unwrap();
        static ref RE16: Regex =
            Regex::new(r"^([[:xdigit:]]+)?\.?([[:xdigit:]]+)?(?:[pP]([+-]?)([[:xdigit:]]+))?$")
                .unwrap();
    }

    let captures = if RADIX == 10 {
        RE10.captures(input).unwrap()
    } else if RADIX == 16 {
        RE16.captures(input).unwrap()
    } else {
        unimplemented!()
    };

    let base = captures.get(1).as_ref().map(Match::as_str);
    let fraction = captures.get(2).as_ref().map(Match::as_str);
    let pm = if let Some(mat) = captures.get(3) {
        let string = mat.as_str();
        if string == "+" {
            PlusMinus::Plus
        } else if string == "-" {
            PlusMinus::Minus
        } else {
            unreachable!()
        }
    } else {
        PlusMinus::Plus
    };
    let exponent = captures.get(4).as_ref().map(Match::as_str);

    if base.is_some() && fraction.is_none() && exponent.is_none() {
        let base = base.unwrap();
        if RADIX == 16 {
            Num::Int(wrapping_hex_parse(base))
        } else if let Ok(result) = base.parse() {
            Num::Int(result)
        } else {
            Num::Float(base.parse().unwrap())
        }
    } else {
        let floaty_parse = |string: &str| {
            string.bytes().fold(0., |acc: f64, digit| {
                acc.mul_add(RADIX as f64, hex_digit(digit) as f64)
            })
        };

        let base: f64 = if let Some(base) = base {
            floaty_parse(base)
        } else {
            0.
        };

        let fraction: f64 = if let Some(fraction) = fraction {
            fraction.bytes().rev().fold(0., |acc, digit| {
                (acc + hex_digit(digit) as f64) * (RADIX as f64).recip()
            })
        } else {
            0.
        };

        let combined = base + fraction;

        let mut exponent = if let Some(exponent) = exponent {
            floaty_parse(exponent)
        } else {
            0.
        };

        exponent *= if let PlusMinus::Plus = pm { 1.0 } else { -1.0 };

        Num::Float(combined * (RADIX as f64).powf(exponent))
    }
}

fn hex_digit(digit: u8) -> i64 {
    (match digit {
        b'0'..=b'9' => digit - b'0',
        b'a'..=b'f' => digit - b'a' + 10,
        b'A'..=b'F' => digit - b'A' + 10,
        _ => unreachable!(),
    }) as i64
}

fn wrapping_hex_parse(string: &str) -> i64 {
    string.bytes().fold(0, |acc, digit| {
        acc.wrapping_shl(4).wrapping_add(hex_digit(digit))
    })
}
