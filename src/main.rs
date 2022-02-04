use std::{env, fs};

fn main() {
    let source = fs::read_to_string(env::args().nth(1).unwrap()).unwrap();

    let parsed = boolua::parse_source(&source);
    let _ = dbg!(parsed);
}
