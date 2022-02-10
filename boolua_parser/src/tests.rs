use insta::assert_debug_snapshot;
use paste::paste;

macro_rules! parse_and_verify {
    ($name:ident, $path:literal) => {
        paste! {
            #[test]
            fn [<parse_and_verify_ $name>]() {
                let source = std::fs::read_to_string($path).unwrap();
                let ast = crate::parse_source(&source).expect("parsing generated errors");
                assert_debug_snapshot!(ast);
            }
        }
    };
}

parse_and_verify!(simple_if, "../programs/simple_if.lua");
parse_and_verify!(simple_expr, "../programs/simple_expr.lua");
parse_and_verify!(numbers, "../programs/numbers.lua");
parse_and_verify!(ambiguity1, "../programs/ambiguous.lua");
parse_and_verify!(ambiguity2, "../programs/unambiguous.lua");
