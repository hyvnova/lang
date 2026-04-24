mod common;

use common::{lex, lex_kinds};
use lang::lexer::Kind;

#[test]
fn lexes_core_syntax_tokens() {
    let kinds = lex_kinds("$a = 1..=3 -> print($a)");

    assert_eq!(
        kinds,
        vec![
            Kind::DOLLAR_SING,
            Kind::IDENTIFIER,
            Kind::ASSIGN,
            Kind::NUMBER,
            Kind::D_DOT,
            Kind::ASSIGN,
            Kind::NUMBER,
            Kind::R_ARROW,
            Kind::IDENTIFIER,
            Kind::L_PARENT,
            Kind::DOLLAR_SING,
            Kind::IDENTIFIER,
            Kind::R_PARENT,
        ]
    );
}

#[test]
fn lexes_rust_like_object_keywords() {
    let tokens = lex("struct User<T> trait Named impl<T> Named<T> for User<T>");

    assert_eq!(
        tokens
            .into_iter()
            .map(|(kind, value)| format!("{kind}:{value}"))
            .collect::<Vec<String>>(),
        vec![
            "STRUCT:struct",
            "IDENTIFIER:User",
            "LT:<",
            "IDENTIFIER:T",
            "GT:>",
            "TRAIT:trait",
            "IDENTIFIER:Named",
            "IMPL:impl",
            "LT:<",
            "IDENTIFIER:T",
            "GT:>",
            "IDENTIFIER:Named",
            "LT:<",
            "IDENTIFIER:T",
            "GT:>",
            "FOR:for",
            "IDENTIFIER:User",
            "LT:<",
            "IDENTIFIER:T",
            "GT:>",
        ]
    );
}

#[test]
fn lexes_attribute_macro_tokens_without_affecting_hash_python_blocks() {
    let tokens = lex("#derive(debug)\nstruct Square { w: int }\n");

    assert_eq!(
        tokens
            .into_iter()
            .take(9)
            .map(|(kind, value)| format!("{kind}:{value}"))
            .collect::<Vec<String>>(),
        vec![
            "HASH:#",
            "IDENTIFIER:derive",
            "L_PARENT:(",
            "IDENTIFIER:debug",
            "R_PARENT:)",
            "NEW_LINE:\n",
            "STRUCT:struct",
            "IDENTIFIER:Square",
            "L_BRACKET:{",
        ]
    );

    assert_eq!(lex_kinds("#[python]\nprint('ok')\n#[endpython]"), vec![Kind::PYTHON]);
}
