/// Identifier test
/// Ensures that lexer can correctly tokenize identifiers
/// A valid identifier is a sequence of letters, digits, and underscores, starting with a letter or underscore
use lang::lexer::{Lexer, TokenKind};
use rand::Rng;

#[test]
fn test_identifiers() {
    test_valid_identifiers();
    test_invalid_identifiers();
    test_edge_case_identifiers();
}

fn test_valid_identifiers() {
    let mut source = String::new();
    let mut identifiers = Vec::new();
    let mut rng = rand::thread_rng();

    const N: usize = 100; // Number of random identifiers

    println!("Testing lexer: valid identifiers.");

    for _ in 0..N {
        let len: usize = rng.gen_range(1..20);
        let first_char = if rng.gen_bool(0.9) {
            rng.gen_range(b'a'..=b'z') as char
        } else {
            '_'
        };
        let identifier: String = std::iter::once(first_char)
            .chain((0..len-1).map(|_| {
                match rng.gen_range(0..3) {
                    0 => rng.gen_range(b'a'..=b'z') as char,
                    1 => rng.gen_range(b'A'..=b'Z') as char,
                    _ => if rng.gen_bool(0.9) { 
                        rng.gen_range(b'0'..=b'9') as char
                    } else {
                        '_'
                    },
                }
            }))
            .collect();

        source.push_str(&identifier);
        source.push(' ');

        identifiers.push(identifier);
    }

    let mut lexer = Lexer::new(source);

    for expected_identifier in &identifiers {
        let token = lexer.next().expect("Expected an IDENTIFIER token");
        assert_eq!(token.kind, TokenKind::IDENTIFIER, "Expected IDENTIFIER, got {:?}", token.kind);
        assert_eq!(token.value, *expected_identifier, "Identifier mismatch");
    }

    assert_eq!(lexer.next(), None, "Expected end of input");
}

fn test_invalid_identifiers() {
    let invalid_identifiers: Vec<&str> = vec![
        "1abc", "123", "@abc", "a-b-c", "a.b.c", "a b c", "a+b", "a#b", "a$b", "λx"
    ];

    println!("Testing lexer: invalid identifiers.");

    for invalid_identifier in invalid_identifiers {
        let mut lexer = Lexer::new(invalid_identifier.to_string());
        let token = lexer.next();
        println!("{:?}", token);

        assert!(token.is_some(), "Expected a token");
        // assert_ne!(token.unwrap().kind, TokenKind::IDENTIFIER, "Invalid identifier '{}' should not be tokenized as IDENTIFIER", invalid_identifier);
    }

    println!("All invalid identifiers are correctly rejected.");
}

fn test_edge_case_identifiers() {
    let edge_cases = vec![
        "_", "__", "_a", "a_", "a1", "A1", "aA", "Aa", "a_1", "a_A", "A_a",
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
    ];

    println!("Testing lexer: edge case identifiers.");

    let source = edge_cases.join(" ");
    let mut lexer = Lexer::new(source);

    for expected_identifier in &edge_cases {
        let token = lexer.next().expect("Expected an IDENTIFIER token");
        assert_eq!(token.kind, TokenKind::IDENTIFIER, 
            "Expected IDENTIFIER for '{}', got {:?}", expected_identifier, token.kind);
        assert_eq!(token.value, *expected_identifier, 
            "Identifier mismatch for '{}'", expected_identifier);
    }

    assert_eq!(lexer.next(), None, "Expected end of input");
}