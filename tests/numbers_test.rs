/// Test Numbers
/// Ensuring that lexer can correctly tokenize numbers (integers and floats)
use lang::lexer::{Lexer, TokenKind};


/// Generate 3 sets of N-digit random numbers
/// Ensure that the lexer can correctly tokenize them
#[test]
fn test_numbers() {
    const N: usize = 10; // Number of random numbers generated per test

    let mut source: String = String::new();

    // INTS
    println!("Testing lexer: ints.\nSource: ");

    let mut ints: Vec<i32> = Vec::with_capacity(N);

    for i in 0..N {
        let n: i32 = rand::random();
        source.push_str(&n.to_string());
        source.push_str(" ");
        
        ints.push(n);
        
        print!("{} ", n);
    }
    println!();

    // Verify that the lexer can correctly tokenize the numbers
    let mut lexer = Lexer::new(source);

    for i in 0..N {
        let n = ints[i];

        if n < 0 {
            let token = lexer.next().expect(&format!("Expected a SUBTRACT token, since the number is negative: {n}"));
            assert_eq!(token.kind, TokenKind::SUBTRACT);

            let token = lexer.next().expect("Expected a NUMBER token, with value: {n}");
            assert_eq!(token.value, n.to_string()[1..], "Expected NUMBER token to have value: {n}, but got: {}", token.value);
        } else {

            let token = lexer.next().expect("Expected a NUMBER token");

            assert_eq!(token.kind, TokenKind::NUMBER);
            assert_eq!(token.value, ints[i].to_string());
        }
    }
    
    // FLOATS
    let mut source: String = String::new();
    let mut floats: Vec<f32> = Vec::with_capacity(N);

    println!("Testing lexer: floats.\nSource: ");
    for i in 0..N {
        let n: f32 = rand::random::<f32>();
        source.push_str(&n.to_string());
        source.push_str(" ");

        floats.push(n);        

        print!("{} ", n);
    }
    println!();

    // Verify that the lexer can correctly tokenize the numbers
    let mut lexer = Lexer::new(source);

    for i in 0..N {
        let n = floats[i];

        if n < 0.0 {
            let token = lexer.next().expect("Expected a SUBTRACT token, since the number is negative");
            assert_eq!(token.kind, TokenKind::SUBTRACT);

            let token = lexer.next().expect("Expected a NUMBER token");
            assert_eq!(token.value, n.to_string());
        } else {
            let token = lexer.next().expect("Expected a NUMBER token");
            assert_eq!(token.kind, TokenKind::NUMBER);
            assert_eq!(token.value, floats[i].to_string());
        }
    }
}
