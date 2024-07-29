/// Test Numbers
/// Ensuring that lexer can correctly tokenize numbers (integers and floats)
use lang::lexer::{Lexer, TokenKind};


/// Generate 3 sets of N-digit random numbers
/// Ensure that the lexer can correctly tokenize them
/// TODO: Negative numbers test 
#[test]
fn test_numbers() {
    
    
    const N: usize = 10; // Number of random numbers generated per test

    let mut source: String = String::new();

    // INTS
    println!("Testing lexer: ints.\nSource: ");

    let mut ints: Vec<u32> = Vec::with_capacity(N);

    for i in 0..N {
        let n: u32 = rand::random();
        source.push_str(&n.to_string());
        source.push_str(" ");
        
        ints.push(n);
        
        print!("{} ", n);
    }
    println!();

    // Verify that the lexer can correctly tokenize the numbers
    let mut lexer = Lexer::new(source);

    for i in 0..N {
        let token = lexer.next().expect("Expected a NUMBER token");
        assert_eq!(token.kind, TokenKind::NUMBER);
        assert_eq!(token.value, ints[i].to_string());
    }
    
    // FLOATS
    let mut source: String = String::new();
    let mut floats: Vec<f32> = Vec::with_capacity(N);

    println!("Testing lexer: floats.\nSource: ");
    for i in 0..N {
        let n: f32 = rand::random::<f32>().abs();
        source.push_str(&n.to_string());
        source.push_str(" ");

        floats.push(n);        

        print!("{} ", n);
    }
    println!();

    // Verify that the lexer can correctly tokenize the numbers
    let mut lexer = Lexer::new(source);

    for i in 0..N {
        let token = lexer.next().expect("Expected a NUMBER token");
        assert_eq!(token.kind, TokenKind::NUMBER);
        assert_eq!(token.value, floats[i].to_string());
    }
}
