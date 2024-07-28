
mod lexer;
mod log_utils;
mod benchmark;
mod parser;
mod ast;

use std::path::PathBuf;

use lexer::Lexer;
use parser::Parser;

fn main() {

    let path = PathBuf::from("test.lang");
    let mut parser = Parser::new(path);

    let ast = parser.parse();

    println!("\n\n");
    println!("-----------------------------------------------------");

    for node in ast.childrem.borrow().iter() {
        println!("{:?}", node);
    }
}   
