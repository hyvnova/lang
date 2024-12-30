use lang::log_utils::add_line_numbers;
use lang::parser::Parser;
use lang::transpilers::python_transpiler::Transpiler;
use std::path::PathBuf;
use std::process::Command;

use std::fs::File;
use std::io::prelude::*;


const OUTPUT_MODE: i8 = 2; // 1. console, 2. file, 0. both

fn main() {
    // If arguments are passed, run cli
    if std::env::args().len() > 1 {
        lang::cli::main();
        return;
    }

    let path = PathBuf::from("test.lang");
    let mut parser = Parser::from_path(path);

    parser.parse();
    let ast =  &parser.ast;

    println!("\n\n");
    println!("-----------------------------------------------------");

    for node in ast.get_scope() {
        println!("{:?}", node);
    }

    let code: String = Transpiler::new().transpile(ast);

    if OUTPUT_MODE == 0 || OUTPUT_MODE == 1 {
        println!("\n----------------------------- [transpile to python] ------------------------");
        // *1. Write code to console

        // First 10 lines are imports so we skip them
        println!(
            "{}",
            add_line_numbers(
                &code.lines().skip(10).collect::<Vec<&str>>().join("\n")
            )
        );
    }

    if OUTPUT_MODE == 0 || OUTPUT_MODE == 2 {
        // * 2.  Wrtie code to file
        let mut file = File::create("out.py").expect("Unable to create file");
        file.write_all(&code.as_bytes()).expect("Unable to write data");
    }

    println!("-------------------------------[py output ]----------------------");

    // run the python code
    let mut handle = Command::new("py")
        .arg("out.py")
        .spawn()
        .expect("failed to execute process");


    handle.wait().unwrap();
}
