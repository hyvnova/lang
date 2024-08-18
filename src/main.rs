use lang::log_utils::add_line_numbers;
use lang::parser::Parser;
use lang::transpilers::python_transpiler::transpile;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    // If arguments are passed, run cli
    if std::env::args().len() > 1 {
        lang::cli::main();
        return;
    }

    let path = PathBuf::from("test.lang");
    let mut parser = Parser::from_path(path);

    let ast = parser.parse();

    println!("\n\n");
    println!("-----------------------------------------------------");

    for node in ast.children.borrow().iter() {
        println!("{:?}", node);
    }

    println!("\n----------------------------- [transpile to python] ------------------------");
    let code = transpile(&ast);

    // First 10 lines are imports so we skip them
    println!(
        "{}",
        add_line_numbers(
            &code.lines().skip(10).collect::<Vec<&str>>().join("\n")
            // &code
        )
    );
    println!("-------------------------------[py output ]----------------------");

    // run the python code
    Command::new("py")
        .arg("-c")
        .arg(&code)
        .spawn()
        .expect("failed to execute process");
}
