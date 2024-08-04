use std::path::PathBuf;
use std::process::Command;
use lang::parser::Parser;
use lang::transpilers::python_transpiler::transpile;

fn main() {
    
    let path = PathBuf::from("test.lang");
    let mut parser = Parser::from_path(path);

    let ast = parser.parse();

    println!("\n\n");
    println!("-----------------------------------------------------");

    for node in ast.children.borrow().iter() {
        println!("{:?}", node);
    }


    println!("\n----------------------------- [transpile to python] ------------------------");
    let pycode = transpile(&ast);
    println!("{}", pycode);
    println!("-------------------------------[py output ]----------------------");

    // run the python code 
    let mut py_process = Command::new("py")
        .arg("-c")
        .arg(&pycode)
        .spawn()
        .expect("failed to execute process");    
}   
