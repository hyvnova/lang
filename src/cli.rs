use crate::{code_highlight::highlight_code, parser::Parser as LangParser, transpilers, use_transpiler};
use std::path::PathBuf;

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(version = "1.0", about = "Simple CLI to use lang...")]
struct Args {
    /// Action to perform.
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run file 
    // ```shell
    // lang run <file: path> [tranpiler: str = "python"]
    // ```
    Run {
        // File to run
        file: PathBuf,

        // Transpiler to use
        #[arg(short, long, default_value = "python")]
        transpiler: Option<String>,
    },

    /// Eval code
    // ```shell
    // lang eval <code: str> [tranpiler: str = "python"]
    // ```
    Eval {
        /// Code to run
        code: String,

        /// Transpiler to use. E.g. python
        #[arg(short, long, default_value = "python")]
        transpiler: Option<String>,
    },
}

pub fn main() {
    let args = Args::parse();

    match args.command {
        Commands::Run { file, transpiler } => {
            let parser = LangParser::from_path(file); 
            run_lang(parser, transpiler.unwrap());
        },

        Commands::Eval { code, transpiler } => {
            let parser = LangParser::new(code);
            run_lang(parser, transpiler.unwrap());
        }
    }
}

fn run_lang(mut parser: LangParser, transpiler: String) {

    // * Parse file
    let ast = parser.parse();

    // * Transpile 
    let code = use_transpiler!(transpiler, ast);

    // * Print code
    println!("{}", highlight_code(&transpiler, &code));

    // * Execute code
    // TODO: Add support for other languages, for now I won't bother.
    std::process::Command::new("py")
        .arg("-c")
        .arg(&code)
        .spawn()
        .expect("failed to execute process");    
}