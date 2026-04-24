use crate::{
    log_utils::add_line_numbers, modules, parser::Parser as LangParser, transpilers, use_transpiler,
};
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

        /// Project root used for absolute module imports.
        #[arg(long)]
        project_root: Option<PathBuf>,

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
        Commands::Run { file, project_root, transpiler } => {
            if transpiler.as_deref() != Some("python") {
                panic!("Only the python transpiler supports modules right now.");
            }

            let output = modules::run_project(file, project_root)
                .unwrap_or_else(|error| panic!("{}", error));

            print!("{}", String::from_utf8_lossy(&output.stdout));
            eprint!("{}", String::from_utf8_lossy(&output.stderr));
            std::process::exit(output.status.code().unwrap_or(1));
        }

        Commands::Eval { code, transpiler } => {
            let parser = LangParser::new(code);
            run_lang(parser, transpiler.unwrap());
        }
    }
}

fn run_lang(mut parser: LangParser, transpiler: String) {
    // * Parse file
    parser.parse();
    let ast = &parser.ast;

    // * Transpile
    let code = use_transpiler!(transpiler, ast);

    // * Print code
    println!("{}", add_line_numbers(&code));

    // * Execute code
    // TODO: Add support for other languages, for now I won't bother.
    let status = std::process::Command::new("py")
        .arg("-c")
        .arg(&code)
        .status()
        .expect("failed to execute process");

    std::process::exit(status.code().unwrap_or(1));
}
