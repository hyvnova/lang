use crate::{
    config::{self, ConfigError},
    language_features,
    log_utils::add_line_numbers,
    modules,
    parser::Parser as LangParser,
    transpilers, use_transpiler,
};
use std::path::PathBuf;

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(version = "1.0", about = "Simple CLI to use lang...")]
struct Args {
    /// Action to perform.
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Check project metadata and generated tool contracts.
    Check {
        #[command(subcommand)]
        command: CheckCommands,
    },

    /// Run file
    // ```shell
    // lang run <file: path> [tranpiler: str = "python"]
    // ```
    Run {
        // File to run
        file: Option<PathBuf>,

        /// Project root used for absolute module imports.
        #[arg(long)]
        project_root: Option<PathBuf>,

        // Transpiler to use
        #[arg(short, long)]
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

    /// Start the Lang language server over stdio.
    Lsp,
}

#[derive(Subcommand)]
enum CheckCommands {
    /// Print syntax highlighting coverage gaps.
    SyntaxCoverage {
        /// Exit with a non-zero status when coverage is missing.
        #[arg(long)]
        strict: bool,
    },
}

pub fn main() {
    let args = Args::parse();

    match args.command {
        Some(Commands::Check { command }) => match command {
            CheckCommands::SyntaxCoverage { strict } => match run_syntax_coverage_check(strict) {
                Ok(()) => {}
                Err(error) => exit_with_error(error),
            },
        },

        Some(Commands::Run {
            file,
            project_root,
            transpiler,
        }) => match run_project_command(file, project_root, transpiler) {
            Ok(output) => exit_with_output(output),
            Err(error) => exit_with_error(error),
        },

        Some(Commands::Eval { code, transpiler }) => {
            let parser = LangParser::new(code);
            run_lang(parser, transpiler.unwrap_or_else(|| "python".to_string()));
        }
        Some(Commands::Lsp) => run_lsp(),
        None => match run_manifest_from_cwd() {
            Ok(output) => exit_with_output(output),
            Err(error) => exit_with_error(error),
        },
    }
}

fn run_lsp() {
    tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .expect("failed to create Tokio runtime for Lang LSP")
        .block_on(crate::lsp::serve_stdio());
}

fn run_syntax_coverage_check(strict: bool) -> Result<(), ConfigError> {
    let cwd = std::env::current_dir()
        .map_err(|error| ConfigError::new(format!("Failed to get current directory: {error}")))?;
    let (report, has_missing) =
        language_features::syntax_coverage_report(&cwd).map_err(ConfigError::new)?;
    println!("{report}");

    if strict && has_missing {
        Err(ConfigError::new("syntax coverage check failed"))
    } else {
        Ok(())
    }
}

pub fn run_manifest_from_cwd() -> Result<std::process::Output, ConfigError> {
    run_project_command(None, None, None)
}

fn run_project_command(
    file: Option<PathBuf>,
    project_root: Option<PathBuf>,
    transpiler: Option<String>,
) -> Result<std::process::Output, ConfigError> {
    let cwd = std::env::current_dir()
        .map_err(|error| ConfigError::new(format!("Failed to get current directory: {error}")))?;
    let resolved = config::resolve_run_config(file, project_root, transpiler, &cwd)?;

    modules::run_project(resolved.entry_file, resolved.project_root)
        .map_err(|error| ConfigError::new(error.message))
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

fn exit_with_output(output: std::process::Output) -> ! {
    print!("{}", String::from_utf8_lossy(&output.stdout));
    eprint!("{}", String::from_utf8_lossy(&output.stderr));
    std::process::exit(output.status.code().unwrap_or(1));
}

fn exit_with_error(error: ConfigError) -> ! {
    eprintln!("{}", error);
    std::process::exit(1);
}
