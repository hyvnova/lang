# WARP.md

This file provides guidance to WARP (warp.dev) when working with code in this repository.

## Commands

### Build and run
- Build debug binary:
  - `cargo build`
- Build optimized binary:
  - `cargo build --release`
- Run the default binary (parses `test.lang`, prints the AST, transpiles to Python, and executes `out.py` via `py`):
  - `cargo run`
- Run the CLI (preferred for normal usage):
  - Run a `.lang` file with the Python transpiler (default):
    - `cargo run -- run path/to/file.lang`
  - Evaluate an inline snippet:
    - `cargo run -- eval "1..10"`

Note: execution shells out to the `py` command (`py out.py` or `py -c <code>`). On non-Windows systems you may need to adjust the code or ensure a `py` launcher is available.

### Linting and formatting
- Format Rust code:
  - `cargo fmt`
- Lint with Clippy (all targets, all features):
  - `cargo clippy --all-targets --all-features`

### Tests
- There are currently no Rust tests defined in this crate, but standard Cargo testing applies once tests are added.
- Run all tests:
  - `cargo test`
- Run tests whose names contain a substring (example):
  - `cargo test parser`

### Lexer benchmark
- The lexer benchmark harness is in `src/benchmark.rs` and uses sample inputs under `lexer_benchmark_data/`.
- To benchmark, either:
  - Temporarily call `benchmark::benchmark_lex(...)` from `main` or another binary target, or
  - Create a dedicated `benches/` target or test that wraps `benchmark_lex`.

## High-level architecture

### Language pipeline
- The language is implemented as a transpiler-based scripting language that compiles to Python.
- Core flow for both `main` and the CLI:
  1. Read source (`Parser::from_path` for files, `Parser::new` for inline strings).
  2. Lex source into tokens via `lexer::Lexer`.
  3. Parse tokens into an abstract syntax tree (`ast::AST` of `ast::Node`).
  4. (Reserved) Optionally analyze/optimize the AST (`analyzer::analyze` – currently a stub).
  5. Transpile AST to target code using a language-specific transpiler (currently Python only).
  6. Execute the generated code via the appropriate runtime (`py`).

### Entry points and CLI
- `src/main.rs`
  - Default binary entrypoint.
  - With no CLI arguments, it:
    - Parses `test.lang` using `Parser::from_path`.
    - Prints the AST for the top-level scope.
    - Transpiles to Python via `Transpiler::new().transpile(&ast)` (Python transpiler) and prints/writes `out.py`.
    - Executes `out.py` with `py out.py`.
  - With CLI arguments, it immediately delegates to `lang::cli::main()`.
- `src/cli.rs`
  - Defines a Clap-based CLI with two subcommands:
    - `Run { file, transpiler }` – parse and run a `.lang` file.
    - `Eval { code, transpiler }` – parse and run an inline string of Lang code.
  - `run_lang` drives the pipeline:
    - Builds a `Parser` for the given input.
    - Calls `parser.parse()`.
    - Passes the resulting AST to the appropriate transpiler via `use_transpiler!`.
    - Prints the transpiled code with line numbers (using `log_utils::add_line_numbers`).
    - Executes the Python code via `py -c`.

### Core language implementation
- `src/lib.rs`
  - Exposes the main modules: `cli`, `parse_utils`, `lexer`, `parser`, `ast`, `signal`, `transpilers`, `log_utils`, and `hyvnts_tools`.
- `src/lexer.rs`
  - Defines the `Kind` enum for all token types and the `Token` struct with position metadata.
  - `Lexer` supports:
    - Keywords (`fn`, `loop`, `for`, `while`, `if`, `else`, `elif`, `return`, etc.).
    - Signals and reactive constructs via `$` and `#[python] ... #[endpython]` blocks.
    - A rich set of operators, including ranges (`..`, `..=`), distribution (`->`), pipes (`|>`, `<|`), walrus (`:=`), and more.
  - Errors are reported via the `error!` macro (`log_utils::log_error`), which prints highlighted source context.
- `src/ast.rs`
  - Central definition of the `Node` enum for all language constructs, including:
    - Expressions (numbers, strings, identifiers, arrays, ranges, member access, indexing, function calls, lambdas, etc.).
    - Control flow (conditionals, loops, break/continue).
    - Assignment/aliasing, destructuring, and distribution operators.
    - Signals and reactive statements (`Signal`, `SignalDef`, `SignalUpdate`, `ReactiveStmt`).
  - `AST` manages a stack of scopes (`Vec<Vec<Node>>`) and helper methods for:
    - Scope management (`new_scope`, `pop_scope`, `current_scope`).
    - Manipulating the current scope (`add_node`, `pop_node`, `pop_until_non_space`, `solve_pop`).
    - Finding signal dependencies (`find_signal_deps`).
  - If you extend the language, this is the canonical place to add new node variants before updating the parser and transpilers.
- `src/parser.rs`
  - Implements a hand-written, stateful parser over `Lexer` that builds an `AST`.
  - Key concepts:
    - `Parser` maintains stacks for stop tokens (`stops`, `stopped_at`) and a `global_stop` to control recursive parsing.
    - `parse_until` builds a temporary scope until a stop token is encountered.
    - `parse_node` is the main dispatch that consumes tokens and constructs `Node`s using helper macros (`bi!` for boxing, `error!`, `log!`).
    - Handles complex constructs such as:
      - Function definitions and calls.
      - Loops (`loop`, `for`, `while`), conditionals (`if`/`elif`/`else`).
      - Signals and reactive statements (`$` prefix and `$ { ... }`).
      - Distribution and chained distribution via `->`.
      - Arrays/indexing, named arguments, deconstruction, and ranges.
  - `parse_utils` provides traits (`IsKind`, `GetFirstOrElse`) used heavily throughout the parser for concise control flow.
- `src/signal.rs`
  - Utilities for working with signal dependencies.
  - `clean_signals` filters out signals whose dependencies are already covered by others, using `AST::find_signal_deps`.
- `src/analyzer.rs`
  - Placeholder for future semantic/optimization passes over the AST (`analyze(ast: &mut AST)`).

### Transpilation and Python runtime
- `src/transpilers/mod.rs`
  - Declares the available transpilers (`python_transpiler` is active; `rust_transpiler` is present but commented out).
  - Exposes the `use_transpiler!` macro:
    - Dispatches by string name (e.g., `"python"`) to the corresponding transpiler and returns the generated code.
    - Adding a new target language requires both a new `*_transpiler.rs` file and a new match arm here.
- `src/transpilers/python_transpiler.rs`
  - Defines the `Transpiler` struct and a `Transpile` trait implemented for `Node` (and other structures as needed).
  - Responsible for converting `AST` into valid Python source code.
  - Notable behaviors:
    - `BUILTINS_PATH` points to `./src/transpilers/python/custom_builtins/`.
    - On start, it conditionally injects imports for custom builtins if the directory exists (skipping files whose names start with `_`).
    - Controls indentation via a `RefCell<usize>` and `indent_char`.
    - Supports formatting features such as string interpolation (converting `%a` / `{a}` style syntax into Python f-strings).
    - Optional feature flags:
      - `auto_sequence_to_iterator` (enabled by default) wraps sequences/arrays in an `Iterator` helper.
      - `auto_vars` is present but disabled by default.
- `src/transpilers/python/custom_builtins/`
  - Python modules that implement runtime behavior for language features, notably:
    - `signals.py` – `Signal` and `ReactiveStmt` runtime implementations used by signal/reactive AST nodes.
    - `iterator.py`, `option.py`, `typecasting.py`, `class_behaviors.py` – additional helpers for the generated Python code.
  - `__init__.py` contains `set_builtins`, which can patch a Python file to import all non-underscored modules from this directory.

### Utilities and support modules
- `src/log_utils.rs`
  - Provides `log_error` and the `error!` macro for rich error reporting, including line/column markers and colored output.
  - Provides a `log!` macro (debug-only) for structured console logging.
  - `add_line_numbers` helper formats source with `N | line` prefixes; used when printing transpiled code.
- `src/hyvnts_tools/`
  - `strings.rs` defines the `StrUtils` trait (e.g., `capitalize`, `title`, `swapcase`, `strip`) implemented for `String`.
  - `all.rs` re-exports helpers; `mod.rs` exposes the module tree.
  - These tools are used by the Python transpiler (for example, capitalizing boolean literals).
- `src/benchmark.rs` and `lexer_benchmark_data/`
  - `benchmark_lex` measures lexer performance against large sample inputs.
  - Use this when optimizing `lexer.rs` or experimenting with tokenization performance.

## When extending the language
- Add or modify syntax:
  - Update `Kind` and tokenization rules in `lexer.rs`.
  - Extend `Node` in `ast.rs` with new variants as needed.
  - Teach `parser.rs` how to parse the new constructs into those AST nodes.
- Make features executable:
  - Implement code generation for new `Node` variants in the Python transpiler (and any future transpilers).
  - If runtime support is required (e.g., new collection or control-flow primitives), add or update Python helpers in `src/transpilers/python/custom_builtins/`.
- Keep `use_transpiler!` and `cli.rs` in sync when adding new target languages or transpilation modes.
