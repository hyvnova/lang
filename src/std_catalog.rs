use crate::api_docs::{DocBlock, DocExample, DocParam};

pub const STD_ROOT: &str = "std";

pub const STD_MODULES: &[StdModule] = &[
    StdModule::new(
        "core",
        DocBlock::module(
            "std.core",
            "Core constructors and debugging helpers on the global `std` object.",
        ),
        &[
            StdExport::new("Some", "Option constructor"),
            StdExport::new("Nothing", "Option absence value"),
            StdExport::new("Ok", "Result success constructor"),
            StdExport::new("Err", "Result error constructor"),
            StdExport::new("panic", "Abort with an error message"),
            StdExport::new("type_name", "Return a Lang runtime type name"),
            StdExport::new("debug", "Return a debug string"),
        ],
    ),
    StdModule::new(
        "iter",
        DocBlock::module(
            "std.iter",
            "Iterator helpers for mapping, filtering, folding, and collecting values from the global `std` object.",
        ),
        &[
            StdExport::new("Iterator", "Iterator runtime wrapper"),
            StdExport::new("iter", "Create an iterator"),
            StdExport::new("range", "Create a numeric range"),
            StdExport::new("enumerate", "Yield index/value pairs"),
            StdExport::new("zip", "Zip iterables"),
            StdExport::new("map", "Map values"),
            StdExport::new("filter", "Filter values"),
            StdExport::new("fold", "Fold values into an accumulator"),
            StdExport::new("collect", "Collect an iterator"),
            StdExport::new("any", "Return true if any value matches"),
            StdExport::new("all", "Return true if all values match"),
        ],
    ),
    StdModule::new(
        "text",
        DocBlock::module(
            "std.text",
            "Compatibility text functions. Prefer method style like `text.strip()` for everyday Lang code through the global `std` object.",
        ),
        &[
            StdExport::new("upper", "Uppercase text"),
            StdExport::new("lower", "Lowercase text"),
            StdExport::new("title", "Title-case text"),
            StdExport::new("strip", "Trim leading and trailing whitespace"),
            StdExport::new("split", "Split text"),
            StdExport::new("join", "Join text values"),
            StdExport::new("replace", "Replace text"),
            StdExport::new("contains", "Check whether text contains a value"),
            StdExport::new("starts_with", "Check text prefix"),
            StdExport::new("ends_with", "Check text suffix"),
        ],
    ),
    StdModule::new(
        "collections",
        DocBlock::module(
            "std.collections",
            "Collection runtime types available from the global `std` object.",
        ),
        &[
            StdExport::new("Vec", "Lang vector"),
            StdExport::new("Map", "Lang map"),
            StdExport::new("Set", "Lang set"),
            StdExport::new("Counter", "Count values"),
        ],
    ),
    StdModule::new(
        "math",
        DocBlock::module(
            "std.math",
            "Small numeric helpers available from the global `std` object.",
        ),
        &[
            StdExport::new("min", "Minimum value"),
            StdExport::new("max", "Maximum value"),
            StdExport::new("abs", "Absolute value"),
            StdExport::new("round", "Round a number"),
            StdExport::new("floor", "Floor a number"),
            StdExport::new("ceil", "Ceiling of a number"),
            StdExport::new("sqrt", "Square root"),
            StdExport::new("clamp", "Clamp a number to a range"),
        ],
    ),
    StdModule::new(
        "random",
        DocBlock::module(
            "std.random",
            "Random values, choices, and shuffling helpers available from the global `std` object.",
        ),
        &[
            StdExport::new("rand", "Random float"),
            StdExport::new("randint", "Random integer in a range"),
            StdExport::new("choice", "Choose a random item"),
            StdExport::new("shuffle", "Shuffle values"),
        ],
    ),
    StdModule::new(
        "path",
        DocBlock::module(
            "std.path",
            "Path helpers and path runtime wrappers available from the global `std` object.",
        ),
        &[
            StdExport::new("Path", "Path runtime wrapper"),
            StdExport::new("cwd", "Current working directory"),
            StdExport::new("home", "Home directory"),
        ],
    ),
    StdModule::new(
        "fs",
        DocBlock::module(
            "std.fs",
            "File-system helpers available from the global `std` object` for small scripts and tools.",
        ),
        &[
            StdExport::documented(
                "read_text",
                DocBlock::full(
                    "Read a UTF-8 text file and return a `Result` instead of crashing.",
                    "std.fs.read_text(path: Str) -> Result<Str, Err>",
                    &[DocParam::new("path", "File path to read, relative to the process working directory unless absolute.")],
                    "`Ok(Str)` with the file contents when the read succeeds.",
                    "`Err(error)` when the file is missing, unreadable, or cannot be decoded as text.",
                    &[DocExample::new(
                        "Read and transform a file",
                        "text = std.fs.read_text(\"input.txt\")\n    .expect(\"Failed to read input.txt\")\n    .strip()\n\nprint(text)",
                    )],
                    "Use `.expect(message)` for tiny scripts and explicit `is_ok` / `is_err` handling when failure is part of the program flow.",
                ),
            ),
            StdExport::documented(
                "write_text",
                DocBlock::full(
                    "Write UTF-8 text to a file and report success or failure as `Result`.",
                    "std.fs.write_text(path: Str, value: Str) -> Result<Bool, Err>",
                    &[
                        DocParam::new("path", "Destination file path."),
                        DocParam::new("value", "Text to write. Lang primitives are unwrapped at the std boundary."),
                    ],
                    "`Ok(Bool(true))` when the write succeeds.",
                    "`Err(error)` when the path cannot be created or written.",
                    &[DocExample::new(
                        "Write output",
                        "std.fs.write_text(\"output.txt\", text.upper())\n    .expect(\"Failed to write output.txt\")",
                    )],
                    "This overwrites the target file.",
                ),
            ),
            StdExport::new("exists", "Check whether a path exists"),
            StdExport::new("is_file", "Check whether a path is a file"),
            StdExport::new("is_dir", "Check whether a path is a directory"),
            StdExport::new("list_dir", "List directory entries"),
        ],
    ),
    StdModule::new(
        "env",
        DocBlock::module(
            "std.env",
            "Environment and process helpers available from the global `std` object.",
        ),
        &[StdExport::new("args", "Return CLI arguments")],
    ),
    StdModule::new(
        "io",
        DocBlock::module(
            "std.io",
            "Standard input and output helpers available from the global `std` object.",
        ),
        &[
            StdExport::new("read_line", "Read one line from stdin"),
            StdExport::new("read_to_end", "Read all stdin"),
            StdExport::new("print", "Print without forcing a newline"),
            StdExport::new("println", "Print with a newline"),
        ],
    ),
    StdModule::new(
        "time",
        DocBlock::module(
            "std.time",
            "Time and sleep helpers available from the global `std` object.",
        ),
        &[
            StdExport::new("now", "Current timestamp"),
            StdExport::new("sleep", "Sleep for seconds"),
            StdExport::new("millis", "Current time in milliseconds"),
        ],
    ),
    StdModule::new(
        "term",
        DocBlock::module(
            "std.term",
            "Terminal styling and prompt helpers available from the global `std` object.",
        ),
        &[
            StdExport::new("color", "Wrap text in an ANSI color"),
            StdExport::new("bold", "Wrap text in ANSI bold style"),
            StdExport::new("clear", "Clear the terminal"),
            StdExport::new("prompt", "Prompt for input"),
        ],
    ),
    StdModule::new(
        "primitives",
        DocBlock::module(
            "std.primitives",
            "Lang primitive runtime wrappers available from the global `std` object.",
        ),
        &[
            StdExport::new("Str", "Lang string primitive"),
            StdExport::new("Num", "Lang number primitive"),
            StdExport::new("Bool", "Lang boolean primitive"),
            StdExport::new("Vec", "Lang vector primitive"),
            StdExport::new("Map", "Lang map primitive"),
            StdExport::new("Set", "Lang set primitive"),
            StdExport::new("wrap", "Wrap a Python value as a Lang value"),
            StdExport::new("unwrap", "Unwrap a Lang value to a Python value"),
        ],
    ),
    StdModule::new(
        "prelude",
        DocBlock::module(
            "std.prelude",
            "Automatically imported common names backing the global Lang scripting experience.",
        ),
        &[
            StdExport::new("Some", "Option constructor"),
            StdExport::new("Nothing", "Option absence value"),
            StdExport::new("Ok", "Result success constructor"),
            StdExport::new("Err", "Result error constructor"),
            StdExport::new("Iterator", "Iterator runtime wrapper"),
            StdExport::new("iter", "Create an iterator"),
            StdExport::new("range", "Create a range"),
            StdExport::new("len", "Return length"),
            StdExport::new("print", "Print a value"),
            StdExport::new("input", "Read input"),
            StdExport::new("str", "Convert to Str"),
            StdExport::new("int", "Convert to Num"),
            StdExport::new("float", "Convert to Num"),
            StdExport::new("bool", "Convert to Bool"),
            StdExport::new("Str", "Lang string primitive"),
            StdExport::new("Num", "Lang number primitive"),
            StdExport::new("Bool", "Lang boolean primitive"),
            StdExport::new("Vec", "Lang vector primitive"),
            StdExport::new("Map", "Lang map primitive"),
            StdExport::new("Set", "Lang set primitive"),
            StdExport::new("wrap", "Wrap a Python value as a Lang value"),
            StdExport::new("unwrap", "Unwrap a Lang value to a Python value"),
        ],
    ),
];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StdModule {
    pub name: &'static str,
    pub docs: DocBlock,
    pub exports: &'static [StdExport],
}

impl StdModule {
    pub const fn new(name: &'static str, docs: DocBlock, exports: &'static [StdExport]) -> Self {
        Self {
            name,
            docs,
            exports,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StdExport {
    pub name: &'static str,
    pub docs: DocBlock,
}

impl StdExport {
    pub const fn new(name: &'static str, summary: &'static str) -> Self {
        Self {
            name,
            docs: DocBlock::brief(summary),
        }
    }

    pub const fn documented(name: &'static str, docs: DocBlock) -> Self {
        Self { name, docs }
    }
}

pub fn std_module(name: &str) -> Option<&'static StdModule> {
    STD_MODULES.iter().find(|module| module.name == name)
}

pub fn std_export(module: &str, name: &str) -> Option<&'static StdExport> {
    std_module(module).and_then(|module| module.exports.iter().find(|export| export.name == name))
}
