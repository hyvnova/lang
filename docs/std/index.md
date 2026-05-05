# Lang Standard Library

Lang ships a compact standard library through two layers:

- a tiny prelude imported into generated project modules
- explicit modules under `std.*`

The compiler emits Python facade modules for `std`, then those facades call the native Rust `lang_std_native` runtime through PyO3.

## Prelude

These names are available in project code without an import:

```lang
value = Some("spark")
print(value.unwrap())
print(Ok(3).unwrap())
print(len(range(1, 4)))
print(" lamp ".strip().upper())
```

Prelude exports:

- `Some`, `Nothing`, `Ok`, `Err`
- `Iterator`, `iter`, `range`, `len`
- `Str`, `Num`, `Bool`, `Vec`, `Map`, `Set`, `wrap`, `unwrap`
- `print`, `input`, `str`, `int`, `float`, `bool`

## Global `std`

The `std` package is always available as a global object in compiled projects:

```lang
print(std.text.upper("lamp"))
print(std.fs.exists("notes.txt"))
print(std.term.bold("hot"))
```

This is the preferred style for short scripts and toy programs.

## Modules

Explicit imports still work when a shorter local name reads better:

```lang
from std.text import upper, contains
from std.random import choice
from std.term import color

print(upper("lamp"))
print(contains("lamp", "am"))
print(choice(range(1, 1)).__class__.__name__)
print(color("hot", "red"))
```

Available modules:

- `std.core`: `Some`, `Nothing`, `Ok`, `Err`, `panic`, `type_name`, `debug`
- `std.iter`: `Iterator`, `iter`, `range`, `enumerate`, `zip`, `map`, `filter`, `fold`, `collect`, `any`, `all`
- `std.text`: `upper`, `lower`, `title`, `strip`, `split`, `join`, `replace`, `contains`, `starts_with`, `ends_with`
- `std.collections`: `Vec`, `Map`, `Set`, `Counter`
- `std.math`: `min`, `max`, `abs`, `round`, `floor`, `ceil`, `sqrt`, `clamp`
- `std.random`: `rand`, `randint`, `choice`, `shuffle`
- `std.path`: `Path`, `cwd`, `home`
- `std.fs`: `read_text`, `write_text`, `exists`, `is_file`, `is_dir`, `list_dir`
- `std.env`: `args`
- `std.io`: `read_line`, `read_to_end`, `print`, `println`
- `std.time`: `now`, `sleep`, `millis`
- `std.term`: `color`, `bold`, `clear`, `prompt`
- `std.primitives`: `Str`, `Num`, `Bool`, `Vec`, `Map`, `Set`, `wrap`, `unwrap`

## Primitive Methods

Primitive values are Lang-owned runtime values with methods:

```lang
text = std.fs.read_text("notes.txt")
    .expect("Failed to read notes.txt")
    .strip()

lines = text.lines()
words = text.words()

print(lines.len)
print(words.len)
print(text.len)
```

Every primitive supports `tap(fn)` and `pipe(fn)`:

```lang
word_count = "small bright language"
    .tap((text) => print(text))
    .words()
    .pipe((words) => words.len)
```

Use `tap` for side effects while keeping the original value. Use `pipe` when the callback result should become the next value.

## Results

Fallible APIs return result-like values:

```lang
from std.fs import read_text

text = read_text("notes.txt")
print(text.unwrap_or("missing"))
```

Use `unwrap()` when failure should stop the program, and `unwrap_or(default)` when a fallback is clearer.

`Result` values also expose `is_ok()` and `is_err()` for branches:

```lang
file = std.fs.read_text("notes.txt")
if file.is_ok() {
    print(file.unwrap())
}
```

## Input And Args

```lang
name = std.io.read_line()
args = std.env.args()
std.io.println("hello " + name)
print(len(args))
```

`std.io.print(value)` writes without adding a newline. `std.io.println(value)` adds one. Global `print` stays available for brief scripts.

## Module Imports

Standard modules are compiler-owned. They do not need files in the user project:

```lang
import std.fs as fs
print(fs.exists("notes.txt"))
```

Unknown std exports fail during project build:

```lang
from std.text import screaming // build error
```
