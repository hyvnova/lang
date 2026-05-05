# Scenario Tests

Scenario tests run disk-backed Lang projects through the same project runner people use from the CLI. Use them for small useful programs: stdin, args, files, text transforms, imports, and `std`.

Create a folder under `tests/scenarios/<name>/` with a `scenario.toml` plus the Lang source and fixture files.

```toml
entry = "main.lang"
stdin = "optional input\n"
args = ["input.txt", "output.txt"]
expected_stdout = "done\n"
expected_stderr_contains = "optional error fragment"

[[expected_files]]
path = "output.txt"
contents = """exact file contents"""
```

Rules:

- Output line endings are normalized to `\n`.
- Stdout is compared exactly after line-ending normalization.
- Expected files are compared exactly.
- `expected_stderr_contains` means the scenario is expected to fail at runtime.
