# `lang.toml`

`lang.toml` is the Lang project manifest. It provides the project root, default entry file, and default runtime settings for CLI execution.

## Schema

```toml
[project]
root = "."
entry = "main.lang"

[run]
transpiler = "python"
```

## Rules

- The file name is exactly `lang.toml`.
- `project.root` is resolved relative to the directory containing `lang.toml`.
- `project.entry` is resolved relative to the resolved `project.root`.
- `run.transpiler` is optional in v1. The default is `python`.
- `lang run` with no file uses `project.entry`.
- Running the executable with no subcommand also uses `project.entry`.
- CLI flags override manifest values.

## Notes

- `lang.toml` is optional. Explicit file execution still works without it.
- In v1, the manifest is a project runner config, not a dependency or package manifest.
- Absolute module imports work without `--project-root` when `lang.toml` supplies the project root.
