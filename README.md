# Lang
A sort of scripting programming language, that is NOT actually a programming language because it's just transpiling in the background.

## Docs

The official syntax reference lives in [docs/index.md](./docs/index.md).

Those docs cover the **currently tested** public syntax surface only.

## Syntax Tooling

Shared editor highlighting now lives under [syntax](./syntax/README.md).

- [syntax/tree-sitter-lamp](./syntax/tree-sitter-lamp/README.md) is the canonical grammar for terminal tooling and Neovim.
- [syntax/vscode-lamp](./syntax/vscode-lamp/README.md) is the minimal VS Code syntax extension.

## Validation

```shell
cargo test
```
