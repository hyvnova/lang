# Lamp Syntax

This directory contains the shared syntax tooling for Lamp.

## Packages

- `tree-sitter-lamp`: canonical grammar, queries, parser sources, and Tree-sitter tests.
- `vscode-lamp`: minimal VS Code extension that contributes `source.lamp` TextMate highlighting.

## Validation

```powershell
cd syntax/tree-sitter-lamp
tree-sitter generate
tree-sitter test
tree-sitter highlight -p . --scope source.lamp ..\..\test.lang
```
