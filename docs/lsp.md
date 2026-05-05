# Lang LSP

Lang ships a language server through the main CLI:

```sh
lang lsp
```

The server speaks LSP over stdio and uses the existing VS Code language id,
`lamp`, so the current syntax extension can launch it directly.

## MVP Capabilities

- diagnostics for malformed delimiters, unclosed strings, and missing imports
- full-document sync
- documented completions for keywords, local symbols, `std`, std modules, std exports, and primitive methods
- rich Markdown hover for local symbols, std exports, std modules, and primitive methods
- signature help for std functions, primitive methods, and user functions
- semantic tokens for smarter editor coloring when the LSP is running
- document symbols for functions, structs, traits, impls, modules, and bindings
- go-to-definition for same-file symbols and resolvable imported modules

The analysis layer is intentionally tolerant. It does not call compiler paths
that can terminate the process while the user is editing a half-written file.

## VS Code

The VS Code extension in `syntax/vscode-lamp` starts the server automatically
for `.lang` files. When the opened workspace is the Lang repository, it first
tries to discover the built debug executable:

- Windows: `target/debug/lang.exe`
- macOS/Linux: `target/debug/lang`

If no built executable is found, it falls back to:

```sh
lang lsp
```

Override the binary path with:

```json
{
  "lamp.serverPath": "/absolute/path/to/lang"
}
```

Install locally:

```powershell
cargo build
cd syntax/vscode-lamp
npm install
npm run check
npm run package
npm run install:local
```

Run in an Extension Development Host:

```powershell
cd syntax/vscode-lamp
npm install
npm run dev
```

Trace language-server traffic:

```json
{
  "lamp.trace.server": "verbose"
}
```

The extension also writes startup details to the `Lamp Language Server` output
channel. If VS Code says nothing is happening, check that channel first; it will
tell you whether `lang` was missing from `PATH` or `lamp.serverPath` points at a
nonexistent file.

## Debugging

Build the CLI first:

```sh
cargo build
```

Then point `lamp.serverPath` at the built executable if `lang` is not on
`PATH`. On Windows this is usually:

```json
{
  "lamp.serverPath": "C:\\Users\\Hyvnt\\T\\Rust\\lang\\target\\debug\\lang.exe"
}
```
