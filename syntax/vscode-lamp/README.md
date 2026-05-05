# Lamp VS Code Extension

Local VS Code extension for Lamp syntax highlighting and `lang lsp`.

## Build Lang

From the repository root:

```powershell
cargo build
```

If you open the Lang repository in VS Code, the extension will try to discover
the built executable at `target/debug/lang.exe` on Windows or
`target/debug/lang` elsewhere. If you open a different folder or want an
explicit path, set `lamp.serverPath`:

```json
{
  "lamp.serverPath": "C:\\Users\\Hyvnt\\T\\Rust\\lang\\target\\debug\\lang.exe"
}
```

## Run locally

```powershell
cd C:\Users\Hyvnt\T\Rust\lang\syntax\vscode-lamp
npm install
npm run dev
```

## Install locally

```powershell
cd C:\Users\Hyvnt\T\Rust\lang\syntax\vscode-lamp
npm install
npm run check
npm run package
npm run install:local
```

Open a `.lang` file after installation. The extension output channel is named
`Lamp Language Server`; it logs the server path it tried to launch. If the
server is noisy or invisible, set:

```json
{
  "lamp.trace.server": "verbose"
}
```
