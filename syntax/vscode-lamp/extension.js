const vscode = require("vscode");
const childProcess = require("child_process");
const fs = require("fs");
const path = require("path");
const { LanguageClient, Trace } = require("vscode-languageclient/node");

let client;

function activate(context) {
  const config = vscode.workspace.getConfiguration("lamp");
  const output = vscode.window.createOutputChannel("Lamp Language Server");
  const command = resolveServerCommand(config.get("serverPath", "lang"), output);
  const trace = config.get("trace.server", "off");
  context.subscriptions.push(output);

  output.appendLine(`Starting Lang language server from: ${command}`);

  if (isBareCommand(command) && !commandOnPath(command)) {
    const message =
      "Lamp could not find `lang` on PATH and no built target/debug/lang executable was discovered from the open workspace. Run `cargo build` from the Lang repo root or set `lamp.serverPath`.";
    output.appendLine(message);
    vscode.window.showErrorMessage(message, "Open Settings").then((selection) => {
      if (selection === "Open Settings") {
        vscode.commands.executeCommand("workbench.action.openSettings", "lamp.serverPath");
      }
    });
    return;
  }

  if (!isBareCommand(command) && !fs.existsSync(command)) {
    const message = `Lamp server path does not exist: ${command}`;
    output.appendLine(message);
    vscode.window.showErrorMessage(message, "Open Settings").then((selection) => {
      if (selection === "Open Settings") {
        vscode.commands.executeCommand("workbench.action.openSettings", "lamp.serverPath");
      }
    });
    return;
  }

  client = new LanguageClient(
    "lampLanguageServer",
    "Lang Language Server",
    {
      command,
      args: ["lsp"],
    },
    {
      documentSelector: [{ scheme: "file", language: "lamp" }],
      outputChannel: output,
      synchronize: {
        fileEvents: vscode.workspace.createFileSystemWatcher("**/*.lang"),
      },
    }
  );

  if (trace === "verbose") {
    client.setTrace(Trace.Verbose);
  } else if (trace === "messages") {
    client.setTrace(Trace.Messages);
  } else {
    client.setTrace(Trace.Off);
  }

  context.subscriptions.push(client.start());
}

function resolveServerCommand(configuredPath, output) {
  const trimmed = typeof configuredPath === "string" ? configuredPath.trim() : "";
  if (trimmed && trimmed !== "lang") {
    return trimmed;
  }

  const discovered = findWorkspaceServer();
  if (discovered) {
    output.appendLine(`Auto-discovered Lang server: ${discovered}`);
    return discovered;
  }

  return trimmed || "lang";
}

function findWorkspaceServer() {
  const folders = vscode.workspace.workspaceFolders || [];
  for (const folder of folders) {
    let current = folder.uri.fsPath;
    while (current && path.dirname(current) !== current) {
      const cargoToml = path.join(current, "Cargo.toml");
      const debugDir = path.join(current, "target", "debug");
      const windowsCandidate = path.join(debugDir, "lang.exe");
      const unixCandidate = path.join(debugDir, "lang");

      if (fs.existsSync(cargoToml)) {
        if (fs.existsSync(windowsCandidate)) {
          return windowsCandidate;
        }
        if (fs.existsSync(unixCandidate)) {
          return unixCandidate;
        }
      }

      current = path.dirname(current);
    }
  }

  return undefined;
}

function isBareCommand(command) {
  return !path.isAbsolute(command) && !command.includes("/") && !command.includes("\\");
}

function commandOnPath(command) {
  const lookup = process.platform === "win32" ? "where.exe" : "which";
  const result = childProcess.spawnSync(lookup, [command], {
    stdio: "ignore",
    windowsHide: true,
  });
  return result.status === 0;
}

function deactivate() {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

module.exports = {
  activate,
  deactivate,
};
