# tree-sitter-lamp

Local Tree-sitter grammar for Lamp.

## Commands

```powershell
tree-sitter generate
tree-sitter test
tree-sitter highlight ..\..\test.lang
```

## Neovim

```lua
vim.filetype.add({
  extension = {
    lang = "lamp",
  },
  filename = {
    ["mod.lang"] = "lamp",
  },
})

local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.lamp = {
  install_info = {
    url = "C:/Users/Hyvnt/T/Rust/lang/syntax/tree-sitter-lamp",
    files = { "src/parser.c", "src/scanner.c" },
  },
  filetype = "lamp",
}

vim.treesitter.language.register("lamp", "lamp")
vim.api.nvim_create_autocmd("FileType", {
  pattern = "lamp",
  callback = function()
    vim.treesitter.start()
  end,
})
```
