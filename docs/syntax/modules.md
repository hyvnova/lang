# Modules

Lang uses a filesystem-backed module system with Python-style import syntax and Rust-style visibility. Files are modules, directories become packages through `mod.lang`, and public API is declared explicitly.

## Syntax

- Use dot paths for module names: `app.math.vec`.
- Use leading dots for relative imports: `.math.vec`, `..core.types`.
- Use a single bare name such as `import util` to import a sibling module or package from the current package.
- Use `mod.lang` to define a package file for a directory.
- Use `pub mod child` inside `mod.lang` to expose a child module or package.
- Use `import path` or `import path as alias` to bind a module namespace.
- Use `from path import name` or `from path import name as alias` to bind exported names.
- Use `from path import *` to import all public exports from a module.
- Use `pub use path.name` to re-export a public item from another module.
- Use `pub name = expr` to export a top-level binding.
- Use `__module_str__ = "..."` to override the default printed form of the current module.

## Examples

### Package file and re-exports

```lang
// app/math/mod.lang
pub mod vec
pub mod state
pub value = 1
pub use .vec.Vec2
```

### Imports and live bindings

```lang
// app/main.lang
from .math import *
from .math.state import value as state_value
from app.math.vec import magnitude

print(value)
state_value = 7
import .math.state as state
print(state.value)
state.value = 8
print(state_value)
```

### Bare sibling import and module display

```lang
// main.lang
import util
print(util)

// util.lang
__module_str__ = "utility"
pub value = 1
```

## Notes

- Modules are private by default. `fn`, `struct`, `trait`, and top-level bindings are importable only when they are public or re-exported publicly.
- `pub mod child` is valid only in `mod.lang`. It must match a real child file or package.
- `pub use` adds a name to the current module's public export table. Wildcard imports include only those public exports.
- A single-name import such as `import util` resolves the sibling module first. If no sibling exists and the project root is explicit, the top-level module with that name is used.
- Absolute imports such as `from app.math.vec import value` require an explicit project root. That can come from `--project-root` or from `lang.toml`.
- Imported names are live bindings. Reading an imported name reads the exporter module binding, and assigning to that imported name updates the exporter binding.
- `import path as alias` binds the module object, so `alias.value = 8` updates the same exporter state as a direct imported binding.
- Imported modules print as `<module package.module>` by default.
- `__module_str__` is reserved private module metadata. It is not publicly exported, not included by wildcard imports, and must evaluate to a string when the module is displayed.
- Import cycles are rejected in v1.
- Importing a module executes its top-level code once, following Python module semantics.
