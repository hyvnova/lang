# Macros

Lang supports built-in attached attribute macros with `#name(...)`. In v1, macro expansion is token-based, runs before normal item parsing, and is limited to built-in macros only.

## Syntax

- Attribute macros attach to the next `struct` or `trait` item.
- Multiple attribute lines can be stacked above one item.
- `#derive(...)` accepts a comma-separated list of derive names.
- `#[python] ... #[endpython]` is separate raw-Python interop, not part of the generic attribute macro system.

## Examples

### Deriving debug behavior for a struct

```lang
#derive(debug)
struct Square {
    w: int,
    h: int,
}

square = Square { w: 3, h: 4 }
print(square.debug())
print(square)
print(square.__repr__())
```

### Expanded behavior

`#derive(debug)` lowers into ordinary Lang items:

```lang
trait Debug {
    fn debug(self) -> str
}

struct Square {
    w: int,
    h: int,
}

impl Debug for Square {
    fn debug(self) -> str {
        "Square { w: " + str(self.w) + ", h: " + str(self.h) + " }"
    }
}

impl Square {
    fn __str__(self) -> str {
        self.debug()
    }

    fn __repr__(self) -> str {
        self.debug()
    }
}
```

## Notes

- `#derive(debug)` is the only official built-in derive in v1.
- `#derive(debug)` is valid on `struct` items only. Using it on a `trait` fails during macro expansion.
- Built-in prelude items such as `trait Debug { ... }` are injected once per module.
- Expanded items are parsed and transpiled through the normal object pipeline. No macro nodes survive in the AST.
