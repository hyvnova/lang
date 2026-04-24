# Objects

Lang exposes a Rust-like object model through `struct`, `trait`, `impl`, and struct literals. These constructs transpile to Python classes plus runtime metadata and validation helpers.

## Syntax

- Structs use `struct Name { field: Type }`.
- Traits use `trait Name { fn method(self) -> Type }`.
- Inherent methods use `impl Name { ... }`.
- Trait implementations use `impl Trait for Name { ... }`.
- Struct literals use `Type { field: value }`.
- Generic parameters use `Name<T>`.
- Attached macros use `#name(...)` on the next item. See [Macros](./macros.md).

## Examples

### Struct definition and literal construction

```lang
struct User<T> {
    name: str,
    age: int,
}

user = User { name: "Ada", age: 36 }
```

### Inherent methods

```lang
impl<T> User<T> {
    fn rename(self, name: str) {
        self.name = name
    }
}
```

### Traits and trait impls

```lang
trait Named<T> {
    fn label(self) -> str
}

impl<T> Named<T> for User<T> {
    fn label(self) -> str {
        self.name
    }
}

user.rename("Vey")
print(user.label())
```

## Notes

- Generic parameters are preserved as metadata in v1. They are parsed and recorded, but they are not fully enforced as runtime generic bindings.
- Concrete field annotations such as `str` and `int` are checked at runtime when a struct value is constructed.
- Concrete method argument annotations are checked at runtime when the method is called.
- Trait implementations are checked at runtime. An incomplete `impl Trait for Type` fails when the generated Python module is loaded.
- Derived behavior such as `#derive(debug)` expands into normal `trait` and `impl` items before AST parsing.
