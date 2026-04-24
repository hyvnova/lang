# Expressions

Lang expressions cover values, operators, calls, data access, and a small amount of syntax sugar over Python execution.

## Syntax

- Comments use `//`.
- Numbers, strings, and booleans are expression values.
- Assignment uses `=`.
- Arithmetic and comparison operators follow standard programming notation such as `+`, `*`, and `==`.
- Function calls use `name(arg1, arg2)`.
- Member access uses `object.member`.
- Arrays use `[a, b, c]`; indexing uses `value[index]`.
- Ranges use `start..end` and `start..=end`.
- Distribution uses `args -> recipient`.
- Lambdas use `(args) => expr`.
- Raw Python blocks use `#[python] ... #[endpython]`.

## Examples

### Literals, comments, and assignment

```lang
// literal smoke test
flag = true
message = "ok"
if flag == true { print(message) } else { print("no") }
```

### Calls and operators

```lang
fn add(a, b) { a + b }
value = add(1, 2)
if value == 3 { print(value) } else { print(0) }
```

### Member access

```lang
counter.value
self.name = name
```

### Arrays and indexing

```lang
values = [1, 2, 3]
print(values[1])
```

### Ranges

```lang
for x in 1..3 {
    print(x)
}
```

Python note: ranges transpile to an iterator-backed Python `range(...)` wrapper.

### Distribution

```lang
1..10 -> print
```

Python note: distribution forwards the left-hand value as arguments to the right-hand callable.

### Lambdas

```lang
value = 2
print(((n) => n + 1)(value))
```

### Raw Python interop

```lang
#[python]
class Counter:
    def __init__(self, value):
        self.value = value
#[endpython]

counter = Counter(5)
print(counter.value)
```

Python note: code inside a raw Python block is emitted directly into the generated Python module.

## Notes

- The official docs cover only tested expression forms.
- Raw Python interop is part of the public surface, but its body is Python, not Lang.
- Arrays currently transpile through the runtime `Iterator` helper rather than a plain Python list object.
