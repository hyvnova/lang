# Control Flow

Lang control flow is expression-oriented in a few places, but it still maps closely to Python statement execution.

## Syntax

- Functions use `fn name(args) { ... }`.
- A function body implicitly returns its final expression.
- Conditionals use `if ... { ... } else { ... }`.
- Infinite loops use `loop { ... }`.
- Conditional loops use `while ... { ... }`.
- Iteration uses `for item in iterable { ... }`.
- Signals use `$name = expr`.
- Reactive statements use `$ { ... }`.

## Examples

### Functions and implicit return

```lang
fn add(a, b) { a + b }
print(add(2, 4))
```

### Conditionals

```lang
if true { loop {} } else {}
```

### `for` over a range

```lang
for x in 1..3 {
    print(x)
}
```

### Signals

```lang
$a = 1
$b = $a + 1
print($b)
$a = 2
print($b)
```

### Reactive statements

```lang
$ {
    print("changed")
}
```

## Notes

- The last expression in a function body becomes `return ...` in generated Python.
- Empty blocks transpile to `pass`.
- Signals are runtime-backed values. Updating one signal re-evaluates dependent signals.
- Reactive statements are public syntax, but the current tests only validate signal update behavior directly; keep examples minimal and semantic claims conservative.
