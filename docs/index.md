# Lang Docs

Lang is a language that transpiles to Python and executes through the Python runtime.

These docs are the **official syntax reference** for the **tested** language surface as of **April 23, 2026**. If a construct is not described here, do not assume it is public or stable just because it appears somewhere in the codebase.

## Reference

- [Expressions](./syntax/expressions.md)
- [Control Flow](./syntax/control-flow.md)
- [Objects](./syntax/objects.md)

## Validation

The documented surface is backed by the integration tests in `tests/`.

```shell
cargo test
```

The tests currently cover:

- tokenization for core syntax and object keywords
- parser shape for precedence, member access, member assignment, and range distribution
- Python transpilation and runtime behavior for functions, loops, signals, arrays, lambdas, raw Python interop, and Rust-like objects
