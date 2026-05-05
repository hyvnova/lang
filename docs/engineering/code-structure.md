# Code Structure

Lang internals should explain themselves through small modules, precise types,
and comments that say why a boundary exists.

## Module Responsibility

Each source module should own one reason to change. Protocol wiring, analysis
facts, rendering, filesystem work, and parser state belong in separate files
even when they are used together.

## Why Comments

Use module docs and short comments for intent, invariants, or surprising control
flow. Do not narrate obvious assignments or repeat type names in prose.

## Macros

Use macros for declarative registries or repetitive boilerplate with one stable
shape. Do not hide normal control flow behind macros just to look clever.

## Line Budgets

Regular source modules should stay under 450 lines. If a file approaches that
limit, split by responsibility before adding more behavior. Catalogs and
generated-style tables may be exempt only when an architecture test explicitly
allowlists them.
