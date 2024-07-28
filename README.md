# Lang
A sort of scripting programming language, that is NOT actually a programming language because it's just transpiling in the background.

# Syntax
Not yet decided, but it will be a mix of Svelte, Python, and Rust. (Hot mess)
```lang
// This is a comment
a = 1
{
    // Block
    b = 2
    c = a + b
}

1..10 // Range

$a = 1 // Signal
$b = a + 1 // when a changes, b changes
$a = b + 1 // Silly infinite loop


```