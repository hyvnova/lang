# Lang
A sort of scripting programming language, that is NOT actually a programming language because it's just transpiling in the background.

### TODO 
- [x] Custom Builtins
- Imports 
- Singals ( need to be re-thinked )

# Syntax
Not yet decided, but it will be a mix of Svelte, Python, and Rust. (Hot mess)
```lang
// This is a comment

1..10 # Range

$a = 1 # Signal
$b = a + 1 # when a changes, b changes
$a = b + 1 # Silly infinite loop

# Distribute
res = | PI, Coords -> Direction, Distance; # Distribute PI and Coords into the Direction and Distance functions.
      # res = (output of direction, output of distance)

# Distribute a arguments as iterable
|> inputs, outputs -> f1, f2; # Iterate over inputs and outputs and distribute them into f1 and f2

# Destructuring
user = {
    "name" : "Jonh",
    "age" : 21,
    "email" : "jonh@sample-email.com",
    "role" : "Developer"
}

{name, role} = user
```