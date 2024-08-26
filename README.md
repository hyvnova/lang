# Lang
A sort of scripting programming language, that is NOT actually a programming language because it's just transpiling in the background.

### TODO 
- Imports 
- Meta-reactivity <- (Currently drafting)
- [ x ] Reactive Statements
 
# Syntax    
Not yet decided, but it will be a mix of Svelte, Python, and Rust. (Hot mess)
```lang
// This is a comment

1..10 // Range

$a = 1 // Signal
$b = $a // when a changes, b changes
$a = $b // Silly infinite loop (not really)

// Reactive Statements
$ {
    print("Log:", $a, $b) // When a or b changes, this will be executed
}

// Distribute
res = | PI, Coords -> Direction, Distance; # Distribute PI and Coords into the Direction and Distance functions.
      # res = (output of direction, output of distance)

// Distribute a arguments as iterable
|> inputs, outputs -> f1, f2; // Iterate over inputs and outputs and distribute them into f1 and f2

// Deconstruct
user = {
    "name" : "Jonh",
    "age" : 21,
    "email" : "jonh@sample-email.com",
    "role" : "Developer"
}

{name, role} = user

// Arrow Function
(name, role) => {
    print(name)
    print(role)
}
```