# Lang
A sort of scripting programming language, that is NOT actually a programming language because it's just transpiling in the background.

### TODO / LOG
- [] Transpiler indent <--
- [x] Lexer error on missing string qoutes

- Parser Refactor
    - [x] Function Def
        - [x] Return keyword

    - [x] FunctionCall

    - [x] Assingments 
        - Multiple assignments (Sequence doesn't work)

    - [x] Signals <-- This shit too easy
        - [x] Signal Def
        - [x] Signal update
        - [x] Reactive stmt

    - []  Arrays/Indexing

- [x] Conditionals 
    - [x] If
    - [x] Else
    - [x] Elif

- [x] loops
    - [x] loop
    - [x] while
    - [x] for

 
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
// Distribute PI and Coords into the Direction and Distance functions.
// Same as: res = (Direction(PI, Coords), Distance(PI, Coords))
res = | PI, Coords -> Direction, Distance; 

// Distribute a arguments as iterable
// Iterate over inputs and outputs and distribute them into f1 and f2
// for args in zip(inputs, outputs): 
//     f1(*args)
//     f2(*args)
|> inputs, outputs -> f1, f2; 

// Deconstruct
user = {"name" : "Jonh", "role" : "Developer" }
{name, role} = user

// Arrow Function
(name, role) => {
    print(name)
    print(role)
}

#[python]
class Person:
    def __init__(self, name, age):
        self.name = name
        self.age = age

    def __str__(self):
        return f"{self.name} is {self.age} years old"
  
#[endpython]

p = Person("Jonh", 21) // Yes, interop with python


// Auto-vars
input("Enter something: ") // _ will contain the input


// Formatting
a = 1
"A's value is %a"
"{a:02d}" // 01
```