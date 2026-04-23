use std::collections::HashSet;


#[derive(Debug, Clone)]
pub enum Node {
    Empty,
    Newline,

    Number(String),
    Identifier(String),
    Str(String),
    Bool(bool),

    /// Represents a member access in an object through the dot operator.
    /// Example: `object.member`
    MemberAccess { 
        object: Box<Node>,
        member: Box<Node>,
    },

    /// Captures expressions inside parenthesis. Ex. ( Node )
    /// Can be empty. Ex. ()
    Group(Option<Box<Node>>),

    BinOp {
        lhs: Box<Node>,
        op: String,
        rhs: Box<Node>,
    },
    
    UnaryOp {
        op: String,
        expr: Box<Node>,
    },

    /// Represents a function call. Ex. `function(name = value)``
    NamedArg(String, Box<Node>),

    /// An array . Ex. `[1, 2, 3]` 
    Array(Box<Node>),


    /// Indexing. Ex. `array[0]` or `object[key]` or `object[1..3]`
    Index {
        object: Box<Node>,
        index: Box<Node>,
    },

    ///  A sequence of expressions. Serves as a way to group expressions without parenthesis. Sort of like a tuple.
    /// {Node}, {Node}, ...
    Sequence(Vec<Node>),

    /// WrappedSequence
    /// A sequence wrapped by parenthesis. Ex. `(1, 2, 3)`
    WrappedSequence(Vec<Node>),

    /// Block of code -- scope
    Block(Vec<Node>),

    /// Similar to a Block, but different because of implicit return statement.
    /// Ex. { print(1) } -> return print(1);
    /// Block can't have return statement
    FnBody(Vec<Node>),

    /// Represents a function call. Ex. `function(1, 2, 3)`
    FunctionCall {
        object: Box<Node>,
        args: Box<Node> // Group or Sequence
    },

    Return(Box<Node>),

    /// Dict { key: value, key: value, ... }
    /// { variable,  ... }
    Dict {
        keys: Vec<Node>,
        values: Vec<Node>,
    },

    /// Alias for something
    /// as {Node}
    Alias(Box<Node>),
    
    /// Range 
    Range {
        start: Box<Node>,
        end: Box<Node>,
        inclusive: bool,
    },

    /// Length
    Len(Box<Node>),

    /// Distribution
    /// {sequence} -> {recipients};
    Distribution {
        args: Vec<Node>, 
        recipients: Vec<Node>,
    },

    /// Iterative Distribution
    /// |> {sequence} -> {recipients};
    IterDistribution {
        args: Vec<Node>,
        recipients: Vec<Node>,
    },

    Comment(String),

    /// Decorator
    /// @{name}({args})
    Decorator {
        name: Box<Node>,
        args: Option<Box<Node>>,
    },

    /// Anon function
    Lambda {
        args: Box<Node>,
        body: Box<Node>,
    },
    
    /// Signal
    /// ${name}
    Signal(String),


    /// Condtional
    /// if {condition} {body} [elif {condition} {body}]* [else {body}]
    Conditional {
        condition: Box<Node>,
        body: Box<Node>,
        elifs: Vec<(Node, Node)>, // (condition, body)
        else_body: Option<Box<Node>>,
    },

    Python(String), // Python code

    /// Represents a variable declaration. 
    /// The identifier can be a single identifier or a sequence of identifiers.
    /// Ex. `a = 1`, `a, b = 1, 2`
    Assign {
        identifiers: Vec<Node>,
        values: Vec<Node>,
        op: String, // =, +=, -=, *=, /=
    },
    
    /// Signal definition
    /// $ {name} = {value}
    SignalDef {
        name: String,
        value: Box<Node>,
        dependencies: HashSet<String>,
    },

    /// Signal update
    /// ${name} = {value}
    /// In different from SignalDef, this is used to update the value of a signal, maintaining it's listeners.
    SignalUpdate {
        name: String,
        value: Box<Node>,
        dependencies: HashSet<String>,
    },

    /// Deconstruction
    /// {a, b} = {Node} -> a = Node.a and b = Node.b
    Deconstruction {
        identifiers: Vec<Node>,
        value: Box<Node>,
        default_values: Vec<Node>
    },

    /// Represents a function declaration.
    /// Ex. `def function(a, b) { return a + b }`
    FunctionDef {
        name: String, 
        args: Box<Node>,
        body: Box<Node>
    },

    /// Represents a reactive statement.
    /// A block of code that will be re-executed when any of the dependencies change.
    ReactiveStmt{
        block: Vec<Node>,
        dependencies: HashSet<String>,
    },

    /// Control flow
    Continue,
    Break,

    /// Loop
    /// Repeats the body until the condition exited by a break statement.
    Loop(Box<Node>), // expects a block as body

    /// For loop
    ForLoop { 
        item: Box<Node>, // Identifier or Deconstruction
        iterable: Box<Node>, // Sequence, Range or expr in general
        body: Box<Node>, // Block
    },

    /// While loop
    WhileLoop {
        condition: Box<Node>,
        body: Box<Node>,
    }
}


/// Implementing PartialEq for Node to allow for comparison of expressions.
/// It only checks for type, not the content of the expressions. 
/// Number(1) == Number(2) will return true.
impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        use Node::*;
        match (self, other) {
            (Empty, Empty)
            | (Newline, Newline)
            | (Number(_), Number(_))
            | (Identifier(_), Identifier(_))
            | (Str(_), Str(_))
            | (Bool(_), Bool(_))
            | (MemberAccess { .. }, MemberAccess { .. })
            | (Group(_), Group(_))
            | (BinOp { .. }, BinOp { .. })
            | (NamedArg(_, _), NamedArg(_, _))
            | (Array(_), Array(_))
            | (Sequence(_), Sequence(_))
            | (WrappedSequence(_), WrappedSequence(_))
            | (Block(_), Block(_))
            | (FnBody(_), FnBody(_))
            | (FunctionCall { .. }, FunctionCall { .. })
            | (Return(_), Return(_))
            | (Dict { .. }, Dict { .. })
            | (Alias(_), Alias(_))
            | (Range { .. }, Range { .. })
            | (Distribution { .. }, Distribution { .. })
            | (IterDistribution { .. }, IterDistribution { .. })
            | (Comment(_), Comment(_))
            | (Decorator { .. }, Decorator { .. })
            | (Lambda { .. }, Lambda { .. })
            | (Signal(_), Signal(_))
            | (Conditional { .. }, Conditional { .. })
            | (Assign { .. }, Assign { .. })
            | (FunctionDef { .. }, FunctionDef { .. })
            | (Python(_), Python(_))
            | (SignalDef { .. }, SignalDef { .. })
            | (SignalUpdate { .. }, SignalUpdate { .. })
            | (Deconstruction { .. }, Deconstruction { .. })
            | (ReactiveStmt { .. }, ReactiveStmt { .. }) => true,
            _ => false,
        }
    }
}   

pub struct AST {
    // Vector of Scopes
    // A scope is a vector of nodes
    pub scopes: Vec<Vec<Node>>
}

impl AST {
    pub fn new() -> Self {
        let childrem = Vec::new();
        
        AST {
            scopes: vec![childrem],
        }
    }

    /// Create a new scope, which will be the current scope.
    pub fn new_scope(&mut self) {
        self.scopes.push(Vec::new());
    }

    /// Pop the current scope.
    pub fn pop_scope(&mut self) -> Option<Vec<Node>> {
        self.scopes.pop()
    }

    /// Returns a reference to the current scope.
    pub fn current_scope(&self) -> &Vec<Node> {
        self.scopes.last().unwrap()
    }
    
    /// Return a copy of the current scope.
    pub fn get_scope(&self) -> Vec<Node> {
        self.scopes.last().unwrap_or_else(|| panic!("No scope found")).clone()
    }

    /// Add a new node to the current scope.
    pub fn add_node(&mut self, node: Node) {
        self.scopes.last_mut().unwrap_or_else(|| panic!("Can't add node -No scope found")).push(node);
    }

    /// Perform a pop operation on the current scope. 
    pub fn pop_node(&mut self) -> Option<Node> {
        self.scopes.last_mut()?.pop()
    }


    /// Finds a Signal's dependencies given it's name.
    pub fn find_signal_deps(&self, target: &str) -> Option<HashSet<String>> {
        for scope in &self.scopes {
            for node in scope {
                match node {
                    Node::SignalDef {name, dependencies, .. } => {
                        if target == name {
                            return Some(dependencies.clone());
                        }
                    }
                    _ => {}
                }
            }
        }

        None
    }

    /// Pop until non-space node from the current scope.
    /// All space nodes in between will be popped as well.
    pub fn pop_until_non_space(&mut self) -> Option<Node> {
        let mut last = None;
        while let Some(node) = self.pop_node() {
            match &node {
                Node::Empty | Node::Newline => continue,
                _ => {
                    last = Some(node);
                    break;
                }
            }
        }
        last
    }
    
    // Pop's last node in scope
    // If capturing sequence then scope will be turned into a sequence node and the scope will be poped
    pub fn solve_pop(&mut self, parsing_sequence: bool) -> Option<Node> {
        if parsing_sequence {
            let sequence = Node::Sequence(self.get_scope());
            
            // If only 1 scope then, just clear it out because it's main scope, if poped, big trouble :D
            dbg!(&self.scopes);
            if self.scopes.len() == 1 {
                self.scopes.get_mut(0).unwrap().clear();
            } else {
                // Scope should be poped but for some reason that breaks things, so, fuck it.
                self.scopes.last_mut().unwrap().clear();
            }
            
            return Some(sequence);
        }
        return self.pop_node();
    }
}

