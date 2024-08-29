
#[derive(Debug, Clone)]
pub enum Node {
    Empty,
    Newline,

    Number(String),
    Identifier(String),
    Str(String),

    /// Represents a member access in an object through the dot operator.
    /// Example: `object.member`
    MemberAccess { 
        object: Box<Expr>,
        member: Box<Expr>,
    },

    /// Captures expressions inside parenthesis. Ex. ( expr )
    Group(Box<Expr>),

    BinOp {
        lhs: Box<Expr>,
        op: String,
        rhs: Box<Expr>,
    },
    
    UnaryOp {
        op: String,
        expr: Box<Expr>,
    },

    /// Represents a function call. Ex. `function(name = value)``
    NamedArg(String, Box<Expr>),

    /// An array . Ex. `[1, 2, 3]` 
    Array(Box<Expr>),


    /// Indexing. Ex. `array[0]` or `object[key]` or `object[1..3]`
    Index {
        object: Box<Expr>,
        index: Box<Expr>,
    },

    ///  A sequence of expressions. Serves as a way to group expressions without parenthesis. Sort of like a tuple.
    /// {expr}, {expr}, ...
    Sequence(Vec<Expr>),

    /// WrappedSequence
    /// A sequence wrapped by parenthesis. Ex. `(1, 2, 3)`
    WrappedSequence(Vec<Expr>),

    /// Block of code
    Block(Vec<Node>),

    /// Represents a function call. Ex. `function(1, 2, 3)`
    FunctionCall {
        name: Box<Expr>,
        args: Box<Expr> // Group or Sequence
    },

    /// Dict { key: value, key: value, ... }
    /// { variable,  ... }
    Dict {
        keys: Vec<Expr>,
        values: Vec<Expr>,
    },

    /// Alias for something
    /// as {expr}
    Alias(Box<Expr>),
    
    /// Range 
    Range {
        start: Box<Expr>,
        end: Box<Expr>,
        inclusive: bool,
    },

    /// Distribution
    /// | {sequence} -> {recipients};
    Distribution {
        args: Vec<Expr>, 
        recipients: Vec<Expr>,
    },

    /// Iterative Distribution
    /// |> {sequence} -> {recipients};
    IterDistribution {
        args: Vec<Expr>,
        recipients: Vec<Expr>,
    },

    Comment(String),

    /// Decorator
    /// @{name}({args})
    Decorator {
        name: Box<Expr>,
        args: Option<Box<Expr>>,
    },

    /// Anon function
    Lambda {
        args: Box<Expr>,
        body: Box<Node>,
    },
    
    /// Signal
    /// ${name}
    Signal(String),


    /// Condtional
    /// if {condition} {body} [elif {condition} {body}]* [else {body}]
    Conditional {
        condition: Box<Expr>,
        body: Box<Expr>,
        elifs: Vec<(Expr, Expr)>,
        else_body: Option<Box<Expr>>,
    },

    Python(String), // Python code

    /// Represents a variable declaration. 
    /// The identifier can be a single identifier or a sequence of identifiers.
    /// Ex. `a = 1`, `a, b = 1, 2`
    Assign {
        identifiers: Vec<Expr>,
        values: Vec<Expr>,
        op: String, // =, +=, -=, *=, /=
    },
    
    /// Signal definition
    /// $ {name} = {value}
    SignalDef {
        name: String,
        value: Expr,
        dependencies: Vec<String>,
    },

    /// Signal update
    /// ${name} = {value}
    /// In different from SignalDef, this is used to update the value of a signal, maintaining it's listeners.
    SignalUpdate {
        name: String,
        value: Expr,
        dependencies: Vec<String>,
    },

    /// Deconstruction
    /// {a, b} = {expr} -> a = expr.a and b = expr.b
    Deconstruction {
        identifiers: Vec<Expr>,
        value: Expr,
    },

    /// Represents a function declaration.
    /// Ex. `def function(a, b) { return a + b }`
    FunctionDef {
        name: String, 
        args: Node,
        body: Node
    },

    ReactiveStmt{
        block: Vec<Node>,
        dependencies: Vec<String>,
    },

    Conditional {
        condition: Box<Expr>,
        body: Box<Expr>,
        elifs: Vec<(Expr, Expr)>,
        else_body: Option<Box<Expr>>,
    },
}



/// Implementing PartialEq for Expr to allow for comparison of expressions.
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
            | (MemberAccess { .. }, MemberAccess { .. })
            | (Group(_), Group(_))
            | (BinOp { .. }, BinOp { .. })
            | (NamedArg(_, _), NamedArg(_, _))
            | (Array(_), Array(_))
            | (Sequence(_), Sequence(_))
            | (WrappedSequence(_), WrappedSequence(_))
            | (Block(_), Block(_))
            | (FunctionCall { .. }, FunctionCall { .. })
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
    pub scope: Vec<Vec<Node>>
}

impl AST {
    pub fn new() -> Self {
        let childrem = Vec::new();
        
        AST {
            scope: vec![childrem],
        }
    }

    /// Create a new scope, which will be the current scope.
    pub fn new_scope(&mut self) {
        self.scope.push(Vec::new());
    }

    /// Pop the current scope.
    pub fn pop_scope(&mut self) -> Option<Vec<Node>> {
        self.scope.pop()
    }

    /// Returns a reference to the current scope.
    pub fn current_scope(&self) -> &Vec<Node> {
        self.scope.last().unwrap()
    }
    
    /// Return a copy of the current scope.
    pub fn get_scope(&self) -> Vec<Node> {
        self.scope.last().unwrap_or_else(|| panic!("No scope found")).clone()
    }

    /// Add a new node to the current scope.
    pub fn add_node(&mut self, node: Node) {
        self.scope.last_mut().unwrap_or_else(|| panic!("Can't add node -No scope found")).push(node);
    }

    /// Perform a pop operation on the current scope. 
    pub fn pop_node(&mut self) -> Option<Node> {
        self.scope.last_mut()?.pop()
    }


    /// Finds a Signal's dependencies given it's name
    /// This asumes that the signal exists..
    pub fn find_signal_deps(&self, name: &str) -> Vec<String> {
        let mut deps = Vec::new();
        for scope in &self.scope {
            for node in scope {
                match node {
                    (Node::SignalDef { name, dependencies, .. }) => {
                        if n == name {
                            deps = dependencies.clone();
                            break;
                        }
                    }
                    _ => {}
                }
            }
        }
        deps
    }
}

