
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
        object: Box<Node>,
        member: Box<Node>,
    },

    /// Captures Nodeessions inside parenthesis. Ex. ( Node )
    Group(Box<Node>),

    BinOp {
        left: Box<Node>,
        op: String,
        right: Box<Node>,
    },
    
    UnaryOp {
        op: String,
        Node: Box<Node>,
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

    ///  A sequence of Nodeessions. Serves as a way to group Nodeessions without parenthesis. Sort of like a tuple.
    /// {Node}, {Node}, ...
    Sequence(Vec<Node>),

    /// WrappedSequence
    /// A sequence wrapped by parenthesis. Ex. `(1, 2, 3)`
    WrappedSequence(Vec<Node>),

    /// Block of code
    Block(Vec<Node>),

    /// Represents a function call. Ex. `function(1, 2, 3)`
    FunctionCall {
        name: Box<Node>,
        args: Box<Node> // Group or Sequence
    },

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

    /// Distribution
    /// | {sequence} -> {recipients};
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
        elifs: Vec<(Node, Node)>,
        else_body: Option<Box<Node>>,
    },
}




/// Implementing PartialEq for Node to allow for comparison of Nodeessions.
/// It only checks for type, not the content of the Nodeessions. 
/// Number(1) == Number(2) will return true.
impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        use Node::*;
        match (self, other) {
            (Empty, Empty) => true,
            (Newline, Newline) => true,
            (Number(_), Number(_)) => true,
            (Identifier(_), Identifier(_)) => true,
            (Str(_), Str(_)) => true,
            (MemberAccess { .. }, MemberAccess { .. }) => true,
            (Group(_), Group(_)) => true,
            (BinOp { .. }, BinOp { .. }) => true,
            (NamedArg(_, _), NamedArg(_, _)) => true,
            (Array(_), Array(_)) => true,
            (Sequence(_), Sequence(_)) => true,
            (WrappedSequence(_), WrappedSequence(_)) => true,
            (Block(_), Block(_)) => true,
            (FunctionCall { .. }, FunctionCall { .. }) => true,
            (Dict { .. }, Dict { .. }) => true,
            (Alias(_), Alias(_)) => true,
            (Range { .. }, Range { .. }) => true,
            (Distribution { .. }, Distribution { .. }) => true,
            (IterDistribution { .. }, IterDistribution { .. }) => true,
            (Comment(_), Comment(_)) => true,
            (Decorator { .. }, Decorator { .. }) => true,
            (Lambda { .. }, Lambda { .. }) => true,
            (Signal(_), Signal(_)) => true,
            (Conditional { .. }, Conditional { .. }) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Empty, // Used to end parse_stmt

    Node(Node),

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
        value: Node,
        dependencies: Vec<String>,
    },

    /// Signal update
    /// ${name} = {value}
    /// In different from SignalDef, this is used to update the value of a signal, maintaining it's listeners.
    SignalUpdate {
        name: String,
        value: Node,
        dependencies: Vec<String>,
    },

    /// Deconstruction
    /// {a, b} = {Node} -> a = Node.a and b = Node.b
    Deconstruction {
        identifiers: Vec<Node>,
        value: Node,
    },

    /// Represents a function declaration.
    /// Ex. `def function(a, b) { return a + b }`
    FunctionDef {
        name: Node, // Node::Identifier
        args: Node, // Node::Sequence
        body: Node, // Node::Block
    },

    Import(Node, Option<Node>), // import <thing> [as <alias>]          last arguments the optional alias
    From(Node, Node, Option<Node>), // from <a> import <b> [as <alias>]
    ReactiveStmt{
        block: Vec<Node>,
        dependencies: Vec<String>,
    },

    Conditional {
        condition: Box<Node>,
        body: Box<Node>,
        elifs: Vec<(Node, Node)>,
        else_body: Option<Box<Node>>,
    },
}

impl PartialEq for Stmt {
    fn eq(&self, other: &Self) -> bool {
        use Stmt::*;
        match (self, other) {
            (Node(_), Node(_)) => true,
            (Assign { .. }, Assign { .. }) => true,
            (FunctionDef { .. }, FunctionDef { .. }) => true,
            (Import(_, _), Import(_, _)) => true,
            (From(_, _, _), From(_, _, _)) => true,
            (Python(_), Python(_)) => true,
            (SignalDef { .. }, SignalDef { .. }) => true,
            (SignalUpdate { .. }, SignalUpdate { .. }) => true,
            (Deconstruction { .. }, Deconstruction { .. }) => true,
            (ReactiveStmt { .. }, ReactiveStmt { .. }) => true,
            (Conditional { .. }, Conditional { .. }) => true,
            _ => false,
        }
    }
}   

#[derive(Debug, Clone)]
pub enum Node {
    Stmt(Stmt),
    Node(Node),
}
/// Implementing PartialEq for Node to allow for comparison of nodes.
/// It only checks for type at lower level, not the content of the Nodeessions.
impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Node::Stmt(a), Node::Stmt(b)) => a == b,
            (Node::Node(a), Node::Node(b)) => a == b,
            _ => false,
        }
    }
}

impl Into<Node> for Node {
    fn into(self) -> Node {
        match self {
            Node::Node(Node) => Node,
            _ => Node::Empty,
        }
    }
}

impl Into<Stmt> for Node {
    fn into(self) -> Stmt {
        match self {
            Node::Stmt(stmt) => stmt,
            _ => Stmt::Node(Node::Empty),
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
    pub fn pop_scope(&mut self) {
        self.scope.pop();
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
                    Node::Stmt(Stmt::SignalDef { name: n, dependencies, .. }) => {
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

