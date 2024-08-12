#[allow(dead_code, unused)]


#[derive(Debug, Clone)]
pub enum Expr {
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
        left: Box<Expr>,
        op: String,
        right: Box<Expr>,
    },
    
    UnaryOp {
        op: String,
        expr: Box<Expr>,
    },

    /// Represents a function call. Ex. `function(name = value)``
    NamedArg(Box<Expr>, Box<Expr>),

    /// An array of expressions. Ex. `[1, 2, 3]`
    Array(Vec<Expr>),

    ///  A sequence of expressions. Serves as a way to group expressions without parenthesis. Sort of like a tuple.
    /// {expr}, {expr}, ...
    Sequence(Vec<Expr>),

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
}

/// Implementing PartialEq for Expr to allow for comparison of expressions.
/// It only checks for type, not the content of the expressions. 
/// Number(1) == Number(2) will return true.
impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        use Expr::*;
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
            (Block(_), Block(_)) => true,
            (FunctionCall { .. }, FunctionCall { .. }) => true,
            (Dict { .. }, Dict { .. }) => true,
            (Alias(_), Alias(_)) => true,
            (Range { .. }, Range { .. }) => true,
            (Distribution { .. }, Distribution { .. }) => true,
            (IterDistribution { .. }, IterDistribution { .. }) => true,
            (Comment(_), Comment(_)) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),

    Python(String), // Python code

    /// Represents a variable declaration. 
    /// The identifier can be a single identifier or a sequence of identifiers.
    /// Ex. `a = 1`, `a, b = 1, 2`
    Assign {
        identifiers: Vec<Expr>,
        values: Vec<Expr>,
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
        name: Expr, // Expr::Identifier
        args: Expr, // Expr::Sequence
        body: Expr, // Expr::Block
    },

    Import(Expr, Option<Expr>), // import <thing> [as <alias>]          last arguments the optional alias
    From(Expr, Expr, Option<Expr>), // from <a> import <b> [as <alias>]
}

impl PartialEq for Stmt {
    fn eq(&self, other: &Self) -> bool {
        use Stmt::*;
        match (self, other) {
            (Expr(_), Expr(_)) => true,
            (Assign { .. }, Assign { .. }) => true,
            (FunctionDef { .. }, FunctionDef { .. }) => true,
            _ => false,
        }
    }
}   

#[derive(Debug, Clone)]
pub enum Node {
    Stmt(Stmt),
    Expr(Expr),
}
/// Implementing PartialEq for Node to allow for comparison of nodes.
/// It only checks for type at lower level, not the content of the expressions.
impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Node::Stmt(a), Node::Stmt(b)) => a == b,
            (Node::Expr(a), Node::Expr(b)) => a == b,
            _ => false,
        }
    }
}

impl Into<Expr> for Node {
    fn into(self) -> Expr {
        match self {
            Node::Expr(expr) => expr,
            _ => Expr::Empty,
        }
    }
}

impl Into<Stmt> for Node {
    fn into(self) -> Stmt {
        match self {
            Node::Stmt(stmt) => stmt,
            _ => Stmt::Expr(Expr::Empty),
        }
    }
}

use std::rc::Rc;
use std::cell::RefCell;

pub struct AST {
    pub children: Rc<RefCell<Vec<Node>>>,
    pub current_scope: Rc<RefCell<Vec<Node>>>, // scope where we add new nodes
}

impl AST {
    pub fn new() -> Self {
        let childrem: Rc<RefCell<Vec<Node>>> = Rc::new(RefCell::new(Vec::new()));

        AST {
            children: Rc::clone(&childrem),
            current_scope: Rc::clone(&childrem),
        }
    }

    /// Add a new node to the current scope.
    pub fn add(&mut self, node: Node) {
        self.current_scope.borrow_mut().push(node);
    }

    /// Perform a pop operation on the current scope. 
    pub fn pop(&mut self) -> Option<Node> {
        self.current_scope.borrow_mut().pop()
    }
}
