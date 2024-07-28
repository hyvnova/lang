#[allow(dead_code, unused)]


#[derive(Debug, Clone)]
pub enum Expr {
    Empty,

    Number(String),
    Identifier(String),
    String(String),

    /// Represents a member access in an object through the dot operator.
    /// Example: `object.member`
    MemberAcess { 
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

    /// Represents a function call. Ex. `function(name = value)``
    NamedArg(String, Box<Expr>),

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
    }
}

/// Implementing PartialEq for Expr to allow for comparison of expressions.
/// It only checks for type, not the content of the expressions. 
/// Number(1) == Number(2) will return true.
impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        use Expr::*;
        match (self, other) {
            (Number(_), Number(_)) => true,
            (Identifier(_), Identifier(_)) => true,
            (String(_), String(_)) => true,
            (MemberAcess { .. }, MemberAcess { .. }) => true,
            (Group(_), Group(_)) => true,
            (BinOp { .. }, BinOp { .. }) => true,
            (NamedArg(_, _), NamedArg(_, _)) => true,
            (Array(_), Array(_)) => true,
            (Sequence(_), Sequence(_)) => true,
            (Block(_), Block(_)) => true,
            (FunctionCall { .. }, FunctionCall { .. }) => true,
            (Dict { .. }, Dict { .. }) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),

    /// Represents a variable declaration. 
    /// The identifier can be a single identifier or a sequence of identifiers.
    /// Ex. `a = 1`, `a, b = 1, 2`
    Assign {
        identifiers: Vec<Expr>,
        values: Vec<Expr>,
    },

    /// Represents a function declaration.
    /// Ex. `def function(a, b) { return a + b }`
    FunctionDef {
        name: Expr, // Expr::Identifier
        args: Expr, // Expr::Sequence
        body: Expr, // Expr::Block
    },
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
    pub childrem: Rc<RefCell<Vec<Node>>>,
    pub current_scope: Rc<RefCell<Vec<Node>>>, // scope where we add new nodes
}

impl AST {
    pub fn new() -> Self {
        let childrem: Rc<RefCell<Vec<Node>>> = Rc::new(RefCell::new(Vec::new()));

        AST {
            childrem: Rc::clone(&childrem),
            current_scope: Rc::clone(&childrem),
        }
    }

    pub fn add(&mut self, node: Node) {
        self.current_scope.borrow_mut().push(node);
    }
}
