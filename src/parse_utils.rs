use crate::{ast::Node, lexer::{Kind, Token}};


/// Checks if a token is of a certain kind.
/// Yes, this is escentially just a shortcut for `token.kind == kind`.
/// I like my code drippy bruh
pub trait IsKind {
    fn is(&self, k: Kind) -> bool;
    fn is_not(&self, k: Kind) -> bool { !self.is(k) }
}

impl IsKind for Option<Token> {
    fn is(&self, k: Kind) -> bool {
        match self {
            Some(token) => token.kind == k,
            None => false,
        }
    }
}

impl IsKind for Token {
    fn is(&self, cmp_fn: Kind) -> bool {
        self.kind == cmp_fn
    }
}


/// Shortcut for `into_iter().next().unwrap_or_else(|| default)`.
/// Converts contractors into an iterator, takes the first element, and if there is none, returns the default value.
/// Overall this is supposed to make code more readable
/// ### Example (without)
/// ```
/// let rhs: Node = self.parse_until(None).into_iter().next().unwrap_or_else(|| {  // <-- This 
///     error!(&self.lexer, "Expected an value after assignment operator.")
/// });
/// ```
/// 
/// ### Example (with)
/// ```
/// let rhs: Node = self.parse_until(None).get_first_or_else(|| { error!(&self.lexer, "Expected an value after assignment operator.") });
/// ```
pub trait GetFirstOrElse {

    fn get_first_or_else<F>(self, default_fn: F) -> Self::Item
        where 
            Self: Sized + IntoIterator,
            F: FnOnce() -> Self::Item 
    { 
        self.into_iter().next().unwrap_or_else(default_fn)
    }
}

impl GetFirstOrElse for Vec<Node> {}