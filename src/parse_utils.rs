use crate::lexer::{Kind, Token};


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