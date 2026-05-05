//! Feature providers stay separate so IDE behavior can grow without a god file.

pub mod completion;
pub mod definition;
pub mod hover;
pub mod inlay;
pub mod semantic;
pub mod signature;
pub mod symbols;
