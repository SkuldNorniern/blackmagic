//! # Tokenizers Module
//!
//! This module contains all the specific tokenization functions
//! for different types of tokens in the Black Magic language.

pub mod comments;
pub mod identifiers;
pub mod literals;
pub mod numbers;
pub mod operators;

// Re-export commonly used tokenization functions
pub use comments::*;
pub use identifiers::*;
pub use literals::*;
pub use numbers::*;
pub use operators::*;
