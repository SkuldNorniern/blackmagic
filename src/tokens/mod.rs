//! # Black Magic Tokens Module
//!
//! This module provides token definitions, parsing, and utilities for the Black Magic language.
//! It includes support for all token types specified in the Black Magic v1.3 specification.

pub mod kinds;
pub mod literals;
pub mod token;
pub mod trie;

// Re-export commonly used types for convenience
pub use kinds::TokenKind;
pub use token::{Span, Token, TokenResult, TokenStream};
pub use trie::TokenTrie;

/// Create a new token trie with all Black Magic keywords and operators
pub fn create_token_trie() -> TokenTrie {
    TokenTrie::new()
}

/// Create a token with the given kind and text at the specified span
pub fn create_token(kind: TokenKind, text: String, span: Span) -> Token {
    Token::new(kind, text, span)
}

/// Create a simple token with default span
pub fn create_simple_token(kind: TokenKind, text: String) -> Token {
    Token::simple(kind, text)
}

/// Check if a string is a keyword in Black Magic
pub fn is_keyword(s: &str) -> bool {
    let trie = TokenTrie::new();
    trie.is_keyword(s)
}

/// Check if a string is an operator in Black Magic
pub fn is_operator(s: &str) -> bool {
    let trie = TokenTrie::new();
    trie.is_operator(s)
}

/// Get the token kind for a keyword or operator string
pub fn lookup_token_kind(s: &str) -> Option<TokenKind> {
    let trie = TokenTrie::new();
    trie.lookup(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokens::literals::parse_literal;

    #[test]
    fn test_public_api() {
        let trie = create_token_trie();
        assert!(trie.is_keyword("circle"));
        assert!(trie.is_operator("+"));
        assert!(!trie.is_keyword("identifier"));
    }

    #[test]
    fn test_token_creation() {
        let span = Span::new(0, 3, 1, 1);
        let token = create_token(TokenKind::Let, "let".to_string(), span);
        assert_eq!(token.kind, TokenKind::Let);
        assert_eq!(token.text, "let");
        assert_eq!(token.span, span);
    }

    #[test]
    fn test_utility_functions() {
        assert!(is_keyword("circle"));
        assert!(is_operator("+"));
        assert_eq!(lookup_token_kind("circle"), Some(TokenKind::Circle));
        assert_eq!(lookup_token_kind("nonexistent"), None);
    }

    #[test]
    fn test_literal_parsing() {
        let result = parse_literal("42");
        assert!(result.is_some());
        let token_result = result.unwrap();
        assert!(token_result.is_ok());
        let token = token_result.unwrap();
        assert_eq!(token.kind, TokenKind::IntLiteral);
        assert_eq!(token.text, "42");
    }
}
