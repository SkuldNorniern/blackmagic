//! # Identifiers Tokenizer Module
//!
//! This module handles tokenization of identifiers and keywords.

use crate::tokens::{
    kinds::TokenKind,
    token::{Span, Token},
    trie::TokenTrie
};

/// Tokenize an identifier or keyword
pub fn tokenize_identifier_or_keyword(
    source: &str,
    position: &mut usize,
    column: &mut usize,
    start_line: usize,
    start_column: usize,
    trie: &TokenTrie
) -> Token {
    let mut text = String::new();

    while *position < source.len() {
        let ch = source.chars().nth(*position).unwrap();
        if ch.is_ascii_alphanumeric() || ch == '_' {
            text.push(ch);
            *position += 1;
            *column += 1;
        } else {
            break;
        }
    }

    let end_position = *position;
    let span = Span::new(*position - text.len(), end_position, start_line, start_column);

    // Check if it's a keyword
    let kind = if trie.is_keyword(&text) {
        trie.lookup(&text).unwrap()
    } else {
        TokenKind::Identifier
    };

    Token::new(kind, text, span)
}

/// Check if the current position starts an identifier
pub fn is_identifier_start(source: &str, position: usize) -> bool {
    if position >= source.len() {
        return false;
    }

    let ch = source.chars().nth(position).unwrap();
    ch.is_ascii_alphabetic() || ch == '_'
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokens::create_token_trie;

    #[test]
    fn test_tokenize_identifier() {
        let trie = create_token_trie();
        let source = "my_variable";
        let mut position = 0;
        let mut column = 1;

        let token = tokenize_identifier_or_keyword(source, &mut position, &mut column, 1, 1, &trie);

        assert_eq!(token.kind, TokenKind::Identifier);
        assert_eq!(token.text, "my_variable");
        assert_eq!(position, 11);
        assert_eq!(column, 12);
    }

    #[test]
    fn test_tokenize_keyword() {
        let trie = create_token_trie();
        let source = "circle";
        let mut position = 0;
        let mut column = 1;

        let token = tokenize_identifier_or_keyword(source, &mut position, &mut column, 1, 1, &trie);

        assert_eq!(token.kind, TokenKind::Circle);
        assert_eq!(token.text, "circle");
        assert_eq!(position, 6);
        assert_eq!(column, 7);
    }

    #[test]
    fn test_tokenize_identifier_with_numbers() {
        let trie = create_token_trie();
        let source = "var123";
        let mut position = 0;
        let mut column = 1;

        let token = tokenize_identifier_or_keyword(source, &mut position, &mut column, 1, 1, &trie);

        assert_eq!(token.kind, TokenKind::Identifier);
        assert_eq!(token.text, "var123");
        assert_eq!(position, 6);
        assert_eq!(column, 7);
    }

    #[test]
    fn test_is_identifier_start() {
        assert!(is_identifier_start("variable", 0));
        assert!(is_identifier_start("_private", 0));
        assert!(is_identifier_start("ClassName", 0));

        assert!(!is_identifier_start("123number", 0)); // Starts with number
        assert!(!is_identifier_start("", 0)); // Empty
        assert!(!is_identifier_start(" ", 0)); // Whitespace
        assert!(!is_identifier_start("+", 0)); // Symbol
    }
}
