//! # Operators Tokenizer Module
//!
//! This module handles tokenization of operators in the Black Magic language.

use crate::lexer::utils::is_operator_char;
use crate::tokens::{
    kinds::TokenKind,
    token::{Span, Token},
    trie::TokenTrie
};

/// Tokenize an operator or return an error for unexpected character
pub fn tokenize_operator_or_error(
    source: &str,
    position: &mut usize,
    line: &mut usize,
    column: &mut usize,
    start_line: usize,
    start_column: usize,
    trie: &TokenTrie
) -> Result<Token, String> {
    let mut matched_text = String::new();
    let mut longest_match: Option<(String, TokenKind)> = None;

    let mut temp_pos = *position;
    let mut temp_line = *line;
    let mut temp_column = *column;

    // Build potential operator by consuming characters
    while temp_pos < source.len() {
        let temp_ch = source.chars().nth(temp_pos).unwrap();
        matched_text.push(temp_ch);

        if let Some((text, kind)) = trie.longest_prefix_match(&matched_text) {
            longest_match = Some((text, kind));
        }

        // Stop if this character can't be part of an operator
        if !is_operator_char(temp_ch) {
            break;
        }

        // Update temporary position
        if temp_ch == '\n' {
            temp_line += 1;
            temp_column = 1;
        } else {
            temp_column += 1;
        }
        temp_pos += temp_ch.len_utf8();
    }

    if let Some((text, kind)) = longest_match {
        // Advance by the length of the matched operator
        for _ in 0..text.len() {
            let ch = source.chars().nth(*position).unwrap();
            if ch == '\n' {
                *line += 1;
                *column = 1;
            } else {
                *column += 1;
            }
            *position += ch.len_utf8();
        }

        let end_position = *position;
        let span = Span::new(*position - text.len(), end_position, start_line, start_column);

        Ok(Token::new(kind, text, span))
    } else {
        // No operator matched, this is an unexpected character
        let ch = source.chars().nth(*position).unwrap();
        // Advance position to avoid infinite loop
        *position += ch.len_utf8();
        *column += 1;
        Err(format!("unexpected character '{}' at {}:{}", ch, *line, *column))
    }
}

/// Create a simple token for single-character operators
pub fn make_single_char_token(
    kind: TokenKind,
    text: &str,
    position: &mut usize,
    column: &mut usize,
    start_line: usize,
    start_column: usize
) -> Token {
    let span = Span::new(*position, *position + text.len(), start_line, start_column);
    let token = Token::new(kind, text.to_string(), span);
    *position += text.len();
    *column += text.len() as usize;
    token
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokens::create_token_trie;

    #[test]
    fn test_tokenize_single_operator() {
        let trie = create_token_trie();
        let source = "+";
        let mut position = 0;
        let mut line = 1;
        let mut column = 1;

        let result = tokenize_operator_or_error(source, &mut position, &mut line, &mut column, 1, 1, &trie);
        assert!(result.is_ok());

        let token = result.unwrap();
        assert_eq!(token.kind, TokenKind::Plus);
        assert_eq!(token.text, "+");
        assert_eq!(position, 1);
    }

    #[test]
    fn test_tokenize_multi_char_operator() {
        let trie = create_token_trie();
        let source = "==";
        let mut position = 0;
        let mut line = 1;
        let mut column = 1;

        let result = tokenize_operator_or_error(source, &mut position, &mut line, &mut column, 1, 1, &trie);
        assert!(result.is_ok());

        let token = result.unwrap();
        assert_eq!(token.kind, TokenKind::EqualEqual);
        assert_eq!(token.text, "==");
        assert_eq!(position, 2);
    }

    #[test]
    fn test_tokenize_longest_prefix_match() {
        let trie = create_token_trie();
        let source = "<<=";
        let mut position = 0;
        let mut line = 1;
        let mut column = 1;

        let result = tokenize_operator_or_error(source, &mut position, &mut line, &mut column, 1, 1, &trie);
        assert!(result.is_ok());

        let token = result.unwrap();
        assert_eq!(token.kind, TokenKind::LessLess);
        assert_eq!(token.text, "<<");
        assert_eq!(position, 2);
    }

    #[test]
    fn test_tokenize_invalid_operator() {
        let trie = create_token_trie();
        let source = "@";
        let mut position = 0;
        let mut line = 1;
        let mut column = 1;

        let result = tokenize_operator_or_error(source, &mut position, &mut line, &mut column, 1, 1, &trie);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("unexpected character"));
    }

    #[test]
    fn test_make_single_char_token() {
        let mut position = 5;
        let mut column = 10;

        let token = make_single_char_token(TokenKind::Plus, "+", &mut position, &mut column, 1, 5);

        assert_eq!(token.kind, TokenKind::Plus);
        assert_eq!(token.text, "+");
        assert_eq!(position, 6);
        assert_eq!(column, 11);
    }
}
