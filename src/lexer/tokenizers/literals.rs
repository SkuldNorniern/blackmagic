//! # Literals Tokenizer Module
//!
//! This module handles tokenization of literal values (strings, etc.)

use crate::tokens::{
    kinds::TokenKind,
    token::{Span, Token}
};

/// Tokenize a string literal with escape sequences
pub fn tokenize_string_literal(
    source: &str,
    position: &mut usize,
    line: &mut usize,
    column: &mut usize,
    start_line: usize,
    start_column: usize
) -> Result<Token, String> {
    let start_pos = *position;
    *position += 1; // Skip opening quote
    *column += 1;

    let mut terminated = false;

    while *position < source.len() {
        let ch = source.chars().nth(*position).unwrap();

        if ch == '"' {
            *position += 1;
            *column += 1;
            terminated = true;
            break;
        } else if ch == '\\' {
            *position += 1;
            *column += 1;
            if *position >= source.len() {
                // Advance position to avoid infinite loop
                *position += 1;
                *column += 1;
                return Err("unterminated string literal".to_string());
            }

            let escape_ch = source.chars().nth(*position).unwrap();
            match escape_ch {
                'n' | 't' | 'r' | '"' | '\\' => {
                    // Valid escape sequence
                }
                _ => {
                    // Advance position to avoid infinite loop
                    *position += 1;
                    *column += 1;
                    return Err(format!("invalid escape sequence: \\{}", escape_ch));
                }
            }
            *position += 1;
            *column += 1;
        } else if ch == '\n' {
            *line += 1;
            *column = 1;
            *position += 1;
        } else {
            *position += 1;
            *column += 1;
        }
    }

    // Check if we reached the end without finding a closing quote
    if !terminated {
        // Advance position to avoid infinite loop
        *position += 1;
        *column += 1;
        return Err("unterminated string literal".to_string());
    }

    let end_position = *position;
    // Preserve the original source text for the token
    let text = &source[start_pos..end_position];
    let span = Span::new(start_pos, end_position, start_line, start_column);

    Ok(Token::new(TokenKind::StringLiteral, text.to_string(), span))
}

/// Tokenize a raw string literal (no escape processing)
pub fn tokenize_raw_string_literal(
    source: &str,
    position: &mut usize,
    line: &mut usize,
    column: &mut usize,
    start_line: usize,
    start_column: usize
) -> Result<Token, String> {
    *position += 2; // Skip r"
    *column += 2;

    let mut content = String::new();
    let mut terminated = false;

    while *position < source.len() {
        let ch = source.chars().nth(*position).unwrap();

        if ch == '"' {
            *position += 1;
            *column += 1;
            terminated = true;
            break;
        } else {
            if ch == '\n' {
                *line += 1;
                *column = 1;
            } else {
                *column += 1;
            }
            content.push(ch);
            *position += 1;
        }
    }

    if !terminated {
        // Advance position to avoid infinite loop
        *position += 1;
        *column += 1;
        return Err("unterminated raw string literal".to_string());
    }

    let end_position = *position;
    let text = format!("r\"{}\"", content);
    let span = Span::new(*position - text.len(), end_position, start_line, start_column);

    Ok(Token::new(TokenKind::RawStringLiteral, text, span))
}

/// Check if the current position starts a string literal
pub fn is_string_literal_start(source: &str, position: usize) -> bool {
    position < source.len() && source.chars().nth(position).unwrap() == '"'
}

/// Check if the current position starts a raw string literal
pub fn is_raw_string_literal_start(source: &str, position: usize) -> bool {
    position + 1 < source.len() &&
    source.chars().nth(position).unwrap() == 'r' &&
    source.chars().nth(position + 1).unwrap() == '"'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_string_literal() {
        let source = "\"hello world\"";
        let mut position = 0;
        let mut line = 1;
        let mut column = 1;

        let result = tokenize_string_literal(source, &mut position, &mut line, &mut column, 1, 1);
        assert!(result.is_ok());

        let token = result.unwrap();
        assert_eq!(token.kind, TokenKind::StringLiteral);
        assert_eq!(token.text, "\"hello world\"");
    }

    #[test]
    fn test_tokenize_string_literal_with_escapes() {
        let source = "\"hello\\nworld\\\"\"";
        let mut position = 0;
        let mut line = 1;
        let mut column = 1;

        let result = tokenize_string_literal(source, &mut position, &mut line, &mut column, 1, 1);
        assert!(result.is_ok());

        let token = result.unwrap();
        assert_eq!(token.kind, TokenKind::StringLiteral);
        assert_eq!(token.text, "\"hello\\nworld\\\"\"");
    }

    #[test]
    fn test_tokenize_unterminated_string() {
        let source = "\"unterminated";
        let mut position = 0;
        let mut line = 1;
        let mut column = 1;

        let result = tokenize_string_literal(source, &mut position, &mut line, &mut column, 1, 1);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "unterminated string literal");
    }

    #[test]
    fn test_tokenize_raw_string_literal() {
        let source = "r\"hello\\nworld\"";
        let mut position = 0;
        let mut line = 1;
        let mut column = 1;

        let result = tokenize_raw_string_literal(source, &mut position, &mut line, &mut column, 1, 1);
        assert!(result.is_ok());

        let token = result.unwrap();
        assert_eq!(token.kind, TokenKind::RawStringLiteral);
        assert_eq!(token.text, "r\"hello\\nworld\"");
    }

    #[test]
    fn test_is_string_literal_start() {
        assert!(is_string_literal_start("\"hello\"", 0));
        assert!(!is_string_literal_start("not a string", 0));
        assert!(!is_string_literal_start("", 0));
    }

    #[test]
    fn test_is_raw_string_literal_start() {
        assert!(is_raw_string_literal_start("r\"hello\"", 0));
        assert!(!is_raw_string_literal_start("\"hello\"", 0));
        assert!(!is_raw_string_literal_start("not raw", 0));
        assert!(!is_raw_string_literal_start("r", 0)); // Too short
    }
}
