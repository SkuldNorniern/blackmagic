//! # Numbers Tokenizer Module
//!
//! This module handles tokenization of numeric literals in the Black Magic language.

use crate::tokens::{
    kinds::TokenKind,
    token::{Span, Token}
};

/// Tokenize a number literal
pub fn tokenize_number(
    source: &str,
    position: &mut usize,
    line: &mut usize,
    column: &mut usize,
    start_line: usize,
    start_column: usize
) -> Result<Token, String> {
    let mut text = String::new();

    // Integer part (including underscores)
    while *position < source.len() {
        let ch = source.chars().nth(*position).unwrap();
        if ch.is_ascii_digit() || ch == '_' {
            text.push(ch);
            *position += 1;
            *column += 1;
        } else {
            break;
        }
    }

    // Decimal part
    if *position < source.len() && source.chars().nth(*position).unwrap() == '.' {
        text.push('.');
        *position += 1;
        *column += 1;

        while *position < source.len() {
            let ch = source.chars().nth(*position).unwrap();
            if ch.is_ascii_digit() || ch == '_' {
                text.push(ch);
                *position += 1;
                *column += 1;
            } else {
                break;
            }
        }
    }

    // Exponent part
    if *position < source.len() {
        let ch = source.chars().nth(*position).unwrap();
        if ch == 'e' || ch == 'E' {
            text.push(ch);
            *position += 1;
            *column += 1;

            // Optional sign
            if *position < source.len() {
                let next_ch = source.chars().nth(*position).unwrap();
                if next_ch == '+' || next_ch == '-' {
                    text.push(next_ch);
                    *position += 1;
                    *column += 1;
                }
            }

            // Exponent digits
            while *position < source.len() {
                let ch = source.chars().nth(*position).unwrap();
                if ch.is_ascii_digit() || ch == '_' {
                    text.push(ch);
                    *position += 1;
                    *column += 1;
                } else {
                    break;
                }
            }
        }
    }

    // Suffix (i32, u64, f32, f64)
    if *position < source.len() {
        let ch = source.chars().nth(*position).unwrap();
        if ch.is_ascii_alphabetic() {
            // Check for complete known suffixes
            let remaining = &source[*position..];
            if remaining.starts_with("f32") {
                text.push_str("f32");
                *position += 3;
                *column += 3;
            } else if remaining.starts_with("f64") {
                text.push_str("f64");
                *position += 3;
                *column += 3;
            } else if remaining.starts_with("i8") {
                text.push_str("i8");
                *position += 2;
                *column += 2;
            } else if remaining.starts_with("i16") {
                text.push_str("i16");
                *position += 3;
                *column += 3;
            } else if remaining.starts_with("i32") {
                text.push_str("i32");
                *position += 3;
                *column += 3;
            } else if remaining.starts_with("i64") {
                text.push_str("i64");
                *position += 3;
                *column += 3;
            } else if remaining.starts_with("u8") {
                text.push_str("u8");
                *position += 2;
                *column += 2;
            } else if remaining.starts_with("u16") {
                text.push_str("u16");
                *position += 3;
                *column += 3;
            } else if remaining.starts_with("u32") {
                text.push_str("u32");
                *position += 3;
                *column += 3;
            } else if remaining.starts_with("u64") {
                text.push_str("u64");
                *position += 3;
                *column += 3;
            } else {
                // Unknown suffix, consume single character
                text.push(ch);
                *position += 1;
                *column += 1;
            }
        }
    }

    let end_position = *position;
    let span = Span::new(*position - text.len(), end_position, start_line, start_column);

    // Determine if it's an integer or float
    let kind = if text.contains('.') || text.contains('e') || text.contains('E') {
        TokenKind::FloatLiteral
    } else {
        TokenKind::IntLiteral
    };

    Ok(Token::new(kind, text, span))
}

/// Check if the current position starts a number literal
pub fn is_number_start(source: &str, position: usize) -> bool {
    position < source.len() && source.chars().nth(position).unwrap().is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_integer() {
        let source = "42";
        let mut position = 0;
        let mut line = 1;
        let mut column = 1;

        let result = tokenize_number(source, &mut position, &mut line, &mut column, 1, 1);
        assert!(result.is_ok());

        let token = result.unwrap();
        assert_eq!(token.kind, TokenKind::IntLiteral);
        assert_eq!(token.text, "42");
        assert_eq!(position, 2);
    }

    #[test]
    fn test_tokenize_integer_with_underscores() {
        let source = "1_000_000";
        let mut position = 0;
        let mut line = 1;
        let mut column = 1;

        let result = tokenize_number(source, &mut position, &mut line, &mut column, 1, 1);
        assert!(result.is_ok());

        let token = result.unwrap();
        assert_eq!(token.kind, TokenKind::IntLiteral);
        assert_eq!(token.text, "1_000_000");
        assert_eq!(position, 9);
    }

    #[test]
    fn test_tokenize_float() {
        let source = "3.14159";
        let mut position = 0;
        let mut line = 1;
        let mut column = 1;

        let result = tokenize_number(source, &mut position, &mut line, &mut column, 1, 1);
        assert!(result.is_ok());

        let token = result.unwrap();
        assert_eq!(token.kind, TokenKind::FloatLiteral);
        assert_eq!(token.text, "3.14159");
        assert_eq!(position, 7);
    }

    #[test]
    fn test_tokenize_scientific_notation() {
        let source = "6.02e23";
        let mut position = 0;
        let mut line = 1;
        let mut column = 1;

        let result = tokenize_number(source, &mut position, &mut line, &mut column, 1, 1);
        assert!(result.is_ok());

        let token = result.unwrap();
        assert_eq!(token.kind, TokenKind::FloatLiteral);
        assert_eq!(token.text, "6.02e23");
        assert_eq!(position, 7);
    }

    #[test]
    fn test_tokenize_integer_with_suffix() {
        let source = "42i32";
        let mut position = 0;
        let mut line = 1;
        let mut column = 1;

        let result = tokenize_number(source, &mut position, &mut line, &mut column, 1, 1);
        assert!(result.is_ok());

        let token = result.unwrap();
        assert_eq!(token.kind, TokenKind::IntLiteral);
        assert_eq!(token.text, "42i32");
        assert_eq!(position, 5);
    }

    #[test]
    fn test_tokenize_float_with_suffix() {
        let source = "3.14f64";
        let mut position = 0;
        let mut line = 1;
        let mut column = 1;

        let result = tokenize_number(source, &mut position, &mut line, &mut column, 1, 1);
        assert!(result.is_ok());

        let token = result.unwrap();
        assert_eq!(token.kind, TokenKind::FloatLiteral);
        assert_eq!(token.text, "3.14f64");
        assert_eq!(position, 7);
    }

    #[test]
    fn test_is_number_start() {
        assert!(is_number_start("42", 0));
        assert!(is_number_start("123abc", 0));
        assert!(!is_number_start("abc", 0));
        assert!(!is_number_start("", 0));
        assert!(!is_number_start(" ", 0));
    }
}
