//! # Lexer Utilities Module
//!
//! This module provides utility functions for character classification
//! and other helper functions used by the lexer.

/// Check if a character can be part of an identifier
pub fn is_identifier_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}

/// Check if a character can be part of an operator
pub fn is_operator_char(ch: char) -> bool {
    matches!(
        ch,
        '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '~' | '<' | '>' | '=' | '!'
    )
}

/// Check if a character is whitespace
pub fn is_whitespace(ch: char) -> bool {
    matches!(ch, ' ' | '\t' | '\n' | '\r')
}

/// Advance position tracking based on character
pub fn advance_position(mut line: usize, mut column: usize, ch: char) -> (usize, usize) {
    if ch == '\n' {
        line += 1;
        column = 1;
    } else {
        column += 1;
    }
    (line, column)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_identifier_char() {
        assert!(is_identifier_char('a'));
        assert!(is_identifier_char('Z'));
        assert!(is_identifier_char('0'));
        assert!(is_identifier_char('_'));
        assert!(!is_identifier_char(' '));
        assert!(!is_identifier_char('+'));
        assert!(!is_identifier_char('ä')); // non-ASCII
    }

    #[test]
    fn test_is_operator_char() {
        assert!(is_operator_char('+'));
        assert!(is_operator_char('-'));
        assert!(is_operator_char('*'));
        assert!(is_operator_char('/'));
        assert!(is_operator_char('%'));
        assert!(is_operator_char('&'));
        assert!(is_operator_char('|'));
        assert!(is_operator_char('^'));
        assert!(is_operator_char('~'));
        assert!(is_operator_char('<'));
        assert!(is_operator_char('>'));
        assert!(is_operator_char('='));

        assert!(is_operator_char('!'));

        assert!(!is_operator_char('a'));
        assert!(!is_operator_char(' '));
        assert!(!is_operator_char('('));
    }

    #[test]
    fn test_is_whitespace() {
        assert!(is_whitespace(' '));
        assert!(is_whitespace('\t'));
        assert!(is_whitespace('\n'));
        assert!(is_whitespace('\r'));

        assert!(!is_whitespace('a'));
        assert!(!is_whitespace('+'));
        assert!(!is_whitespace('0'));
    }

    #[test]
    fn test_advance_position() {
        // Regular character
        let (line, col) = advance_position(1, 5, 'a');
        assert_eq!(line, 1);
        assert_eq!(col, 6);

        // Newline character
        let (line, col) = advance_position(1, 10, '\n');
        assert_eq!(line, 2);
        assert_eq!(col, 1);

        // Tab character (should count as one column)
        let (line, col) = advance_position(1, 1, '\t');
        assert_eq!(line, 1);
        assert_eq!(col, 2);
    }
}
