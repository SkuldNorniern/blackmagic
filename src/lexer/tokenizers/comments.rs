//! # Comments Tokenizer Module
//!
//! This module handles tokenization of comments in the Black Magic language.

use crate::tokens::kinds::TokenKind;

/// Skip a line comment (// ...)
pub fn skip_line_comment(source: &str, position: &mut usize) {
    while *position < source.len() && source.chars().nth(*position).unwrap() != '\n' {
        *position += source.chars().nth(*position).unwrap().len_utf8();
    }
}

/// Skip a block comment (/* ... */)
pub fn skip_block_comment(source: &str, position: &mut usize, line: &mut usize, column: &mut usize) -> Result<(), String> {
    // Skip the opening /*
    *position += 2; // Skip '/' and '*'
    *column += 2;

    let mut nesting_level = 1;

    while *position < source.len() && nesting_level > 0 {
        let ch = source.chars().nth(*position).unwrap();
        *position += ch.len_utf8();

        if ch == '/' && *position < source.len() && source.chars().nth(*position).unwrap() == '*' {
            *position += 1;
            *column += 1;
            nesting_level += 1;
        } else if ch == '*' && *position < source.len() && source.chars().nth(*position).unwrap() == '/' {
            *position += 1;
            *column += 1;
            nesting_level -= 1;
        } else if ch == '\n' {
            *line += 1;
            *column = 1;
        } else {
            *column += 1;
        }
    }

    if nesting_level > 0 {
        return Err("unterminated block comment".to_string());
    }

    Ok(())
}

/// Check if the current position starts a comment
pub fn is_comment_start(source: &str, position: usize) -> bool {
    if position + 1 >= source.len() {
        return false;
    }

    let chars: Vec<char> = source.chars().collect();
    if chars[position] == '/' {
        return chars[position + 1] == '/' || chars[position + 1] == '*';
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_skip_line_comment() {
        let mut position = 0;
        let source = "// this is a comment\nnext line";
        skip_line_comment(source, &mut position);

        // Should skip to the newline
        assert_eq!(position, 20); // Length of "// this is a comment"
    }

    #[test]
    fn test_skip_block_comment() {
        let mut position = 0;
        let mut line = 1;
        let mut column = 1;
        let source = "/* comment */ after";
        let result = skip_block_comment(source, &mut position, &mut line, &mut column);

        assert!(result.is_ok());
        assert_eq!(position, 13); // Length of "/* comment */"
    }

    #[test]
    fn test_skip_unterminated_block_comment() {
        let mut position = 0;
        let mut line = 1;
        let mut column = 1;
        let source = "/* unterminated";
        let result = skip_block_comment(source, &mut position, &mut line, &mut column);

        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "unterminated block comment");
    }

    #[test]
    fn test_is_comment_start() {
        assert!(is_comment_start("// comment", 0));
        assert!(is_comment_start("/* comment */", 0));
        assert!(!is_comment_start("not a comment", 0));
        assert!(!is_comment_start("/", 0)); // Too short
    }
}
