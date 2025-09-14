//! # Black Magic Lexer Module
//!
//! This module provides lexical analysis for the Black Magic language.
//! It tokenizes source code according to the Black Magic v1.3 specification.

use crate::tokens::{
    create_token_trie, kinds::TokenKind,
    token::{Span, Token, TokenResult, TokenStream}, trie::TokenTrie,
};

// Internal modules
mod tokenizers;
mod utils;

use tokenizers::*;
use utils::*;

/// Errors that can occur during lexical analysis
#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
    /// Unexpected character encountered
    UnexpectedCharacter(char, usize, usize),
    /// Unterminated comment
    UnterminatedComment,
    /// Invalid number literal
    InvalidNumber(String),
    /// General error with message
    General(String),
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::UnexpectedCharacter(ch, line, col) => {
                write!(f, "unexpected character '{}' at {}:{}", ch, line, col)
            }
            LexerError::UnterminatedComment => write!(f, "unterminated comment"),
            LexerError::InvalidNumber(s) => write!(f, "invalid number: {}", s),
            LexerError::General(msg) => write!(f, "{}", msg),
        }
    }
}

impl std::error::Error for LexerError {}

impl From<String> for LexerError {
    fn from(error: String) -> Self {
        LexerError::General(error)
    }
}

/// Result type for lexer operations
pub type LexerResult<T> = Result<T, LexerError>;

/// The Black Magic lexer
#[derive(Debug)]
pub struct Lexer<'a> {
    /// Source code to tokenize
    source: &'a str,
    /// Current position in the source
    position: usize,
    /// Current line number (1-indexed)
    line: usize,
    /// Current column number (1-indexed)
    column: usize,
    /// Token trie for efficient keyword/operator lookup
    trie: TokenTrie,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given source code
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            position: 0,
            line: 1,
            column: 1,
            trie: create_token_trie(),
        }
    }

    /// Tokenize the entire source code
    pub fn tokenize(&mut self) -> LexerResult<TokenStream> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            match self.tokenize_next() {
                Ok(Some(token)) => tokens.push(TokenResult::token(token)),
                Ok(None) => {} // Skip whitespace, comments, etc.
                Err(e) => tokens.push(TokenResult::error(e.to_string(), self.current_span())),
            }
        }

        // Add EOF token
        tokens.push(TokenResult::token(Token::new(
            TokenKind::Eof,
            String::new(),
            self.current_span(),
        )));

        Ok(TokenStream::new(tokens))
    }

    /// Tokenize the next token from the source
    pub fn tokenize_next(&mut self) -> LexerResult<Option<Token>> {
        self.skip_whitespace();

        if self.is_at_end() {
            return Ok(None);
        }

        let ch = self.peek();

        // First, try to match multi-character operators
        if self.is_operator_char(ch) {
            return self.tokenize_operator_or_error();
        }

        match ch {
            // Single-character tokens
            '(' => {
                let start_line = self.line;
                let start_column = self.column;
                Ok(Some(make_single_char_token(TokenKind::LeftParen, "(", &mut self.position, &mut self.column, start_line, start_column)))
            },
            ')' => {
                let start_line = self.line;
                let start_column = self.column;
                Ok(Some(make_single_char_token(TokenKind::RightParen, ")", &mut self.position, &mut self.column, start_line, start_column)))
            },
            '[' => {
                let start_line = self.line;
                let start_column = self.column;
                Ok(Some(make_single_char_token(TokenKind::LeftBracket, "[", &mut self.position, &mut self.column, start_line, start_column)))
            },
            ']' => {
                let start_line = self.line;
                let start_column = self.column;
                Ok(Some(make_single_char_token(TokenKind::RightBracket, "]", &mut self.position, &mut self.column, start_line, start_column)))
            },
            '{' => {
                let start_line = self.line;
                let start_column = self.column;
                Ok(Some(make_single_char_token(TokenKind::LeftBrace, "{", &mut self.position, &mut self.column, start_line, start_column)))
            },
            '}' => {
                let start_line = self.line;
                let start_column = self.column;
                Ok(Some(make_single_char_token(TokenKind::RightBrace, "}", &mut self.position, &mut self.column, start_line, start_column)))
            },
            ',' => {
                let start_line = self.line;
                let start_column = self.column;
                Ok(Some(make_single_char_token(TokenKind::Comma, ",", &mut self.position, &mut self.column, start_line, start_column)))
            },
            ';' => {
                let start_line = self.line;
                let start_column = self.column;
                Ok(Some(make_single_char_token(TokenKind::Semicolon, ";", &mut self.position, &mut self.column, start_line, start_column)))
            },
            ':' => {
                let start_line = self.line;
                let start_column = self.column;
                Ok(Some(make_single_char_token(TokenKind::Colon, ":", &mut self.position, &mut self.column, start_line, start_column)))
            },
            '@' => {
                let start_line = self.line;
                let start_column = self.column;
                Ok(Some(make_single_char_token(TokenKind::At, "@", &mut self.position, &mut self.column, start_line, start_column)))
            },
            '#' => {
                let start_line = self.line;
                let start_column = self.column;
                Ok(Some(make_single_char_token(TokenKind::Hash, "#", &mut self.position, &mut self.column, start_line, start_column)))
            },
            '?' => {
                let start_line = self.line;
                let start_column = self.column;
                Ok(Some(make_single_char_token(TokenKind::Question, "?", &mut self.position, &mut self.column, start_line, start_column)))
            },
            '$' => {
                let start_line = self.line;
                let start_column = self.column;
                Ok(Some(make_single_char_token(TokenKind::Dollar, "$", &mut self.position, &mut self.column, start_line, start_column)))
            },
            '!' => {
                let start_line = self.line;
                let start_column = self.column;
                Ok(Some(make_single_char_token(TokenKind::Exclamation, "!", &mut self.position, &mut self.column, start_line, start_column)))
            },

            // Comments
            '/' => {
                if self.peek_next() == Some('/') {
                    skip_line_comment(self.source, &mut self.position);
                    Ok(None)
                } else if self.peek_next() == Some('*') {
                    skip_block_comment(self.source, &mut self.position, &mut self.line, &mut self.column)?;
                    Ok(None)
                } else {
                    let start_line = self.line;
                    let start_column = self.column;
                    Ok(Some(make_single_char_token(TokenKind::Slash, "/", &mut self.position, &mut self.column, start_line, start_column)))
                }
            }

            // String literals
            '"' => {
                let start_line = self.line;
                let start_column = self.column;
                match tokenize_string_literal(self.source, &mut self.position, &mut self.line, &mut self.column, start_line, start_column) {
                    Ok(token) => Ok(Some(token)),
                    Err(e) => Err(LexerError::General(e)),
                }
            }
            'r' => {
                if self.peek_next() == Some('"') {
                    let start_line = self.line;
                    let start_column = self.column;
                    match tokenize_raw_string_literal(self.source, &mut self.position, &mut self.line, &mut self.column, start_line, start_column) {
                        Ok(token) => Ok(Some(token)),
                        Err(e) => Err(LexerError::General(e)),
                    }
                } else {
                    Ok(Some(self.tokenize_identifier_or_keyword()))
                }
            }

            // Numbers
            '0'..='9' => {
                let start_line = self.line;
                let start_column = self.column;
                match tokenize_number(self.source, &mut self.position, &mut self.line, &mut self.column, start_line, start_column) {
                    Ok(token) => Ok(Some(token)),
                    Err(e) => Err(LexerError::General(e)),
                }
            }

            // Identifiers and keywords
            'a'..='z' | 'A'..='Z' | '_' => Ok(Some(self.tokenize_identifier_or_keyword())),

            // Unexpected characters
            _ => self.tokenize_operator_or_error(),
        }
    }

    /// Skip whitespace characters
    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            let ch = self.peek();
            if is_whitespace(ch) {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Skip a line comment (// ...)
    fn skip_line_comment(&mut self) {
        while !self.is_at_end() && self.peek() != '\n' {
            self.advance();
        }
    }

    /// Skip a block comment (/* ... */)
    fn skip_block_comment(&mut self) -> LexerResult<()> {
        self.advance(); // Skip /
        self.advance(); // Skip *

        let mut nesting_level = 1;

        while !self.is_at_end() && nesting_level > 0 {
            let ch = self.peek();
            self.advance();

            if ch == '/' && !self.is_at_end() && self.peek() == '*' {
                self.advance();
                nesting_level += 1;
            } else if ch == '*' && !self.is_at_end() && self.peek() == '/' {
                self.advance();
                nesting_level -= 1;
            }
        }

        if nesting_level > 0 {
            return Err(LexerError::UnterminatedComment);
        }

        Ok(())
    }

    /// Tokenize a string literal
    fn tokenize_string_literal(&mut self) -> LexerResult<Option<Token>> {
        let start = self.current_span();
        self.advance(); // Skip opening quote

        let mut content = String::new();
        let mut terminated = false;

        while !self.is_at_end() {
            let ch = self.peek();

            if ch == '"' {
                self.advance();
                terminated = true;
                break;
            } else if ch == '\\' {
                self.advance(); // Skip backslash
                if self.is_at_end() {
                    break;
                }
                let escape_ch = self.peek();
                match escape_ch {
                    'n' | 't' | 'r' | '"' | '\\' => {
                        content.push('\\');
                        content.push(escape_ch);
                        self.advance();
                    }
                    _ => {
                        return Err(LexerError::UnexpectedCharacter(escape_ch, self.line, self.column));
                    }
                }
            } else if ch == '\n' {
                self.line += 1;
                self.column = 1;
                content.push(ch);
                self.advance();
            } else {
                content.push(ch);
                self.advance();
            }
        }

        if !terminated {
            return Err(LexerError::General("unterminated string literal".to_string()));
        }

        let end_pos = self.position;
        let text = format!("\"{}\"", content);
        let span = Span::new(start.start, end_pos, start.line, start.column);

        Ok(Some(Token::new(TokenKind::StringLiteral, text, span)))
    }

    /// Tokenize a raw string literal
    fn tokenize_raw_string_literal(&mut self) -> LexerResult<Option<Token>> {
        let start = self.current_span();
        self.advance(); // Skip r
        self.advance(); // Skip "

        let mut content = String::new();
        let mut terminated = false;

        while !self.is_at_end() {
            let ch = self.peek();

            if ch == '"' {
                self.advance();
                terminated = true;
                break;
            } else {
                if ch == '\n' {
                    self.line += 1;
                    self.column = 1;
                }
                content.push(ch);
                self.advance();
            }
        }

        if !terminated {
            return Err(LexerError::General("unterminated raw string literal".to_string()));
        }

        let end_pos = self.position;
        let text = format!("r\"{}\"", content);
        let span = Span::new(start.start, end_pos, start.line, start.column);

        Ok(Some(Token::new(TokenKind::RawStringLiteral, text, span)))
    }

    /// Tokenize a number literal
    fn tokenize_number(&mut self) -> LexerResult<Option<Token>> {
        let start = self.current_span();
        let mut text = String::new();

        // Integer part (including underscores)
        while !self.is_at_end() && (self.peek().is_ascii_digit() || self.peek() == '_') {
            text.push(self.peek());
            self.advance();
        }

        // Decimal part
        if !self.is_at_end() && self.peek() == '.' {
            text.push('.');
            self.advance();

            while !self.is_at_end() && (self.peek().is_ascii_digit() || self.peek() == '_') {
                text.push(self.peek());
                self.advance();
            }
        }

        // Exponent part
        if !self.is_at_end() && (self.peek() == 'e' || self.peek() == 'E') {
            text.push(self.peek());
            self.advance();

            if !self.is_at_end() && (self.peek() == '+' || self.peek() == '-') {
                text.push(self.peek());
                self.advance();
            }

            while !self.is_at_end() && (self.peek().is_ascii_digit() || self.peek() == '_') {
                text.push(self.peek());
                self.advance();
            }
        }

        // Suffix (i32, u64, f32, f64)
        while !self.is_at_end() && self.peek().is_ascii_alphabetic() {
            text.push(self.peek());
            self.advance();
        }

        let end_pos = self.position;
        let span = Span::new(start.start, end_pos, start.line, start.column);

        // Determine if it's an integer or float
        let kind = if text.contains('.') || text.contains('e') || text.contains('E') {
            TokenKind::FloatLiteral
        } else {
            TokenKind::IntLiteral
        };

        Ok(Some(Token::new(kind, text, span)))
    }

    /// Tokenize an identifier or keyword
    fn tokenize_identifier_or_keyword(&mut self) -> Token {
        let start_line = self.line;
        let start_column = self.column;
        tokenize_identifier_or_keyword(
            self.source,
            &mut self.position,
            &mut self.column,
            start_line,
            start_column,
            &self.trie
        )
    }

    /// Tokenize an operator or report an error for unexpected character
    fn tokenize_operator_or_error(&mut self) -> LexerResult<Option<Token>> {
        let start_line = self.line;
        let start_column = self.column;
        match tokenize_operator_or_error(
            self.source,
            &mut self.position,
            &mut self.line,
            &mut self.column,
            start_line,
            start_column,
            &self.trie
        ) {
            Ok(token) => Ok(Some(token)),
            Err(e) => Err(LexerError::General(e)),
        }
    }

    /// Check if a character can be part of an operator
    fn is_operator_char(&self, ch: char) -> bool {
        is_operator_char(ch)
    }

    /// Create a simple token for single characters
    fn make_single_char_token(&mut self, kind: TokenKind, text: &str) -> Token {
        let start_line = self.line;
        let start_column = self.column;
        make_single_char_token(kind, text, &mut self.position, &mut self.column, start_line, start_column)
    }

    /// Get the current span
    fn current_span(&self) -> Span {
        Span::new(self.position, self.position + 1, self.line, self.column)
    }

    /// Peek at the current character without advancing
    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source.chars().nth(self.position).unwrap()
        }
    }

    /// Peek at the next character without advancing
    fn peek_next(&self) -> Option<char> {
        if self.position + 1 >= self.source.len() {
            None
        } else {
            self.source.chars().nth(self.position + 1)
        }
    }

    /// Advance to the next character
    fn advance(&mut self) {
        if !self.is_at_end() {
            let ch = self.peek();
            self.position += ch.len_utf8();
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
    }

    /// Check if we've reached the end of the source
    fn is_at_end(&self) -> bool {
        self.position >= self.source.len()
    }
}

/// Convenience function to tokenize a string
pub fn tokenize(source: &str) -> LexerResult<TokenStream> {
    let mut lexer = Lexer::new(source);
    lexer.tokenize()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_keywords() {
        let source = "circle spell let";
        let result = tokenize(source).unwrap();
        let tokens = result.valid_tokens();

        assert_eq!(tokens.len(), 4); // 3 tokens + EOF
        assert_eq!(tokens[0].kind, TokenKind::Circle);
        assert_eq!(tokens[1].kind, TokenKind::Spell);
        assert_eq!(tokens[2].kind, TokenKind::Let);
        assert_eq!(tokens[3].kind, TokenKind::Eof);
    }

    #[test]
    fn test_tokenize_identifiers() {
        let source = "my_var anotherVar";
        let result = tokenize(source).unwrap();
        let tokens = result.valid_tokens();

        assert_eq!(tokens.len(), 3); // 2 tokens + EOF
        assert_eq!(tokens[0].kind, TokenKind::Identifier);
        assert_eq!(tokens[0].text, "my_var");
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[1].text, "anotherVar");
    }

    #[test]
    fn test_tokenize_numbers() {
        let source = "42 3.14 1_000";
        let result = tokenize(source).unwrap();
        let tokens = result.valid_tokens();

        assert_eq!(tokens.len(), 4); // 3 tokens + EOF
        assert_eq!(tokens[0].kind, TokenKind::IntLiteral);
        assert_eq!(tokens[0].text, "42");
        assert_eq!(tokens[1].kind, TokenKind::FloatLiteral);
        assert_eq!(tokens[1].text, "3.14");
        assert_eq!(tokens[2].kind, TokenKind::IntLiteral);
        assert_eq!(tokens[2].text, "1_000");
    }

    #[test]
    fn test_tokenize_operators() {
        let source = "+ - * == !=";
        let result = tokenize(source).unwrap();
        let tokens = result.valid_tokens();

        assert_eq!(tokens.len(), 6); // 5 tokens + EOF
        assert_eq!(tokens[0].kind, TokenKind::Plus);
        assert_eq!(tokens[1].kind, TokenKind::Minus);
        assert_eq!(tokens[2].kind, TokenKind::Star);
        assert_eq!(tokens[3].kind, TokenKind::EqualEqual);
        assert_eq!(tokens[4].kind, TokenKind::BangEqual);
    }

    #[test]
    fn test_tokenize_string_literals() {
        let source = "\"hello\" r\"world\"";
        let result = tokenize(source).unwrap();
        let tokens = result.valid_tokens();

        assert_eq!(tokens.len(), 3); // 2 tokens + EOF
        assert_eq!(tokens[0].kind, TokenKind::StringLiteral);
        assert_eq!(tokens[0].text, "\"hello\"");
        assert_eq!(tokens[1].kind, TokenKind::RawStringLiteral);
        assert_eq!(tokens[1].text, "r\"world\"");
    }

    #[test]
    fn test_tokenize_punctuation() {
        let source = "( ) [ ] { } , ;";
        let result = tokenize(source).unwrap();
        let tokens = result.valid_tokens();

        assert_eq!(tokens.len(), 9); // 8 tokens + EOF
        assert_eq!(tokens[0].kind, TokenKind::LeftParen);
        assert_eq!(tokens[1].kind, TokenKind::RightParen);
        assert_eq!(tokens[2].kind, TokenKind::LeftBracket);
        assert_eq!(tokens[3].kind, TokenKind::RightBracket);
        assert_eq!(tokens[4].kind, TokenKind::LeftBrace);
        assert_eq!(tokens[5].kind, TokenKind::RightBrace);
        assert_eq!(tokens[6].kind, TokenKind::Comma);
        assert_eq!(tokens[7].kind, TokenKind::Semicolon);
    }
}
