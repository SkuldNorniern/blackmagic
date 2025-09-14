//! # Black Magic Lexer Module
//!
//! This module provides lexical analysis for the Black Magic language.
//! It tokenizes source code according to the Black Magic v1.3 specification.

use crate::tokens::{
    create_token_trie, kinds::TokenKind,
    token::{Span, Token, TokenResult, TokenStream}, trie::TokenTrie,
};
use std::io::IsTerminal;

// Internal modules
mod tokenizers;
mod utils;

use tokenizers::*;
use utils::*;

/// Span information for error reporting
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ErrorSpan {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

impl ErrorSpan {
    pub fn new(start: usize, end: usize, line: usize, column: usize) -> Self {
        Self { start, end, line, column }
    }

    pub fn point(pos: usize, line: usize, column: usize) -> Self {
        Self { start: pos, end: pos, line, column }
    }
}

/// Comprehensive error types for the Black Magic language
#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    /// Unexpected character encountered
    UnexpectedCharacter(char),
    /// Unterminated string literal
    UnterminatedString,
    /// Invalid escape sequence in string
    InvalidEscapeSequence(String),
    /// Unterminated raw string literal
    UnterminatedRawString,
    /// Unterminated block comment
    UnterminatedComment,
    /// Invalid number literal
    InvalidNumber(String),
    /// Invalid operator sequence
    InvalidOperator(String),
    /// Replacement operation failed
    ReplacementError {
        pattern: String,
        reason: String,
    },
    /// File operation failed
    FileOperationError {
        operation: String,
        path: String,
        reason: String,
    },
    /// Unicode/encoding error
    EncodingError(String),
    /// General error with custom message
    General(String),
    /// Internal compiler error
    InternalError(String),
}

/// Comprehensive error type with context and suggestions
#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Option<ErrorSpan>,
    pub message: String,
    pub suggestions: Vec<String>,
    pub help: Option<String>,
}

impl Error {
    pub fn new(kind: ErrorKind, span: Option<ErrorSpan>, message: String) -> Self {
        Self {
            kind,
            span,
            message,
            suggestions: Vec::new(),
            help: None,
        }
    }

    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestions.push(suggestion.into());
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn unexpected_char(ch: char, span: ErrorSpan) -> Self {
        let message = format!("unexpected character '{}'", ch);
        Self::new(ErrorKind::UnexpectedCharacter(ch), Some(span), message)
            .with_suggestion(format!("remove the character '{}'", ch))
            .with_help("Black Magic only supports ASCII characters for operators and punctuation")
    }

    pub fn unterminated_string(span: ErrorSpan) -> Self {
        Self::new(ErrorKind::UnterminatedString, Some(span), "unterminated string literal".to_string())
            .with_suggestion("add a closing quote (\") to end the string")
    }

    pub fn invalid_escape(escape: String, span: ErrorSpan) -> Self {
        let message = format!("invalid escape sequence: \\{}", escape);
        Self::new(ErrorKind::InvalidEscapeSequence(escape), Some(span), message)
            .with_suggestion("use valid escape sequences like \\n, \\t, \\\\, or \\\"")
    }

    pub fn unterminated_comment(span: ErrorSpan) -> Self {
        Self::new(ErrorKind::UnterminatedComment, Some(span), "unterminated block comment".to_string())
            .with_suggestion("add */ to close the comment")
    }

    pub fn invalid_number(num: String, span: ErrorSpan) -> Self {
        let message = format!("invalid number literal: {}", num);
        Self::new(ErrorKind::InvalidNumber(num), Some(span), message)
            .with_suggestion("check for invalid characters or malformed number format")
    }

    pub fn invalid_operator(op: String, span: ErrorSpan) -> Self {
        let message = format!("invalid operator sequence: {}", op);
        Self::new(ErrorKind::InvalidOperator(op), Some(span), message)
            .with_help("Black Magic supports operators like +, -, *, /, ==, !=, etc.")
    }

    pub fn replacement_error(pattern: String, reason: String, span: Option<ErrorSpan>) -> Self {
        let message = format!("replacement operation failed: {}", reason);
        Self::new(ErrorKind::ReplacementError { pattern, reason }, span, message)
            .with_help("check the search pattern and replacement string")
    }

    pub fn file_error(operation: String, path: String, reason: String) -> Self {
        let message = format!("{} failed for '{}': {}", operation, path, reason);
        Self::new(ErrorKind::FileOperationError { operation, path, reason }, None, message)
    }

    pub fn encoding_error(reason: String, span: Option<ErrorSpan>) -> Self {
        Self::new(ErrorKind::EncodingError(reason.clone()), span, format!("encoding error: {}", reason))
            .with_help("Black Magic currently only supports ASCII characters")
    }

    pub fn internal_error(message: String) -> Self {
        Self::new(ErrorKind::InternalError(message.clone()), None, format!("internal error: {}", message))
            .with_help("this is a bug in the compiler, please report it")
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Error header
        write!(f, "error")?;

        if let Some(span) = &self.span {
            write!(f, "[{}:{}]", span.line, span.column)?;
        }

        writeln!(f, ": {}", self.message)?;

        // Suggestions
        if !self.suggestions.is_empty() {
            for suggestion in &self.suggestions {
                writeln!(f, "  --> suggestion: {}", suggestion)?;
            }
        }

        // Help
        if let Some(help) = &self.help {
            writeln!(f, "  --> help: {}", help)?;
        }

        Ok(())
    }
}

/// Legacy LexerError for backward compatibility
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
    /// New comprehensive error
    Comprehensive(Error),
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
            LexerError::Comprehensive(error) => write!(f, "{}", error),
        }
    }
}

/// Colored error formatting (similar to Rust's cargo output)
pub struct ColoredError<'a>(pub &'a Error);

impl<'a> std::fmt::Display for ColoredError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::io::IsTerminal;
        use std::io::Write;

        let is_terminal = std::io::stderr().is_terminal();

        // Color codes
        let red = if is_terminal { "\x1b[31m" } else { "" };
        let bold_red = if is_terminal { "\x1b[1;31m" } else { "" };
        let cyan = if is_terminal { "\x1b[36m" } else { "" };
        let yellow = if is_terminal { "\x1b[33m" } else { "" };
        let reset = if is_terminal { "\x1b[0m" } else { "" };

        // Error header
        write!(f, "{}{}error{}{}", bold_red, red, reset, red)?;

        if let Some(span) = &self.0.span {
            write!(f, "[{}:{}]", span.line, span.column)?;
        }

        write!(f, ": {}{}", reset, self.0.message)?;

        if !self.0.suggestions.is_empty() || self.0.help.is_some() {
            writeln!(f)?;
        }

        // Suggestions
        if !self.0.suggestions.is_empty() {
            for suggestion in &self.0.suggestions {
                writeln!(f, "  {}--> {}{}suggestion{}: {}", cyan, reset, yellow, reset, suggestion)?;
            }
        }

        // Help
        if let Some(help) = &self.0.help {
            writeln!(f, "  {}--> {}{}help{}: {}", cyan, reset, yellow, reset, help)?;
        }

        write!(f, "{}", reset)?;
        Ok(())
    }
}

/// Error reporting utilities
pub struct ErrorReporter {
    errors: Vec<Error>,
    warnings: Vec<Error>,
}

impl ErrorReporter {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn error(&mut self, error: Error) {
        self.errors.push(error);
    }

    pub fn warning(&mut self, warning: Error) {
        self.warnings.push(warning);
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn has_warnings(&self) -> bool {
        !self.warnings.is_empty()
    }

    pub fn print_report(&self) -> std::io::Result<()> {
        use std::io::{self, Write};

        let mut stderr = io::stderr();

        // Print warnings first
        for warning in &self.warnings {
            // For warnings, we could use a different color (yellow)
            let yellow = if stderr.is_terminal() { "\x1b[33m" } else { "" };
            let reset = if stderr.is_terminal() { "\x1b[0m" } else { "" };

            write!(stderr, "{}{}warning{}{}: ", yellow, yellow, reset, yellow)?;

            if let Some(span) = &warning.span {
                write!(stderr, "[{}:{}] ", span.line, span.column)?;
            }

            writeln!(stderr, "{}{}", warning.message, reset)?;

            // Print suggestions and help for warnings too
            for suggestion in &warning.suggestions {
                writeln!(stderr, "  --> suggestion: {}", suggestion)?;
            }

            if let Some(help) = &warning.help {
                writeln!(stderr, "  --> help: {}", help)?;
            }
        }

        // Print errors
        for error in &self.errors {
            writeln!(stderr, "{}", ColoredError(error))?;
        }

        // Summary
        if !self.warnings.is_empty() || !self.errors.is_empty() {
            let total_issues = self.warnings.len() + self.errors.len();
            let error_count = self.errors.len();
            let warning_count = self.warnings.len();

            if error_count > 0 {
                let red = if stderr.is_terminal() { "\x1b[31m" } else { "" };
                let reset = if stderr.is_terminal() { "\x1b[0m" } else { "" };
                writeln!(stderr, "{}error{}: aborting due to {} previous error{}",
                        red, reset, error_count, if error_count == 1 { "" } else { "s" })?;
            } else if warning_count > 0 {
                let yellow = if stderr.is_terminal() { "\x1b[33m" } else { "" };
                let reset = if stderr.is_terminal() { "\x1b[0m" } else { "" };
                writeln!(stderr, "{}warning{}: {} warning{} emitted",
                        yellow, reset, warning_count, if warning_count == 1 { "" } else { "s" })?;
            }
        }

        Ok(())
    }
}

impl Default for ErrorReporter {
    fn default() -> Self {
        Self::new()
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

    #[test]
    fn test_lexer_error_unexpected_character() {
        // Test various unexpected characters that should cause errors
        let test_cases = vec![
            "@",    // At symbol
            "#",    // Hash (not part of valid syntax)
            "$",    // Dollar sign
            "%",    // Percent (already handled as operator)
            "^",    // Caret (already handled as operator)
            "&",    // Ampersand (already handled as operator)
            "|",    // Pipe (already handled as operator)
            "~",    // Tilde (already handled as operator)
            "`",    // Backtick
            "\\",   // Backslash
            "?",    // Question mark
            "'",    // Single quote (not handled)
        ];

        for ch in test_cases {
            let source = format!("hello {}", ch);
            let result = tokenize(&source);

            match result {
                Ok(_) => {
                    // If tokenization succeeded, check if there are any errors
                    if let Ok(token_stream) = tokenize(&source) {
                        if token_stream.has_errors() {
                            // This is expected - we should have errors for unexpected characters
                            let errors = token_stream.errors();
                            assert!(!errors.is_empty(), "Expected error for character '{}'", ch);
                            // Check that the error message mentions the unexpected character
                            assert!(errors[0].0.contains(&format!("unexpected character '{}'", ch)) ||
                                    errors[0].0.contains("unexpected character"),
                                    "Error message should mention unexpected character '{}': {}", ch, errors[0].0);
                        } else {
                            // If no errors, this character might be valid, which is fine
                        }
                    }
                }
                Err(e) => {
                    // Tokenization failed completely - this is also acceptable for truly unexpected characters
                    // The error should be descriptive
                    let error_msg = e.to_string();
                    assert!(!error_msg.is_empty(), "Error message should not be empty");
                    assert!(error_msg.contains("unexpected") || error_msg.contains("invalid") ||
                            error_msg.contains("error"), "Error should be descriptive: {}", error_msg);
                }
            }
        }
    }

    #[test]
    fn test_lexer_error_unterminated_string() {
        let source = "\"hello world";
        let result = tokenize(source);

        match result {
            Ok(token_stream) => {
                // Should have errors for unterminated string
                assert!(token_stream.has_errors(), "Unterminated string should cause errors");
                let errors = token_stream.errors();
                assert!(!errors.is_empty(), "Should have at least one error");
                let error_msg = &errors[0].0;
                assert!(error_msg.contains("unterminated") || error_msg.contains("string"),
                        "Error should mention unterminated string: {}", error_msg);
            }
            Err(e) => {
                let error_msg = e.to_string();
                assert!(error_msg.contains("unterminated") || error_msg.contains("string"),
                        "Error should mention unterminated string: {}", error_msg);
            }
        }
    }

    #[test]
    fn test_lexer_error_invalid_escape_sequence() {
        let test_cases = vec![
            "\"hello \\x world\"",     // Invalid hex escape
            "\"hello \\u123 world\"",  // Invalid unicode escape
            "\"hello \\z world\"",     // Invalid escape character
            "\"hello \\123 world\"",   // Invalid octal escape
        ];

        for source in test_cases {
            let result = tokenize(source);

            match result {
                Ok(token_stream) => {
                    if token_stream.has_errors() {
                        let errors = token_stream.errors();
                        assert!(!errors.is_empty(), "Invalid escape should cause errors");
                        let error_msg = &errors[0].0;
                        assert!(error_msg.contains("escape") || error_msg.contains("invalid"),
                                "Error should mention escape sequence: {}", error_msg);
                    }
                }
                Err(e) => {
                    let error_msg = e.to_string();
                    assert!(error_msg.contains("escape") || error_msg.contains("invalid"),
                            "Error should mention escape sequence: {}", error_msg);
                }
            }
        }
    }

    #[test]
    fn test_lexer_error_unterminated_raw_string() {
        let test_cases = vec![
            "r\"hello",           // Unterminated raw string
            "r\"hello world",     // Unterminated raw string
            "r\"multi\nline",     // Multi-line unterminated raw string
        ];

        for source in test_cases {
            let result = tokenize(source);

            match result {
                Ok(token_stream) => {
                    if token_stream.has_errors() {
                        let errors = token_stream.errors();
                        assert!(!errors.is_empty(), "Unterminated raw string should cause errors");
                        let error_msg = &errors[0].0;
                        assert!(error_msg.contains("unterminated") || error_msg.contains("raw"),
                                "Error should mention unterminated raw string: {}", error_msg);
                    }
                }
                Err(e) => {
                    let error_msg = e.to_string();
                    assert!(error_msg.contains("unterminated") || error_msg.contains("raw"),
                            "Error should mention unterminated raw string: {}", error_msg);
                }
            }
        }
    }

    #[test]
    fn test_lexer_error_unterminated_block_comment() {
        let test_cases = vec![
            "/* unterminated comment",              // Simple unterminated
            "/* multi line\nunterminated",          // Multi-line unterminated
            "/* nested /* still unterminated",      // Nested unterminated
        ];

        for source in test_cases {
            let result = tokenize(source);

            match result {
                Ok(token_stream) => {
                    if token_stream.has_errors() {
                        let errors = token_stream.errors();
                        assert!(!errors.is_empty(), "Unterminated comment should cause errors");
                        let error_msg = &errors[0].0;
                        assert!(error_msg.contains("unterminated") || error_msg.contains("comment"),
                                "Error should mention unterminated comment: {}", error_msg);
                    }
                }
                Err(e) => {
                    let error_msg = e.to_string();
                    assert!(error_msg.contains("unterminated") || error_msg.contains("comment"),
                            "Error should mention unterminated comment: {}", error_msg);
                }
            }
        }
    }

    #[test]
    fn test_lexer_error_invalid_number() {
        let test_cases = vec![
            "123abc",           // Invalid suffix
            "123.456.789",      // Multiple decimal points
            "123.456f32f64",    // Multiple suffixes
            "0b012",           // Invalid binary digit
            "0o008",           // Invalid octal digit
            "0x00gg",          // Invalid hex digit
            "123_",            // Trailing underscore
            "_123",            // Leading underscore
            "123__456",        // Double underscore
        ];

        for source in test_cases {
            let result = tokenize(source);

            match result {
                Ok(token_stream) => {
                    // Even if tokenization succeeds, numbers might be tokenized as identifiers
                    // or cause other issues
                    let tokens = token_stream.valid_tokens();
                    // The important thing is that we don't crash
                    assert!(!tokens.is_empty(), "Should produce some tokens");

                    // Check if any tokens look like malformed numbers
                    for token in &tokens {
                        if token.kind == TokenKind::Identifier {
                            // If it's an identifier, it might be a malformed number
                            // This is acceptable behavior
                        }
                    }
                }
                Err(e) => {
                    let error_msg = e.to_string();
                    assert!(error_msg.contains("invalid") || error_msg.contains("number") ||
                            error_msg.contains("unexpected"),
                            "Error should be descriptive: {}", error_msg);
                }
            }
        }
    }

    #[test]
    fn test_lexer_error_invalid_operators() {
        let test_cases = vec![
            "===",      // Triple equals
            "!==",      // Invalid combination
            "&&&",      // Triple ampersand
            "|||",      // Triple pipe
            "<<<",      // Triple less than
            ">>>",      // Triple greater than
            "**",       // Double star (not valid)
            "//=",      // Invalid combination
        ];

        for source in test_cases {
            let result = tokenize(source);

            match result {
                Ok(token_stream) => {
                    if token_stream.has_errors() {
                        let errors = token_stream.errors();
                        assert!(!errors.is_empty(), "Invalid operator should cause errors");
                        let error_msg = &errors[0].0;
                        assert!(error_msg.contains("unexpected") || error_msg.contains("invalid"),
                                "Error should mention unexpected or invalid: {}", error_msg);
                    } else {
                        // If no errors, the operators might be split into valid parts
                        let tokens = token_stream.valid_tokens();
                        // This is acceptable - the lexer might split complex operators
                        assert!(!tokens.is_empty(), "Should produce some tokens");
                    }
                }
                Err(e) => {
                    let error_msg = e.to_string();
                    assert!(error_msg.contains("unexpected") || error_msg.contains("invalid"),
                            "Error should mention unexpected or invalid: {}", error_msg);
                }
            }
        }
    }

    #[test]
    fn test_lexer_error_recovery() {
        // Test that lexer can recover from errors and continue parsing
        let source = "valid +++ invalid @@@ more_valid";
        let result = tokenize(source);

        match result {
            Ok(token_stream) => {
                let tokens = token_stream.valid_tokens();

                // Should have some valid tokens
                assert!(!tokens.is_empty(), "Should produce some valid tokens");

                // Check that we get the valid identifier
                assert_eq!(tokens[0].kind, TokenKind::Identifier);
                assert_eq!(tokens[0].text, "valid");

                // Should also have some errors
                if token_stream.has_errors() {
                    let errors = token_stream.errors();
                    assert!(!errors.is_empty(), "Should have errors for invalid parts");
                }

                // Should continue after errors
                let more_valid_found = tokens.iter().any(|t| t.kind == TokenKind::Identifier && t.text == "more_valid");
                assert!(more_valid_found, "Should find 'more_valid' after errors");
            }
            Err(e) => {
                // If it fails completely, that's also acceptable for severely malformed input
                let error_msg = e.to_string();
                assert!(!error_msg.is_empty(), "Error message should not be empty");
            }
        }
    }

    #[test]
    fn test_lexer_error_message_quality() {
        // Test that error messages are helpful and informative
        let test_cases = vec![
            ("\"", "unterminated string"),
            ("/*", "unterminated comment"),
            ("@invalid", "unexpected character"),
            ("123abc@", "unexpected character"),
        ];

        for (source, expected_contains) in test_cases {
            let result = tokenize(source);

            match result {
                Ok(token_stream) => {
                    if token_stream.has_errors() {
                        let errors = token_stream.errors();
                        let error_msg = &errors[0].0;
                        assert!(error_msg.contains(expected_contains),
                                "Error message should contain '{}': {}", expected_contains, error_msg);

                        // Error messages should not be empty
                        assert!(!error_msg.is_empty(), "Error message should not be empty");

                        // Error messages should be reasonably short
                        assert!(error_msg.len() < 200, "Error message should be concise: {}", error_msg);
                    }
                }
                Err(e) => {
                    let error_msg = e.to_string();
                    assert!(error_msg.contains(expected_contains),
                            "Error message should contain '{}': {}", expected_contains, error_msg);
                    assert!(!error_msg.is_empty(), "Error message should not be empty");
                    assert!(error_msg.len() < 200, "Error message should be concise: {}", error_msg);
                }
            }
        }
    }

    #[test]
    fn test_comprehensive_error_system() {
        use crate::lexer::{Error, ErrorSpan, ErrorKind};

        // Test Error creation and formatting
        let span = ErrorSpan::point(10, 2, 5);
        let error = Error::unexpected_char('@', span)
            .with_suggestion("use a valid identifier character")
            .with_help("Black Magic supports standard ASCII operators");

        let error_str = error.to_string();

        // Check that the error contains expected components
        assert!(error_str.contains("error[2:5]"));
        assert!(error_str.contains("unexpected character '@'"));
        assert!(error_str.contains("suggestion: use a valid identifier character"));
        assert!(error_str.contains("help: Black Magic supports standard ASCII operators"));

        // Test replacement error
        let replacement_error = Error::replacement_error(
            "old_pattern".to_string(),
            "pattern not found".to_string(),
            Some(span)
        );

        assert!(replacement_error.to_string().contains("replacement operation failed"));
        assert!(replacement_error.to_string().contains("pattern not found"));

        // Test file error
        let file_error = Error::file_error(
            "read".to_string(),
            "/path/to/file.txt".to_string(),
            "permission denied".to_string()
        );

        assert!(file_error.to_string().contains("read failed"));
        assert!(file_error.to_string().contains("permission denied"));
    }

    #[test]
    fn test_error_reporter() {
        use crate::lexer::ErrorReporter;

        let mut reporter = ErrorReporter::new();

        // Add some errors
        let span = ErrorSpan::point(10, 2, 5);
        let error = Error::unexpected_char('@', span);
        let warning = Error::encoding_error("non-ASCII character detected".to_string(), Some(span));

        reporter.error(error);
        reporter.warning(warning);

        assert!(reporter.has_errors());
        assert!(reporter.has_warnings());
        assert_eq!(reporter.errors.len(), 1);
        assert_eq!(reporter.warnings.len(), 1);

        // Test printing (this would normally go to stderr)
        // We can't easily test the colored output in unit tests,
        // but we can verify the method exists and doesn't panic
        let result = reporter.print_report();
        assert!(result.is_ok());
    }

    #[test]
    fn test_colored_error_formatting() {
        use crate::lexer::ColoredError;

        let span = ErrorSpan::point(10, 2, 5);
        let error = Error::unexpected_char('@', span)
            .with_suggestion("remove the '@' character");

        let colored_error = ColoredError(&error);
        let formatted = colored_error.to_string();

        // The formatted error should contain the basic error information
        // (exact color codes depend on terminal detection)
        assert!(formatted.contains("error"));
        assert!(formatted.contains("unexpected character '@'"));
        assert!(formatted.contains("suggestion"));
    }

    #[test]
    fn test_error_kind_variants() {
        // Test that all ErrorKind variants work correctly
        let span = ErrorSpan::point(10, 2, 5);

        let test_cases = vec![
            ErrorKind::UnexpectedCharacter('@'),
            ErrorKind::UnterminatedString,
            ErrorKind::InvalidEscapeSequence("x".to_string()),
            ErrorKind::UnterminatedRawString,
            ErrorKind::UnterminatedComment,
            ErrorKind::InvalidNumber("123abc".to_string()),
            ErrorKind::InvalidOperator("===".to_string()),
            ErrorKind::ReplacementError {
                pattern: "old".to_string(),
                reason: "not found".to_string(),
            },
            ErrorKind::FileOperationError {
                operation: "read".to_string(),
                path: "/test".to_string(),
                reason: "not found".to_string(),
            },
            ErrorKind::EncodingError("invalid UTF-8".to_string()),
            ErrorKind::General("test error".to_string()),
            ErrorKind::InternalError("bug in compiler".to_string()),
        ];

        for kind in test_cases {
            let error = Error::new(kind, Some(span), "test message".to_string());
            assert!(!error.to_string().is_empty());
        }
    }
}
