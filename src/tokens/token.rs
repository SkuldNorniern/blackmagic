use super::kinds::TokenKind;
use std::fmt;

/// Represents a span in the source code
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    /// Starting byte offset in the source
    pub start: usize,
    /// Ending byte offset in the source
    pub end: usize,
    /// Starting line number (1-indexed)
    pub line: usize,
    /// Starting column number (1-indexed)
    pub column: usize,
}

impl Span {
    /// Create a new span
    pub fn new(start: usize, end: usize, line: usize, column: usize) -> Self {
        Self {
            start,
            end,
            line,
            column,
        }
    }

    /// Create an empty span at the given position
    pub fn empty(line: usize, column: usize) -> Self {
        Self::new(0, 0, line, column)
    }

    /// Get the length of this span
    pub fn len(&self) -> usize {
        self.end.saturating_sub(self.start)
    }

    /// Check if this span is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Merge this span with another span
    pub fn merge(&self, other: Span) -> Span {
        Span::new(
            self.start.min(other.start),
            self.end.max(other.end),
            self.line.min(other.line),
            self.column.min(other.column),
        )
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}-{}:{}", self.line, self.column, self.line, self.column + self.len())
    }
}

/// A token in the Black Magic language
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The kind of token
    pub kind: TokenKind,
    /// The text content of the token
    pub text: String,
    /// The span of this token in the source
    pub span: Span,
}

impl Token {
    /// Create a new token
    pub fn new(kind: TokenKind, text: String, span: Span) -> Self {
        Self { kind, text, span }
    }

    /// Create a token with empty span
    pub fn simple(kind: TokenKind, text: String) -> Self {
        Self::new(kind, text, Span::empty(0, 0))
    }

    /// Get the length of this token
    pub fn len(&self) -> usize {
        self.text.len()
    }

    /// Check if this token is empty
    pub fn is_empty(&self) -> bool {
        self.text.is_empty()
    }

    /// Check if this token represents an end-of-file
    pub fn is_eof(&self) -> bool {
        self.kind == TokenKind::Eof
    }

    /// Check if this token represents an error
    pub fn is_error(&self) -> bool {
        self.kind == TokenKind::Error
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}({}) at {}", self.kind, self.text, self.span)
    }
}

/// A tokenized result that may contain an error
#[derive(Debug, Clone, PartialEq)]
pub enum TokenResult {
    /// A valid token
    Token(Token),
    /// An error occurred during tokenization
    Error {
        /// The error message
        message: String,
        /// The span where the error occurred
        span: Span,
    },
}

impl TokenResult {
    /// Create a successful token result
    pub fn token(token: Token) -> Self {
        Self::Token(token)
    }

    /// Create an error result
    pub fn error(message: String, span: Span) -> Self {
        Self::Error { message, span }
    }

    /// Check if this result is a token
    pub fn is_token(&self) -> bool {
        matches!(self, Self::Token(_))
    }

    /// Check if this result is an error
    pub fn is_error(&self) -> bool {
        matches!(self, Self::Error { .. })
    }

    /// Get the token if this is a successful result
    pub fn as_token(&self) -> Option<&Token> {
        match self {
            Self::Token(token) => Some(token),
            _ => None,
        }
    }

    /// Get the error details if this is an error result
    pub fn as_error(&self) -> Option<(&str, &Span)> {
        match self {
            Self::Error { message, span } => Some((message, span)),
            _ => None,
        }
    }

    /// Convert this result to a token, panicking if it's an error
    ///
    /// # Panics
    /// Panics if this result is an error
    pub fn unwrap_token(self) -> Token {
        match self {
            Self::Token(token) => token,
            Self::Error { message, .. } => panic!("TokenResult is an error: {}", message),
        }
    }

    /// Get the span of this result regardless of success or error
    pub fn span(&self) -> &Span {
        match self {
            Self::Token(token) => &token.span,
            Self::Error { span, .. } => span,
        }
    }
}

/// A sequence of tokens
#[derive(Debug, Clone, PartialEq)]
pub struct TokenStream {
    tokens: Vec<TokenResult>,
    current_index: usize,
}

impl TokenStream {
    /// Create a new token stream
    pub fn new(tokens: Vec<TokenResult>) -> Self {
        Self {
            tokens,
            current_index: 0,
        }
    }

    /// Create an empty token stream
    pub fn empty() -> Self {
        Self::new(Vec::new())
    }

    /// Get the current token without advancing
    pub fn peek(&self) -> Option<&TokenResult> {
        self.tokens.get(self.current_index)
    }

    /// Get the current token and advance to the next one
    pub fn next(&mut self) -> Option<&TokenResult> {
        if self.current_index < self.tokens.len() {
            let token = &self.tokens[self.current_index];
            self.current_index += 1;
            Some(token)
        } else {
            None
        }
    }

    /// Advance to the next token without returning it
    pub fn advance(&mut self) {
        if self.current_index < self.tokens.len() {
            self.current_index += 1;
        }
    }

    /// Check if we've reached the end of the stream
    pub fn is_at_end(&self) -> bool {
        self.current_index >= self.tokens.len()
    }

    /// Get the current position in the stream
    pub fn position(&self) -> usize {
        self.current_index
    }

    /// Reset the stream to the beginning
    pub fn reset(&mut self) {
        self.current_index = 0;
    }

    /// Get all tokens as a slice
    pub fn tokens(&self) -> &[TokenResult] {
        &self.tokens
    }

    /// Get all valid tokens, filtering out errors
    pub fn valid_tokens(&self) -> Vec<&Token> {
        self.tokens
            .iter()
            .filter_map(|result| result.as_token())
            .collect()
    }

    /// Get all errors in the stream
    pub fn errors(&self) -> Vec<(String, Span)> {
        self.tokens
            .iter()
            .filter_map(|result| match result {
                TokenResult::Error { message, span } => Some((message.clone(), *span)),
                _ => None,
            })
            .collect()
    }

    /// Check if the stream contains any errors
    pub fn has_errors(&self) -> bool {
        self.tokens.iter().any(|result| result.is_error())
    }
}

impl Default for TokenStream {
    fn default() -> Self {
        Self::empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_creation() {
        let span = Span::new(10, 20, 5, 3);
        assert_eq!(span.start, 10);
        assert_eq!(span.end, 20);
        assert_eq!(span.line, 5);
        assert_eq!(span.column, 3);
        assert_eq!(span.len(), 10);
    }

    #[test]
    fn test_span_merge() {
        let span1 = Span::new(10, 20, 5, 3);
        let span2 = Span::new(15, 25, 6, 1);
        let merged = span1.merge(span2);

        assert_eq!(merged.start, 10);
        assert_eq!(merged.end, 25);
        assert_eq!(merged.line, 5);
        assert_eq!(merged.column, 1);
    }

    #[test]
    fn test_token_creation() {
        let span = Span::new(0, 5, 1, 1);
        let token = Token::new(TokenKind::Let, "let".to_string(), span);

        assert_eq!(token.kind, TokenKind::Let);
        assert_eq!(token.text, "let");
        assert_eq!(token.span, span);
        assert_eq!(token.len(), 3);
    }

    #[test]
    fn test_token_result() {
        let span = Span::new(0, 5, 1, 1);
        let token = Token::new(TokenKind::Let, "let".to_string(), span);
        let result = TokenResult::token(token.clone());

        assert!(result.is_token());
        assert!(!result.is_error());
        assert_eq!(result.as_token(), Some(&token));
        assert_eq!(result.unwrap_token(), token);
    }

    #[test]
    fn test_token_stream() {
        let span1 = Span::new(0, 3, 1, 1);
        let span2 = Span::new(4, 6, 1, 5);
        let token1 = TokenResult::token(Token::new(TokenKind::Let, "let".to_string(), span1));
        let token2 = TokenResult::token(Token::new(TokenKind::Identifier, "x".to_string(), span2));

        let mut stream = TokenStream::new(vec![token1.clone(), token2.clone()]);

        assert!(!stream.is_at_end());
        assert_eq!(stream.peek(), Some(&token1));
        assert_eq!(stream.next(), Some(&token1));
        assert_eq!(stream.peek(), Some(&token2));
        assert_eq!(stream.next(), Some(&token2));
        assert!(stream.is_at_end());
        assert_eq!(stream.next(), None);
    }
}
