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
    fn test_span_empty() {
        let span = Span::empty(5, 10);
        assert_eq!(span.start, 0);
        assert_eq!(span.end, 0);
        assert_eq!(span.line, 5);
        assert_eq!(span.column, 10);
        assert_eq!(span.len(), 0);
        assert!(span.is_empty());
    }

    #[test]
    fn test_span_is_empty() {
        let empty_span = Span::new(5, 5, 1, 1);
        let non_empty_span = Span::new(5, 10, 1, 1);

        assert!(empty_span.is_empty());
        assert!(!non_empty_span.is_empty());
    }

    #[test]
    fn test_token_simple() {
        let token = Token::simple(TokenKind::Let, "let".to_string());
        assert_eq!(token.kind, TokenKind::Let);
        assert_eq!(token.text, "let");
        assert_eq!(token.span, Span::empty(0, 0));
    }

    #[test]
    fn test_token_is_empty() {
        let empty_token = Token::simple(TokenKind::Identifier, String::new());
        let non_empty_token = Token::simple(TokenKind::Let, "let".to_string());

        assert!(empty_token.is_empty());
        assert!(!non_empty_token.is_empty());
    }

    #[test]
    fn test_token_is_eof() {
        let eof_token = Token::simple(TokenKind::Eof, String::new());
        let non_eof_token = Token::simple(TokenKind::Let, "let".to_string());

        assert!(eof_token.is_eof());
        assert!(!non_eof_token.is_eof());
    }

    #[test]
    fn test_token_is_error() {
        let error_token = Token::simple(TokenKind::Error, "error".to_string());
        let non_error_token = Token::simple(TokenKind::Let, "let".to_string());

        assert!(error_token.is_error());
        assert!(!non_error_token.is_error());
    }

    #[test]
    fn test_token_result_error() {
        let span = Span::new(0, 5, 1, 1);
        let error = TokenResult::error("parse error".to_string(), span);

        assert!(error.is_error());
        assert!(!error.is_token());
        assert_eq!(error.as_error(), Some(("parse error", &span)));
        assert_eq!(error.span(), &span);
    }

    #[test]
    fn test_token_result_as_error() {
        let span = Span::new(0, 5, 1, 1);
        let error = TokenResult::error("parse error".to_string(), span);
        let token_result = TokenResult::token(Token::simple(TokenKind::Let, "let".to_string()));

        assert_eq!(error.as_error(), Some(("parse error", &span)));
        assert_eq!(token_result.as_error(), None);
    }

    #[test]
    fn test_token_result_span() {
        let span1 = Span::new(0, 5, 1, 1);
        let span2 = Span::new(5, 10, 1, 6);
        let token_result = TokenResult::token(Token::new(TokenKind::Let, "let".to_string(), span1));
        let error_result = TokenResult::error("error".to_string(), span2);

        assert_eq!(token_result.span(), &span1);
        assert_eq!(error_result.span(), &span2);
    }

    #[test]
    fn test_token_stream_empty() {
        let mut stream = TokenStream::empty();
        assert!(stream.is_at_end());
        assert_eq!(stream.position(), 0);
        assert_eq!(stream.peek(), None);
        assert_eq!(stream.next(), None);
        assert_eq!(stream.tokens().len(), 0);
    }

    #[test]
    fn test_token_stream_peek() {
        let span1 = Span::new(0, 3, 1, 1);
        let span2 = Span::new(4, 6, 1, 5);
        let token1 = TokenResult::token(Token::new(TokenKind::Let, "let".to_string(), span1));
        let token2 = TokenResult::token(Token::new(TokenKind::Identifier, "x".to_string(), span2));

        let mut stream = TokenStream::new(vec![token1.clone(), token2.clone()]);

        // Peek should not advance position
        assert_eq!(stream.peek(), Some(&token1));
        assert_eq!(stream.peek(), Some(&token1));
        assert_eq!(stream.position(), 0);

        // After next(), peek should return next token
        assert_eq!(stream.next(), Some(&token1));
        assert_eq!(stream.peek(), Some(&token2));
        assert_eq!(stream.position(), 1);
    }

    #[test]
    fn test_token_stream_advance() {
        let span1 = Span::new(0, 3, 1, 1);
        let span2 = Span::new(4, 6, 1, 5);
        let token1 = TokenResult::token(Token::new(TokenKind::Let, "let".to_string(), span1));
        let token2 = TokenResult::token(Token::new(TokenKind::Identifier, "x".to_string(), span2));

        let mut stream = TokenStream::new(vec![token1.clone(), token2.clone()]);

        assert_eq!(stream.position(), 0);
        stream.advance();
        assert_eq!(stream.position(), 1);
        stream.advance();
        assert_eq!(stream.position(), 2);
        assert!(stream.is_at_end());

        // Advance beyond end should not panic
        stream.advance();
        assert_eq!(stream.position(), 2);
    }

    #[test]
    fn test_token_stream_position() {
        let span = Span::new(0, 3, 1, 1);
        let token = TokenResult::token(Token::new(TokenKind::Let, "let".to_string(), span));

        let mut stream = TokenStream::new(vec![token.clone(), token.clone(), token.clone()]);

        assert_eq!(stream.position(), 0);
        stream.advance();
        assert_eq!(stream.position(), 1);
        stream.next();
        assert_eq!(stream.position(), 2);
    }

    #[test]
    fn test_token_stream_reset() {
        let span = Span::new(0, 3, 1, 1);
        let token = TokenResult::token(Token::new(TokenKind::Let, "let".to_string(), span));

        let mut stream = TokenStream::new(vec![token.clone(), token.clone()]);

        stream.advance();
        assert_eq!(stream.position(), 1);

        stream.reset();
        assert_eq!(stream.position(), 0);
        assert_eq!(stream.peek(), Some(&token));
    }

    #[test]
    fn test_token_stream_tokens() {
        let span = Span::new(0, 3, 1, 1);
        let token1 = TokenResult::token(Token::new(TokenKind::Let, "let".to_string(), span));
        let token2 = TokenResult::token(Token::new(TokenKind::Identifier, "x".to_string(), span));
        let error = TokenResult::error("error".to_string(), span);

        let stream = TokenStream::new(vec![token1.clone(), error.clone(), token2.clone()]);

        let tokens = stream.tokens();
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0], token1);
        assert_eq!(tokens[1], error);
        assert_eq!(tokens[2], token2);
    }

    #[test]
    fn test_token_stream_valid_tokens() {
        let span = Span::new(0, 3, 1, 1);
        let token1 = TokenResult::token(Token::new(TokenKind::Let, "let".to_string(), span));
        let token2 = TokenResult::token(Token::new(TokenKind::Identifier, "x".to_string(), span));
        let error = TokenResult::error("error".to_string(), span);

        let stream = TokenStream::new(vec![token1.clone(), error, token2.clone()]);

        let valid_tokens = stream.valid_tokens();
        assert_eq!(valid_tokens.len(), 2);
        assert_eq!(valid_tokens[0].kind, TokenKind::Let);
        assert_eq!(valid_tokens[1].kind, TokenKind::Identifier);
    }

    #[test]
    fn test_token_stream_errors() {
        let span1 = Span::new(0, 3, 1, 1);
        let span2 = Span::new(3, 6, 1, 4);
        let token = TokenResult::token(Token::new(TokenKind::Let, "let".to_string(), span1));
        let error1 = TokenResult::error("error1".to_string(), span1);
        let error2 = TokenResult::error("error2".to_string(), span2);

        let stream = TokenStream::new(vec![token.clone(), error1, error2]);

        let errors = stream.errors();
        assert_eq!(errors.len(), 2);
        assert_eq!(errors[0].0, "error1");
        assert_eq!(errors[1].0, "error2");
    }

    #[test]
    fn test_token_stream_has_errors() {
        let span = Span::new(0, 3, 1, 1);
        let token = TokenResult::token(Token::new(TokenKind::Let, "let".to_string(), span));
        let error = TokenResult::error("error".to_string(), span);

        let stream_no_errors = TokenStream::new(vec![token.clone()]);
        let stream_with_errors = TokenStream::new(vec![token, error]);

        assert!(!stream_no_errors.has_errors());
        assert!(stream_with_errors.has_errors());
    }

    #[test]
    fn test_token_stream_default() {
        let stream: TokenStream = Default::default();
        assert!(stream.is_at_end());
        assert_eq!(stream.tokens().len(), 0);
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
