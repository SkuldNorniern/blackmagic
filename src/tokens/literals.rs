use super::kinds::TokenKind;
use super::token::Token;

/// Errors that can occur during literal parsing
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralError {
    /// Invalid integer literal
    InvalidInteger(String),
    /// Invalid float literal
    InvalidFloat(String),
    /// Invalid boolean literal
    InvalidBoolean(String),
    /// Unterminated string literal
    UnterminatedString,
    /// Invalid escape sequence in string
    InvalidEscapeSequence(char),
    /// Invalid raw string literal
    InvalidRawString(String),
}

impl std::fmt::Display for LiteralError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralError::InvalidInteger(s) => write!(f, "invalid integer literal: {}", s),
            LiteralError::InvalidFloat(s) => write!(f, "invalid float literal: {}", s),
            LiteralError::InvalidBoolean(s) => write!(f, "invalid boolean literal: {}", s),
            LiteralError::UnterminatedString => write!(f, "unterminated string literal"),
            LiteralError::InvalidEscapeSequence(c) => write!(f, "invalid escape sequence: \\{}", c),
            LiteralError::InvalidRawString(s) => write!(f, "invalid raw string literal: {}", s),
        }
    }
}

impl std::error::Error for LiteralError {}

/// Result type for literal parsing operations
pub type LiteralResult<T> = Result<T, LiteralError>;

/// Parse an integer literal according to Black Magic specification
///
/// Supports:
/// - Decimal: `42`, `1_000`
/// - Optional suffixes: `42i32`, `7u64`
pub fn parse_integer_literal(input: &str) -> LiteralResult<Token> {
    if input.is_empty() {
        return Err(LiteralError::InvalidInteger("empty string".to_string()));
    }

    // Handle underscores in numbers (1_000_000)
    let normalized = input.replace('_', "");

    // Split into number and optional suffix
    let (number_part, suffix) = if let Some(pos) = normalized.find(|c: char| !c.is_ascii_digit()) {
        let (num, suf) = normalized.split_at(pos);
        if suf.starts_with(|c: char| c.is_ascii_alphabetic()) {
            (num, Some(suf))
        } else {
            return Err(LiteralError::InvalidInteger(format!("invalid character in integer: {}", suf.chars().next().unwrap())));
        }
    } else {
        (normalized.as_str(), None)
    };

    // Parse the number part
    let value: Result<u64, _> = number_part.parse();
    if value.is_err() {
        return Err(LiteralError::InvalidInteger(format!("cannot parse '{}'", number_part)));
    }

    // Validate suffix if present
    if let Some(suf) = suffix {
        match suf {
            "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" => {
                // Valid suffixes
            }
            _ => return Err(LiteralError::InvalidInteger(format!("invalid integer suffix: {}", suf))),
        }
    }

    Ok(Token::simple(TokenKind::IntLiteral, input.to_string()))
}

/// Parse a float literal according to Black Magic specification
///
/// Supports:
/// - Standard: `3.14`, `6.02e23`, `1e-3`
/// - Optional suffixes: `3.14f32`, `6.02f64`
pub fn parse_float_literal(input: &str) -> LiteralResult<Token> {
    if input.is_empty() {
        return Err(LiteralError::InvalidFloat("empty string".to_string()));
    }

    // Handle underscores in numbers (1_000.500_000)
    let normalized = input.replace('_', "");

    // Split into number and optional suffix
    // Check for known suffixes at the end
    let (number_part, suffix) = if normalized.ends_with("f32") {
        (&normalized[..normalized.len() - 3], Some("f32".to_string()))
    } else if normalized.ends_with("f64") {
        (&normalized[..normalized.len() - 3], Some("f64".to_string()))
    } else {
        (normalized.as_str(), None)
    };

    // Parse the number part
    let value: Result<f64, _> = number_part.parse();
    if value.is_err() {
        return Err(LiteralError::InvalidFloat(format!("cannot parse '{}'", number_part)));
    }

    // Validate suffix if present
    if let Some(suf) = suffix {
        match suf.as_str() {
            "f32" | "f64" => {
                // Valid suffixes
            }
            _ => return Err(LiteralError::InvalidFloat(format!("invalid float suffix: {}", suf))),
        }
    }

    Ok(Token::simple(TokenKind::FloatLiteral, input.to_string()))
}

/// Parse a boolean literal
pub fn parse_boolean_literal(input: &str) -> LiteralResult<Token> {
    match input {
        "true" => Ok(Token::simple(TokenKind::BoolLiteral, "true".to_string())),
        "false" => Ok(Token::simple(TokenKind::BoolLiteral, "false".to_string())),
        _ => Err(LiteralError::InvalidBoolean(format!("expected 'true' or 'false', got '{}'", input))),
    }
}

/// Parse a string literal with escape sequences
pub fn parse_string_literal(input: &str) -> LiteralResult<Token> {
    if input.len() < 2 || !input.starts_with('"') || !input.ends_with('"') {
        return Err(LiteralError::UnterminatedString);
    }

    let content = &input[1..input.len() - 1];
    let mut result = String::new();
    let mut chars = content.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some('"') => result.push('"'),
                Some('\\') => result.push('\\'),
                Some(c) => return Err(LiteralError::InvalidEscapeSequence(c)),
                None => return Err(LiteralError::UnterminatedString),
            }
        } else {
            result.push(ch);
        }
    }

    Ok(Token::simple(TokenKind::StringLiteral, input.to_string()))
}

/// Parse a raw string literal (no escape processing)
pub fn parse_raw_string_literal(input: &str) -> LiteralResult<Token> {
    if !input.starts_with("r\"") || !input.ends_with('"') {
        return Err(LiteralError::InvalidRawString(format!("raw string must start with r\" and end with \", got '{}'", input)));
    }

    // Extract content between r" and "
    let content_start = 2;
    let content_end = input.len() - 1;
    if content_start > content_end {
        return Err(LiteralError::InvalidRawString("empty raw string".to_string()));
    }

    let _content = &input[content_start..content_end];
    Ok(Token::simple(TokenKind::RawStringLiteral, input.to_string()))
}

/// Check if a string looks like an integer literal
pub fn is_integer_literal_start(input: &str) -> bool {
    !input.is_empty() && input.chars().next().unwrap().is_ascii_digit()
}

/// Check if a string looks like a float literal
pub fn is_float_literal_start(input: &str) -> bool {
    !input.is_empty() && {
        let first = input.chars().next().unwrap();
        first.is_ascii_digit() || (first == '.' && input.len() > 1 && input.chars().nth(1).unwrap().is_ascii_digit())
    }
}

/// Check if a string is a boolean literal
pub fn is_boolean_literal(input: &str) -> bool {
    input == "true" || input == "false"
}

/// Check if a string starts a string literal
pub fn is_string_literal_start(input: &str) -> bool {
    input.starts_with('"')
}

/// Check if a string starts a raw string literal
pub fn is_raw_string_literal_start(input: &str) -> bool {
    input.starts_with("r\"")
}

/// Attempt to parse any literal from the input string
pub fn parse_literal(input: &str) -> Option<LiteralResult<Token>> {
    if is_raw_string_literal_start(input) {
        Some(parse_raw_string_literal(input))
    } else if is_string_literal_start(input) {
        Some(parse_string_literal(input))
    } else if is_boolean_literal(input) {
        Some(parse_boolean_literal(input))
    } else if is_float_literal_start(input) {
        // Check for float first (contains '.')
        if input.contains('.') || input.contains('e') || input.contains('E') {
            Some(parse_float_literal(input))
        } else if is_integer_literal_start(input) {
            Some(parse_integer_literal(input))
        } else {
            None
        }
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_integer_literal() {
        assert!(parse_integer_literal("42").is_ok());
        assert!(parse_integer_literal("1_000").is_ok());
        assert!(parse_integer_literal("42i32").is_ok());
        assert!(parse_integer_literal("7u64").is_ok());
        assert!(parse_integer_literal("abc").is_err());
        assert!(parse_integer_literal("42invalid").is_err());
    }

    #[test]
    fn test_parse_float_literal() {
        assert!(parse_float_literal("3.14").is_ok());
        assert!(parse_float_literal("6.02e23").is_ok());
        assert!(parse_float_literal("1e-3").is_ok());
        assert!(parse_float_literal("3.14f32").is_ok());
        assert!(parse_float_literal("abc").is_err());
        assert!(parse_float_literal("3.14invalid").is_err());
    }

    #[test]
    fn test_parse_boolean_literal() {
        assert!(parse_boolean_literal("true").is_ok());
        assert!(parse_boolean_literal("false").is_ok());
        assert!(parse_boolean_literal("maybe").is_err());
    }

    #[test]
    fn test_parse_string_literal() {
        assert!(parse_string_literal("\"hello\"").is_ok());
        assert!(parse_string_literal("\"hello\\nworld\"").is_ok());
        assert!(parse_string_literal("\"unterminated").is_err());
        assert!(parse_string_literal("\"invalid\\escape\"").is_err());
    }

    #[test]
    fn test_parse_raw_string_literal() {
        assert!(parse_raw_string_literal("r\"hello\"").is_ok());
        assert!(parse_raw_string_literal("r\"hello\\nworld\"").is_ok());
        assert!(parse_raw_string_literal("\"hello\"").is_err());
    }

    #[test]
    fn test_parse_literal() {
        assert!(parse_literal("42").is_some());
        assert!(parse_literal("3.14").is_some());
        assert!(parse_literal("true").is_some());
        assert!(parse_literal("\"hello\"").is_some());
        assert!(parse_literal("r\"hello\"").is_some());
        assert!(parse_literal("abc").is_none());
    }
}
