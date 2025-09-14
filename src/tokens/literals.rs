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
        match &suf[..] {
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
        let chars: Vec<char> = input.chars().collect();
        let first = chars[0];

        // Leading dot followed by digit (e.g., ".5")
        (first == '.' && chars.len() > 1 && chars[1].is_ascii_digit()) ||
        // Digit followed by dot (e.g., "3.", "0.")
        (first.is_ascii_digit() && input.contains('.'))
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
    let result = if is_raw_string_literal_start(input) {
        parse_raw_string_literal(input)
    } else if is_string_literal_start(input) {
        parse_string_literal(input)
    } else if is_boolean_literal(input) {
        parse_boolean_literal(input)
    } else if is_float_literal_start(input) {
        parse_float_literal(input)
    } else if is_integer_literal_start(input) {
        parse_integer_literal(input)
    } else {
        return None;
    };

    // Only return Some(result) if parsing succeeded
    if result.is_ok() {
        Some(result)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_integer_literal() {
        // Test basic integers
        assert!(parse_integer_literal("42").is_ok());
        assert!(parse_integer_literal("0").is_ok());
        assert!(parse_integer_literal("123456").is_ok());

        // Test integers with underscores
        assert!(parse_integer_literal("1_000").is_ok());
        assert!(parse_integer_literal("123_456_789").is_ok());

        // Test integers with suffixes
        assert!(parse_integer_literal("42i32").is_ok());
        assert!(parse_integer_literal("7u64").is_ok());
        assert!(parse_integer_literal("255u8").is_ok());
        assert!(parse_integer_literal("123i16").is_ok());

        // Test invalid integers
        assert!(parse_integer_literal("").is_err());
        assert!(parse_integer_literal("abc").is_err());
        assert!(parse_integer_literal("42invalid").is_err());
        assert!(parse_integer_literal("42i99").is_err()); // Invalid suffix
        assert!(parse_integer_literal("42.5").is_err()); // Float, not integer
    }

    #[test]
    fn test_parse_float_literal() {
        // Test basic floats
        assert!(parse_float_literal("3.14").is_ok());
        assert!(parse_float_literal("0.5").is_ok());
        assert!(parse_float_literal("123.456").is_ok());

        // Test scientific notation
        assert!(parse_float_literal("6.02e23").is_ok());
        assert!(parse_float_literal("1.23E-45").is_ok());
        assert!(parse_float_literal("2.5e+10").is_ok());

        // Test floats with underscores
        assert!(parse_float_literal("1_000.500_000").is_ok());
        assert!(parse_float_literal("1_234.567_890e12").is_ok());

        // Test floats with suffixes
        assert!(parse_float_literal("3.14f32").is_ok());
        assert!(parse_float_literal("6.02f64").is_ok());

        // Test invalid floats
        assert!(parse_float_literal("").is_err());
        assert!(parse_float_literal("abc").is_err());
        assert!(parse_float_literal("3.14invalid").is_err());
        assert!(parse_float_literal("3.14f99").is_err()); // Invalid suffix
    }

    #[test]
    fn test_parse_boolean_literal() {
        // Test valid booleans
        assert!(parse_boolean_literal("true").is_ok());
        assert!(parse_boolean_literal("false").is_ok());

        // Test invalid booleans
        assert!(parse_boolean_literal("maybe").is_err());
        assert!(parse_boolean_literal("True").is_err()); // Case sensitive
        assert!(parse_boolean_literal("FALSE").is_err());
        assert!(parse_boolean_literal("").is_err());
    }

    #[test]
    fn test_parse_string_literal() {
        // Test basic strings
        assert!(parse_string_literal("\"hello\"").is_ok());
        assert!(parse_string_literal("\"world\"").is_ok());
        assert!(parse_string_literal("\"\"").is_ok()); // Empty string

        // Test strings with escapes
        assert!(parse_string_literal("\"hello\\nworld\"").is_ok());
        assert!(parse_string_literal("\"tab:\\there\"").is_ok());
        assert!(parse_string_literal("\"quote: \\\"\"").is_ok());
        assert!(parse_string_literal("\"backslash: \\\\\"").is_ok());

        // Test invalid strings
        assert!(parse_string_literal("").is_err()); // Empty
        assert!(parse_string_literal("\"unterminated").is_err()); // Unterminated
        assert!(parse_string_literal("hello\"").is_err()); // Missing opening quote
        assert!(parse_string_literal("\"invalid\\escape\"").is_err()); // Invalid escape
    }

    #[test]
    fn test_parse_raw_string_literal() {
        // Test basic raw strings
        assert!(parse_raw_string_literal("r\"hello\"").is_ok());
        assert!(parse_raw_string_literal("r\"world\"").is_ok());
        assert!(parse_raw_string_literal("r\"\"").is_ok()); // Empty raw string

        // Test raw strings with special characters (no escapes processed)
        assert!(parse_raw_string_literal("r\"hello\\nworld\"").is_ok());
        assert!(parse_raw_string_literal("r\"path\\to\\file\"").is_ok());

        // Test invalid raw strings
        assert!(parse_raw_string_literal("").is_err()); // Empty
        assert!(parse_raw_string_literal("\"hello\"").is_err()); // Not raw string
        assert!(parse_raw_string_literal("r\"unterminated").is_err()); // Unterminated
        assert!(parse_raw_string_literal("hello\"").is_err()); // Missing r and opening quote
    }

    #[test]
    fn test_is_integer_literal_start() {
        // Test valid starts
        assert!(is_integer_literal_start("0"));
        assert!(is_integer_literal_start("42"));
        assert!(is_integer_literal_start("123abc"));

        // Test invalid starts
        assert!(!is_integer_literal_start(""));
        assert!(!is_integer_literal_start("abc"));
        assert!(!is_integer_literal_start(" "));
        assert!(!is_integer_literal_start("."));
    }

    #[test]
    fn test_is_float_literal_start() {
        // Test valid starts
        assert!(is_float_literal_start("0.5"));
        assert!(is_float_literal_start("3.14"));
        assert!(is_float_literal_start(".5")); // Leading dot
        assert!(is_float_literal_start("123."));

        // Test invalid starts
        assert!(!is_float_literal_start(""));
        assert!(!is_float_literal_start("abc"));
        assert!(!is_float_literal_start(" "));
        assert!(!is_float_literal_start("0"));
        assert!(!is_float_literal_start("42"));
    }

    #[test]
    fn test_is_boolean_literal() {
        // Test valid booleans
        assert!(is_boolean_literal("true"));
        assert!(is_boolean_literal("false"));

        // Test invalid values
        assert!(!is_boolean_literal("maybe"));
        assert!(!is_boolean_literal("True")); // Case sensitive
        assert!(!is_boolean_literal("FALSE"));
        assert!(!is_boolean_literal(""));
        assert!(!is_boolean_literal("tru"));
        assert!(!is_boolean_literal("falsehood"));
    }

    #[test]
    fn test_is_string_literal_start() {
        // Test valid starts
        assert!(is_string_literal_start("\""));
        assert!(is_string_literal_start("\"hello"));

        // Test invalid starts
        assert!(!is_string_literal_start(""));
        assert!(!is_string_literal_start("hello"));
        assert!(!is_string_literal_start("'"));
        assert!(!is_string_literal_start("r\""));
    }

    #[test]
    fn test_is_raw_string_literal_start() {
        // Test valid starts
        assert!(is_raw_string_literal_start("r\""));
        assert!(is_raw_string_literal_start("r\"hello"));

        // Test invalid starts
        assert!(!is_raw_string_literal_start(""));
        assert!(!is_raw_string_literal_start("hello"));
        assert!(!is_raw_string_literal_start("\""));
        assert!(!is_raw_string_literal_start("r"));
        assert!(!is_raw_string_literal_start("r'"));
    }

    #[test]
    fn test_parse_literal() {
        // Test integer literal
        let result = parse_literal("42");
        assert!(result.is_some());
        let token_result = result.unwrap();
        assert!(token_result.is_ok());
        let token = token_result.unwrap();
        assert_eq!(token.kind, TokenKind::IntLiteral);
        assert_eq!(token.text, "42");

        // Test float literal
        let result = parse_literal("3.14");
        assert!(result.is_some());
        let token_result = result.unwrap();
        assert!(token_result.is_ok());
        let token = token_result.unwrap();
        assert_eq!(token.kind, TokenKind::FloatLiteral);
        assert_eq!(token.text, "3.14");

        // Test scientific notation
        let result = parse_literal("6.02e23");
        assert!(result.is_some());
        let token_result = result.unwrap();
        assert!(token_result.is_ok());
        let token = token_result.unwrap();
        assert_eq!(token.kind, TokenKind::FloatLiteral);
        assert_eq!(token.text, "6.02e23");

        // Test string literal
        let result = parse_literal("\"hello\"");
        assert!(result.is_some());
        let token_result = result.unwrap();
        assert!(token_result.is_ok());
        let token = token_result.unwrap();
        assert_eq!(token.kind, TokenKind::StringLiteral);
        assert_eq!(token.text, "\"hello\"");

        // Test raw string literal
        let result = parse_literal("r\"hello\"");
        assert!(result.is_some());
        let token_result = result.unwrap();
        assert!(token_result.is_ok());
        let token = token_result.unwrap();
        assert_eq!(token.kind, TokenKind::RawStringLiteral);
        assert_eq!(token.text, "r\"hello\"");

        // Test boolean literal
        let result = parse_literal("true");
        assert!(result.is_some());
        let token_result = result.unwrap();
        assert!(token_result.is_ok());
        let token = token_result.unwrap();
        assert_eq!(token.kind, TokenKind::BoolLiteral);
        assert_eq!(token.text, "true");

        // Test invalid literal
        let result = parse_literal("invalid");
        assert!(result.is_none());

        // Test cases that should return None (not literals)
        assert!(parse_literal("abc").is_none()); // Not a literal
        assert!(parse_literal("identifier").is_none()); // Not a literal

        assert!(parse_literal("123abc").is_none()); // Mixed alphanumeric, not a valid literal
    }
}
