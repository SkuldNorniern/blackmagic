/// Token kinds for the Black Magic language
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // Keywords
    Circle,
    Spell,
    Ritual,
    Rune,
    Pact,
    Form,
    When,
    Ward,
    Calls,
    Effects,
    Summon,
    As,
    Seal,
    Banish,
    Let,
    Bind,
    Const,
    Offer,
    Consume,
    Scry,
    Attempt,
    Otherwise,
    Dispel,
    Checkpoint,
    Rewind,
    Wait,
    Tick,
    If,
    Else,
    While,
    For,
    Match,
    Return,
    Break,
    Continue,
    Mode,
    White,
    Black,
    Veil,
    Requires,
    Yield,
    Pass,
    Fail,
    Pub,
    PubCrate,
    PubSuper,
    Struct,
    Enum,
    Type,
    Void,
    Bottom,
    VoidType,

    // Literals
    Identifier,
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    RawStringLiteral,
    BoolLiteral,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Ampersand,
    Pipe,
    Caret,
    Tilde,
    LessLess,
    GreaterGreater,
    EqualEqual,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    And,
    Or,
    Not,

    // Punctuation
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Comma,
    Semicolon,
    Colon,
    DoubleColon,
    Arrow,
    At,
    Hash,
    Question,
    Dollar,
    Exclamation,

    // Special tokens
    Comment,
    Whitespace,
    Newline,
    Eof,

    // Error token
    Error,
}

impl TokenKind {
    /// Check if this token kind represents a keyword
    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            TokenKind::Circle
                | TokenKind::Spell
                | TokenKind::Ritual
                | TokenKind::Rune
                | TokenKind::Pact
                | TokenKind::Form
                | TokenKind::When
                | TokenKind::Ward
                | TokenKind::Calls
                | TokenKind::Effects
                | TokenKind::Summon
                | TokenKind::As
                | TokenKind::Seal
                | TokenKind::Banish
                | TokenKind::Let
                | TokenKind::Bind
                | TokenKind::Const
                | TokenKind::Offer
                | TokenKind::Consume
                | TokenKind::Scry
                | TokenKind::Attempt
                | TokenKind::Otherwise
                | TokenKind::Dispel
                | TokenKind::Checkpoint
                | TokenKind::Rewind
                | TokenKind::Wait
                | TokenKind::Tick
                | TokenKind::If
                | TokenKind::Else
                | TokenKind::While
                | TokenKind::For
                | TokenKind::Match
                | TokenKind::Return
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::Mode
                | TokenKind::White
                | TokenKind::Black
                | TokenKind::Veil
                | TokenKind::Requires
                | TokenKind::Yield
                | TokenKind::Pass
                | TokenKind::Fail
                | TokenKind::Pub
                | TokenKind::Struct
                | TokenKind::Enum
                | TokenKind::Type
                | TokenKind::Void
                | TokenKind::Bottom
                | TokenKind::VoidType
        )
    }

    /// Check if this token kind represents an operator
    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Percent
                | TokenKind::Ampersand
                | TokenKind::Pipe
                | TokenKind::Caret
                | TokenKind::Tilde
                | TokenKind::LessLess
                | TokenKind::GreaterGreater
                | TokenKind::EqualEqual
                | TokenKind::BangEqual
                | TokenKind::Less
                | TokenKind::LessEqual
                | TokenKind::Greater
                | TokenKind::GreaterEqual
                | TokenKind::Equal
                | TokenKind::And
                | TokenKind::Or
                | TokenKind::Not
        )
    }

    /// Check if this token kind represents punctuation
    pub fn is_punctuation(&self) -> bool {
        matches!(
            self,
            TokenKind::LeftParen
                | TokenKind::RightParen
                | TokenKind::LeftBracket
                | TokenKind::RightBracket
                | TokenKind::LeftBrace
                | TokenKind::RightBrace
                | TokenKind::Comma
                | TokenKind::Semicolon
                | TokenKind::Colon
                | TokenKind::DoubleColon
                | TokenKind::Arrow
                | TokenKind::At
                | TokenKind::Hash
                | TokenKind::Question
                | TokenKind::Dollar
                | TokenKind::Exclamation
        )
    }

    /// Check if this token kind represents a literal
    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            TokenKind::IntLiteral
                | TokenKind::FloatLiteral
                | TokenKind::StringLiteral
                | TokenKind::RawStringLiteral
                | TokenKind::BoolLiteral
        )
    }

    /// Get the string representation of this token kind
    pub fn as_str(&self) -> &'static str {
        match self {
            TokenKind::Circle => "circle",
            TokenKind::Spell => "spell",
            TokenKind::Ritual => "ritual",
            TokenKind::Rune => "rune",
            TokenKind::Pact => "pact",
            TokenKind::Form => "form",
            TokenKind::When => "when",
            TokenKind::Ward => "ward",
            TokenKind::Calls => "calls",
            TokenKind::Effects => "effects",
            TokenKind::Summon => "summon",
            TokenKind::As => "as",
            TokenKind::Seal => "seal",
            TokenKind::Banish => "banish",
            TokenKind::Let => "let",
            TokenKind::Bind => "bind",
            TokenKind::Const => "const",
            TokenKind::Offer => "offer",
            TokenKind::Consume => "consume",
            TokenKind::Scry => "scry",
            TokenKind::Attempt => "attempt",
            TokenKind::Otherwise => "otherwise",
            TokenKind::Dispel => "dispel",
            TokenKind::Checkpoint => "checkpoint",
            TokenKind::Rewind => "rewind",
            TokenKind::Wait => "wait",
            TokenKind::Tick => "tick",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::While => "while",
            TokenKind::For => "for",
            TokenKind::Match => "match",
            TokenKind::Return => "return",
            TokenKind::Break => "break",
            TokenKind::Continue => "continue",
            TokenKind::Mode => "mode",
            TokenKind::White => "white",
            TokenKind::Black => "black",
            TokenKind::Veil => "veil",
            TokenKind::Requires => "requires",
            TokenKind::Yield => "yield",
            TokenKind::Pass => "pass",
            TokenKind::Fail => "fail",
            TokenKind::Pub => "pub",
            TokenKind::PubCrate => "pub(crate)",
            TokenKind::PubSuper => "pub(super)",
            TokenKind::Struct => "struct",
            TokenKind::Enum => "enum",
            TokenKind::Type => "type",
            TokenKind::Void => "void",
            TokenKind::Bottom => "BOTTOM",
            TokenKind::VoidType => "Void",
            TokenKind::Identifier => "identifier",
            TokenKind::IntLiteral => "integer literal",
            TokenKind::FloatLiteral => "float literal",
            TokenKind::StringLiteral => "string literal",
            TokenKind::RawStringLiteral => "raw string literal",
            TokenKind::BoolLiteral => "boolean literal",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Star => "*",
            TokenKind::Slash => "/",
            TokenKind::Percent => "%",
            TokenKind::Ampersand => "&",
            TokenKind::Pipe => "|",
            TokenKind::Caret => "^",
            TokenKind::Tilde => "~",
            TokenKind::LessLess => "<<",
            TokenKind::GreaterGreater => ">>",
            TokenKind::EqualEqual => "==",
            TokenKind::BangEqual => "!=",
            TokenKind::Less => "<",
            TokenKind::LessEqual => "<=",
            TokenKind::Greater => ">",
            TokenKind::GreaterEqual => ">=",
            TokenKind::Equal => "=",
            TokenKind::And => "and",
            TokenKind::Or => "or",
            TokenKind::Not => "not",
            TokenKind::LeftParen => "(",
            TokenKind::RightParen => ")",
            TokenKind::LeftBracket => "[",
            TokenKind::RightBracket => "]",
            TokenKind::LeftBrace => "{",
            TokenKind::RightBrace => "}",
            TokenKind::Comma => ",",
            TokenKind::Semicolon => ";",
            TokenKind::Colon => ":",
            TokenKind::DoubleColon => "::",
            TokenKind::Arrow => "->",
            TokenKind::At => "@",
            TokenKind::Hash => "#",
            TokenKind::Question => "?",
            TokenKind::Dollar => "$",
            TokenKind::Exclamation => "!",
            TokenKind::Comment => "comment",
            TokenKind::Whitespace => "whitespace",
            TokenKind::Newline => "newline",
            TokenKind::Eof => "end of file",
            TokenKind::Error => "error",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_keyword() {
        // Test keywords
        assert!(TokenKind::Circle.is_keyword());
        assert!(TokenKind::Spell.is_keyword());
        assert!(TokenKind::Let.is_keyword());
        assert!(TokenKind::If.is_keyword());
        assert!(TokenKind::While.is_keyword());
        assert!(TokenKind::Return.is_keyword());
        assert!(TokenKind::Struct.is_keyword());
        assert!(TokenKind::Enum.is_keyword());
        assert!(TokenKind::Void.is_keyword());
        assert!(TokenKind::Bottom.is_keyword());

        // Test non-keywords
        assert!(!TokenKind::Identifier.is_keyword());
        assert!(!TokenKind::IntLiteral.is_keyword());
        assert!(!TokenKind::Plus.is_keyword());
        assert!(!TokenKind::LeftParen.is_keyword());
        assert!(!TokenKind::Comment.is_keyword());
        assert!(!TokenKind::Eof.is_keyword());
    }

    #[test]
    fn test_is_operator() {
        // Test operators
        assert!(TokenKind::Plus.is_operator());
        assert!(TokenKind::Minus.is_operator());
        assert!(TokenKind::Star.is_operator());
        assert!(TokenKind::Slash.is_operator());
        assert!(TokenKind::EqualEqual.is_operator());
        assert!(TokenKind::BangEqual.is_operator());
        assert!(TokenKind::Less.is_operator());
        assert!(TokenKind::GreaterEqual.is_operator());
        assert!(TokenKind::And.is_operator());
        assert!(TokenKind::Or.is_operator());
        assert!(TokenKind::Not.is_operator());
        assert!(TokenKind::Ampersand.is_operator());
        assert!(TokenKind::Pipe.is_operator());
        assert!(TokenKind::Caret.is_operator());

        // Test non-operators
        assert!(!TokenKind::Circle.is_operator());
        assert!(!TokenKind::Identifier.is_operator());
        assert!(!TokenKind::IntLiteral.is_operator());
        assert!(!TokenKind::LeftParen.is_operator());
        assert!(!TokenKind::Comment.is_operator());
    }

    #[test]
    fn test_is_punctuation() {
        // Test punctuation
        assert!(TokenKind::LeftParen.is_punctuation());
        assert!(TokenKind::RightParen.is_punctuation());
        assert!(TokenKind::LeftBracket.is_punctuation());
        assert!(TokenKind::RightBracket.is_punctuation());
        assert!(TokenKind::LeftBrace.is_punctuation());
        assert!(TokenKind::RightBrace.is_punctuation());
        assert!(TokenKind::Comma.is_punctuation());
        assert!(TokenKind::Semicolon.is_punctuation());
        assert!(TokenKind::Colon.is_punctuation());
        assert!(TokenKind::DoubleColon.is_punctuation());
        assert!(TokenKind::Arrow.is_punctuation());
        assert!(TokenKind::Exclamation.is_punctuation());

        // Test non-punctuation
        assert!(!TokenKind::Circle.is_punctuation());
        assert!(!TokenKind::Identifier.is_punctuation());
        assert!(!TokenKind::IntLiteral.is_punctuation());
        assert!(!TokenKind::Plus.is_punctuation());
        assert!(!TokenKind::Comment.is_punctuation());
    }

    #[test]
    fn test_is_literal() {
        // Test literals
        assert!(TokenKind::IntLiteral.is_literal());
        assert!(TokenKind::FloatLiteral.is_literal());
        assert!(TokenKind::StringLiteral.is_literal());
        assert!(TokenKind::RawStringLiteral.is_literal());
        assert!(TokenKind::BoolLiteral.is_literal());

        // Test non-literals
        assert!(!TokenKind::Circle.is_literal());
        assert!(!TokenKind::Identifier.is_literal());
        assert!(!TokenKind::Plus.is_literal());
        assert!(!TokenKind::LeftParen.is_literal());
        assert!(!TokenKind::Comment.is_literal());
        assert!(!TokenKind::Eof.is_literal());
    }

    #[test]
    fn test_as_str() {
        // Test keyword strings
        assert_eq!(TokenKind::Circle.as_str(), "circle");
        assert_eq!(TokenKind::Spell.as_str(), "spell");
        assert_eq!(TokenKind::Let.as_str(), "let");
        assert_eq!(TokenKind::If.as_str(), "if");
        assert_eq!(TokenKind::While.as_str(), "while");
        assert_eq!(TokenKind::Return.as_str(), "return");

        // Test operator strings
        assert_eq!(TokenKind::Plus.as_str(), "+");
        assert_eq!(TokenKind::Minus.as_str(), "-");
        assert_eq!(TokenKind::Star.as_str(), "*");
        assert_eq!(TokenKind::EqualEqual.as_str(), "==");
        assert_eq!(TokenKind::BangEqual.as_str(), "!=");
        assert_eq!(TokenKind::LessLess.as_str(), "<<");
        assert_eq!(TokenKind::And.as_str(), "and");
        assert_eq!(TokenKind::Or.as_str(), "or");

        // Test punctuation strings
        assert_eq!(TokenKind::LeftParen.as_str(), "(");
        assert_eq!(TokenKind::RightParen.as_str(), ")");
        assert_eq!(TokenKind::LeftBracket.as_str(), "[");
        assert_eq!(TokenKind::RightBracket.as_str(), "]");
        assert_eq!(TokenKind::Comma.as_str(), ",");
        assert_eq!(TokenKind::Semicolon.as_str(), ";");
        assert_eq!(TokenKind::Colon.as_str(), ":");
        assert_eq!(TokenKind::DoubleColon.as_str(), "::");
        assert_eq!(TokenKind::Arrow.as_str(), "->");

        // Test literal strings
        assert_eq!(TokenKind::IntLiteral.as_str(), "integer literal");
        assert_eq!(TokenKind::FloatLiteral.as_str(), "float literal");
        assert_eq!(TokenKind::StringLiteral.as_str(), "string literal");
        assert_eq!(TokenKind::RawStringLiteral.as_str(), "raw string literal");
        assert_eq!(TokenKind::BoolLiteral.as_str(), "boolean literal");

        // Test special strings
        assert_eq!(TokenKind::Identifier.as_str(), "identifier");
        assert_eq!(TokenKind::Comment.as_str(), "comment");
        assert_eq!(TokenKind::Whitespace.as_str(), "whitespace");
        assert_eq!(TokenKind::Newline.as_str(), "newline");
        assert_eq!(TokenKind::Eof.as_str(), "end of file");
        assert_eq!(TokenKind::Error.as_str(), "error");
    }
}
