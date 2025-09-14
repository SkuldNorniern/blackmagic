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
