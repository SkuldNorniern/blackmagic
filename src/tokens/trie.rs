use super::kinds::TokenKind;
use std::collections::HashMap;

/// A trie node for efficient keyword and operator lookup
#[derive(Debug, Clone)]
struct TrieNode {
    children: HashMap<char, TrieNode>,
    token_kind: Option<TokenKind>,
}

impl TrieNode {
    fn new() -> Self {
        Self {
            children: HashMap::new(),
            token_kind: None,
        }
    }

    fn insert(&mut self, key: &str, token_kind: TokenKind) {
        let mut node = self;
        for ch in key.chars() {
            node = node.children.entry(ch).or_insert_with(TrieNode::new);
        }
        node.token_kind = Some(token_kind);
    }

    fn lookup(&self, key: &str) -> Option<TokenKind> {
        let mut node = self;
        for ch in key.chars() {
            if let Some(child) = node.children.get(&ch) {
                node = child;
            } else {
                return None;
            }
        }
        node.token_kind
    }

    fn longest_prefix_match(&self, input: &str) -> Option<(String, TokenKind)> {
        let mut node = self;
        let mut matched = String::new();
        let mut result = None;

        for ch in input.chars() {
            if let Some(child) = node.children.get(&ch) {
                node = child;
                matched.push(ch);
                if node.token_kind.is_some() {
                    result = Some((matched.clone(), node.token_kind.unwrap()));
                }
            } else {
                break;
            }
        }

        result
    }
}

/// Trie-based lookup table for keywords and operators
#[derive(Debug)]
pub struct TokenTrie {
    root: TrieNode,
}

impl TokenTrie {
    /// Create a new token trie with all Black Magic keywords and operators
    pub fn new() -> Self {
        let mut trie = Self {
            root: TrieNode::new(),
        };
        trie.initialize_keywords();
        trie.initialize_operators();
        trie
    }

    /// Initialize all keywords from the Black Magic specification
    fn initialize_keywords(&mut self) {
        let keywords = [
            ("circle", TokenKind::Circle),
            ("spell", TokenKind::Spell),
            ("ritual", TokenKind::Ritual),
            ("rune", TokenKind::Rune),
            ("pact", TokenKind::Pact),
            ("form", TokenKind::Form),
            ("when", TokenKind::When),
            ("ward", TokenKind::Ward),
            ("calls", TokenKind::Calls),
            ("effects", TokenKind::Effects),
            ("summon", TokenKind::Summon),
            ("as", TokenKind::As),
            ("seal", TokenKind::Seal),
            ("banish", TokenKind::Banish),
            ("let", TokenKind::Let),
            ("bind", TokenKind::Bind),
            ("const", TokenKind::Const),
            ("offer", TokenKind::Offer),
            ("consume", TokenKind::Consume),
            ("scry", TokenKind::Scry),
            ("attempt", TokenKind::Attempt),
            ("otherwise", TokenKind::Otherwise),
            ("dispel", TokenKind::Dispel),
            ("checkpoint", TokenKind::Checkpoint),
            ("rewind", TokenKind::Rewind),
            ("wait", TokenKind::Wait),
            ("tick", TokenKind::Tick),
            ("if", TokenKind::If),
            ("else", TokenKind::Else),
            ("while", TokenKind::While),
            ("for", TokenKind::For),
            ("match", TokenKind::Match),
            ("return", TokenKind::Return),
            ("break", TokenKind::Break),
            ("continue", TokenKind::Continue),
            ("mode", TokenKind::Mode),
            ("white", TokenKind::White),
            ("black", TokenKind::Black),
            ("veil", TokenKind::Veil),
            ("requires", TokenKind::Requires),
            ("yield", TokenKind::Yield),
            ("pass", TokenKind::Pass),
            ("fail", TokenKind::Fail),
            ("pub", TokenKind::Pub),
            ("struct", TokenKind::Struct),
            ("enum", TokenKind::Enum),
            ("type", TokenKind::Type),
            ("void", TokenKind::Void),
            ("BOTTOM", TokenKind::Bottom),
            ("Void", TokenKind::VoidType),
        ];

        for (keyword, kind) in keywords {
            self.root.insert(keyword, kind);
        }
    }

    /// Initialize all operators from the Black Magic specification
    fn initialize_operators(&mut self) {
        let operators = [
            ("+", TokenKind::Plus),
            ("-", TokenKind::Minus),
            ("*", TokenKind::Star),
            ("/", TokenKind::Slash),
            ("%", TokenKind::Percent),
            ("&", TokenKind::Ampersand),
            ("|", TokenKind::Pipe),
            ("^", TokenKind::Caret),
            ("~", TokenKind::Tilde),
            ("<<", TokenKind::LessLess),
            (">>", TokenKind::GreaterGreater),
            ("==", TokenKind::EqualEqual),
            ("!=", TokenKind::BangEqual),
            ("<", TokenKind::Less),
            ("<=", TokenKind::LessEqual),
            (">", TokenKind::Greater),
            (">=", TokenKind::GreaterEqual),
            ("=", TokenKind::Equal),
            ("and", TokenKind::And),
            ("or", TokenKind::Or),
            ("not", TokenKind::Not),
        ];

        // Sort by length descending to handle longer operators first
        let mut sorted_operators: Vec<_> = operators.iter().collect();
        sorted_operators.sort_by(|a, b| b.0.len().cmp(&a.0.len()));

        for (op, kind) in sorted_operators {
            self.root.insert(op, *kind);
        }
    }

    /// Look up a keyword or operator
    pub fn lookup(&self, key: &str) -> Option<TokenKind> {
        self.root.lookup(key)
    }

    /// Find the longest prefix match for operators
    pub fn longest_prefix_match(&self, input: &str) -> Option<(String, TokenKind)> {
        self.root.longest_prefix_match(input)
    }

    /// Check if a string is a keyword
    pub fn is_keyword(&self, key: &str) -> bool {
        self.lookup(key).map_or(false, |kind| kind.is_keyword())
    }

    /// Check if a string is an operator
    pub fn is_operator(&self, key: &str) -> bool {
        self.lookup(key).map_or(false, |kind| kind.is_operator())
    }
}

impl Default for TokenTrie {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword_lookup() {
        let trie = TokenTrie::new();

        assert_eq!(trie.lookup("circle"), Some(TokenKind::Circle));
        assert_eq!(trie.lookup("spell"), Some(TokenKind::Spell));
        assert_eq!(trie.lookup("let"), Some(TokenKind::Let));
        assert_eq!(trie.lookup("void"), Some(TokenKind::Void));
        assert_eq!(trie.lookup("nonexistent"), None);
    }

    #[test]
    fn test_operator_lookup() {
        let trie = TokenTrie::new();

        assert_eq!(trie.lookup("+"), Some(TokenKind::Plus));
        assert_eq!(trie.lookup("=="), Some(TokenKind::EqualEqual));
        assert_eq!(trie.lookup("<<"), Some(TokenKind::LessLess));
        assert_eq!(trie.lookup("and"), Some(TokenKind::And));
    }

    #[test]
    fn test_longest_prefix_match() {
        let trie = TokenTrie::new();

        assert_eq!(
            trie.longest_prefix_match("<<="),
            Some(("<<".to_string(), TokenKind::LessLess))
        );
        assert_eq!(
            trie.longest_prefix_match("!="),
            Some(("!=".to_string(), TokenKind::BangEqual))
        );
    }

    #[test]
    fn test_is_keyword() {
        let trie = TokenTrie::new();

        assert!(trie.is_keyword("circle"));
        assert!(trie.is_keyword("spell"));
        assert!(!trie.is_keyword("+"));
        assert!(!trie.is_keyword("identifier"));
    }

    #[test]
    fn test_is_operator() {
        let trie = TokenTrie::new();

        assert!(trie.is_operator("+"));
        assert!(trie.is_operator("=="));
        assert!(trie.is_operator("and"));
        assert!(!trie.is_operator("circle"));
        assert!(!trie.is_operator("identifier"));
    }
}
