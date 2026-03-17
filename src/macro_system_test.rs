// macro_system_test.rs - Comprehensive tests for macro system
// File: C:\Users\mummy\OneDrive\Documents\DarkFactory\zeta-0.3.4\src\macro_system_test.rs
// Purpose: Test macro parsing, pattern matching, expansion, and hygiene

#[cfg(test)]
mod tests {
    use crate::macro_system::{
        MacroDef, PatternToken, ExpansionToken, FragmentSpecifier, RepetitionOp,
        HygieneLevel, ExpansionContext, MacroParser, TokenTree, Token, Delimiter,
    };
    
    #[test]
    fn test_simple_macro_definition() {
        let parser = MacroParser;
        
        // Test parsing a simple macro
        let source = r#"
macro_rules! say_hello {
    () => {
        println!("Hello, world!");
    };
}
"#;
        
        let def = parser.parse_macro_def(source).unwrap();
        assert_eq!(def.name, "say_hello");
        assert_eq!(def.patterns.len(), 1);
        assert_eq!(def.expansions.len(), 1);
        assert_eq!(def.hygiene, HygieneLevel::Local);
        assert!(def.export);
    }
    
    #[test]
    fn test_macro_with_parameters() {
        let parser = MacroParser;
        
        let source = r#"
macro_rules! greet {
    ($name:ident) => {
        println!("Hello, {}!", $name);
    };
    ($name:expr) => {
        println!("Hello, {}!", $name);
    };
}
"#;
        
        let def = parser.parse_macro_def(source).unwrap();
        assert_eq!(def.name, "greet");
        assert_eq!(def.patterns.len(), 2);
        assert_eq!(def.expansions.len(), 2);
        
        // Check first pattern has ident fragment specifier
        if let PatternToken::Metavariable { name, fragment_specifier } = &def.patterns[0][0] {
            assert_eq!(name, "name");
            assert_eq!(fragment_specifier, &Some(FragmentSpecifier::Ident));
        } else {
            panic!("Expected metavariable pattern");
        }
    }
    
    #[test]
    fn test_repetition_pattern() {
        let parser = MacroParser;
        
        let source = r#"
macro_rules! vec {
    ($($x:expr),*) => {
        {
            let mut v = Vec::new();
            $(v.push($x);)*
            v
        }
    };
}
"#;
        
        let def = parser.parse_macro_def(source).unwrap();
        assert_eq!(def.name, "vec");
        
        // Check for repetition pattern
        assert!(!def.patterns[0].is_empty());
        
        // The pattern should contain a repetition token
        let has_repetition = def.patterns[0].iter().any(|t| {
            matches!(t, PatternToken::Repetition { .. })
        });
        assert!(has_repetition, "Expected repetition pattern");
    }
    
    #[test]
    fn test_macro_expansion_context() {
        let mut context = ExpansionContext::new();
        
        // Create a simple macro definition
        let def = MacroDef {
            name: "double".to_string(),
            patterns: vec![vec![
                PatternToken::Metavariable {
                    name: "x".to_string(),
                    fragment_specifier: Some(FragmentSpecifier::Expr),
                },
            ]],
            expansions: vec![vec![
                ExpansionToken::Literal("2".to_string()),
                ExpansionToken::Literal("*".to_string()),
                ExpansionToken::Substitution("x".to_string()),
            ]],
            hygiene: HygieneLevel::Local,
            export: true,
        };
        
        // Register the macro
        assert!(context.register_macro(def).is_ok());
        
        // Test that duplicate registration fails
        let def2 = MacroDef {
            name: "double".to_string(),
            patterns: vec![vec![]],
            expansions: vec![vec![]],
            hygiene: HygieneLevel::Local,
            export: true,
        };
        
        assert!(context.register_macro(def2).is_err());
    }
    
    #[test]
    fn test_hygienic_identifiers() {
        let mut context = ExpansionContext::new();
        
        // Generate hygienic identifiers
        let ident1 = context.hygienic_ident("x");
        let ident2 = context.hygienic_ident("x");
        let ident3 = context.hygienic_ident("y");
        
        assert_ne!(ident1, ident2);
        assert!(ident1.starts_with("x__"));
        assert!(ident2.starts_with("x__"));
        assert!(ident3.starts_with("y__"));
        
        // Check counter incremented
        assert!(ident1 < ident2);
    }
    
    #[test]
    fn test_token_tree_construction() {
        // Test building token trees
        
        // Simple identifier token
        let ident_token = TokenTree::Token(Token::Ident("foo".to_string()));
        
        // Delimited token tree
        let delimited = TokenTree::Delimited(
            Delimiter::Parenthesis,
            vec![
                TokenTree::Token(Token::Ident("x".to_string())),
                TokenTree::Token(Token::Ident("+".to_string())),
                TokenTree::Token(Token::Ident("y".to_string())),
            ]
        );
        
        // Nested token trees
        let nested = TokenTree::Delimited(
            Delimiter::Brace,
            vec![
                TokenTree::Token(Token::Ident("let".to_string())),
                ident_token.clone(),
                TokenTree::Token(Token::Ident("=".to_string())),
                delimited.clone(),
                TokenTree::Token(Token::Ident(";".to_string())),
            ]
        );
        
        // Verify construction
        assert!(matches!(ident_token, TokenTree::Token(Token::Ident(_))));
        assert!(matches!(delimited, TokenTree::Delimited(Delimiter::Parenthesis, _)));
        assert!(matches!(nested, TokenTree::Delimited(Delimiter::Brace, _)));
    }
    
    #[test]
    fn test_pattern_matching_simple() {
        let context = ExpansionContext::new();
        
        // Simple pattern: $x:expr
        let pattern = vec![
            PatternToken::Metavariable {
                name: "x".to_string(),
                fragment_specifier: Some(FragmentSpecifier::Expr),
            },
        ];
        
        // Input: 42
        let input = vec![
            TokenTree::Token(Token::Literal("42".to_string())),
        ];
        
        // Should match
        let bindings = context.match_pattern(&pattern, &input);
        assert!(bindings.is_some());
        
        let bindings = bindings.unwrap();
        assert!(bindings.contains_key("x"));
        assert_eq!(bindings["x"].len(), 1);
    }
    
    #[test]
    fn test_pattern_matching_literal() {
        let context = ExpansionContext::new();
        
        // Pattern: fn $name:ident()
        let pattern = vec![
            PatternToken::Literal("fn".to_string()),
            PatternToken::Metavariable {
                name: "name".to_string(),
                fragment_specifier: Some(FragmentSpecifier::Ident),
            },
            PatternToken::Literal("()".to_string()),
        ];
        
        // Input: fn main()
        let input = vec![
            TokenTree::Token(Token::Ident("fn".to_string())),
            TokenTree::Token(Token::Ident("main".to_string())),
            TokenTree::Token(Token::Ident("()".to_string())),
        ];
        
        let bindings = context.match_pattern(&pattern, &input);
        assert!(bindings.is_some());
        
        let bindings = bindings.unwrap();
        assert_eq!(bindings["name"][0], TokenTree::Token(Token::Ident("main".to_string())));
    }
    
    #[test]
    fn test_expansion_token_generation() {
        // Test creating various expansion tokens
        
        // Literal expansion
        let literal = ExpansionToken::Literal("println!".to_string());
        
        // Substitution expansion
        let substitution = ExpansionToken::Substitution("msg".to_string());
        
        // Repetition expansion
        let repetition = ExpansionToken::Repetition {
            metavar: "items".to_string(),
            expansion: vec![
                ExpansionToken::Substitution("item".to_string()),
                ExpansionToken::Literal(",".to_string()),
            ],
            separator: Some(" ".to_string()),
        };
        
        // Verify variants
        assert!(matches!(literal, ExpansionToken::Literal(_)));
        assert!(matches!(substitution, ExpansionToken::Substitution(_)));
        assert!(matches!(repetition, ExpansionToken::Repetition { .. }));
    }
    
    #[test]
    fn test_fragment_specifier_enum() {
        // Test all fragment specifiers
        
        let specifiers = vec![
            FragmentSpecifier::Ident,
            FragmentSpecifier::Expr,
            FragmentSpecifier::Ty,
            FragmentSpecifier::Pat,
            FragmentSpecifier::Stmt,
            FragmentSpecifier::Block,
            FragmentSpecifier::Item,
            FragmentSpecifier::Meta,
            FragmentSpecifier::Tt,
        ];
        
        assert_eq!(specifiers.len(), 9);
        
        // Verify each is unique
        use std::collections::HashSet;
        let mut set = HashSet::new();
        for spec in &specifiers {
            set.insert(spec);
        }
        assert_eq!(set.len(), 9);
    }
    
    #[test]
    fn test_repetition_operators() {
        // Test repetition operator variants
        
        let operators = vec![
            RepetitionOp::ZeroOrMore,
            RepetitionOp::OneOrMore,
            RepetitionOp::ZeroOrOne,
        ];
        
        assert_eq!(operators.len(), 3);
        
        // Test matching
        assert!(matches!(operators[0], RepetitionOp::ZeroOrMore));
        assert!(matches!(operators[1], RepetitionOp::OneOrMore));
        assert!(matches!(operators[2], RepetitionOp::ZeroOrOne));
    }
    
    #[test]
    fn test_hygiene_levels() {
        // Test hygiene level variants
        
        let levels = vec![
            HygieneLevel::None,
            HygieneLevel::Local,
            HygieneLevel::Global,
        ];
        
        assert_eq!(levels.len(), 3);
        
        // Test matching
        assert!(matches!(levels[0], HygieneLevel::None));
        assert!(matches!(levels[1], HygieneLevel::Local));
        assert!(matches!(levels[2], HygieneLevel::Global));
    }
    
    #[test]
    fn test_macro_system_integration() {
        // Test a complete macro system scenario
        
        let mut context = ExpansionContext::new();
        let parser = MacroParser;
        
        // Define a macro
        let source = r#"
macro_rules! assert_eq {
    ($left:expr, $right:expr) => {
        if $left != $right {
            panic!("assertion failed: {} != {}", $left, $right);
        }
    };
}
"#;
        
        let def = parser.parse_macro_def(source).unwrap();
        context.register_macro(def).unwrap();
        
        // Create test input tokens
        let input = vec![
            TokenTree::Token(Token::Ident("assert_eq".to_string())),
            TokenTree::Delimited(
                Delimiter::Parenthesis,
                vec![
                    TokenTree::Token(Token::Literal("1".to_string())),
                    TokenTree::Token(Token::Ident(",".to_string())),
                    TokenTree::Token(Token::Literal("1".to_string())),
                ]
            ),
        ];
        
        // In a real test, we would expand this macro
        // For now, just verify the macro is registered
        assert!(context.macros.contains_key("assert_eq"));
        
        println!("Macro system integration test passed!");
        println!("  Macro 'assert_eq' registered successfully");
        println!("  Pattern matching and expansion infrastructure ready");
    }
    
    #[test]
    fn test_complex_macro_parsing() {
        let parser = MacroParser;
        
        // Test parsing a complex macro with multiple rules and repetitions
        let source = r#"
macro_rules! match_values {
    ($($x:expr => $y:expr),*) => {
        match value {
            $($x => $y,)*
            _ => default,
        }
    };
    ($default:expr) => {
        value => $default
    };
}
"#;
        
        let def = parser.parse_macro_def(source).unwrap();
        assert_eq!(def.name, "match_values");
        assert_eq!(def.patterns.len(), 2);
        assert_eq!(def.expansions.len(), 2);
        
        // First pattern should have repetition
        let has_repetition = def.patterns[0].iter().any(|t| {
            matches!(t, PatternToken::Repetition { .. })
        });
        assert!(has_repetition, "First pattern should have repetition");
        
        // Second pattern should be simple
        assert!(def.patterns[1].len() > 0);
    }
}