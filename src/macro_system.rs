// macro_system.rs - Declarative macro system for Zeta v0.3.5
// File: C:\Users\mummy\OneDrive\Documents\DarkFactory\zeta-0.3.4\src\macro_system.rs
// Purpose: Macro-by-example syntax, pattern matching, expansion, hygiene

use std::collections::{HashMap, HashSet};
use std::fmt;

/// Macro pattern matcher token
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PatternToken {
    /// Literal token: `ident`, `+`, `[`, etc.
    Literal(String),
    /// Metavariable: `$ident` or `$(ident)*`
    Metavariable {
        name: String,
        repetition: Option<Repetition>,
    },
    /// Token tree: `(...)` or `[...]` or `{...}`
    TokenTree(Vec<PatternToken>),
}

/// Repetition specifier for metavariables
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Repetition {
    /// Zero or more: `*`
    ZeroOrMore,
    /// One or more: `+`
    OneOrMore,
    /// Zero or one: `?`
    ZeroOrOne,
}

/// Expansion token for macro output
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExpansionToken {
    /// Literal token
    Literal(String),
    /// Metavariable substitution
    Substitution {
        name: String,
        repetition: Option<Repetition>,
    },
    /// Conditional expansion
    Conditional {
        condition: String,
        then_branch: Vec<ExpansionToken>,
        else_branch: Option<Vec<ExpansionToken>>,
    },
}

/// Hygiene level for macro expansion
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HygieneLevel {
    /// No hygiene - identifiers can capture
    Unhygienic,
    /// Local hygiene - identifiers are scoped
    Local,
    /// Full hygiene - fresh identifiers generated
    Full,
}

/// Macro pattern with expansion
#[derive(Debug, Clone)]
pub struct MacroPattern {
    pub name: String,
    pub pattern: Vec<PatternToken>,
    pub expansion: Vec<ExpansionToken>,
    pub hygiene: HygieneLevel,
}

/// Macro expander with pattern matching
#[derive(Debug, Clone, Default)]
pub struct MacroExpander {
    patterns: Vec<MacroPattern>,
    hygiene_counter: u64,
}

impl MacroExpander {
    pub fn new() -> Self {
        Self {
            patterns: Vec::new(),
            hygiene_counter: 0,
        }
    }
    
    pub fn add_pattern(&mut self, pattern: MacroPattern) {
        self.patterns.push(pattern);
    }
    
    pub fn expand(&mut self, input: &[TokenTree], hygiene: HygieneLevel) -> Result<Vec<TokenTree>, String> {
        for pattern in &self.patterns {
            if let Some(bindings) = self.match_pattern(&pattern.pattern, input) {
                return self.expand_tokens(&pattern.expansion, &bindings, &hygiene);
            }
        }
        Err("No matching pattern found".to_string())
    }
    
    fn match_pattern(&self, pattern: &[PatternToken], input: &[TokenTree]) -> Option<HashMap<String, Vec<TokenTree>>> {
        let mut bindings = HashMap::new();
        let mut pattern_idx = 0;
        let mut input_idx = 0;
        
        while pattern_idx < pattern.len() && input_idx < input.len() {
            match &pattern[pattern_idx] {
                PatternToken::Literal(lit) => {
                    if let TokenTree::Token(Token::Ident(ref ident)) = &input[input_idx] {
                        if ident == lit {
                            pattern_idx += 1;
                            input_idx += 1;
                            continue;
                        }
                    }
                    return None;
                }
                PatternToken::Metavariable { name, repetition } => {
                    match repetition {
                        Some(Repetition::ZeroOrMore) | Some(Repetition::OneOrMore) => {
                            let mut captured = Vec::new();
                            while input_idx < input.len() {
                                captured.push(input[input_idx].clone());
                                input_idx += 1;
                            }
                            if let Some(Repetition::OneOrMore) = repetition {
                                if captured.is_empty() {
                                    return None;
                                }
                            }
                            bindings.insert(name.clone(), captured);
                            pattern_idx += 1;
                        }
                        Some(Repetition::ZeroOrOne) => {
                            if input_idx < input.len() {
                                bindings.insert(name.clone(), vec![input[input_idx].clone()]);
                                input_idx += 1;
                            } else {
                                bindings.insert(name.clone(), Vec::new());
                            }
                            pattern_idx += 1;
                        }
                        None => {
                            bindings.insert(name.clone(), vec![input[input_idx].clone()]);
                            pattern_idx += 1;
                            input_idx += 1;
                        }
                    }
                }
                PatternToken::TokenTree(subpattern) => {
                    if let TokenTree::Delimited(_, _, ref tokens) = &input[input_idx] {
                        if let Some(sub_bindings) = self.match_pattern(subpattern, tokens) {
                            for (k, v) in sub_bindings {
                                bindings.insert(k, v);
                            }
                            pattern_idx += 1;
                            input_idx += 1;
                            continue;
                        }
                    }
                    return None;
                }
            }
        }
        
        if pattern_idx == pattern.len() && input_idx == input.len() {
            Some(bindings)
        } else {
            None
        }
    }
    
    fn expand_tokens(&mut self, tokens: &[ExpansionToken], bindings: &HashMap<String, Vec<TokenTree>>, hygiene: &HygieneLevel) -> Result<Vec<TokenTree>, String> {
        let mut result = Vec::new();
        
        for token in tokens {
            match token {
                ExpansionToken::Literal(lit) => {
                    result.push(TokenTree::Token(Token::Ident(lit.clone())));
                }
                ExpansionToken::Substitution { name, repetition } => {
                    if let Some(bound_tokens) = bindings.get(name) {
                        match repetition {
                            Some(Repetition::ZeroOrMore) | Some(Repetition::OneOrMore) => {
                                for token in bound_tokens {
                                    result.push(token.clone());
                                }
                            }
                            Some(Repetition::ZeroOrOne) => {
                                if !bound_tokens.is_empty() {
                                    result.push(bound_tokens[0].clone());
                                }
                            }
                            None => {
                                if bound_tokens.len() == 1 {
                                    result.push(bound_tokens[0].clone());
                                } else {
                                    return Err(format!("Expected single token for {}, got {}", name, bound_tokens.len()));
                                }
                            }
                        }
                    } else {
                        return Err(format!("Unbound metavariable: {}", name));
                    }
                }
                ExpansionToken::Conditional { condition, then_branch, else_branch } => {
                    // Simple condition evaluation - check if binding exists and is non-empty
                    let condition_met = if let Some(bound_tokens) = bindings.get(condition) {
                        !bound_tokens.is_empty()
                    } else {
                        false
                    };
                    
                    if condition_met {
                        let expanded = self.expand_tokens(then_branch, bindings, hygiene)?;
                        result.extend(expanded);
                    } else if let Some(else_branch) = else_branch {
                        let expanded = self.expand_tokens(else_branch, bindings, hygiene)?;
                        result.extend(expanded);
                    }
                }
            }
        }
        
        Ok(result)
    }
    
    fn parse_pattern(&self, input: &str) -> Result<Vec<PatternToken>, String> {
        let mut tokens = Vec::new();
        let mut chars = input.chars().peekable();
        
        while let Some(c) = chars.next() {
            match c {
                '$' => {
                    // Metavariable
                    let mut name = String::new();
                    while let Some(&c) = chars.peek() {
                        if c.is_alphanumeric() || c == '_' {
                            name.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    
                    if name.is_empty() {
                        return Err("Expected metavariable name".to_string());
                    }
                    
                    // Check for repetition
                    let repetition = if let Some(&c) = chars.peek() {
                        match c {
                            '*' => {
                                chars.next();
                                Some(Repetition::ZeroOrMore)
                            }
                            '+' => {
                                chars.next();
                                Some(Repetition::OneOrMore)
                            }
                            '?' => {
                                chars.next();
                                Some(Repetition::ZeroOrOne)
                            }
                            _ => None,
                        }
                    } else {
                        None
                    };
                    
                    tokens.push(PatternToken::Metavariable { name, repetition });
                }
                '(' | '[' | '{' => {
                    // Find matching delimiter
                    let closing = match c {
                        '(' => ')',
                        '[' => ']',
                        '{' => '}',
                        _ => unreachable!(),
                    };
                    
                    let mut content = String::new();
                    let mut depth = 1;
                    
                    while let Some(next_c) = chars.next() {
                        if next_c == c {
                            depth += 1;
                        } else if next_c == closing {
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                        }
                        content.push(next_c);
                    }
                    
                    if depth != 0 {
                        return Err(format!("Unclosed delimiter: {}", c));
                    }
                    
                    let sub_tokens = self.parse_pattern(&content)?;
                    tokens.push(PatternToken::TokenTree(sub_tokens));
                }
                ')' | ']' | '}' => {
                    return Err(format!("Unexpected closing delimiter: {}", c));
                }
                _ if c.is_whitespace() => {
                    // Skip whitespace
                }
                _ => {
                    // Literal token
                    let mut lit = String::new();
                    lit.push(c);
                    while let Some(&c) = chars.peek() {
                        if !c.is_whitespace() && !"$()[]{}".contains(c) {
                            lit.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    tokens.push(PatternToken::Literal(lit));
                }
            }
        }
        
        Ok(tokens)
    }
    
    fn parse_expansion(&self, input: &str) -> Result<Vec<ExpansionToken>, String> {
        let mut tokens = Vec::new();
        let mut chars = input.chars().peekable();
        
        while let Some(c) = chars.next() {
            match c {
                '$' => {
                    // Metavariable substitution
                    let mut name = String::new();
                    while let Some(&c) = chars.peek() {
                        if c.is_alphanumeric() || c == '_' {
                            name.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    
                    if name.is_empty() {
                        return Err("Expected metavariable name".to_string());
                    } else {
                        // Check for repetition
                        let repetition = if let Some(&c) = chars.peek() {
                            match c {
                                '*' => {
                                    chars.next();
                                    Some(Repetition::ZeroOrMore)
                                }
                                '+' => {
                                    chars.next();
                                    Some(Repetition::OneOrMore)
                                }
                                '?' => {
                                    chars.next();
                                    Some(Repetition::ZeroOrOne)
                                }
                                _ => None,
                            }
                        } else {
                            None
                        };
                        
                        tokens.push(ExpansionToken::Substitution { name, repetition });
                    }
                }
                '#' => {
                    // Conditional expansion
                    if let Some(&'(') = chars.peek() {
                        chars.next(); // Skip '('
                        
                        let mut condition = String::new();
                        while let Some(&c) = chars.peek() {
                            if c != ')' {
                                condition.push(chars.next().unwrap());
                            } else {
                                break;
                            }
                        }
                        
                        if let Some(&')') = chars.peek() {
                            chars.next(); // Skip ')'
                        } else {
                            return Err("Expected ')' after condition".to_string());
                        }
                        
                        // Parse then branch
                        let mut then_branch = String::new();
                        while let Some(&c) = chars.peek() {
                            if c != '#' {
                                then_branch.push(chars.next().unwrap());
                            } else {
                                break;
                            }
                        }
                        
                        let mut else_branch = None;
                        
                        // Check for else branch
                        if let Some(&'#') = chars.peek() {
                            chars.next(); // Skip '#'
                            if let Some(&'(') = chars.peek() {
                                chars.next(); // Skip '('
                                
                                let mut else_content = String::new();
                                while let Some(&c) = chars.peek() {
                                    if c != ')' {
                                        else_content.push(chars.next().unwrap());
                                    } else {
                                        break;
                                    }
                                }
                                
                                if let Some(&')') = chars.peek() {
                                    chars.next(); // Skip ')'
                                    else_branch = Some(else_content);
                                } else {
                                    return Err("Expected ')' after else branch".to_string());
                                }
                            }
                        }
                        
                        let then_tokens = self.parse_expansion(&then_branch)?;
                        let else_tokens = else_branch.map(|eb| self.parse_expansion(&eb)).transpose()?;
                        
                        tokens.push(ExpansionToken::Conditional {
                            condition,
                            then_branch: then_tokens,
                            else_branch: else_tokens,
                        });
                    } else {
                        // Literal '#'
                        tokens.push(ExpansionToken::Literal("#".to_string()));
                    }
                }
                _ => {
                    // Literal token
                    let mut lit = String::new();
                    lit.push(c);
                    while let Some(&c) = chars.peek() {
                        if !c.is_whitespace() && !"$#".contains(c) {
                            lit.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    tokens.push(ExpansionToken::Literal(lit));
                }
            }
        }
        
        Ok(tokens)
    }
}

// Token and TokenTree definitions (simplified)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Ident(String),
    Punct(char),
    Literal(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenTree {
    Token(Token),
    Delimited(char, char, Vec<TokenTree>),
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_pattern_parsing() {
        let expander = MacroExpander::new();
        
        // Test simple pattern
        let pattern = expander.parse_pattern("$x + $y").unwrap();
        assert_eq!(pattern.len(), 3);
        
        // Test metavariable with repetition
        let pattern = expander.parse_pattern("$(args)*").unwrap();
        assert_eq!(pattern.len(), 1);
        
        // Test token tree
        let pattern = expander.parse_pattern("fn $name($args)").unwrap();
        assert!(pattern.len() > 0);
    }
    
    #[test]
    fn test_expansion_parsing() {
        let expander = MacroExpander::new();
        
        // Test simple substitution
        let expansion = expander.parse_expansion("$x + $y").unwrap();
        assert_eq!(expansion.len(), 3);
        
        // Test conditional
        let expansion = expander.parse_expansion("#(cond) then #(else) otherwise").unwrap();
        assert_eq!(expansion.len(), 3);
    }
    
    #[test]
    fn test_macro_expansion() {
        let mut expander = MacroExpander::new();
        
        // Add a simple macro
        let pattern = expander.parse_pattern("add $x $y").unwrap();
        let expansion = expander.parse_expansion("$x + $y").unwrap();
        
        expander.add_pattern(MacroPattern {
            name: "add".to_string(),
            pattern,
            expansion,
            hygiene: HygieneLevel::Local,
        });
        
        // Create input tokens
        let input = vec![
            TokenTree::Token(Token::Ident("add".to_string())),
            TokenTree::Token(Token::Ident("5".to_string())),
            TokenTree::Token(Token::Ident("3".to_string())),
        ];
        
        // Expand
        let result = expander.expand(&input, HygieneLevel::Local).unwrap();
        assert_eq!(result.len(), 3); // "5 + 3"
    }
}