// macro_system.rs - Declarative macro system for Zeta v0.3.5
// File: C:\Users\mummy\OneDrive\Documents\DarkFactory\zeta-0.3.4\src\macro_system.rs
// Purpose: Macro-by-example syntax, pattern matching, expansion, hygiene

use std::collections::{HashMap, HashSet};
use std::fmt;

/// Macro pattern matcher token
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PatternToken {
    /// Literal token that must match exactly
    Literal(String),
    /// Metavariable: `$ident` or `$ident:ty`
    Metavariable {
        name: String,
        fragment_specifier: Option<FragmentSpecifier>,
    },
    /// Repetition: `$(pattern)*`, `$(pattern)+`, `$(pattern)?`
    Repetition {
        pattern: Vec<PatternToken>,
        separator: Option<String>,
        op: RepetitionOp,
    },
}

/// Fragment specifier for metavariables
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FragmentSpecifier {
    /// `ident` - identifier
    Ident,
    /// `expr` - expression
    Expr,
    /// `ty` - type
    Ty,
    /// `pat` - pattern
    Pat,
    /// `stmt` - statement
    Stmt,
    /// `block` - block of code
    Block,
    /// `item` - item (function, struct, etc.)
    Item,
    /// `meta` - meta item (attribute)
    Meta,
    /// `tt` - token tree
    Tt,
}

/// Repetition operator
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RepetitionOp {
    /// Zero or more: `*`
    ZeroOrMore,
    /// One or more: `+`
    OneOrMore,
    /// Zero or one: `?`
    ZeroOrOne,
}

/// Macro definition
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MacroDef {
    pub name: String,
    pub patterns: Vec<Vec<PatternToken>>,
    pub expansions: Vec<Vec<ExpansionToken>>,
    pub hygiene: HygieneLevel,
    pub export: bool,
}

/// Expansion token (output of macro)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExpansionToken {
    /// Literal text
    Literal(String),
    /// Metavariable substitution
    Substitution(String),
    /// Repetition expansion
    Repetition {
        metavar: String,
        expansion: Vec<ExpansionToken>,
        separator: Option<String>,
    },
}

/// Hygiene level for macro expansion
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HygieneLevel {
    /// No hygiene - identifiers are used as-is
    None,
    /// Local hygiene - identifiers are hygienic within macro
    Local,
    /// Global hygiene - identifiers are hygienic across expansions
    Global,
}

/// Macro expansion context
pub struct ExpansionContext {
    pub macros: HashMap<String, MacroDef>,
    pub hygiene_counter: u64,
    pub source_map: HashMap<usize, (String, usize)>, // position → (file, line)
}

impl ExpansionContext {
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
            hygiene_counter: 0,
            source_map: HashMap::new(),
        }
    }
    
    /// Register a macro definition
    pub fn register_macro(&mut self, def: MacroDef) -> Result<(), String> {
        if self.macros.contains_key(&def.name) {
            return Err(format!("Macro '{}' already defined", def.name));
        }
        
        // Validate pattern/expansion count match
        if def.patterns.len() != def.expansions.len() {
            return Err(format!(
                "Macro '{}' has {} patterns but {} expansions",
                def.name, def.patterns.len(), def.expansions.len()
            ));
        }
        
        self.macros.insert(def.name.clone(), def);
        Ok(())
    }
    
    /// Expand a macro invocation
    pub fn expand_macro(&mut self, name: &str, args: &[TokenTree]) -> Result<Vec<TokenTree>, String> {
        let def = self.macros.get(name)
            .ok_or_else(|| format!("Macro '{}' not found", name))?;
        
        // Try each pattern
        for (pattern_idx, pattern) in def.patterns.iter().enumerate() {
            if let Some(bindings) = self.match_pattern(pattern, args) {
                let expansion = &def.expansions[pattern_idx];
                return self.expand_tokens(expansion, &bindings, &def.hygiene);
            }
        }
        
        Err(format!("No pattern matched for macro '{}'", name))
    }
    
    /// Match pattern against input tokens
    fn match_pattern(&self, pattern: &[PatternToken], input: &[TokenTree]) -> Option<HashMap<String, Vec<TokenTree>>> {
        let mut bindings = HashMap::new();
        let mut pattern_idx = 0;
        let mut input_idx = 0;
        
        while pattern_idx < pattern.len() && input_idx < input.len() {
            match &pattern[pattern_idx] {
                PatternToken::Literal(lit) => {
                    if let TokenTree::Token(Token::Ident(ident)) = &input[input_idx] {
                        if ident == lit {
                            pattern_idx += 1;
                            input_idx += 1;
                            continue;
                        }
                    }
                    return None;
                }
                PatternToken::Metavariable { name, fragment_specifier } => {
                    // For now, accept any single token
                    bindings.insert(name.clone(), vec![input[input_idx].clone()]);
                    pattern_idx += 1;
                    input_idx += 1;
                }
                PatternToken::Repetition { pattern: rep_pattern, separator, op } => {
                    // Match repetition
                    let mut matched = Vec::new();
                    
                    loop {
                        // Try to match one instance
                        if let Some(sub_bindings) = self.match_pattern(rep_pattern, &input[input_idx..]) {
                            // TODO: Handle sub-bindings properly
                            matched.push(input[input_idx].clone());
                            input_idx += 1;
                            
                            // Check for separator
                            if let Some(sep) = separator {
                                if input_idx < input.len() {
                                    if let TokenTree::Token(Token::Ident(ident)) = &input[input_idx] {
                                        if ident == sep {
                                            input_idx += 1;
                                            continue;
                                        }
                                    }
                                }
                            }
                        } else {
                            break;
                        }
                    }
                    
                    // Check repetition operator constraints
                    match op {
                        RepetitionOp::ZeroOrMore => {
                            // Always valid
                        }
                        RepetitionOp::OneOrMore => {
                            if matched.is_empty() {
                                return None;
                            }
                        }
                        RepetitionOp::ZeroOrOne => {
                            if matched.len() > 1 {
                                return None;
                            }
                        }
                    }
                    
                    // Store repetition binding
                    bindings.insert(format!("${}", pattern_idx), matched);
                    pattern_idx += 1;
                }
            }
        }
        
        // Check if we consumed all input
        if pattern_idx == pattern.len() && input_idx == input.len() {
            Some(bindings)
        } else {
            None
        }
    }
    
    /// Expand tokens using bindings
    fn expand_tokens(&mut self, tokens: &[ExpansionToken], bindings: &HashMap<String, Vec<TokenTree>>, hygiene: &HygieneLevel) -> Result<Vec<TokenTree>, String> {
        let mut result = Vec::new();
        
        for token in tokens {
            match token {
                ExpansionToken::Literal(lit) => {
                    result.push(TokenTree::Token(Token::Ident(lit.clone())));
                }
                ExpansionToken::Substitution(name) => {
                    if let Some(bound_tokens) = bindings.get(name) {
                        result.extend(bound_tokens.clone());
                    } else {
                        return Err(format!("Metavariable '{}' not bound", name));
                    }
                }
                ExpansionToken::Repetition { metavar, expansion, separator } => {
                    if let Some(bound_tokens) = bindings.get(metavar) {
                        for (i, token) in bound_tokens.iter().enumerate() {
                            // Expand each token in the repetition
                            // For now, just push the token
                            result.push(token.clone());
                            
                            // Add separator if not last
                            if i < bound_tokens.len() - 1 {
                                if let Some(sep) = separator {
                                    result.push(TokenTree::Token(Token::Ident(sep.clone())));
                                }
                            }
                        }
                    } else {
                        return Err(format!("Repetition metavariable '{}' not bound", metavar));
                    }
                }
            }
        }
        
        Ok(result)
    }
    
    /// Generate a hygienic identifier
    pub fn hygienic_ident(&mut self, base: &str) -> String {
        self.hygiene_counter += 1;
        format!("{}__{}", base, self.hygiene_counter)
    }
}

/// Token tree for macro expansion
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenTree {
    Token(Token),
    Delimited(Delimiter, Vec<TokenTree>),
}

/// Token types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Ident(String),
    Literal(String),
    Punct(char),
}

/// Delimiter types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Delimiter {
    Parenthesis,  // ( )
    Brace,        // { }
    Bracket,      // [ ]
}

/// Macro parser for `macro_rules!` style syntax
pub struct MacroParser;

impl MacroParser {
    /// Parse a macro definition from source
    pub fn parse_macro_def(&self, source: &str) -> Result<MacroDef, String> {
        // Simplified parsing for now
        let lines: Vec<&str> = source.lines().collect();
        if lines.is_empty() {
            return Err("Empty macro definition".to_string());
        }
        
        // Parse first line: `macro_rules! name { ... }`
        let first_line = lines[0].trim();
        if !first_line.starts_with("macro_rules!") {
            return Err("Expected 'macro_rules!'".to_string());
        }
        
        let parts: Vec<&str> = first_line.split_whitespace().collect();
        if parts.len() < 2 {
            return Err("Expected macro name after 'macro_rules!'".to_string());
        }
        
        let name = parts[1].trim_end_matches('{').to_string();
        
        // Parse patterns and expansions
        let mut patterns = Vec::new();
        let mut expansions = Vec::new();
        
        // Simplified: assume pattern → expansion separated by `=>`
        let content = &lines[1..lines.len()-1].join("\n");
        let rules: Vec<&str> = content.split("=>").collect();
        
        for rule in rules {
            let parts: Vec<&str> = rule.splitn(2, "=>").collect();
            if parts.len() == 2 {
                let pattern_str = parts[0].trim();
                let expansion_str = parts[1].trim().trim_end_matches(';');
                
                let pattern = self.parse_pattern(pattern_str)?;
                let expansion = self.parse_expansion(expansion_str)?;
                
                patterns.push(pattern);
                expansions.push(expansion);
            }
        }
        
        Ok(MacroDef {
            name,
            patterns,
            expansions,
            hygiene: HygieneLevel::Local,
            export: true,
        })
    }
    
    /// Parse pattern tokens
    fn parse_pattern(&self, input: &str) -> Result<Vec<PatternToken>, String> {
        let mut tokens = Vec::new();
        let mut chars = input.chars().peekable();
        
        while let Some(c) = chars.next() {
            match c {
                '$' => {
                    // Metavariable
                    let mut name = String::new();
                    while let Some(&nc) = chars.peek() {
                        if nc.is_alphanumeric() || nc == '_' {
                            name.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    
                    let fragment_specifier = if let Some(':') = chars.peek() {
                        chars.next(); // Skip ':'
                        let mut spec = String::new();
                        while let Some(&nc) = chars.peek() {
                            if nc.is_alphabetic() {
                                spec.push(chars.next().unwrap());
                            } else {
                                break;
                            }
                        }
                        
                        match spec.as_str() {
                            "ident" => Some(FragmentSpecifier::Ident),
                            "expr" => Some(FragmentSpecifier::Expr),
                            "ty" => Some(FragmentSpecifier::Ty),
                            "pat" => Some(FragmentSpecifier::Pat),
                            "stmt" => Some(FragmentSpecifier::Stmt),
                            "block" => Some(FragmentSpecifier::Block),
                            "item" => Some(FragmentSpecifier::Item),
                            "meta" => Some(FragmentSpecifier::Meta),
                            "tt" => Some(FragmentSpecifier::Tt),
                            _ => return Err(format!("Unknown fragment specifier: {}", spec)),
                        }
                    } else {
                        None
                    };
                    
                    tokens.push(PatternToken::Metavariable {
                        name,
                        fragment_specifier,
                    });
                }
                '(' if chars.peek() == Some(&'$') => {
                    // Repetition pattern: $(...)*
                    chars.next(); // Skip '$'
                    
                    let mut pattern_str = String::new();
                    let mut depth = 1;
                    
                    while let Some(c) = chars.next() {
                        match c {
                            '(' => depth += 1,
                            ')' => {
                                depth -= 1;
                                if depth == 0 {
                                    break;
                                }
                            }
                            _ => {}
                        }
                        pattern_str.push(c);
                    }
                    
                    // Parse repetition operator
                    let op = match chars.next() {
                        Some('*') => RepetitionOp::ZeroOrMore,
                        Some('+') => RepetitionOp::OneOrMore,
                        Some('?') => RepetitionOp::ZeroOrOne,
                        Some(c) => return Err(format!("Invalid repetition operator: {}", c)),
                        None => return Err("Expected repetition operator".to_string()),
                    };
                    
                    // Parse separator if present
                    let separator = if let Some(' ') = chars.peek() {
                        chars.next();
                        let mut sep = String::new();
                        while let Some(&c) = chars.peek() {
                            if c.is_alphanumeric() {
                                sep.push(chars.next().unwrap());
                            } else {
                                break;
                            }
                        }
                        Some(sep)
                    } else {
                        None
                    };
                    
                    let pattern = self.parse_pattern(&pattern_str)?;
                    tokens.push(PatternToken::Repetition {
                        pattern,
                        separator,
                        op,
                    });
                }
                c if c.is_alphanumeric() || c == '_' => {
                    // Identifier
                    let mut ident = String::from(c);
                    while let Some(&nc) = chars.peek() {
                        if nc.is_alphanumeric() || nc == '_' {
                            ident.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    tokens.push(PatternToken::Literal(ident));
                }
                ' ' | '\t' | '\n' => {
                    // Skip whitespace
                    continue;
                }
                _ => {
                    // Other punctuation
                    tokens.push(PatternToken::Literal(c.to_string()));
                }
            }
        }
        
        Ok(tokens)
    }
    
    /// Parse expansion tokens
    fn parse_expansion(&self, input: &str) -> Result<Vec<ExpansionToken>, String> {
        let mut tokens = Vec::new();
        let mut chars = input.chars().peekable();
        
        while let Some(c) = chars.next() {
            match c {
                '$' => {
                    // Metavariable substitution or repetition
                    if let Some('(') = chars.peek() {
                        // Repetition: $(...)*
                        chars.next(); // Skip '('
                        
                        let mut metavar = String::new();
                        while let Some(&c) = chars.peek() {
                            if c.is_alphanumeric() || c == '_' {
                                metavar.push(chars.next().unwrap());
                            } else {
                                break;
                            }
                        }
                        
                        if metavar.is_empty() {
                            return Err("Expected metavariable name in repetition".to_string());
                        }
                        
                        // Skip ')'
                        if chars.next() != Some(')') {
                            return Err("Expected ')' after metavariable".to_string());
                        }
                        
                        // Parse repetition operator
                        let op_char = chars.next().ok_or("Expected repetition operator".to_string())?;
                        
                        // Parse expansion inside repetition
                        let mut expansion_str = String::new();
                        while let Some(&c) = chars.peek() {
                            if c == '$' {
                                break;
                            }
                            expansion_str.push(chars.next().unwrap());
                        }
                        
                        let expansion = self.parse_expansion(&expansion_str)?;
                        
                        // Parse separator if present
                        let separator = if let Some(' ') = chars.peek() {
                            chars.next();
                            let mut sep = String::new();
                            while let Some(&c) = chars.peek() {
                                if c.is_alphanumeric() {
                                    sep.push(chars.next().unwrap());
                                } else {
                                    break;
                                }
                            }
                            Some(sep)
                        } else {
                            None
                        };
                        
                        tokens.push(ExpansionToken::Repetition {
                            metavar,
                            expansion,
                            separator,
                        });
                    } else {
                        // Simple substitution
                        let mut name = String::new();
                        while let Some(&c) = chars.peek() {
                            if c.is_alphanumeric() || c == '_' {
                                name.push(chars.next().unwrap());
                            } else {
                                break;
                            }
                        }
                        
                        if name.is_empty() {
                            return Err("Expected metavariable name