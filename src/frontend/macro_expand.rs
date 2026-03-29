// src/frontend/macro_expand.rs
//! Macro expansion infrastructure for Zeta v0.5.0
//!
//! This module handles:
//! 1. Declarative macros (macro_rules!)
//! 2. Procedural macros (derive, attribute, function)
//! 3. Macro expansion during compilation

use crate::frontend::ast::AstNode;
use std::collections::HashMap;

/// Represents a declarative macro defined with macro_rules!
#[derive(Debug, Clone)]
pub struct DeclarativeMacro {
    pub name: String,
    pub patterns: Vec<MacroPattern>,
}

/// A pattern in a declarative macro
#[derive(Debug, Clone)]
pub struct MacroPattern {
    pub matcher: Vec<MacroToken>,
    pub expansion: Vec<MacroToken>,
}

/// Tokens in macro patterns and expansions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MacroToken {
    Ident(String),
    Literal(String),
    Punct(char),
    Group(Vec<MacroToken>, char), // tokens and delimiter
    Repetition(Vec<MacroToken>, char, Option<usize>, Option<usize>), // pattern, separator, min, max
}

/// Macro expander that processes macro calls
pub struct MacroExpander {
    declarative_macros: HashMap<String, DeclarativeMacro>,
}

impl Default for MacroExpander {
    fn default() -> Self {
        Self::new()
    }
}

impl MacroExpander {
    pub fn new() -> Self {
        Self {
            declarative_macros: HashMap::new(),
        }
    }

    /// Register a declarative macro
    pub fn register_declarative_macro(&mut self, name: String, macro_def: DeclarativeMacro) {
        self.declarative_macros.insert(name, macro_def);
    }

    /// Expand a macro call
    pub fn expand_macro_call(&self, name: &str, args: &[AstNode]) -> Result<Vec<AstNode>, String> {
        // Check for built-in macros first
        match name {
            "println" => self.expand_println(args),
            "vec" => self.expand_vec(args),
            _ => {
                // Check for registered declarative macros
                if let Some(macro_def) = self.declarative_macros.get(name) {
                    self.expand_declarative_macro(macro_def, args)
                } else {
                    Err(format!("Unknown macro: {}", name))
                }
            }
        }
    }

    /// Expand println! macro
    fn expand_println(&self, args: &[AstNode]) -> Result<Vec<AstNode>, String> {
        if args.is_empty() {
            return Err("println! requires at least one argument".to_string());
        }

        // For now, simple expansion to a function call
        // In a full implementation, this would handle format strings
        let call = AstNode::Call {
            receiver: None,
            method: "println".to_string(),
            args: args.to_vec(),
            type_args: Vec::new(),
            structural: false,
        };

        Ok(vec![AstNode::ExprStmt {
            expr: Box::new(call),
        }])
    }

    /// Expand vec! macro
    fn expand_vec(&self, args: &[AstNode]) -> Result<Vec<AstNode>, String> {
        // vec![1, 2, 3] expands to creating a vector
        // For now, expand to an array literal
        // In a full implementation, this would create a Vec<T>
        Ok(vec![AstNode::ArrayLit(args.to_vec())])
    }

    /// Expand a declarative macro
    fn expand_declarative_macro(
        &self,
        macro_def: &DeclarativeMacro,
        args: &[AstNode],
    ) -> Result<Vec<AstNode>, String> {
        // TODO: Implement pattern matching and expansion
        // For now, return a placeholder
        Err(format!(
            "Declarative macro expansion not yet implemented for: {}",
            macro_def.name
        ))
    }
}

/// Parse macro_rules! definition
pub fn parse_macro_rules(input: &str) -> Result<DeclarativeMacro, String> {
    // Simple parser for basic macro_rules! syntax
    // Format: macro_rules! name { (pattern) => { expansion }; ... }

    let mut chars = input.chars().peekable();

    // Skip whitespace
    while chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
        chars.next();
    }

    // Parse "macro_rules!"
    let keyword: String = chars.by_ref().take(11).collect();
    if keyword != "macro_rules!" {
        return Err("Expected 'macro_rules!'".to_string());
    }

    // Skip whitespace
    while chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
        chars.next();
    }

    // Parse macro name
    let mut name = String::new();
    while let Some(&c) = chars.peek() {
        if c.is_alphanumeric() || c == '_' {
            name.push(chars.next().unwrap());
        } else {
            break;
        }
    }

    if name.is_empty() {
        return Err("Expected macro name".to_string());
    }

    // Skip whitespace
    while chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
        chars.next();
    }

    // Expect '{'
    if chars.next() != Some('{') {
        return Err("Expected '{' after macro name".to_string());
    }

    let mut patterns = Vec::new();

    // Parse patterns
    loop {
        // Skip whitespace
        while chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
            chars.next();
        }

        // Check for '}' to end macro definition
        if chars.peek() == Some(&'}') {
            chars.next();
            break;
        }

        // Expect '(' for pattern
        if chars.next() != Some('(') {
            return Err("Expected '(' for pattern".to_string());
        }

        // Parse pattern tokens
        let mut pattern_tokens = Vec::new();
        parse_macro_tokens(&mut chars, &mut pattern_tokens, ')')?;

        // Skip whitespace
        while chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
            chars.next();
        }

        // Expect "=>"
        let arrow: String = chars.by_ref().take(2).collect();
        if arrow != "=>" {
            return Err("Expected '=>' after pattern".to_string());
        }

        // Skip whitespace
        while chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
            chars.next();
        }

        // Expect '{' for expansion
        if chars.next() != Some('{') {
            return Err("Expected '{' for expansion".to_string());
        }

        // Parse expansion tokens
        let mut expansion_tokens = Vec::new();
        parse_macro_tokens(&mut chars, &mut expansion_tokens, '}')?;

        // Skip whitespace
        while chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
            chars.next();
        }

        // Expect ';' between rules (optional at end)
        if chars.peek() == Some(&';') {
            chars.next();
        }

        patterns.push(MacroPattern {
            matcher: pattern_tokens,
            expansion: expansion_tokens,
        });
    }

    Ok(DeclarativeMacro { name, patterns })
}

/// Parse macro tokens until closing delimiter
fn parse_macro_tokens<I: Iterator<Item = char>>(
    chars: &mut std::iter::Peekable<I>,
    tokens: &mut Vec<MacroToken>,
    close: char,
) -> Result<(), String> {
    let mut current_ident = String::new();

    while let Some(&c) = chars.peek() {
        if c == close {
            chars.next();
            break;
        }

        if c.is_alphanumeric() || c == '_' {
            current_ident.push(chars.next().unwrap());
        } else {
            // Flush identifier if we have one
            if !current_ident.is_empty() {
                tokens.push(MacroToken::Ident(current_ident));
                current_ident = String::new();
            }

            match c {
                '(' | '[' | '{' => {
                    chars.next();
                    let delimiter = c;
                    let close_delim = match delimiter {
                        '(' => ')',
                        '[' => ']',
                        '{' => '}',
                        _ => unreachable!(),
                    };

                    let mut group_tokens = Vec::new();
                    parse_macro_tokens(chars, &mut group_tokens, close_delim)?;
                    tokens.push(MacroToken::Group(group_tokens, delimiter));
                }
                ')' | ']' | '}' => {
                    // Should be handled by caller
                    break;
                }
                '$' => {
                    chars.next(); // Skip '$'
                    // Parse repetition: $(pattern) separator* or +
                    if chars.next() != Some('(') {
                        return Err("Expected '(' after '$'".to_string());
                    }

                    let mut pattern_tokens = Vec::new();
                    parse_macro_tokens(chars, &mut pattern_tokens, ')')?;

                    // Parse separator and quantifier
                    let mut separator = None;
                    let mut min = Some(0);
                    let mut max = None;

                    if let Some(&next) = chars.peek()
                        && (next.is_alphanumeric() || next == '_')
                    {
                        // It's a separator identifier
                        let mut sep = String::new();
                        while let Some(&c) = chars.peek() {
                            if c.is_alphanumeric() || c == '_' {
                                sep.push(chars.next().unwrap());
                            } else {
                                break;
                            }
                        }
                        separator = Some(sep);
                    }

                    // Parse quantifier
                    if let Some(&quant) = chars.peek() {
                        match quant {
                            '*' => {
                                chars.next();
                                min = Some(0);
                                max = None;
                            }
                            '+' => {
                                chars.next();
                                min = Some(1);
                                max = None;
                            }
                            '?' => {
                                chars.next();
                                min = Some(0);
                                max = Some(1);
                            }
                            _ => {}
                        }
                    }

                    tokens.push(MacroToken::Repetition(
                        pattern_tokens,
                        separator
                            .unwrap_or(','.to_string())
                            .chars()
                            .next()
                            .unwrap_or(','),
                        min,
                        max,
                    ));
                }
                _ => {
                    // Punctuation
                    tokens.push(MacroToken::Punct(chars.next().unwrap()));
                }
            }
        }
    }

    // Flush any remaining identifier
    if !current_ident.is_empty() {
        tokens.push(MacroToken::Ident(current_ident));
    }

    Ok(())
}

/// Process attributes on AST nodes
pub fn process_attributes(attrs: &[String], node: &AstNode) -> Result<Vec<AstNode>, String> {
    let mut expansions = Vec::new();

    for attr in attrs {
        if attr.starts_with("derive(") {
            // Handle derive attribute
            let type_name = match node {
                AstNode::StructDef { name, .. } => name.clone(),
                AstNode::EnumDef { name, .. } => name.clone(),
                _ => return Err("derive attribute can only be used on struct or enum".to_string()),
            };

            // Use existing derive attribute handler
            let impls = crate::middle::types::handle_derive_attribute(attr, &type_name)?;

            // Convert implementation strings to AST nodes
            for impl_str in impls {
                // TODO: Parse impl_str into AST nodes
                // For now, we'll skip as the existing system handles this differently
            }
        } else if attr == "test" {
            // Handle #[test] attribute
            expansions.push(create_test_function(node)?);
        } else if attr == "inline" {
            // Handle #[inline] attribute - no expansion needed, just metadata
            // This would be handled during code generation
        } else if attr.starts_with("test(") {
            // Handle #[test(name = "test_name")] with arguments
            expansions.push(create_test_function_with_args(attr, node)?);
        }
    }

    Ok(expansions)
}

/// Create a test function from a function with #[test] attribute
fn create_test_function(node: &AstNode) -> Result<AstNode, String> {
    match node {
        AstNode::FuncDef {
            name,
            params,
            ret,
            body,
            async_,
            const_,
            ..
        } => {
            // Create a test wrapper function
            let test_name = format!("test_{}", name);
            Ok(AstNode::FuncDef {
                name: test_name,
                generics: Vec::new(),
                params: params.clone(),
                ret: ret.clone(),
                body: body.clone(),
                attrs: Vec::new(),
                ret_expr: None,
                single_line: false,
                doc: format!("Test for {}", name),
                pub_: false,
                async_: *async_,
                const_: *const_,
            })
        }
        _ => Err("#[test] can only be used on functions".to_string()),
    }
}

/// Create a test function with arguments
fn create_test_function_with_args(attr: &str, node: &AstNode) -> Result<AstNode, String> {
    // Parse arguments like #[test(name = "test_name")]
    // For now, just create a basic test function
    create_test_function(node)
}
