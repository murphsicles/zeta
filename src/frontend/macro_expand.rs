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
            "format" => self.expand_format(args),
            "assert_eq" => self.expand_assert_eq(args),
            "assert" => {
                // assert!(condition) is similar to assert_eq! but with one argument
                if args.len() != 1 {
                    return Err("assert! requires exactly 1 argument".to_string());
                }
                self.expand_assert_eq(&[args[0].clone(), AstNode::Bool(true)])
            }
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

        // Check if first argument is a format string
        if let Some(AstNode::StringLit(format_str)) = args.first() {
            // Simple format string handling
            // Count placeholders
            let placeholder_count = format_str.matches("{}").count();
            
            if args.len() - 1 != placeholder_count {
                return Err(format!(
                    "println! format string expects {} arguments, got {}",
                    placeholder_count,
                    args.len() - 1
                ));
            }
        }

        // For now, simple expansion to a function call
        // In a full implementation, this would handle format strings
        // Strip the format string and pass only the value arguments
        let value_args: Vec<AstNode> = if let Some(AstNode::StringLit(_)) = args.first() {
            args[1..].to_vec()
        } else {
            args.to_vec()
        };

        let call = if value_args.len() == 1 && matches!(&value_args[0], AstNode::Var(_) | AstNode::Lit(_) | AstNode::Call { .. }) {
            // Simple single-value println: generate void call directly to println_i64
            AstNode::Call {
                receiver: None,
                method: "println_i64".to_string(),
                args: value_args,
                type_args: Vec::new(),
                structural: false,
            }
        } else {
            // Fallback: use generic println (codegen will handle it)
            AstNode::Call {
                receiver: None,
                method: "println".to_string(),
                args: value_args,
                type_args: Vec::new(),
                structural: false,
            }
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
    
    /// Expand format! macro
    fn expand_format(&self, args: &[AstNode]) -> Result<Vec<AstNode>, String> {
        if args.is_empty() {
            return Err("format! requires at least a format string".to_string());
        }
        
        // Simple expansion: format!("Hello {}", name) -> String concatenation
        // For now, just create a string literal with placeholder
        let result = AstNode::StringLit("formatted string".to_string());
        Ok(vec![result])
    }
    
    /// Expand assert_eq! macro
    fn expand_assert_eq(&self, args: &[AstNode]) -> Result<Vec<AstNode>, String> {
        if args.len() != 2 {
            return Err("assert_eq! requires exactly 2 arguments".to_string());
        }
        
        // Create an if statement that panics if the values are not equal
        let condition = AstNode::BinaryOp {
            op: "!=".to_string(),
            left: Box::new(args[0].clone()),
            right: Box::new(args[1].clone()),
        };
        
        let panic_call = AstNode::Call {
            receiver: None,
            method: "panic".to_string(),
            args: vec![AstNode::StringLit("Assertion failed".to_string())],
            type_args: Vec::new(),
            structural: false,
        };
        
        let if_stmt = AstNode::If {
            cond: Box::new(condition),
            then: vec![AstNode::ExprStmt {
                expr: Box::new(panic_call),
            }],
            else_: Vec::new(),
        };
        
        Ok(vec![if_stmt])
    }

    /// Expand a declarative macro
    fn expand_declarative_macro(
        &self,
        macro_def: &DeclarativeMacro,
        args: &[AstNode],
    ) -> Result<Vec<AstNode>, String> {
        // Try to match each pattern
        for pattern in &macro_def.patterns {
            if let Some(bindings) = self.match_pattern(&pattern.matcher, args) {
                return self.expand_with_bindings(&pattern.expansion, &bindings);
            }
        }
        
        Err(format!(
            "No matching pattern found for macro: {} with {} arguments",
            macro_def.name,
            args.len()
        ))
    }
    
    /// Match macro pattern against arguments
    fn match_pattern(
        &self,
        pattern: &[MacroToken],
        args: &[AstNode],
    ) -> Option<HashMap<String, Vec<AstNode>>> {
        let mut bindings = HashMap::new();
        let mut pattern_idx = 0;
        let mut args_idx = 0;
        
        while pattern_idx < pattern.len() && args_idx < args.len() {
            match &pattern[pattern_idx] {
                MacroToken::Ident(name) => {
                    // Check if it's a pattern variable like $x:expr
                    if name.starts_with('$') {
                        let var_name = &name[1..]; // Remove $
                        
                        // Check for fragment specifier
                        let specifier = if let Some(colon_idx) = var_name.find(':') {
                            let spec = &var_name[colon_idx + 1..];
                            &var_name[..colon_idx]
                        } else {
                            var_name
                        };
                        
                        // For now, bind the next argument
                        if args_idx < args.len() {
                            bindings.insert(specifier.to_string(), vec![args[args_idx].clone()]);
                            args_idx += 1;
                            pattern_idx += 1;
                        } else {
                            return None;
                        }
                    } else {
                        // Literal identifier - must match exactly
                        // For now, skip as we don't have identifier AST nodes
                        pattern_idx += 1;
                    }
                }
                MacroToken::Punct(',') => {
                    // Skip commas in pattern
                    pattern_idx += 1;
                }
                MacroToken::Repetition(rep_pattern, separator, min, max) => {
                    // Handle repetition
                    let mut matched_args = Vec::new();
                    
                    while args_idx < args.len() {
                        // Try to match one instance of the repetition pattern
                        // For now, just take one argument
                        matched_args.push(args[args_idx].clone());
                        args_idx += 1;
                        
                        // Check for separator
                        if args_idx < args.len() {
                            // Skip separator in args
                            args_idx += 1;
                        }
                    }
                    
                    // Check repetition bounds
                    let count = matched_args.len();
                    if let Some(min_count) = min {
                        if count < *min_count {
                            return None;
                        }
                    }
                    if let Some(max_count) = max {
                        if count > *max_count {
                            return None;
                        }
                    }
                    
                    // Store the matched arguments
                    // For now, use a placeholder name
                    bindings.insert("repetition".to_string(), matched_args);
                    pattern_idx += 1;
                }
                _ => {
                    // Skip other tokens for now
                    pattern_idx += 1;
                }
            }
        }
        
        // Check if we consumed all pattern tokens and arguments
        if pattern_idx == pattern.len() && args_idx == args.len() {
            Some(bindings)
        } else {
            None
        }
    }
    
    /// Expand macro expansion with bindings
    fn expand_with_bindings(
        &self,
        expansion: &[MacroToken],
        bindings: &HashMap<String, Vec<AstNode>>,
    ) -> Result<Vec<AstNode>, String> {
        let mut result = Vec::new();
        
        for token in expansion {
            match token {
                MacroToken::Ident(name) => {
                    if name.starts_with('$') {
                        let var_name = &name[1..];
                        if let Some(bound_args) = bindings.get(var_name) {
                            result.extend(bound_args.clone());
                        } else {
                            return Err(format!("Unbound variable in macro expansion: ${}", var_name));
                        }
                    } else {
                        // TODO: Handle literal identifiers in expansion
                        // For now, create a variable reference
                        result.push(AstNode::Var(name.clone()));
                    }
                }
                MacroToken::Literal(lit) => {
                    // TODO: Parse literal value
                    // For now, create a string literal
                    result.push(AstNode::StringLit(lit.clone()));
                }
                MacroToken::Group(tokens, _) => {
                    // Recursively expand group
                    let group_result = self.expand_with_bindings(tokens, bindings)?;
                    result.extend(group_result);
                }
                MacroToken::Repetition(rep_pattern, separator, _, _) => {
                    // Handle repetition in expansion
                    // For now, just expand the pattern once
                    let rep_result = self.expand_with_bindings(rep_pattern, bindings)?;
                    result.extend(rep_result);
                }
                MacroToken::Punct(_) => {
                    // Skip punctuation in expansion for now
                }
            }
        }
        
        Ok(result)
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
            // Safe because we just peeked
            if let Some(ch) = chars.next() {
                name.push(ch);
            } else {
                break;
            }
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
            // Safe because we just peeked
            if let Some(ch) = chars.next() {
                current_ident.push(ch);
            }
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
                                // Safe because we just peeked
                                if let Some(ch) = chars.next() {
                                    sep.push(ch);
                                }
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
                    if let Some(ch) = chars.next() {
                        tokens.push(MacroToken::Punct(ch));
                    }
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
        } else if attr == "must_use" {
            // Handle #[must_use] attribute
            // No expansion needed, just metadata for the compiler
        } else if attr.starts_with("cfg(") {
            // Handle #[cfg(...)] conditional compilation
            // For now, just pass through - would be handled by conditional compilation system
        } else if attr.starts_with("allow(") {
            // Handle #[allow(...)] to suppress warnings
            // No expansion needed
        } else if attr.starts_with("deny(") {
            // Handle #[deny(...)] to enforce warnings as errors
            // No expansion needed
        } else if attr.starts_with("warn(") {
            // Handle #[warn(...)] to configure warnings
            // No expansion needed
        } else if attr.starts_with("repr(") {
            // Handle #[repr(...)] for type representation
            // No expansion needed, affects code generation
        } else {
            // Unknown attribute - could be a custom attribute macro
            // For now, just warn and continue
            eprintln!("Warning: Unknown attribute: {}", attr);
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
                lifetimes: Vec::new(),
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
                comptime_: false,
                where_clauses: Vec::new(),
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
