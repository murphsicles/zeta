//! Macro expander for expanding macro calls in AST.

use crate::frontend::ast::AstNode;
use super::registry::MacroRegistry;
use super::hygiene::HygieneContext;

/// Macro expander that processes AST nodes and expands macro calls
#[derive(Debug)]
pub struct MacroExpander {
    /// Macro registry
    registry: MacroRegistry,
    /// Hygiene context
    hygiene: HygieneContext,
    /// Whether to expand recursively
    recursive: bool,
}

impl MacroExpander {
    /// Create a new macro expander with empty registry
    pub fn new() -> Self {
        Self {
            registry: MacroRegistry::new(),
            hygiene: HygieneContext::new(),
            recursive: true,
        }
    }

    /// Create a macro expander with existing registry
    pub fn with_registry(registry: MacroRegistry) -> Self {
        Self {
            registry,
            hygiene: HygieneContext::new(),
            recursive: true,
        }
    }

    /// Set whether to expand recursively
    pub fn set_recursive(&mut self, recursive: bool) {
        self.recursive = recursive;
    }

    /// Get mutable reference to macro registry
    pub fn registry_mut(&mut self) -> &mut MacroRegistry {
        &mut self.registry
    }

    /// Expand macro calls in an AST node
    pub fn expand(&mut self, node: &AstNode) -> Result<AstNode, String> {
        match node {
            AstNode::MacroCall { name, args } => {
                self.expand_macro_call(name, args)
            }
            // Recursively expand child nodes
            AstNode::Block { stmts } => {
                let expanded_stmts: Result<Vec<_>, _> = stmts
                    .iter()
                    .map(|stmt| self.expand(stmt))
                    .collect();
                Ok(AstNode::Block { stmts: expanded_stmts? })
            }
            AstNode::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let expanded_cond = self.expand(cond)?;
                let expanded_then = self.expand(then_branch)?;
                let expanded_else = else_branch.as_ref().map(|e| self.expand(e)).transpose()?;
                Ok(AstNode::If {
                    cond: Box::new(expanded_cond),
                    then_branch: Box::new(expanded_then),
                    else_branch: expanded_else.map(Box::new),
                })
            }
            // Add more cases as needed for other AST nodes
            _ => Ok(node.clone()),
        }
    }

    /// Expand a macro call
    fn expand_macro_call(&mut self, name: &str, args: &[AstNode]) -> Result<AstNode, String> {
        // Look up macro definition
        let defs = self.registry.lookup(name)
            .ok_or_else(|| format!("Macro '{}' not found", name))?;

        // Try each definition until one matches
        for def in defs {
            if let Ok(expanded) = self.try_expand(def, args) {
                return Ok(expanded);
            }
        }

        Err(format!("No matching pattern for macro '{}'", name))
    }

    /// Try to expand a macro definition with given arguments
    fn try_expand(&mut self, def: &super::registry::MacroDef, args: &[AstNode]) -> Result<AstNode, String> {
        // Basic pattern matching for now - just check if number of args matches
        // In a full implementation, we would parse the pattern and match against args
        
        // For now, we'll do a simple substitution if the template contains placeholders
        self.substitute_template(&def.template, args)
    }
    
    /// Substitute arguments into template (simplified implementation)
    fn substitute_template(&self, template: &AstNode, args: &[AstNode]) -> Result<AstNode, String> {
        match template {
            AstNode::Ident(name) if name.starts_with('$') => {
                // Simple placeholder substitution: $0, $1, etc.
                let index_str = &name[1..];
                if let Ok(index) = index_str.parse::<usize>() {
                    if index < args.len() {
                        Ok(args[index].clone())
                    } else {
                        Err(format!("Argument index {} out of bounds (max {})", index, args.len() - 1))
                    }
                } else {
                    // Not a numeric placeholder, keep as-is
                    Ok(template.clone())
                }
            }
            // Recursively substitute in child nodes
            AstNode::Block { stmts } => {
                let substituted_stmts: Result<Vec<_>, _> = stmts
                    .iter()
                    .map(|stmt| self.substitute_template(stmt, args))
                    .collect();
                Ok(AstNode::Block { stmts: substituted_stmts? })
            }
            AstNode::If { cond, then_branch, else_branch } => {
                let substituted_cond = self.substitute_template(cond, args)?;
                let substituted_then = self.substitute_template(then_branch, args)?;
                let substituted_else = else_branch.as_ref()
                    .map(|e| self.substitute_template(e, args))
                    .transpose()?;
                Ok(AstNode::If {
                    cond: Box::new(substituted_cond),
                    then_branch: Box::new(substituted_then),
                    else_branch: substituted_else.map(Box::new),
                })
            }
            // Add more cases as needed
            _ => Ok(template.clone()),
        }
    }

    /// Expand all macros in a program (list of AST nodes)
    pub fn expand_program(&mut self, program: &[AstNode]) -> Result<Vec<AstNode>, String> {
        let mut result = Vec::new();
        
        for node in program {
            let expanded = self.expand(node)?;
            result.push(expanded);
        }

        Ok(result)
    }
}