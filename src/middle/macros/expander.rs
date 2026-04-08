//! Macro expander for expanding macro calls

use crate::frontend::ast::AstNode;
use super::registry::MacroRegistry;
use super::hygiene::HygieneContext;

/// Expands macro calls in AST
#[derive(Debug)]
pub struct MacroExpander {
    /// Macro registry
    registry: MacroRegistry,
    /// Hygiene context
    hygiene: HygieneContext,
}

impl MacroExpander {
    /// Create a new macro expander
    pub fn new() -> Self {
        Self {
            registry: MacroRegistry::new(),
            hygiene: HygieneContext::new(),
        }
    }
    
    /// Register a macro definition
    pub fn register_macro(&mut self, name: String, params: Vec<String>, body: AstNode, hygienic: bool) {
        self.registry.register(name, params, body, hygienic);
    }
    
    /// Expand macro calls in an AST
    pub fn expand(&mut self, ast: &AstNode) -> AstNode {
        self.expand_node(ast)
    }
    
    /// Expand a single AST node
    fn expand_node(&mut self, node: &AstNode) -> AstNode {
        match node {
            // Handle macro calls
            AstNode::MacroCall { name, args } => {
                self.expand_macro_call(name, args)
            }
            
            // Recursively expand child nodes
            _ => self.expand_children(node),
        }
    }
    
    /// Expand a macro call
    fn expand_macro_call(&mut self, name: &str, args: &[AstNode]) -> AstNode {
        if let Some(macro_def) = self.registry.get(name) {
            // Expand arguments
            let expanded_args: Vec<AstNode> = args.iter()
                .map(|arg| self.expand_node(arg))
                .collect();
            
            // Apply macro expansion
            self.apply_macro_expansion(&macro_def.body, &macro_def.params, &expanded_args)
        } else {
            // Unknown macro - keep as is
            AstNode::MacroCall {
                name: name.to_string(),
                args: args.to_vec(),
            }
        }
    }
    
    /// Apply macro expansion by substituting parameters
    fn apply_macro_expansion(&self, body: &AstNode, params: &[String], args: &[AstNode]) -> AstNode {
        // Simple substitution for now
        // TODO: Implement proper pattern matching and hygiene
        self.substitute_params(body, params, args)
    }
    
    /// Substitute parameters in macro body
    fn substitute_params(&self, body: &AstNode, params: &[String], args: &[AstNode]) -> AstNode {
        match body {
            // Simple identifier substitution
            AstNode::Ident(name) => {
                if let Some(index) = params.iter().position(|p| p == name) {
                    if index < args.len() {
                        args[index].clone()
                    } else {
                        body.clone()
                    }
                } else {
                    body.clone()
                }
            }
            
            // Recursively substitute in child nodes
            _ => self.substitute_children(body, params, args),
        }
    }
    
    /// Substitute parameters in child nodes
    fn substitute_children(&self, node: &AstNode, params: &[String], args: &[AstNode]) -> AstNode {
        match node {
            // Binary operation
            AstNode::BinaryOp { op, left, right } => {
                AstNode::BinaryOp {
                    op: op.clone(),
                    left: Box::new(self.substitute_params(left, params, args)),
                    right: Box::new(self.substitute_params(right, params, args)),
                }
            }
            
            // Unary operation
            AstNode::UnaryOp { op, expr } => {
                AstNode::UnaryOp {
                    op: op.clone(),
                    expr: Box::new(self.substitute_params(expr, params, args)),
                }
            }
            
            // Function call
            AstNode::Call { func, args: call_args } => {
                AstNode::Call {
                    func: Box::new(self.substitute_params(func, params, args)),
                    args: call_args.iter()
                        .map(|arg| self.substitute_params(arg, params, args))
                        .collect(),
                }
            }
            
            // Let binding
            AstNode::Let { name, ty, value } => {
                AstNode::Let {
                    name: name.clone(),
                    ty: ty.clone(),
                    value: Box::new(self.substitute_params(value, params, args)),
                }
            }
            
            // Default: clone the node
            _ => node.clone(),
        }
    }
    
    /// Expand children of a node
    fn expand_children(&mut self, node: &AstNode) -> AstNode {
        match node {
            // Binary operation
            AstNode::BinaryOp { op, left, right } => {
                AstNode::BinaryOp {
                    op: op.clone(),
                    left: Box::new(self.expand_node(left)),
                    right: Box::new(self.expand_node(right)),
                }
            }
            
            // Unary operation
            AstNode::UnaryOp { op, expr } => {
                AstNode::UnaryOp {
                    op: op.clone(),
                    expr: Box::new(self.expand_node(expr)),
                }
            }
            
            // Function call
            AstNode::Call { func, args } => {
                AstNode::Call {
                    func: Box::new(self.expand_node(func)),
                    args: args.iter()
                        .map(|arg| self.expand_node(arg))
                        .collect(),
                }
            }
            
            // Let binding
            AstNode::Let { name, ty, value } => {
                AstNode::Let {
                    name: name.clone(),
                    ty: ty.clone(),
                    value: Box::new(self.expand_node(value)),
                }
            }
            
            // Default: clone the node
            _ => node.clone(),
        }
    }
    
    /// Get the macro registry
    pub fn registry(&self) -> &MacroRegistry {
        &self.registry
    }
    
    /// Get mutable access to the macro registry
    pub fn registry_mut(&mut self) -> &mut MacroRegistry {
        &mut self.registry
    }
}

impl Default for MacroExpander {
    fn default() -> Self {
        Self::new()
    }
}