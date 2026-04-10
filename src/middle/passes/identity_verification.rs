//! # Identity Verification Pass
//!
//! Compile-time verification of identity capabilities and constraints.

use crate::frontend::ast::AstNode;
use crate::middle::types::identity::inference::{CapabilityInferencer, IdentityInferenceContext};

/// Identity verification pass
pub struct IdentityVerificationPass {
    /// Current identity context
    identity_context: IdentityInferenceContext,
    /// Capability inferencer
    capability_inferencer: CapabilityInferencer,
    /// Error messages
    errors: Vec<String>,
    /// Warnings
    warnings: Vec<String>,
}

impl IdentityVerificationPass {
    /// Create a new identity verification pass
    pub fn new() -> Self {
        Self {
            identity_context: IdentityInferenceContext::new(),
            capability_inferencer: CapabilityInferencer::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    /// Run the verification pass on an AST
    pub fn verify(&mut self, ast: &AstNode) -> Result<(), Vec<String>> {
        self.visit_ast(ast);
        
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    /// Visit an AST node
    fn visit_ast(&mut self, node: &AstNode) {
        match node {
            AstNode::Program(decls) => {
                for decl in decls {
                    self.visit_ast(decl);
                }
            }
            AstNode::FuncDef { name, params, ret, body, .. } => {
                // Check function parameters for identity types
                for (param_name, param_type_str) in params {
                    self.check_type_string_for_identity(param_type_str, &format!("parameter {}", param_name));
                }
                
                // Check return type for identity types
                self.check_type_string_for_identity(ret, "return type");
                
                // Visit function body
                for stmt in body {
                    self.visit_ast(stmt);
                }
            }
            AstNode::Let { ty, expr, .. } => {
                // Check type annotation for identity types
                if let Some(type_str) = ty {
                    self.check_type_string_for_identity(type_str, "variable");
                }
                
                // Check value expression
                self.visit_ast(expr);
            }
            AstNode::Var(name) => {
                // Check if this identifier has an identity type
                self.check_identifier_for_identity(name);
            }
            AstNode::Call { method, args, .. } => {
                // Check function call for capability requirements
                self.check_function_call_for_capabilities(method, args);
                
                // Visit children
                self.visit_children(node);
            }
            AstNode::BinaryOp { .. } => {
                // For now, just visit children
                self.visit_children(node);
            }
            AstNode::StringLit(value) => {
                // Check if literal is an identity string
                self.check_literal_for_identity(value);
            }
            AstNode::Assign(target, value) => {
                self.visit_ast(target);
                self.visit_ast(value);
                
                // Check assignment for identity capabilities
                self.check_assignment_for_identity(target, value);
            }
            AstNode::FieldAccess { .. } => {
                // For now, just visit children
                self.visit_children(node);
            }
            AstNode::Subscript { .. } => {
                // For now, just visit children
                self.visit_children(node);
            }
            _ => {
                // Visit children for other node types
                self.visit_children(node);
            }
        }
    }

    /// Visit all children of an AST node
    fn visit_children(&mut self, node: &AstNode) {
        match node {
            AstNode::Program(decls) => {
                for decl in decls {
                    self.visit_ast(decl);
                }
            }
            AstNode::FuncDef { body, .. } => {
                for stmt in body {
                    self.visit_ast(stmt);
                }
            }
            AstNode::Call { receiver, args, .. } => {
                if let Some(recv) = receiver {
                    self.visit_ast(recv);
                }
                for arg in args {
                    self.visit_ast(arg);
                }
            }
            AstNode::BinaryOp { left, right, .. } => {
                self.visit_ast(left);
                self.visit_ast(right);
            }
            AstNode::Let { expr, .. } => {
                self.visit_ast(expr);
            }
            AstNode::Assign(target, value) => {
                self.visit_ast(target);
                self.visit_ast(value);
            }
            AstNode::FieldAccess { base, .. } => {
                self.visit_ast(base);
            }
            AstNode::Subscript { base, index, .. } => {
                self.visit_ast(base);
                self.visit_ast(index);
            }
            AstNode::If { cond, then, else_, .. } => {
                self.visit_ast(cond);
                for stmt in then {
                    self.visit_ast(stmt);
                }
                for stmt in else_ {
                    self.visit_ast(stmt);
                }
            }
            AstNode::While { cond, body, .. } => {
                self.visit_ast(cond);
                for stmt in body {
                    self.visit_ast(stmt);
                }
            }
            AstNode::For { expr, body, .. } => {
                self.visit_ast(expr);
                for stmt in body {
                    self.visit_ast(stmt);
                }
            }
            AstNode::Block { body } => {
                for stmt in body {
                    self.visit_ast(stmt);
                }
            }
            AstNode::Return(expr) => {
                self.visit_ast(expr);
            }
            _ => {
                // No children to visit
            }
        }
    }

    /// Check a type string for identity types
    fn check_type_string_for_identity(&mut self, type_str: &str, context: &str) {
        // Check if this looks like an identity type
        if type_str.starts_with("identity") {
            self.warnings.push(format!("Identity type found in {} - will be verified at compile time", context));
        }
        
        // Check for identity in generic types
        if type_str.contains("identity") {
            self.warnings.push(format!("Potential identity type in {}: {}", context, type_str));
        }
        
        // Check for capability constraints
        self.check_type_annotation_for_constraints(type_str, context);
    }

    /// Check if an identifier has an identity type
    fn check_identifier_for_identity(&mut self, name: &str) {
        // For now, just check if it looks like an identity
        if name.starts_with("identity_") || name.ends_with("_id") || name.ends_with("_token") {
            self.warnings.push(format!("Identifier '{}' appears to be an identity - consider using identity type", name));
        }
    }

    /// Check if a literal is an identity string
    fn check_literal_for_identity(&mut self, value: &str) {
        // Heuristic: strings that look like IDs, tokens, or secrets
        let identity_patterns = vec![
            ("id_", "ID prefix"),
            ("_token", "token suffix"),
            ("secret", "contains 'secret'"),
            ("key", "contains 'key'"),
            ("password", "contains 'password'"),
            ("auth", "contains 'auth'"),
        ];
        
        for (pattern, description) in identity_patterns {
            if value.contains(pattern) {
                self.warnings.push(format!("String literal '{}' appears to be an identity ({}) - consider using identity type", 
                    if value.len() > 20 { format!("{}...", &value[..20]) } else { value.to_string() }, 
                    description));
                break;
            }
        }
    }

    /// Check assignment for identity capabilities
    fn check_assignment_for_identity(&mut self, target: &AstNode, value: &AstNode) {
        // Check if we're assigning to something that looks like an identity
        if let AstNode::Var(name) = target {
            if name.starts_with("identity_") || name.ends_with("_id") || name.ends_with("_token") {
                self.warnings.push(format!("Assignment to identity-like variable '{}' - capability transfer may be needed", name));
            }
        }
        
        // Check if we're assigning an identity-like string
        if let AstNode::StringLit(value_str) = value {
            self.check_literal_for_identity(value_str);
        }
    }

    /// Check a function call for capability requirements
    fn check_function_call_for_capabilities(&mut self, func_name: &String, args: &[AstNode]) {
        // Check if this function has known capability requirements
        if let Some(required_caps) = self.capability_inferencer.infer_capabilities(func_name) {
            // For now, just warn about capability requirements
            let caps_str = required_caps.iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            
            self.warnings.push(format!(
                "Function '{}' requires capabilities: {}",
                func_name, caps_str
            ));
            
            // Check if any arguments look like identities
            for arg in args {
                if let AstNode::Var(name) = arg {
                    if name.starts_with("identity_") || name.ends_with("_id") || name.ends_with("_token") {
                        self.warnings.push(format!(
                            "Argument '{}' to function '{}' appears to be an identity - ensure it has required capabilities",
                            name, func_name
                        ));
                    }
                }
            }
        }
    }

    /// Check type annotation for capability constraints
    fn check_type_annotation_for_constraints(&mut self, type_str: &str, context: &str) {
        // Parse capability constraints from type string
        if type_str.contains("[identity:") {
            // Extract capability list
            if let Some(start) = type_str.find("[identity:") {
                if let Some(end) = type_str[start..].find(']') {
                    let constraint_str = &type_str[start + 9..start + end];
                    let capabilities: Vec<&str> = constraint_str.split('+').collect();
                    
                    if !capabilities.is_empty() {
                        self.warnings.push(format!(
                            "Type annotation for {} has capability constraints: {}",
                            context, constraint_str
                        ));
                    }
                }
            }
        }
    }

    /// Get all warnings from the verification pass
    pub fn warnings(&self) -> &[String] {
        &self.warnings
    }

    /// Get all errors from the verification pass
    pub fn errors(&self) -> &[String] {
        &self.errors
    }

    /// Clear all warnings and errors
    pub fn clear(&mut self) {
        self.warnings.clear();
        self.errors.clear();
        self.identity_context = IdentityInferenceContext::new();
    }
}

/// Simple identity verification that can be used as a standalone pass
pub fn verify_identities(ast: &AstNode) -> Result<Vec<String>, Vec<String>> {
    let mut pass = IdentityVerificationPass::new();
    
    match pass.verify(ast) {
        Ok(_) => {
            let warnings = pass.warnings().to_vec();
            Ok(warnings)
        }
        Err(errors) => Err(errors),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_verification_pass_creation() {
        let pass = IdentityVerificationPass::new();
        assert!(pass.warnings().is_empty());
        assert!(pass.errors().is_empty());
    }
    
    #[test]
    fn test_check_type_string_for_identity() {
        let mut pass = IdentityVerificationPass::new();
        
        // Test identity type
        pass.check_type_string_for_identity("identity(\"user\")[read]", "test");
        assert!(!pass.warnings().is_empty());
        assert!(pass.warnings()[0].contains("Identity type found"));
        
        // Test non-identity type
        pass.clear();
        pass.check_type_string_for_identity("i64", "test");
        assert!(pass.warnings().is_empty());
    }
    
    #[test]
    fn test_check_identifier_for_identity() {
        let mut pass = IdentityVerificationPass::new();
        
        // Test identity-like identifier
        pass.check_identifier_for_identity("identity_user");
        assert!(!pass.warnings().is_empty());
        assert!(pass.warnings()[0].contains("appears to be an identity"));
        
        // Test non-identity identifier
        pass.clear();
        pass.check_identifier_for_identity("counter");
        assert!(pass.warnings().is_empty());
    }
    
    #[test]
    fn test_check_literal_for_identity() {
        let mut pass = IdentityVerificationPass::new();
        
        // Test identity-like literal
        pass.check_literal_for_identity("user_id_12345");
        assert!(!pass.warnings().is_empty());
        assert!(pass.warnings()[0].contains("appears to be an identity"));
        
        // Test secret-like literal
        pass.clear();
        pass.check_literal_for_identity("secret_token_abc");
        assert!(!pass.warnings().is_empty());
        
        // Test normal literal
        pass.clear();
        pass.check_literal_for_identity("hello world");
        assert!(pass.warnings().is_empty());
    }
    
    #[test]
    fn test_verify_identities_function() {
        // Create a simple AST with an identity-like string
        let ast = AstNode::Program(vec![
            AstNode::Let {
                mut_: false,
                pattern: Box::new(AstNode::Var("token".to_string())),
                ty: Some("String".to_string()),
                expr: Box::new(AstNode::StringLit("secret_auth_token".to_string())),
            }
        ]);
        
        let result = verify_identities(&ast);
        assert!(result.is_ok());
        
        let warnings = result.unwrap();
        assert!(!warnings.is_empty());
        assert!(warnings[0].contains("appears to be an identity"));
    }
}