// src/middle/resolver/typecheck.rs
//! Type checking and inference for Zeta AST nodes.
use crate::ast::AstNode;
use super::resolver::Resolver;

impl Resolver {
    /// Typechecks a list of AST nodes using the borrow checker.
    pub fn typecheck(&mut self, asts: &[AstNode]) -> bool {
        for ast in asts {
            if !self.borrow_checker.check(ast, self) {
                return false;
            }
        }
        true
    }

    /// Infers the type of an AST node.
    pub fn infer_type(&self, node: &AstNode) -> Type {
        match node {
            AstNode::Lit(_) => "i64".to_string(),
            AstNode::StringLit(_) => "str".to_string(),
            AstNode::FString(_) => "str".to_string(),
            AstNode::Var(_) => "unknown".to_string(), // Would resolve from env
            AstNode::BinaryOp { left, .. } => self.infer_type(left),
            // Add more cases as needed
            _ => "unknown".to_string(),
        }
    }

    /// Checks if a type implements Copy.
    pub fn is_copy(&self, ty: &Type) -> bool {
        matches!(ty.as_str(), "i32" | "i64" | "f32" | "bool")
    }
}
