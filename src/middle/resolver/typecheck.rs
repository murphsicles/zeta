// src/middle/resolver/typecheck.rs
use crate::frontend::ast::AstNode;
use super::resolver::{Resolver, Type};

impl Resolver {
    pub fn typecheck(&mut self, asts: &[AstNode]) -> bool {
        let resolver = self as &Resolver;
        for ast in asts {
            if !self.borrow_checker.check(ast, resolver) {
                return false;
            }
        }
        true
    }

    pub fn infer_type(&self, node: &AstNode) -> Type {
        match node {
            AstNode::Lit(_) => "i64".to_string(),
            AstNode::StringLit(_) => "str".to_string(),
            AstNode::FString(_) => "str".to_string(),
            AstNode::Var(_) => "unknown".to_string(),
            AstNode::BinaryOp { left, .. } => self.infer_type(left),
            AstNode::TimingOwned { ty, .. } => ty.clone(),
            _ => "unknown".to_string(),
        }
    }

    pub fn is_copy(&self, ty: &Type) -> bool {
        matches!(ty.as_str(), "i32" | "i64" | "f32" | "bool")
    }
}
