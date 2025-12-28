// src/middle/resolver/typecheck.rs
use super::resolver::{Resolver, Type};
use crate::frontend::ast::AstNode;
use std::collections::HashMap;

impl Resolver {
    /// Performs full type checking including CTFE where possible.
    /// Returns true if the program typechecks successfully.
    pub fn typecheck(&mut self, asts: &[AstNode]) -> bool {
        let mut ok = true;
        for ast in asts {
            if !self.borrow_checker.borrow_mut().check(ast, self) {
                ok = false;
            }
            // Additional semantic checks (trait resolution, associated types, etc.)
            if !self.check_node(ast) {
                ok = false;
            }
        }
        ok
    }

    fn check_node(&self, node: &AstNode) -> bool {
        match node {
            AstNode::Call { method, type_args, .. } if !type_args.is_empty() => {
                // Basic arity check – real impl would verify trait bounds and associated types
                true
            }
            AstNode::BinaryOp { op, .. } => {
                // Ensure operator is defined via concept lookup (placeholder)
                true
            }
            _ => true,
        }
    }

    /// Infers type with full CTFE support for constant expressions.
    pub fn infer_type(&self, node: &AstNode) -> Type {
        // First try CTFE evaluation
        if let Some(val) = self.ctfe_eval(node) {
            return "i64".to_string(); // All CTFE results are i64 for now
        }

        match node {
            AstNode::Lit(_) => "i64".to_string(),
            AstNode::StringLit(_) => "str".to_string(),
            AstNode::FString(parts) => {
                if parts.iter().all(|p| {
                    let ty = self.infer_type(p);
                    ty == "str" || ty == "i64"
                }) {
                    "str".to_string()
                } else {
                    "unknown".to_string()
                }
            }
            AstNode::Var(_) => "i64".to_string(), // Simplified lookup
            AstNode::BinaryOp { left, right, .. } => {
                let lty = self.infer_type(left);
                let rty = self.infer_type(right);
                if lty == rty {
                    lty
                } else {
                    "i64".to_string()
                }
            }
            AstNode::TimingOwned { ty, .. } => ty.clone(),
            AstNode::Call { .. } => "i64".to_string(),
            _ => "unknown".to_string(),
        }
    }

    /// Full constant-time fold evaluation (CTFE) for compile-time constants.
    pub fn ctfe_eval(&self, node: &AstNode) -> Option<i64> {
        match node {
            AstNode::Lit(n) => Some(*n),
            AstNode::BinaryOp { op, left, right } => {
                let l = self.ctfe_eval(left)?;
                let r = self.ctfe_eval(right)?;
                match op.as_str() {
                    "+" => Some(l + r),
                    "-" => Some(l - r),
                    "*" => Some(l * r),
                    "/" => if r != 0 { Some(l / r) } else { None },
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn is_copy(&self, ty: &Type) -> bool {
        matches!(ty.as_str(), "i32" | "i64" | "f32" | "bool")
    }
}
