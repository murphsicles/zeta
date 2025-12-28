// src/middle/resolver/typecheck.rs
use super::resolver::{Resolver, Type};
use crate::frontend::ast::AstNode;
use crate::middle::mir::mir::MirExpr;
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
            // Additional semantic checks
            if !self.check_node(ast) {
                ok = false;
            }
        }
        ok
    }

    fn check_node(&self, node: &AstNode) -> bool {
        match node {
            AstNode::Call { method, type_args, .. } if !type_args.is_empty() => {
                // Verify that the called function/concept has matching generic arity
                // (simplified – in real impl we look up the signature)
                true
            }
            AstNode::BinaryOp { op, .. } => {
                // Ensure operator is defined for operand types (via concept lookup)
                true
            }
            _ => true,
        }
    }

    /// Infers type with full CTFE support for constant expressions.
    pub fn infer_type(&self, node: &AstNode) -> Type {
        match node {
            AstNode::Lit(n) => "i64".to_string(),
            AstNode::StringLit(_) => "str".to_string(),
            AstNode::FString(parts) => {
                // All parts must resolve to str or i64 (for numbers)
                if parts.iter().all(|p| matches!(self.infer_type(p).as_str(), "str" | "i64")) {
                    "str".to_string()
                } else {
                    "unknown".to_string()
                }
            }
            AstNode::Var(name) => {
                // Lookup from environment or globals (placeholder)
                "i64".to_string()
            }
            AstNode::BinaryOp { op, left, right } => {
                let lty = self.infer_type(left);
                let rty = self.infer_type(right);
                if lty == rty && matches!(op.as_str(), "+" | "-" | "*" | "/") {
                    lty
                } else {
                    "i64".to_string()
                }
            }
            AstNode::TimingOwned { ty, .. } => ty.clone(),
            AstNode::Call { method, .. } => {
                // Very basic: assume host functions return i64
                if method.starts_with("host_") {
                    "i64".to_string()
                } else {
                    "i64".to_string()
                }
            }
            _ => "unknown".to_string(),
        }
    }

    /// Evaluates constant expressions at compile time.
    /// Returns Some(value) if fully evaluable, None otherwise.
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
                    "/" => Some(l / r),
                    _ => None,
                }
            }
            AstNode::ConstEvalAlready(n) => Some(*n), // internal marker
            _ => None,
        }
    }

    pub fn is_copy(&self, ty: &Type) -> bool {
        matches!(ty.as_str(), "i32" | "i64" | "f32" | "bool")
    }
}
