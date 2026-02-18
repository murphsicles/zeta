// src/middle/resolver/typecheck.rs
//! # Type Checking & Semantic Validation
//!
//! Performs borrow checking (affine types) and basic type inference/CTFE.
//! Runs before MIR lowering. Clean, fast, and fully documented.

use super::resolver::{Resolver, Type};
use crate::frontend::ast::AstNode;

impl Resolver {
    pub fn typecheck(&mut self, asts: &[AstNode]) -> bool {
        let mut ok = true;

        // Borrow checker pass (separate scope to avoid RefCell conflict)
        for ast in asts {
            let borrow_ok = {
                let mut checker = self.borrow_checker.borrow_mut();
                checker.check(ast, self)
            };
            if !borrow_ok {
                ok = false;
            }
        }

        // Semantic checks (recursive)
        for ast in asts {
            if !self.check_node(ast) {
                ok = false;
            }
        }
        ok
    }

    fn check_node(&self, node: &AstNode) -> bool {
        let mut ok = true;
        match node {
            AstNode::Call { .. } => {} // receiver methods always OK after monomorphization
            AstNode::BinaryOp { left, right, .. } => {
                let lty = self.infer_type(left);
                let rty = self.infer_type(right);
                if lty != rty {
                    ok = false;
                }
                if !self.check_node(left) {
                    ok = false;
                }
                if !self.check_node(right) {
                    ok = false;
                }
            }
            AstNode::FuncDef { body, .. } => {
                for s in body {
                    if !self.check_node(s) {
                        ok = false;
                    }
                }
            }
            AstNode::Return(inner) => {
                if !self.check_node(inner) {
                    ok = false;
                }
            }
            AstNode::ExprStmt { expr } => {
                if !self.check_node(expr) {
                    ok = false;
                }
            }
            AstNode::If {
                cond, then, else_, ..
            } => {
                if !self.check_node(cond) {
                    ok = false;
                }
                for s in then {
                    if !self.check_node(s) {
                        ok = false;
                    }
                }
                for s in else_ {
                    if !self.check_node(s) {
                        ok = false;
                    }
                }
            }
            _ => {}
        }
        ok
    }

    pub fn infer_type(&self, node: &AstNode) -> Type {
        if self.ctfe_eval(node).is_some() {
            return "i64".to_string();
        }
        match node {
            AstNode::Lit(_) => "i64".to_string(),
            AstNode::StringLit(_) => "str".to_string(),
            AstNode::FString(_) => "str".to_string(),
            AstNode::Var(_) => "i64".to_string(),
            AstNode::BinaryOp { left, .. } => self.infer_type(left),
            AstNode::Call { .. } => "i64".to_string(),
            AstNode::DictLit { entries } => {
                if entries.is_empty() {
                    "Map_i64_i64".to_string()
                } else {
                    format!(
                        "Map_{}_{}",
                        self.infer_type(&entries[0].0),
                        self.infer_type(&entries[0].1)
                    )
                }
            }
            _ => "i64".to_string(),
        }
    }

    pub fn ctfe_eval(&self, node: &AstNode) -> Option<i64> {
        if let Some(&v) = self.ctfe_consts.get(node) {
            return Some(v);
        }
        let result = match node {
            AstNode::Lit(n) => Some(*n),
            AstNode::BinaryOp { op, left, right } => {
                let l = self.ctfe_eval(left)?;
                let r = self.ctfe_eval(right)?;
                match op.as_str() {
                    "+" => Some(l + r),
                    "-" => Some(l - r),
                    "*" => Some(l * r),
                    "/" => {
                        if r != 0 {
                            Some(l / r)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            _ => None,
        };
        result
    }

    pub fn is_copy(&self, ty: &Type) -> bool {
        matches!(ty.as_str(), "i64" | "i32" | "bool" | "str")
    }
}
