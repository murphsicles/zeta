// src/middle/resolver/typecheck.rs
use super::resolver::{Resolver, Type};
use crate::frontend::ast::AstNode;

impl Resolver {
    pub fn typecheck(&mut self, asts: &[AstNode]) -> bool {
        let mut ok = true;
        for ast in asts {
            if !self.borrow_checker.borrow_mut().check(ast, self) {
                ok = false;
            }
            if !self.check_node(ast) {
                ok = false;
            }
        }
        ok
    }

    fn check_node(&self, node: &AstNode) -> bool {
        match node {
            AstNode::Call { type_args, .. } => {
                // Arity check for generics (placeholder - real check would use signature lookup)
                !type_args.is_empty()
            }
            AstNode::BinaryOp { left, right, .. } => {
                let lty = self.infer_type(left);
                let rty = self.infer_type(right);
                lty == rty
            }
            _ => true,
        }
    }

    pub fn infer_type(&self, node: &AstNode) -> Type {
        if self.ctfe_eval(node).is_some() {
            return "i64".to_string();
        }

        match node {
            AstNode::Lit(_) => "i64".to_string(),
            AstNode::StringLit(_) => "str".to_string(),
            AstNode::FString(parts) => {
                for part in parts {
                    let ty = self.infer_type(part);
                    if ty != "str" && ty != "i64" {
                        return "unknown".to_string();
                    }
                }
                "str".to_string()
            }
            AstNode::Var(_) => "i64".to_string(),
            AstNode::BinaryOp { left, .. } => self.infer_type(left),
            AstNode::TimingOwned { ty, .. } => ty.clone(),
            AstNode::Call { .. } => "i64".to_string(),
            AstNode::DictLit { entries } => {
                if entries.is_empty() {
                    "Map_i64_i64".to_string()
                } else {
                    let key_ty = self.infer_type(&entries[0].0);
                    let val_ty = self.infer_type(&entries[0].1);
                    format!("Map_{}_{}", key_ty, val_ty)
                }
            }
            _ => "unknown".to_string(),
        }
    }

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
                    "/" => {
                        if r == 0 {
                            None
                        } else {
                            Some(l / r)
                        }
                    }
                    _ => None,
                }
            }
            AstNode::Call { method, args, .. } => {
                if method == "const_add" && args.len() == 2 {
                    let a = self.ctfe_eval(&args[0])?;
                    let b = self.ctfe_eval(&args[1])?;
                    Some(a + b)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn is_copy(&self, ty: &Type) -> bool {
        matches!(ty.as_str(), "i32" | "i64" | "f32" | "bool")
    }
}
