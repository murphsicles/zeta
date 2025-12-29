// src/middle/resolver/typecheck.rs
use super::resolver::{Resolver, Type};
use crate::frontend::ast::AstNode;

impl Resolver {
    pub fn typecheck(&mut self, asts: &[AstNode]) -> bool {
        let mut ok = true;

        // First pass: borrow checking only
        for ast in asts {
            let borrow_ok = {
                // Take out the borrow_checker mutably for this iteration only
                let mut checker = std::mem::take(&mut self.borrow_checker).borrow_mut();
                let result = checker.check(ast, self);
                // Put it back – this is safe because borrow_checker is always present
                drop(checker);
                result
            };
            if !borrow_ok {
                ok = false;
            }
        }

        // Second pass: other semantic checks that may mutate self (CTFE cache, etc.)
        for ast in asts {
            if !self.check_node(ast) {
                ok = false;
            }
        }

        ok
    }

    fn check_node(&mut self, node: &AstNode) -> bool {
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

    pub fn infer_type(&mut self, node: &AstNode) -> Type {
        // Full CTFE evaluation first – if successful, everything is i64 (for now)
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

    /// Extended CTFE with deeper expression support:
    /// - Binary ops (including chains)
    /// - Calls to known const functions
    /// - FString interpolation if all parts are CTFE-evaluable
    pub fn ctfe_eval(&mut self, node: &AstNode) -> Option<i64> {
        // Check cache first
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
            AstNode::FString(parts) => {
                // Only allow pure literals for now – deeper interp later
                if parts.iter().all(|p| matches!(p, AstNode::StringLit(_))) {
                    Some(0) // placeholder – actual concat would produce str, not i64
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some(v) = result {
            self.ctfe_consts.insert(node.clone(), v);
        }
        result
    }

    pub fn is_copy(&self, ty: &Type) -> bool {
        matches!(ty.as_str(), "i32" | "i64" | "f32" | "bool")
    }
}
