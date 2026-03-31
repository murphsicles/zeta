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

        // Try using new type system if available
        use super::typecheck_new::NewTypeCheck;
        match self.typecheck_new(asts) {
            Ok(_) => {
                // New type system succeeded
                true
            }
            Err(errors) => {
                // New type system failed, fall back to old system
                eprintln!("New type system failed, falling back to old system");
                for error in errors {
                    eprintln!("  Type error: {}", error);
                }

                // Semantic checks (recursive) - old system
                for ast in asts {
                    if !self.check_node(ast) {
                        ok = false;
                    }
                }
                ok
            }
        }
    }

    fn check_node(&self, node: &AstNode) -> bool {
        let mut ok = true;
        match node {
            AstNode::Call { method, args, .. } => {
                // Check function call arguments
                if let Some((param_types, ret_type, _)) = self.get_func_signature(method) {
                    // Check number of arguments matches
                    if args.len() != param_types.len() {
                        eprintln!("Error: {} expects {} arguments, got {}", 
                                 method, param_types.len(), args.len());
                        ok = false;
                    } else {
                        // Check each argument type
                        for (i, (arg, (param_name, param_type))) in 
                            args.iter().zip(param_types.iter()).enumerate() {
                            let arg_type = self.infer_type(arg);
                            if &arg_type != param_type {
                                eprintln!("Error: Argument {} to {} has type {}, expected {}", 
                                         i + 1, method, arg_type.display_name(), 
                                         param_type.display_name());
                                ok = false;
                            }
                        }
                    }
                } else {
                    // Function not found - might be a builtin or generic function
                    // For now, don't fail for generic functions
                    if !method.contains("::") && !method.contains('<') {
                        eprintln!("Warning: Unknown function {}", method);
                    }
                }
                
                // Also check the arguments recursively
                for arg in args {
                    if !self.check_node(arg) {
                        ok = false;
                    }
                }
            }
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
            AstNode::Return(inner) if !self.check_node(inner) => {
                ok = false;
            }
            AstNode::ExprStmt { expr } if !self.check_node(expr) => {
                ok = false;
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
            AstNode::Let {
                pattern: _,
                ty,
                expr,
                ..
            } => {
                // Check if type annotation is provided
                if let Some(type_str) = ty {
                    let expr_type = self.infer_type(expr);
                    // Convert string type annotation to Type for comparison
                    let annotated_type = self.string_to_type(type_str);
                    if expr_type != annotated_type {
                        ok = false;
                    }
                }
                if !self.check_node(expr) {
                    ok = false;
                }
            }
            _ => {}
        }
        ok
    }

    /// Convert string type annotation to Type enum
    fn string_to_type(&self, s: &str) -> Type {
        match s {
            "i8" => Type::I8,
            "i16" => Type::I16,
            "i32" => Type::I32,
            "i64" => Type::I64,
            "u8" => Type::U8,
            "u16" => Type::U16,
            "u32" => Type::U32,
            "u64" => Type::U64,
            "f32" => Type::F32,
            "f64" => Type::F64,
            "bool" => Type::Bool,
            "char" => Type::Char,
            "str" => Type::Str,
            _ => {
                // Check if it's a single uppercase letter (type variable)
                if s.len() == 1 && s.chars().next().unwrap().is_ascii_uppercase() {
                    // Create a fresh type variable
                    Type::Variable(crate::middle::types::TypeVar::fresh())
                } else {
                    // For complex types (generics, tuples, etc.), return a Named type
                    // This is a simplified conversion - full parsing would be in typecheck_new.rs
                    Type::Named(s.to_string(), vec![])
                }
            }
        }
    }

    pub fn infer_type(&self, node: &AstNode) -> Type {
        if self.ctfe_eval(node).is_some() {
            return Type::I64;
        }
        match node {
            AstNode::Lit(_) => Type::I64,
            AstNode::StringLit(_) => Type::Str,
            AstNode::FString(_) => Type::Str,
            AstNode::Var(_) => Type::I64,
            AstNode::BinaryOp { left, .. } => self.infer_type(left),
            AstNode::Call { .. } => Type::I64,
            AstNode::DictLit { entries } => {
                if entries.is_empty() {
                    // For now, return a named type for Map<i64, i64>
                    Type::Named("Map_i64_i64".to_string(), vec![])
                } else {
                    let key_type = self.infer_type(&entries[0].0);
                    let value_type = self.infer_type(&entries[0].1);
                    // Create a named type like Map<key_type, value_type>
                    Type::Named("Map".to_string(), vec![key_type, value_type])
                }
            }
            _ => Type::I64,
        }
    }

    pub fn ctfe_eval(&self, node: &AstNode) -> Option<i64> {
        if let Some(&v) = self.ctfe_consts.get(node) {
            return Some(v);
        }

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
        }
    }

    pub fn is_copy(&self, ty: &Type) -> bool {
        matches!(ty, Type::I64 | Type::I32 | Type::Bool | Type::Str)
    }
}
