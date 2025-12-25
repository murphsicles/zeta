// src/middle/resolver/lower.rs
use super::Resolver;
use crate::frontend::ast::AstNode;
use std::collections::HashMap;

impl Resolver {
    pub fn collect_used_specializations(&self, asts: &[AstNode]) -> HashMap<String, Vec<Vec<String>>> {
        let mut used = HashMap::new();
        for ast in asts {
            self.collect_in_ast(&mut used, ast);
        }
        used
    }

    fn collect_in_ast(&self, used: &mut HashMap<String, Vec<Vec<String>>>, ast: &AstNode) {
        match ast {
            AstNode::FuncDef { body, .. } => {
                for stmt in body {
                    self.collect_in_ast(used, stmt);
                }
            }
            AstNode::Call { method, type_args, receiver, args, .. } if !type_args.is_empty() => {
                used.entry(method.clone()).or_insert(vec![]).push(type_args.clone());
                if let Some(recv) = receiver {
                    self.collect_in_ast(used, recv);
                }
                for arg in args {
                    self.collect_in_ast(used, arg);
                }
            }
            AstNode::BinaryOp { left, right, .. } => {
                self.collect_in_ast(used, left);
                self.collect_in_ast(used, right);
            }
            AstNode::If { cond, then, else_, .. } => {
                self.collect_in_ast(used, cond);
                for s in then {
                    self.collect_in_ast(used, s);
                }
                for s in else_ {
                    self.collect_in_ast(used, s);
                }
            }
            _ => {},
        }
    }
}
