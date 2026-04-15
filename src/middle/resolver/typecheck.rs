// src/middle/resolver/typecheck.rs
//! # Type Checking & Semantic Validation
//!
//! Performs borrow checking (affine types) and basic type inference/CTFE.
//! Runs before MIR lowering. Clean, fast, and fully documented.

use super::resolver::{Resolver, Type};
use super::unified_typecheck::{TypeCheckResult, UnifiedTypeCheck};
use crate::frontend::ast::AstNode;
use crate::middle::passes::identity_verification::verify_identities;
use crate::middle::types::identity::{CapabilityLevel, IdentityType};

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
                let _ = ok; // Mark as used to avoid warning
                ok = false;
            }
        }

        // Use unified type checking interface
        let typecheck_result = match self.typecheck_unified(asts) {
            TypeCheckResult::Success(_) => {
                // Unified type checking succeeded
                true
            }
            TypeCheckResult::Failure(errors) => {
                // Type checking failed with errors
                eprintln!("Type checking failed with errors:");
                for error in &errors {
                    eprintln!("  Type error: {}", error);
                }
                false
            }
            TypeCheckResult::Fallback => {
                // Fallback to simple type checking
                eprintln!("Using fallback type checking");
                let mut ok = true;
                for ast in asts {
                    if !self.check_node(ast) {
                        ok = false;
                    }
                }
                ok
            }
        };
        
        // Run identity verification pass if type checking succeeded
        if typecheck_result {
            self.run_identity_verification(asts)
        } else {
            false
        }
    }

    fn check_node(&self, node: &AstNode) -> bool {
        let mut ok = true;
        match node {
            AstNode::Call { method, args, .. } => {
                // Check function call arguments
                if let Some(sig) = self.get_func_signature(method) {
                    // Check number of arguments matches
                    if args.len() != sig.0.len() {
                        eprintln!(
                            "Error: {} expects {} arguments, got {}",
                            method,
                            sig.0.len(),
                            args.len()
                        );
                        ok = false;
                    } else {
                        // Check each argument type
                        for (i, (arg, (param_name, param_type))) in
                            args.iter().zip(sig.0.iter()).enumerate()
                        {
                            let arg_type = self.infer_type(arg);
                            if &arg_type != param_type {
                                eprintln!(
                                    "Error: Argument {} to {} has type {}, expected {}",
                                    i + 1,
                                    method,
                                    arg_type.display_name(),
                                    param_type.display_name()
                                );
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
            AstNode::BinaryOp { op, left, right, .. } => {
                let lty = self.infer_type(left);
                let rty = self.infer_type(right);
                
                // For logical operators (&&, ||), both operands must be bool
                if op == "&&" || op == "||" {
                    if lty != Type::Bool {
                        eprintln!("Error: Left operand of '{}' must be bool, got {}", op, lty.display_name());
                        ok = false;
                    }
                    if rty != Type::Bool {
                        eprintln!("Error: Right operand of '{}' must be bool, got {}", op, rty.display_name());
                        ok = false;
                    }
                } else if lty != rty {
                    // For other binary operators, types must match
                    // Check for SIMD vector operations
                    if lty.is_vector() && rty.is_vector() {
                        // Both are vectors, check if they have same element type and size
                        if let (Some((l_inner, l_size)), Some((r_inner, r_size))) = (lty.as_vector(), rty.as_vector()) {
                            if l_inner != r_inner {
                                eprintln!("Error: SIMD vector element type mismatch in '{}': {} vs {}", op, l_inner.display_name(), r_inner.display_name());
                                ok = false;
                            } else if l_size != r_size {
                                eprintln!("Error: SIMD vector size mismatch in '{}': {} vs {}", op, l_size, r_size);
                                ok = false;
                            }
                            // If types match, operation is valid
                        }
                    } else {
                        // Not both vectors, types must match exactly
                        eprintln!("Error: Type mismatch in binary operation '{}': {} vs {}", op, lty.display_name(), rty.display_name());
                        ok = false;
                    }
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
            AstNode::While { cond, body } => {
                // Check condition - should be bool
                let cond_type = self.infer_type(&cond);
                if cond_type != Type::Bool {
                    eprintln!("Error: While condition must be bool, got {}", cond_type.display_name());
                    ok = false;
                }
                if !self.check_node(&cond) {
                    ok = false;
                }
                for s in body {
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
                    if !self.types_compatible(&expr_type, &annotated_type, expr) {
                        ok = false;
                    }
                }
                if !self.check_node(expr) {
                    ok = false;
                }
            }
            AstNode::Subscript { base, index } => {
                // Check base and index expressions
                if !self.check_node(base) {
                    ok = false;
                }
                if !self.check_node(index) {
                    ok = false;
                }
                // For now, we don't check the types of subscript operations
                // This would require knowing if base is an array/slice and index is integer
            }
            _ => {}
        }
        ok
    }

    /// Convert string type annotation to Type enum
    /// Uses unified type parsing interface
    fn string_to_type(&self, s: &str) -> Type {
        // Use the unified type parsing
        match self.parse_type_string(s) {
            Ok(ty) => ty,
            Err(err) => {
                eprintln!("Warning: Failed to parse type '{}': {}", s, err);
                // Fallback to Named type
                Type::Named(s.to_string(), vec![])
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
            AstNode::Bool(_) => Type::Bool,
            AstNode::Range { .. } => Type::Range,
            AstNode::BinaryOp { op, left, .. } => {
                // Handle range operator specially
                if op == ".." {
                    Type::Range
                } else if op == "&&" || op == "||" {
                    // Logical operators return bool
                    Type::Bool
                } else if op == "==" || op == "!=" || op == "<" || op == ">" || op == "<=" || op == ">=" {
                    // Comparison operators return bool
                    Type::Bool
                } else {
                    // For other operators, return type of left operand
                    // For SIMD vectors, return vector type
                    let lty = self.infer_type(left);
                    if lty.is_vector() {
                        lty
                    } else {
                        lty
                    }
                }
            },
            AstNode::Call { method, .. } => {
                // Get the return type from the function signature
                if let Some(sig) = self.get_func_signature(method) {
                    sig.1.clone()
                } else {
                    // Default to i64 for unknown functions
                    Type::I64
                }
            },
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
            AstNode::For { .. } => {
                // For loops have unit type (they don't produce a value)
                Type::Tuple(vec![])
            }
            AstNode::While { .. } => {
                // While loops have unit type (they don't produce a value)
                Type::Tuple(vec![])
            }
            AstNode::Subscript { base, .. } => {
                // For array subscript a[i], return the element type
                // For now, we assume arrays of i64
                // TODO: Actually infer the element type from the base
                Type::I64
            }
            _ => Type::I64,
        }
    }

    pub fn ctfe_eval(&self, node: &AstNode) -> Option<i64> {
        // Try to get constant value by name if it's a variable reference
        if let AstNode::Var(name) = node {
            if let Some(const_val) = self.ctfe_consts.get(name) {
                return const_val.as_int();
            }
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

    /// Check if two types are compatible, allowing some implicit conversions
    pub fn types_compatible(&self, expr_type: &Type, annotated_type: &Type, expr: &AstNode) -> bool {
        // Exact match is always compatible
        if expr_type == annotated_type {
            return true;
        }

        // Allow i64 to be assigned to unsigned integers (common in PrimeZeta code)
        // This is unsafe for negative values but matches common practice
        if expr_type == &Type::I64 {
            match annotated_type {
                Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::Usize => return true,
                Type::I8 | Type::I16 | Type::I32 => return true, // Allow truncation
                _ => {}
            }
        }

        // Allow unsigned integers to be assigned to i64 (with potential overflow)
        if annotated_type == &Type::I64 {
            match expr_type {
                Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::Usize => return true,
                _ => {}
            }
        }

        // Allow i64 to be assigned to i32 (with potential truncation)
        if expr_type == &Type::I64 && annotated_type == &Type::I32 {
            return true;
        }

        // Allow u64 to be assigned to i64 (with potential overflow)
        if expr_type == &Type::U64 && annotated_type == &Type::I64 {
            return true;
        }

        // Allow i64 to be assigned to usize
        if expr_type == &Type::I64 && annotated_type == &Type::Usize {
            return true;
        }

        // Allow usize to be assigned to i64
        if expr_type == &Type::Usize && annotated_type == &Type::I64 {
            return true;
        }

        // TODO: Add more compatibility rules as needed
        false
    }
    
    /// Run identity verification pass on the AST
    fn run_identity_verification(&self, asts: &[AstNode]) -> bool {
        let mut all_ok = true;
        
        for ast in asts {
            match verify_identities(ast) {
                Ok(warnings) => {
                    // Print warnings but don't fail compilation
                    for warning in warnings {
                        eprintln!("Identity warning: {}", warning);
                    }
                }
                Err(errors) => {
                    // Identity verification failed
                    eprintln!("Identity verification failed with errors:");
                    for error in errors {
                        eprintln!("  Identity error: {}", error);
                    }
                    all_ok = false;
                }
            }
        }
        
        all_ok
    }
    
    /// Infer identity type for an expression based on usage context
    fn infer_identity_type(&self, node: &AstNode, context_capabilities: &[CapabilityLevel]) -> Option<Type> {
        match node {
            AstNode::StringLit(s) => {
                // Create an identity type with the required capabilities
                let identity_type = IdentityType {
                    value: Some(s.clone()),
                    capabilities: context_capabilities.to_vec(),
                    delegatable: false,
                    constraints: Vec::new(),
                    type_params: vec![],
                };
                Some(Type::Identity(Box::new(identity_type)))
            }
            AstNode::Var(_name) => {
                // For now, don't infer identity types for variables
                // TODO: Implement variable type tracking for identity inference
                None
            }
            _ => None,
        }
    }
    
    /// Get required capabilities for a function argument
    fn get_required_capabilities(&self, func_name: &str, arg_index: usize) -> Vec<CapabilityLevel> {
        // Check if this is an identity-aware function
        match func_name {
            "str_len" | "str_concat" | "str_split" => vec![CapabilityLevel::Read],
            "str_replace" | "str_to_uppercase" | "str_to_lowercase" => vec![CapabilityLevel::Write],
            "read_only_string" => vec![CapabilityLevel::Read],
            "read_write_string" => vec![CapabilityLevel::Read, CapabilityLevel::Write],
            "owned_string" => vec![CapabilityLevel::Owned],
            _ => vec![],
        }
    }
}
