//! New resolver with proper type system
//! Migration from string-based types to algebraic types

use crate::frontend::ast::AstNode;
use crate::middle::types::{Substitution, Type, UnifyError};
use std::collections::HashMap;

/// Type inference context
pub struct InferContext {
    /// Variable to type mapping
    variables: HashMap<String, Type>,

    /// Current substitution
    substitution: Substitution,

    /// Collected constraints
    constraints: Vec<(Type, Type)>,

    /// Type of the last expression (for debugging)
    last_type: Option<Type>,
}

impl Default for InferContext {
    fn default() -> Self {
        Self::new()
    }
}

impl InferContext {
    pub fn new() -> Self {
        InferContext {
            variables: HashMap::new(),
            substitution: Substitution::new(),
            constraints: Vec::new(),
            last_type: None,
        }
    }

    /// Look up variable type
    pub fn lookup(&self, name: &str) -> Option<Type> {
        self.variables
            .get(name)
            .cloned()
            .map(|ty| self.substitution.apply(&ty))
    }

    /// Declare variable with type
    pub fn declare(&mut self, name: String, ty: Type) {
        self.variables.insert(name, ty);
    }

    /// Add type constraint
    pub fn constrain(&mut self, t1: Type, t2: Type) {
        self.constraints.push((t1, t2));
    }

    /// Infer type for AST node
    pub fn infer(&mut self, node: &AstNode) -> Result<Type, String> {
        let ty = match node {
            AstNode::Lit(n) => {
                // Infer integer literal type
                if *n >= i32::MIN as i64 && *n <= i32::MAX as i64 {
                    Type::I32
                } else {
                    Type::I64
                }
            }

            AstNode::FloatLit(_) => {
                // Float literals default to f64
                Type::F64
            }

            AstNode::StringLit(_) => Type::Str,

            AstNode::Bool(_b) => Type::Bool,

            AstNode::Var(name) => self
                .lookup(name)
                .ok_or_else(|| format!("Undefined variable: {}", name))?,

            AstNode::BinaryOp { op, left, right } => {
                let left_ty = self.infer(left)?;
                let right_ty = self.infer(right)?;

                // Constraint: left and right types must unify
                self.constrain(left_ty.clone(), right_ty.clone());

                // Determine result type based on operator
                match op.as_str() {
                    "+" | "-" | "*" | "/" | "%" => {
                        // Numeric operations return same type
                        left_ty
                    }
                    "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                        // Comparisons return bool
                        Type::Bool
                    }
                    "&&" | "||" => {
                        // Logical operators require bool operands
                        self.constrain(left_ty.clone(), Type::Bool);
                        self.constrain(right_ty.clone(), Type::Bool);
                        Type::Bool
                    }
                    "&" | "|" | "^" | "<<" | ">>" => {
                        // Bitwise operations
                        left_ty
                    }
                    _ => return Err(format!("Unknown operator: {}", op)),
                }
            }

            AstNode::UnaryOp { op, expr } => {
                let expr_ty = self.infer(expr)?;

                match op.as_str() {
                    "-" => {
                        // Numeric negation
                        expr_ty
                    }
                    "!" => {
                        // Logical NOT - requires bool
                        self.constrain(expr_ty.clone(), Type::Bool);
                        Type::Bool
                    }
                    "~" => {
                        // Bitwise NOT
                        expr_ty
                    }
                    _ => return Err(format!("Unknown unary operator: {}", op)),
                }
            }

            AstNode::Assign(lhs, rhs) => {
                let rhs_ty = self.infer(rhs)?;

                match &**lhs {
                    AstNode::Var(name) => {
                        // Declare or update variable type
                        if let Some(existing_ty) = self.variables.get(name) {
                            // Existing variable - constrain types
                            let existing_ty: &Type = existing_ty;
                            self.constrain(existing_ty.clone(), rhs_ty.clone());
                        } else {
                            // New variable
                            self.declare(name.clone(), rhs_ty.clone());
                        }
                        rhs_ty
                    }
                    _ => return Err("Complex assignment not yet supported".to_string()),
                }
            }

            AstNode::Let {
                pattern, ty, expr, ..
            } => {
                let expr_ty = self.infer(expr)?;

                // If type annotation provided, constrain to it
                if let Some(type_str) = ty {
                    // TODO: Parse type string to Type
                    let annotated_ty = match type_str.as_str() {
                        "i64" => Type::I64,
                        "i32" => Type::I32,
                        "bool" => Type::Bool,
                        "str" => Type::Str,
                        "f32" => Type::F32,
                        "f64" => Type::F64,
                        _ => return Err(format!("Unsupported type annotation: {}", type_str)),
                    };
                    self.constrain(expr_ty.clone(), annotated_ty);
                }

                // Register pattern variables
                if let AstNode::Var(name) = &**pattern {
                    self.declare(name.clone(), expr_ty.clone());
                }

                // Let statements have unit type
                Type::Tuple(vec![]) // Unit type
            }

            _ => {
                // Default to error for unimplemented nodes
                return Err(format!("Type inference not implemented for: {:?}", node));
            }
        };

        self.last_type = Some(ty.clone());
        Ok(ty)
    }

    /// Get the current substitution
    pub fn substitution(&self) -> &Substitution {
        &self.substitution
    }

    /// Take the substitution (consumes self)
    pub fn take_substitution(self) -> Substitution {
        self.substitution
    }

    /// Solve all collected constraints
    pub fn solve(&mut self) -> Result<(), Vec<UnifyError>> {
        let mut errors = Vec::new();

        for (t1, t2) in self.constraints.drain(..) {
            if let Err(e) = self.substitution.unify(&t1, &t2) {
                errors.push(e);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Get final type of expression after solving constraints
    pub fn final_type(&self, node: &AstNode) -> Option<Type> {
        // Re-infer with current substitution applied
        let mut temp = self.clone();
        temp.infer(node).ok().map(|ty| self.substitution.apply(&ty))
    }
}

impl Clone for InferContext {
    fn clone(&self) -> Self {
        InferContext {
            variables: self.variables.clone(),
            substitution: self.substitution.clone(),
            constraints: self.constraints.clone(),
            last_type: self.last_type.clone(),
        }
    }
}

/// Type checking entry point
pub fn type_check(ast: &[AstNode]) -> Result<(), String> {
    let mut context = InferContext::new();

    // First pass: infer types and collect constraints
    for node in ast {
        if let Err(e) = context.infer(node) {
            return Err(format!("Type inference error: {}", e));
        }
    }

    // Second pass: solve constraints
    match context.solve() {
        Ok(()) => Ok(()),
        Err(errors) => {
            let error_msgs: Vec<String> =
                errors.iter().map(|e: &UnifyError| e.to_string()).collect();
            Err(format!("Type errors:\n{}", error_msgs.join("\n")))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::ast::AstNode;

    #[test]
    fn test_literal_inference() {
        let mut ctx = InferContext::new();

        // Integer literal
        let lit = AstNode::Lit(42);
        assert_eq!(ctx.infer(&lit).unwrap(), Type::I32);

        // Large integer
        let big_lit = AstNode::Lit(1_000_000_000_000);
        assert_eq!(ctx.infer(&big_lit).unwrap(), Type::I64);

        // String literal
        let str_lit = AstNode::StringLit("hello".to_string());
        assert_eq!(ctx.infer(&str_lit).unwrap(), Type::Str);

        // Boolean literal
        let bool_lit = AstNode::Bool(true);
        assert_eq!(ctx.infer(&bool_lit).unwrap(), Type::Bool);
    }

    #[test]
    fn test_binary_operations() {
        let mut ctx = InferContext::new();

        // 1 + 2
        let add = AstNode::BinaryOp {
            op: "+".to_string(),
            left: Box::new(AstNode::Lit(1)),
            right: Box::new(AstNode::Lit(2)),
        };
        assert_eq!(ctx.infer(&add).unwrap(), Type::I32);

        // 1 == 2
        let eq = AstNode::BinaryOp {
            op: "==".to_string(),
            left: Box::new(AstNode::Lit(1)),
            right: Box::new(AstNode::Lit(2)),
        };
        assert_eq!(ctx.infer(&eq).unwrap(), Type::Bool);

        // true && false
        let and = AstNode::BinaryOp {
            op: "&&".to_string(),
            left: Box::new(AstNode::Bool(true)),
            right: Box::new(AstNode::Bool(false)),
        };
        assert_eq!(ctx.infer(&and).unwrap(), Type::Bool);
    }

    #[test]
    fn test_variable_assignment() {
        let mut ctx = InferContext::new();

        // x = 42
        let assign = AstNode::Assign(
            Box::new(AstNode::Var("x".to_string())),
            Box::new(AstNode::Lit(42)),
        );
        assert_eq!(ctx.infer(&assign).unwrap(), Type::I32);

        // Now x should be defined
        let var = AstNode::Var("x".to_string());
        assert_eq!(ctx.infer(&var).unwrap(), Type::I32);
    }

    #[test]
    fn test_type_constraints() {
        let mut ctx = InferContext::new();

        // x = 1
        let assign1 = AstNode::Assign(
            Box::new(AstNode::Var("x".to_string())),
            Box::new(AstNode::Lit(1)),
        );
        ctx.infer(&assign1).unwrap();

        // x = 2 (should unify)
        let assign2 = AstNode::Assign(
            Box::new(AstNode::Var("x".to_string())),
            Box::new(AstNode::Lit(2)),
        );
        ctx.infer(&assign2).unwrap();

        // Solve constraints
        assert!(ctx.solve().is_ok());
    }

    #[test]
    fn test_type_error() {
        let mut ctx = InferContext::new();

        // x = 1
        let assign1 = AstNode::Assign(
            Box::new(AstNode::Var("x".to_string())),
            Box::new(AstNode::Lit(1)),
        );
        ctx.infer(&assign1).unwrap();

        // x = true (should fail)
        let assign2 = AstNode::Assign(
            Box::new(AstNode::Var("x".to_string())),
            Box::new(AstNode::Bool(true)),
        );
        ctx.infer(&assign2).unwrap();

        // Solve constraints - should fail
        assert!(ctx.solve().is_err());
    }
}
