//! New resolver with proper type system
//! Migration from string-based types to algebraic types

use crate::frontend::ast::AstNode;
use crate::middle::types::{Substitution, Type, TypeVar, UnifyError};
use std::collections::HashMap;

/// Type inference context
pub struct InferContext {
    /// Variable to type mapping
    variables: HashMap<String, Type>,

    /// Function signatures (name -> return type)
    functions: HashMap<String, Type>,

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
            functions: HashMap::new(),
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

    /// Parse type string to Type
    fn parse_type_string(&self, s: &str) -> Result<Type, String> {
        let s = s.trim();

        // Check for reference types
        if s.starts_with("&mut ") {
            let inner = s.trim_start_matches("&mut ").trim();
            let inner_ty = self.parse_type_string(inner)?;
            return Ok(Type::Ref(
                Box::new(inner_ty),
                crate::middle::types::Lifetime::Static,
                crate::middle::types::Mutability::Mutable,
            ));
        } else if s.starts_with("&") {
            let inner = s.trim_start_matches("&").trim();
            let inner_ty = self.parse_type_string(inner)?;
            return Ok(Type::Ref(
                Box::new(inner_ty),
                crate::middle::types::Lifetime::Static,
                crate::middle::types::Mutability::Immutable,
            ));
        }

        // Check for array/slice type
        if s.starts_with('[') {
            if !s.ends_with(']') {
                return Err("Array/slice type missing closing ']'".to_string());
            }

            let inner = &s[1..s.len() - 1]; // Remove brackets
            if let Some((type_part, size_part)) = inner.split_once(';') {
                let inner_type = self.parse_type_string(type_part.trim())?;
                let size = size_part
                    .trim()
                    .parse::<usize>()
                    .map_err(|e| format!("Invalid array size: {}", e))?;
                return Ok(Type::Array(Box::new(inner_type), size));
            } else {
                // Slice type: [T]
                let inner_type = self.parse_type_string(inner.trim())?;
                return Ok(Type::Slice(Box::new(inner_type)));
            }
        }

        // Check for tuple type: (T1, T2, T3)
        if s.starts_with('(') {
            if !s.ends_with(')') {
                return Err("Tuple type missing closing ')'".to_string());
            }

            let inner = &s[1..s.len() - 1]; // Remove parentheses
            if inner.is_empty() {
                // Empty tuple: ()
                return Ok(Type::Tuple(Vec::new()));
            }

            // Split by commas, but be careful about nested tuples
            let mut types = Vec::new();
            let mut current = String::new();
            let mut depth = 0;

            for ch in inner.chars() {
                match ch {
                    '(' => {
                        depth += 1;
                        current.push(ch);
                    }
                    ')' => {
                        depth -= 1;
                        current.push(ch);
                    }
                    ',' if depth == 0 => {
                        if !current.is_empty() {
                            types.push(self.parse_type_string(current.trim())?);
                            current.clear();
                        }
                    }
                    _ => current.push(ch),
                }
            }

            if !current.is_empty() {
                types.push(self.parse_type_string(current.trim())?);
            }

            return Ok(Type::Tuple(types));
        }

        // Check for Zeta's lt() syntax: lt(Result, i64)
        if s.starts_with("lt(") && s.ends_with(')') {
            let inner = &s[3..s.len() - 1]; // Remove "lt(" and ")"
            // Parse type name and arguments
            let parts: Vec<&str> = inner.split(',').map(|s| s.trim()).collect();
            if parts.is_empty() {
                return Ok(Type::Named(s.to_string(), Vec::new()));
            }

            let type_name = parts[0];
            let mut args = Vec::new();
            for arg in parts.iter().skip(1) {
                args.push(self.parse_type_string(arg)?);
            }

            return Ok(Type::Named(type_name.to_string(), args));
        }

        // Check for generic type: Vec<i32>, Option<T>, Result<T, E>
        // Look for < followed by > with content in between
        if let Some(open_angle) = s.find('<')
            && let Some(close_angle) = s.rfind('>')
            && open_angle < close_angle
        {
            let type_name = &s[..open_angle];
            let inner = &s[open_angle + 1..close_angle];

            // Parse type arguments, handling nested generics
            let mut args = Vec::new();
            let mut current = String::new();
            let mut depth = 0;

            for ch in inner.chars() {
                match ch {
                    '<' => {
                        depth += 1;
                        current.push(ch);
                    }
                    '>' => {
                        depth -= 1;
                        current.push(ch);
                    }
                    ',' if depth == 0 => {
                        if !current.is_empty() {
                            args.push(self.parse_type_string(current.trim())?);
                            current.clear();
                        }
                    }
                    _ => current.push(ch),
                }
            }

            if !current.is_empty() {
                args.push(self.parse_type_string(current.trim())?);
            }

            return Ok(Type::Named(type_name.to_string(), args));
        }

        // Check for function type: fn(T) -> R or (T) -> R
        if s.starts_with("fn(") || (s.starts_with('(') && s.contains("->")) {
            // Parse function type
            let s = s.trim();

            // Remove "fn" prefix if present
            let s = if s.starts_with("fn(") {
                &s[2..] // Keep the '('
            } else {
                s
            };

            // Find the arrow
            let arrow_pos = s.find("->").ok_or("Function type missing '->'")?;

            // Parse parameter types (could be single type or tuple)
            let params_str = &s[..arrow_pos].trim();

            // Remove parentheses if present
            let params_str = if params_str.starts_with('(') && params_str.ends_with(')') {
                &params_str[1..params_str.len() - 1]
            } else {
                params_str
            };

            // Parse return type
            let ret_str = &s[arrow_pos + 2..].trim();
            let ret_ty = self.parse_type_string(ret_str)?;

            // Parse parameter types
            let param_types = if params_str.is_empty() {
                Vec::new()
            } else {
                // Split by comma and parse each type
                let mut types = Vec::new();
                let mut current = String::new();
                let mut depth = 0;

                for ch in params_str.chars() {
                    match ch {
                        '(' | '<' | '[' => {
                            depth += 1;
                            current.push(ch);
                        }
                        ')' | '>' | ']' => {
                            depth -= 1;
                            current.push(ch);
                        }
                        ',' if depth == 0 => {
                            if !current.is_empty() {
                                types.push(self.parse_type_string(current.trim())?);
                                current.clear();
                            }
                        }
                        _ => current.push(ch),
                    }
                }

                if !current.is_empty() {
                    types.push(self.parse_type_string(current.trim())?);
                }

                types
            };

            return Ok(Type::Function(param_types, Box::new(ret_ty)));
        }

        // Handle base types
        match s {
            "i64" => Ok(Type::I64),
            "i32" => Ok(Type::I32),
            "bool" => Ok(Type::Bool),
            "str" => Ok(Type::Str),
            "f32" => Ok(Type::F32),
            "f64" => Ok(Type::F64),
            "char" => Ok(Type::Char),
            "u8" => Ok(Type::U8),
            "u16" => Ok(Type::U16),
            "u32" => Ok(Type::U32),
            "u64" => Ok(Type::U64),
            "String" => Ok(Type::Named("String".to_string(), Vec::new())),
            _ => {
                // Named type
                Ok(Type::Named(s.to_string(), Vec::new()))
            }
        }
    }

    /// Check a pattern against a type, registering variables in the context
    fn check_pattern(&mut self, pattern: &AstNode, expected_ty: &Type) -> Result<(), String> {
        match pattern {
            AstNode::Ignore => {
                // Wildcard pattern matches anything, no variables to register
                Ok(())
            }
            AstNode::Var(name) => {
                // Variable pattern: register the variable with the expected type
                self.declare(name.clone(), expected_ty.clone());
                Ok(())
            }
            AstNode::Tuple(patterns) => {
                // Tuple pattern: expected type must be a tuple
                match expected_ty {
                    Type::Tuple(element_types) => {
                        if patterns.len() != element_types.len() {
                            return Err(format!(
                                "Tuple pattern has {} elements but type has {}",
                                patterns.len(),
                                element_types.len()
                            ));
                        }
                        // Check each pattern against corresponding element type
                        for (i, (pattern, elem_ty)) in
                            patterns.iter().zip(element_types).enumerate()
                        {
                            self.check_pattern(pattern, elem_ty)
                                .map_err(|e| format!("In tuple element {}: {}", i, e))?;
                        }
                        Ok(())
                    }
                    _ => Err(format!(
                        "Tuple pattern used with non-tuple type: {:?}",
                        expected_ty
                    )),
                }
            }
            AstNode::StructPattern {
                variant,
                fields,
                rest: _,
            } => {
                // Struct pattern: we need to look up the struct definition
                // For now, we'll create a placeholder - in a real implementation,
                // we would look up the struct from the resolver

                // Check if it's a tuple struct (fields are indexed by string numbers)
                let is_tuple_struct = fields.iter().all(|(name, _)| name.parse::<usize>().is_ok());

                if is_tuple_struct {
                    // Tuple struct: create a tuple type
                    let field_types: Vec<Type> = fields
                        .iter()
                        .map(|(_, _pat)| {
                            // Create a fresh type variable for each field
                            Type::Variable(TypeVar::fresh())
                        })
                        .collect();

                    // Constrain the expected type to be a named type with the variant name
                    // and field types as type arguments
                    let struct_ty = Type::Named(variant.clone(), field_types.clone());
                    self.constrain(expected_ty.clone(), struct_ty);

                    // Check each pattern against its corresponding field type
                    for (i, (_, pat)) in fields.iter().enumerate() {
                        if i < field_types.len() {
                            self.check_pattern(pat, &field_types[i])?;
                        }
                    }
                } else {
                    // Named struct: for now, just create a named type
                    // In a real implementation, we would look up field types
                    let struct_ty = Type::Named(variant.clone(), vec![]);
                    self.constrain(expected_ty.clone(), struct_ty);

                    // Check each field pattern
                    for (_field_name, pat) in fields {
                        // Create a fresh type variable for this field
                        let field_ty = Type::Variable(TypeVar::fresh());
                        self.check_pattern(pat, &field_ty)?;
                    }
                }

                Ok(())
            }
            AstNode::Lit(_n) => {
                // Literal pattern: expected type must be i64
                self.constrain(expected_ty.clone(), Type::I64);
                Ok(())
            }
            AstNode::Bool(_b) => {
                // Boolean literal pattern
                self.constrain(expected_ty.clone(), Type::Bool);
                Ok(())
            }
            _ => Err(format!("Pattern type not supported yet: {:?}", pattern)),
        }
    }

    /// Infer type for AST node
    pub fn infer(&mut self, node: &AstNode) -> Result<Type, String> {
        let ty = match node {
            AstNode::Lit(_n) => {
                // Integer literals default to i64 (Zeta v0.5.0 standard)
                // This avoids type mismatches with const annotations
                Type::I64
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
                    let annotated_ty = self.parse_type_string(type_str)?;
                    self.constrain(expr_ty.clone(), annotated_ty.clone());
                    // Check pattern against annotated type
                    self.check_pattern(pattern, &annotated_ty)?;
                } else {
                    // Check pattern against inferred expression type
                    self.check_pattern(pattern, &expr_ty)?;
                }

                // Let statements have unit type
                Type::Tuple(vec![]) // Unit type
            }

            AstNode::ConstDef {
                name,
                ty,
                value,
                pub_: _,
            } => {
                // Parse the type string to Type
                let const_ty = self.parse_type_string(ty)?;

                // Infer type of the value expression
                let value_ty = self.infer(value)?;

                // Constrain value type to match const type
                self.constrain(value_ty, const_ty.clone());

                // Register constant in context
                self.declare(name.clone(), const_ty.clone());

                // Const definitions have unit type at top level
                Type::Tuple(vec![]) // Unit type
            }

            AstNode::FuncDef {
                name,
                params,
                ret,
                body,
                ret_expr,
                ..
            } => {
                // Parse return type
                let return_ty = self.parse_type_string(ret)?;

                // Register function signature
                self.functions.insert(name.clone(), return_ty.clone());

                // Add parameters to variable context
                for (param_name, param_type_str) in params {
                    let param_ty = self.parse_type_string(param_type_str)?;
                    self.declare(param_name.clone(), param_ty);
                }

                // Type check function body
                for stmt in body {
                    self.infer(stmt)?;
                }

                // Type check return expression if present
                if let Some(expr) = ret_expr {
                    let expr_ty = self.infer(expr)?;
                    // Constrain return expression type to match function return type
                    self.constrain(expr_ty, return_ty);
                }

                // Function definitions have unit type at top level
                Type::Tuple(vec![]) // Unit type
            }

            AstNode::Call {
                receiver,
                method,
                args: _,
                ..
            } => {
                if let Some(receiver_expr) = receiver {
                    // Method call: type check receiver and look up method
                    let _receiver_ty = self.infer(receiver_expr)?;

                    // For now, just return a fresh type variable for method calls
                    // In a complete implementation, we would look up the method
                    // from the receiver type's method table
                    Type::Variable(TypeVar::fresh())
                } else {
                    // Function call: look up function return type
                    if let Some(return_ty) = self.functions.get(method) {
                        return_ty.clone()
                    } else {
                        return Err(format!("Unknown function: {}", method));
                    }
                }
            }

            AstNode::StructDef { name, fields, .. } => {
                // Register struct type in context
                // For now, we'll just create a named type with no type parameters
                let _struct_ty = Type::Named(name.clone(), Vec::new());

                // Store field types for later use
                // We need to parse field type strings to Type objects
                let mut _field_types = Vec::new();
                for (field_name, type_str) in fields {
                    let field_ty = self.parse_type_string(type_str)?;
                    // We could store this in a separate map for field access
                    _field_types.push((field_name.clone(), field_ty));
                }

                // Store struct definition in context for later use
                // For now, we'll just return unit type since struct definitions
                // don't have a value type at the top level
                Type::Tuple(vec![]) // Unit type
            }

            AstNode::EnumDef { name, variants, .. } => {
                // Register enum type in context
                // For now, we'll just create a named type with no type parameters
                let _enum_ty = Type::Named(name.clone(), Vec::new());

                // Store variant information for later use
                // We could register variant constructors as functions here
                for (variant_name, variant_params) in variants {
                    // Create a function name like "EnumName::VariantName"
                    let func_name = format!("{}::{}", name, variant_name);

                    // Create return type for the variant constructor
                    // For variants with parameters, create a function type
                    if variant_params.is_empty() {
                        // Nullary variant: () -> Enum
                        let ret_type = Type::Named(name.clone(), Vec::new());
                        self.functions.insert(func_name, ret_type);
                    } else {
                        // Variant with parameters: (T1, T2, ...) -> Enum
                        // Parse the parameter types
                        let mut param_types = Vec::new();
                        for param_str in variant_params {
                            // Try to parse the type string
                            match self.parse_type_string(param_str) {
                                Ok(ty) => param_types.push(ty),
                                Err(_) => {
                                    // If parsing fails, use a fresh type variable
                                    param_types.push(Type::Variable(TypeVar::fresh()));
                                }
                            }
                        }

                        // The return type is just the enum (non-generic)
                        let ret_type = Type::Named(name.clone(), Vec::new());

                        // The variant constructor is a function from param_types to ret_type
                        let func_type = Type::Function(param_types, Box::new(ret_type));
                        self.functions.insert(func_name, func_type);
                    }
                }

                // Enum definitions have unit type at top level
                Type::Tuple(vec![]) // Unit type
            }

            AstNode::StructLit { variant, fields } => {
                // Look up struct type
                let struct_ty = Type::Named(variant.clone(), Vec::new());

                // Type check each field
                for (_field_name, field_expr) in fields {
                    let _field_ty = self.infer(field_expr)?;
                    // TODO: Look up expected field type from struct definition
                    // and constrain field_ty to match
                }

                struct_ty
            }

            AstNode::FieldAccess {
                base,
                field: _field,
            } => {
                let _base_ty = self.infer(base)?;

                // For now, return a fresh type variable for field access
                // In a complete implementation, we would look up the field type
                // from the struct definition
                Type::Variable(TypeVar::fresh())
            }

            AstNode::PathCall {
                path,
                method,
                args: _,
            } => {
                // Path call like Point::new(10, 20)
                // Try qualified name first: Type::method
                let qualified_name = format!("{}::{}", path.join("::"), method);

                // Try qualified name first
                if let Some(return_ty) = self.functions.get(&qualified_name) {
                    return_ty.clone()
                } else if let Some(return_ty) = self.functions.get(method.as_str()) {
                    // Fall back to simple name
                    return_ty.clone()
                } else {
                    return Err(format!("Unknown function: {}", qualified_name));
                }
            }

            AstNode::ImplBlock { body, .. } => {
                // Process functions in impl block to register them
                for func in body {
                    self.infer(func)?;
                }
                // Impl blocks don't have a type, they're declarations
                // Return unit type
                Type::Tuple(vec![])
            }

            AstNode::Closure { params, body } => {
                // Create fresh type variables for parameters
                let param_types: Vec<Type> = params
                    .iter()
                    .map(|_| Type::Variable(TypeVar::fresh()))
                    .collect();

                // Create a fresh type variable for the return type
                let return_type_var = Type::Variable(TypeVar::fresh());

                // Enter a new scope for closure parameters
                let mut inner_ctx = self.clone();

                // Declare parameters in the inner context
                for (param_name, param_ty) in params.iter().zip(param_types.iter()) {
                    inner_ctx.declare(param_name.clone(), param_ty.clone());
                }

                // Infer the body type in the inner context
                let body_ty = inner_ctx.infer(body)?;

                // Constrain the body type to be the return type
                inner_ctx.constrain(body_ty, return_type_var.clone());

                // Solve constraints in the inner context
                if let Err(errors) = inner_ctx.solve() {
                    let error_msgs: Vec<String> = errors.iter().map(|e| e.to_string()).collect();
                    return Err(format!("Closure type error: {}", error_msgs.join(", ")));
                }

                // Apply substitution to get final parameter and return types
                let final_param_types: Vec<Type> = param_types
                    .iter()
                    .map(|ty| inner_ctx.substitution.apply(ty))
                    .collect();
                let final_return_type = inner_ctx.substitution.apply(&return_type_var);

                // The closure type is a function type
                Type::Function(final_param_types, Box::new(final_return_type))
            }

            AstNode::IfLet {
                pattern,
                expr,
                then,
                else_,
            } => {
                // Type check the expression
                let expr_ty = self.infer(expr)?;

                // Check the pattern against the expression type
                self.check_pattern(pattern, &expr_ty)?;

                // Type check the then branch
                let mut then_ty = Type::Tuple(vec![]); // Default to unit
                for stmt in then {
                    then_ty = self.infer(stmt)?;
                }

                // Type check the else branch if present
                let mut else_ty = Type::Tuple(vec![]); // Default to unit
                for stmt in else_ {
                    else_ty = self.infer(stmt)?;
                }

                // Both branches must have the same type
                self.constrain(then_ty.clone(), else_ty.clone());

                // The if-let expression has the type of its branches
                then_ty
            }

            AstNode::Match { scrutinee, arms } => {
                // Type check the scrutinee
                let scrutinee_ty = self.infer(scrutinee)?;

                // Type check each arm
                let mut arm_types = Vec::new();
                for arm in arms {
                    // Check pattern against scrutinee type
                    self.check_pattern(&arm.pattern, &scrutinee_ty)?;

                    // Type check guard if present
                    if let Some(guard) = &arm.guard {
                        let guard_ty = self.infer(guard)?;
                        self.constrain(guard_ty, Type::Bool);
                    }

                    // Type check body
                    let body_ty = self.infer(&arm.body)?;
                    arm_types.push(body_ty);
                }

                // All arms must have the same type
                if let Some(first_ty) = arm_types.first() {
                    for other_ty in arm_types.iter().skip(1) {
                        self.constrain(first_ty.clone(), other_ty.clone());
                    }
                    first_ty.clone()
                } else {
                    // Empty match - return unit type
                    Type::Tuple(vec![])
                }
            }

            AstNode::Use { .. } => {
                // Use statements are processed by the resolver before type inference
                // They don't have a type themselves
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
            functions: self.functions.clone(),
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

        // Integer literal - now defaults to i64 (Zeta v0.5.0 standard)
        let lit = AstNode::Lit(42);
        assert_eq!(ctx.infer(&lit).unwrap(), Type::I64);

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

        // 1 + 2 - now defaults to i64 (Zeta v0.5.0 standard)
        let add = AstNode::BinaryOp {
            op: "+".to_string(),
            left: Box::new(AstNode::Lit(1)),
            right: Box::new(AstNode::Lit(2)),
        };
        assert_eq!(ctx.infer(&add).unwrap(), Type::I64);

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

        // x = 42 - now defaults to i64 (Zeta v0.5.0 standard)
        let assign = AstNode::Assign(
            Box::new(AstNode::Var("x".to_string())),
            Box::new(AstNode::Lit(42)),
        );
        assert_eq!(ctx.infer(&assign).unwrap(), Type::I64);

        // Now x should be defined
        let var = AstNode::Var("x".to_string());
        assert_eq!(ctx.infer(&var).unwrap(), Type::I64);
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

    #[test]
    fn test_complex_type_parsing() {
        let ctx = InferContext::new();

        // Test array types
        assert_eq!(
            ctx.parse_type_string("[i32; 10]").unwrap(),
            Type::Array(Box::new(Type::I32), 10)
        );
        assert_eq!(
            ctx.parse_type_string("[bool; 5]").unwrap(),
            Type::Array(Box::new(Type::Bool), 5)
        );

        // Test slice types
        assert_eq!(
            ctx.parse_type_string("[i64]").unwrap(),
            Type::Slice(Box::new(Type::I64))
        );
        assert_eq!(
            ctx.parse_type_string("[&str]").unwrap(),
            Type::Slice(Box::new(Type::Ref(
                Box::new(Type::Str),
                crate::middle::types::Lifetime::Static,
                crate::middle::types::Mutability::Immutable
            )))
        );

        // Test tuple types
        assert_eq!(
            ctx.parse_type_string("()").unwrap(),
            Type::Tuple(Vec::new())
        );
        assert_eq!(
            ctx.parse_type_string("(i32, bool)").unwrap(),
            Type::Tuple(vec![Type::I32, Type::Bool])
        );

        // Test nested types
        assert_eq!(
            ctx.parse_type_string("[(i32, bool); 3]").unwrap(),
            Type::Array(Box::new(Type::Tuple(vec![Type::I32, Type::Bool])), 3)
        );

        // Test error cases
        assert!(ctx.parse_type_string("[i32; not_a_number]").is_err());
        assert!(ctx.parse_type_string("[i32;").is_err());
        assert!(ctx.parse_type_string("(i32, bool").is_err());
    }
}
