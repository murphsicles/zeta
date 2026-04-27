//! New resolver with proper type system
//! Migration from string-based types to algebraic types

use crate::frontend::ast::AstNode;
use crate::middle::types::{
    ArraySize, GenericContext, Kind, Substitution, TraitBound, Type, TypeParam, TypeVar, UnifyError,
};
use crate::middle::types::identity::{CapabilityLevel, IdentityType};
use std::collections::HashMap;

/// Type inference constraint
#[derive(Debug, Clone)]
pub enum Constraint {
    Equality(Type, Type),
    Bound(Type, TraitBound),
}

/// Type inference context
pub struct InferContext {
    /// Variable to type mapping
    variables: HashMap<String, Type>,

    /// Function signatures (name -> return type)
    functions: HashMap<String, Type>,

    /// Type definitions (structs, enums, etc.) - name -> type constructor
    types: HashMap<String, Type>,

    /// Current substitution
    substitution: Substitution,

    /// Collected constraints
    constraints: Vec<Constraint>,

    /// Generic context for type parameters
    generic_context: GenericContext,

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
        let mut ctx = InferContext {
            variables: HashMap::new(),
            functions: HashMap::new(),
            types: HashMap::new(),
            substitution: Substitution::new(),
            constraints: Vec::new(),
            generic_context: GenericContext::new(),
            last_type: None,
        };

        // Register built-in generic types
        ctx.register_builtin_generics();

        ctx
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

    /// Add a function signature
    pub fn add_function(&mut self, name: String, ty: Type) {
        self.functions.insert(name, ty);
    }

    /// Add type constraint
    pub fn constrain(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }

    /// Helper to add equality constraint (backward compatibility)
    #[track_caller]
    fn constrain_eq(&mut self, t1: Type, t2: Type) {
        let location = std::panic::Location::caller();
        self.constrain(Constraint::Equality(t1, t2));
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

        // Check for trait object: dyn Trait
        if s.starts_with("dyn ") {
            let inner = s.trim_start_matches("dyn ").trim();
            // Parse the trait name (could be a path like std::fmt::Debug)
            return Ok(Type::TraitObject(inner.to_string()));
        }

        // Check for array/slice type
        if s.starts_with('[') {
            // Check for dynamic array: [dynamic]T (special case, doesn't end with ])
            if s.starts_with("[dynamic]") {
                let type_part = &s["[dynamic]".len()..];
                let inner_type = self.parse_type_string(type_part.trim())?;
                return Ok(Type::DynamicArray(Box::new(inner_type)));
            }
            
            // Regular array/slice must end with ]
            if !s.ends_with(']') {
                return Err("Array/slice type missing closing ']'".to_string());
            }

            let inner = &s[1..s.len() - 1]; // Remove brackets
            
            if let Some((type_part, size_part)) = inner.split_once(';') {
                let inner_type = self.parse_type_string(type_part.trim())?;
                let size_str = size_part.trim();
                
                // Try to parse as literal usize first
                if let Ok(size) = size_str.parse::<usize>() {
                    return Ok(Type::Array(Box::new(inner_type), crate::middle::types::ArraySize::Literal(size)));
                }
                
                // If not a literal, treat as const parameter
                return Ok(Type::Array(Box::new(inner_type), crate::middle::types::ArraySize::ConstParam(size_str.to_string())));
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

        // Check for Vector<T, N> type (special case - second argument is a number)
        if s.starts_with("Vector<") && s.ends_with('>') {
            let inner = &s[7..s.len() - 1]; // Remove "Vector<" and ">"
            
            // Parse element type and size
            let mut parts = Vec::new();
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
                            parts.push(current.trim().to_string());
                            current.clear();
                        }
                    }
                    _ => current.push(ch),
                }
            }
            
            if !current.is_empty() {
                parts.push(current.trim().to_string());
            }
            
            if parts.len() != 2 {
                return Err(format!("Vector type expects 2 arguments (element type and size), got {}", parts.len()));
            }
            
            let elem_type = self.parse_type_string(&parts[0])?;
            let size = parts[1].parse::<usize>()
                .map_err(|e| format!("Invalid vector size '{}': {}", parts[1], e))?;
            
            return Ok(Type::Vector(Box::new(elem_type), ArraySize::Literal(size)));
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

        // Check for identity types: string[identity:read] or string[identity:read+write]
        if s.starts_with("string[identity:") && s.ends_with(']') {
            // Extract capabilities from between [identity: and ]
            let start_idx = "string[identity:".len();
            let capabilities_str = &s[start_idx..s.len() - 1];
            
            // Parse capabilities
            let mut capabilities = Vec::new();
            let capability_parts: Vec<&str> = capabilities_str.split('+').map(|p| p.trim()).collect();
            
            for cap_str in capability_parts {
                match cap_str.to_lowercase().as_str() {
                    "immutable" => capabilities.push(CapabilityLevel::Immutable),
                    "read" => capabilities.push(CapabilityLevel::Read),
                    "write" => capabilities.push(CapabilityLevel::Write),
                    "execute" => capabilities.push(CapabilityLevel::Execute),
                    "owned" => capabilities.push(CapabilityLevel::Owned),
                    _ => return Err(format!("Unknown capability: {}", cap_str)),
                }
            }
            
            // Create identity type
            let identity_type = IdentityType::new(capabilities);
            
            return Ok(Type::Identity(Box::new(identity_type)));
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
            "usize" => Ok(Type::Usize),
            "String" => Ok(Type::Named("String".to_string(), Vec::new())),
            _ => {
                // Check if it's a single uppercase letter (type variable)
                if s.len() == 1 && s.chars().next().unwrap().is_ascii_uppercase() {
                    // Check if it's a type parameter in the current generic context
                    if self.generic_context.find_type_param(s).is_some() {
                        // Create a fresh type variable for this type parameter
                        Ok(Type::Variable(TypeVar::fresh()))
                    } else {
                        // Not a known type parameter, treat as named type
                        Ok(Type::Named(s.to_string(), Vec::new()))
                    }
                } else {
                    // Check if it's a type alias
                    if let Some(aliased_ty) = self.types.get(s) {
                        // Return the underlying type
                        Ok(aliased_ty.clone())
                    } else {
                        // Named type
                        Ok(Type::Named(s.to_string(), Vec::new()))
                    }
                }
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
                    self.constrain_eq(expected_ty.clone(), struct_ty);

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
                    self.constrain_eq(expected_ty.clone(), struct_ty);

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
                self.constrain_eq(expected_ty.clone(), Type::I64);
                Ok(())
            }
            AstNode::Bool(_b) => {
                // Boolean literal pattern
                self.constrain_eq(expected_ty.clone(), Type::Bool);
                Ok(())
            }
            AstNode::TypeAnnotatedPattern { pattern, ty } => {
                // Type-annotated pattern: check that the pattern matches the annotated type,
                // and that the annotated type is compatible with the expected type
                
                // First, parse the type string to get a Type
                let annotated_ty = self.parse_type_string(ty)?;
                
                // Check that the annotated type is compatible with the expected type
                self.constrain_eq(expected_ty.clone(), annotated_ty.clone());
                
                // Check the pattern against the annotated type
                self.check_pattern(pattern, &annotated_ty)
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
                Ok(Type::I64)
            }

            AstNode::FloatLit(_) => {
                // Float literals default to f64
                Ok(Type::F64)
            }

            AstNode::StringLit(_) => Ok(Type::Str),

            AstNode::Bool(_b) => Ok(Type::Bool),

            AstNode::ArrayLit(elements) => {
                if elements.is_empty() {
                    // Empty array - we don't know the element type
                    // Create a type variable for the element type
                    let elem_var = Type::Variable(TypeVar::fresh());
                    Ok(Type::Array(Box::new(elem_var), crate::middle::types::ArraySize::Literal(0)))
                } else {
                    // Infer type of first element
                    let first_ty = self.infer(&elements[0])?;
                    
                    // Constrain all elements to have the same type
                    for element in elements.iter().skip(1) {
                        let elem_ty = self.infer(element)?;
                        self.constrain_eq(first_ty.clone(), elem_ty);
                    }
                    
                    Ok(Type::Array(Box::new(first_ty), crate::middle::types::ArraySize::Literal(elements.len())))
                }
            },

            AstNode::ArrayRepeat { value, size } => {
                // Infer type of the value
                let value_ty = self.infer(value)?;
                
                // Infer type of the size (should be integer)
                let size_ty = self.infer(size)?;
                
                // Size should be an integer type
                self.constrain_eq(size_ty, Type::I64);
                
                // Try to get the size value if it's a literal
                let array_size = if let AstNode::Lit(size_val) = size.as_ref() {
                    // Convert i64 to usize
                    crate::middle::types::ArraySize::Literal(*size_val as usize)
                } else {
                    // Not a literal, use 0 as placeholder
                    crate::middle::types::ArraySize::Literal(0)
                };
                
                // Return array type with repeated value
                Ok(Type::Array(Box::new(value_ty), array_size))
            },

            AstNode::DynamicArrayLit { elem_type, elements } => {
                // Parse the element type string into a Type
                let parsed_type = self.parse_type_string(elem_type)
                    .map_err(|e| format!("Failed to parse type '{}': {}", elem_type, e))?;
                
                // Check that all elements have the correct type
                for element in elements {
                    let elem_ty = self.infer(element)?;
                    self.constrain_eq(parsed_type.clone(), elem_ty);
                }
                
                Ok(Type::DynamicArray(Box::new(parsed_type)))
            },

            AstNode::Var(name) => Ok(self
                .lookup(name)
                .ok_or_else(|| format!("Undefined variable: {}", name))?),

            AstNode::BinaryOp { op, left, right } => {
                let left_ty = self.infer(left)?;
                let right_ty = self.infer(right)?;

                // Constraint: left and right types must unify
                self.constrain_eq(left_ty.clone(), right_ty.clone());

                // Determine result type based on operator
                match op.as_str() {
                    "+" | "-" | "*" | "/" | "%" => {
                        // Numeric operations return same type
                        Ok(left_ty)
                    }
                    "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                        // Comparisons return bool
                        Ok(Type::Bool)
                    }
                    "&&" | "||" => {
                        // Logical operators require bool operands
                        self.constrain_eq(left_ty.clone(), Type::Bool);
                        self.constrain_eq(right_ty.clone(), Type::Bool);
                        Ok(Type::Bool)
                    }
                    "&" | "|" | "^" | "<<" | ">>" => {
                        // Bitwise operations
                        Ok(left_ty)
                    }
                    ".." => {
                        // Range operator - creates a Range type
                        // Both sides should be numeric types
                        // For now, we'll accept any numeric type and return Range
                        Ok(Type::Range)
                    }
                    _ => return Err(format!("Unknown operator: {}", op)),
                }
            }

            AstNode::UnaryOp { op, expr } => {
                let expr_ty = self.infer(expr)?;

                match op.as_str() {
                    "-" => {
                        // Numeric negation
                        Ok(expr_ty)
                    }
                    "!" => {
                        // Logical NOT - requires bool
                        self.constrain_eq(expr_ty.clone(), Type::Bool);
                        Ok(Type::Bool)
                    }
                    "~" => {
                        // Bitwise NOT
                        Ok(expr_ty)
                    }
                    _ => return Err(format!("Unknown unary operator: {}", op)),
                }
            }

            AstNode::Await(expr) => {
                // Await expression: expr.await where expr must be a Future
                let expr_ty = self.infer(expr)?;
                
                // Check if the expression type is a Future
                // For now, we'll assume any async function returns a Future
                // In a complete implementation, we would check if expr_ty implements Future trait
                
                // The await expression returns the inner type of the Future
                // For simplicity, we'll return the expression type itself for now
                // TODO: Implement proper Future trait checking
                Ok(expr_ty)
            }

            AstNode::Cast { expr, ty } => {
                let expr_ty = self.infer(expr)?;
                let target_ty = self.parse_type_string(ty)?;
                
                // Check if conversion is valid
                // For now, allow all numeric conversions
                // TODO: Add proper conversion validation
                Ok(target_ty)
            }

            AstNode::Assign(lhs, rhs) => {
                let rhs_ty = self.infer(rhs)?;

                match &**lhs {
                    AstNode::Var(name) => {
                        // Declare or update variable type
                        if let Some(existing_ty) = self.variables.get(name) {
                            // Existing variable - constrain types
                            let existing_ty: &Type = existing_ty;
                            self.constrain_eq(existing_ty.clone(), rhs_ty.clone());
                        } else {
                            // New variable
                            self.declare(name.clone(), rhs_ty.clone());
                        }
                        Ok(rhs_ty)
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
                    self.constrain_eq(expr_ty.clone(), annotated_ty.clone());
                    // Check pattern against annotated type
                    self.check_pattern(pattern, &annotated_ty)?;
                } else {
                    // Check pattern against inferred expression type
                    self.check_pattern(pattern, &expr_ty)?;
                }

                // Let statements have unit type
                Ok(Type::Tuple(vec![])) // Unit type
            }

            AstNode::ConstDef {
                name,
                ty,
                value,
                attrs: _,
                pub_: _,
                comptime_: _,
            } => {
                // Parse the type string to Type
                let const_ty = self.parse_type_string(ty)?;

                // Infer type of the value expression
                let value_ty = self.infer(value)?;

                // Constrain value type to match const type
                self.constrain_eq(value_ty, const_ty.clone());

                // Register constant in context
                self.declare(name.clone(), const_ty.clone());

                // Const definitions have unit type at top level
                Ok(Type::Tuple(vec![])) // Unit type
            }

            AstNode::FuncDef {
                name,
                generics,
                params,
                ret,
                body,
                ret_expr,
                async_,
                comptime_,
                ..
            } => {
                // Parse generic type parameters if present
                let type_params = if !generics.is_empty() {
                    // Parse "T: Clone + Copy" style bounds
                    // Convert GenericParam to strings
                    let generic_strings: Vec<String> = generics.iter().map(|gp| {
                        match gp {
                            crate::frontend::ast::GenericParam::Type { name, bounds } => {
                                if bounds.is_empty() {
                                    name.clone()
                                } else {
                                    format!("{}: {}", name, bounds.join(" + "))
                                }
                            }
                            crate::frontend::ast::GenericParam::Lifetime { name } => name.clone(),
                            crate::frontend::ast::GenericParam::Const { name, ty } => format!("const {}: {}", name, ty),
                        }
                    }).collect();
                    self.parse_generic_params(&generic_strings)?
                } else {
                    Vec::new()
                };

                // Enter generic scope
                self.enter_generic_scope(type_params);

                // Parse return type in generic context
                let return_ty = self.parse_type_string(ret)?;

                // Handle comptime functions
                if *comptime_ {
                    // For comptime functions, we need to ensure all expressions
                    // can be evaluated at compile time
                    // TODO: Implement proper compile-time evaluation checking
                    // For now, we just mark it as comptime and continue
                    eprintln!("Note: Comptime function '{}' - compile-time evaluation not fully implemented yet", name);
                }

                // Register function signature as a proper function type
                // Collect parameter types
                let mut param_types = Vec::new();
                for (_, param_type_str) in params {
                    let param_ty = self.parse_type_string(param_type_str)?;
                    param_types.push(param_ty);
                }

                // Create function type: (param_types...) -> return_ty
                // Use AsyncFunction for async functions
                let func_type = if *async_ {
                    Type::AsyncFunction(param_types, Box::new(return_ty.clone()))
                } else {
                    Type::Function(param_types, Box::new(return_ty.clone()))
                };
                self.functions.insert(name.clone(), func_type);

                // Add parameters to variable context
                for (param_name, param_type_str) in params {
                    let param_ty = self.parse_type_string(param_type_str)?;
                    self.declare(param_name.clone(), param_ty);
                }

                // Type check function body
                let len = body.len();
                let mut last_stmt_type = Type::Tuple(vec![]); // default unit
                
                for (i, stmt) in body.iter().enumerate() {
                    let is_last = i == len - 1;
                    let stmt_type = self.infer(stmt)?;
                    
                    if is_last {
                        // For the last statement, check if it's an expression statement
                        // that should be treated as implicit return
                        match stmt {
                            AstNode::ExprStmt { expr: _ } => {
                                // Expression statement normally returns unit,
                                // but for implicit return we need the expression's type
                                // We already have stmt_type which is unit.
                                // We need to get the expression type separately.
                                // For now, we'll handle this in the constraint below.
                                last_stmt_type = stmt_type;
                            }
                            _ => {
                                last_stmt_type = stmt_type;
                            }
                        }
                    }
                }

                // Type check return expression if present
                if let Some(expr) = ret_expr {
                    let expr_ty = self.infer(expr)?;
                    // Constrain return expression type to match function return type
                    self.constrain_eq(expr_ty, return_ty);
                } else if !body.is_empty() {
                    // Implicit return: constrain last statement type to match function return type
                    // If last statement is ExprStmt, we need to get the expression type
                    let last_node = &body[len - 1];
                    let implicit_return_ty = match last_node {
                        AstNode::ExprStmt { expr } => {
                            // Re-infer the expression type (we already have it from earlier inference)
                            // But we need to get the type of the expression, not the statement
                            let expr_ty = self.infer(expr)?;
                            expr_ty
                        }
                        AstNode::Return(expr) => {
                            // Return statement: use the inner expression's type
                            let expr_ty = self.infer(expr)?;
                            expr_ty
                        }
                        _ => {
                            last_stmt_type
                        },
                    };
                    self.constrain_eq(implicit_return_ty, return_ty);
                }

                // Exit generic scope
                self.exit_generic_scope();

                // Function definitions have unit type at top level
                Ok(Type::Tuple(vec![])) // Unit type
            }

            AstNode::Call {
                receiver,
                method,
                args,
                type_args,
                ..
            } => {
                if let Some(receiver_expr) = receiver {
                    // Method call: type check receiver and look up method
                    let _receiver_ty = self.infer(receiver_expr)?;

                    // For now, just return a fresh type variable for method calls
                    // In a complete implementation, we would look up the method
                    // from the receiver type's method table
                    Ok(Type::Variable(TypeVar::fresh()))
                } else {
                    // Function call: look up function return type
                    if !type_args.is_empty() {
                        // Generic function call
                        let parsed_type_args: Result<Vec<Type>, String> = type_args
                            .iter()
                            .map(|s| self.parse_type_string(s))
                            .collect();

                        self.infer_generic_call(method, &parsed_type_args?, args)
                    } else {
                        // Regular function call
                        // Get function type first to avoid borrow issues
                        let func_type = self.functions.get(method).cloned();

                        if let Some(return_ty) = func_type {
                            // Check if it's a function type that needs argument checking
                            match return_ty {
                                Type::Function(param_types, ret_ty) => {
                                    // Check arguments
                                    if args.len() != param_types.len() {
                                        return Err(format!(
                                            "Wrong number of arguments: expected {}, got {}",
                                            param_types.len(),
                                            args.len()
                                        ));
                                    }

                                    // Clone the return type before mutable borrows
                                    let ret_ty_clone = *ret_ty.clone();

                                    // Type check each argument
                                    for (i, (arg, param_ty)) in
                                        args.iter().zip(param_types.iter()).enumerate()
                                    {
                                        let arg_ty = self.infer(arg)?;
                                        self.constrain_eq(arg_ty, param_ty.clone());
                                    }

                                    Ok(ret_ty_clone)
                                }
                                _ => Ok(return_ty.clone()),
                            }
                        } else {
                            return Err(format!("Unknown function: {}", method));
                        }
                    }
                }
            }

            AstNode::StructDef {
                name,
                generics,
                fields,
                ..
            } => {
                // Parse generic type parameters if present
                let type_params = if !generics.is_empty() {
                    // Parse "T: Clone + Copy" style bounds
                    // Convert GenericParam to strings
                    let generic_strings: Vec<String> = generics.iter().map(|gp| {
                        match gp {
                            crate::frontend::ast::GenericParam::Type { name, bounds } => {
                                if bounds.is_empty() {
                                    name.clone()
                                } else {
                                    format!("{}: {}", name, bounds.join(" + "))
                                }
                            }
                            crate::frontend::ast::GenericParam::Lifetime { name } => name.clone(),
                            crate::frontend::ast::GenericParam::Const { name, ty } => format!("const {}: {}", name, ty),
                        }
                    }).collect();
                    self.parse_generic_params(&generic_strings)?
                } else {
                    Vec::new()
                };

                // Enter generic scope for the struct definition
                self.enter_generic_scope(type_params);

                // Create type variables for each generic parameter
                let mut type_args = Vec::new();
                for _ in 0..generics.len() {
                    type_args.push(Type::Variable(TypeVar::fresh()));
                }

                // Create the struct type constructor: Pair<T1, T2> where T1, T2 are type variables
                let struct_ty = Type::Named(name.clone(), type_args.clone());

                // Register struct type in context for type checking
                self.types.insert(name.clone(), struct_ty);

                // Store field types for later use (for field access)
                // We need to parse field type strings to Type objects
                let mut _field_types = Vec::new();
                for (field_name, type_str) in fields {
                    let field_ty = self.parse_type_string(type_str)?;
                    // We could store this in a separate map for field access
                    _field_types.push((field_name.clone(), field_ty));
                }

                // Exit generic scope
                self.exit_generic_scope();

                // Struct definitions don't have a value type at the top level
                Ok(Type::Tuple(vec![])) // Unit type
            }

            AstNode::EnumDef {
                name,
                generics,
                variants,
                ..
            } => {
                // Parse generic type parameters if present
                let type_params = if !generics.is_empty() {
                    // Parse "T: Clone + Copy" style bounds
                    // Convert GenericParam to strings
                    let generic_strings: Vec<String> = generics.iter().map(|gp| {
                        match gp {
                            crate::frontend::ast::GenericParam::Type { name, bounds } => {
                                if bounds.is_empty() {
                                    name.clone()
                                } else {
                                    format!("{}: {}", name, bounds.join(" + "))
                                }
                            }
                            crate::frontend::ast::GenericParam::Lifetime { name } => name.clone(),
                            crate::frontend::ast::GenericParam::Const { name, ty } => format!("const {}: {}", name, ty),
                        }
                    }).collect();
                    self.parse_generic_params(&generic_strings)?
                } else {
                    Vec::new()
                };

                // Enter generic scope for the enum definition
                self.enter_generic_scope(type_params);

                // Create type variables for each generic parameter
                let mut type_args = Vec::new();
                for _ in 0..generics.len() {
                    type_args.push(Type::Variable(TypeVar::fresh()));
                }

                // Create the enum type constructor: EnumName<T1, T2> where T1, T2 are type variables
                let enum_ty = Type::Named(name.clone(), type_args.clone());

                // Register enum type in context for type checking
                self.types.insert(name.clone(), enum_ty);

                // Store variant information for later use
                // We could register variant constructors as functions here
                for (variant_name, variant_params) in variants {
                    // Create a function name like "EnumName::VariantName"
                    let func_name = format!("{}::{}", name, variant_name);

                    // Create return type for the variant constructor
                    // For variants with parameters, create a function type
                    if variant_params.is_empty() {
                        // Nullary variant: () -> Enum
                        let ret_type = Type::Named(name.clone(), type_args.clone());
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

                        // The return type is the enum with its type arguments
                        let ret_type = Type::Named(name.clone(), type_args.clone());

                        // The variant constructor is a function from param_types to ret_type
                        let func_type = Type::Function(param_types, Box::new(ret_type));
                        self.functions.insert(func_name, func_type);
                    }
                }

                // Exit generic scope
                self.exit_generic_scope();

                // Enum definitions have unit type at top level
                Ok(Type::Tuple(vec![])) // Unit type
            }

            AstNode::StructLit { variant, fields } => {
                // Look up struct type definition
                // For generic structs, we need to create fresh type variables for type arguments
                // For now, create a named type with fresh type variables
                // TODO: Look up actual struct definition to get correct number of type parameters
                let struct_ty = if let Some(defined_ty) = self.types.get(variant) {
                    // Clone the type definition
                    defined_ty.clone()
                } else {
                    // Unknown struct - create with no type arguments
                    // This will cause arity mismatch if the struct is actually generic
                    Type::Named(variant.clone(), Vec::new())
                };

                // Type check each field
                for (_field_name, field_expr) in fields {
                    let _field_ty = self.infer(field_expr)?;
                    // TODO: Look up expected field type from struct definition
                    // and constrain field_ty to match
                }

                Ok(struct_ty)
            }

            AstNode::FieldAccess {
                base,
                field: _field,
            } => {
                let _base_ty = self.infer(base)?;

                // For now, return a fresh type variable for field access
                // In a complete implementation, we would look up the field type
                // from the struct definition
                Ok(Type::Variable(TypeVar::fresh()))
            }

            AstNode::Subscript { base, index } => {
                let base_ty = self.infer(base)?;
                let index_ty = self.infer(index)?;
                
                // Constrain index to be integer
                self.constrain_eq(index_ty, Type::I64);
                
                // Create a fresh type variable for the element type
                let elem_var = Type::Variable(TypeVar::fresh());
                
                // Constrain base type to be an array type with element type elem_var
                // We don't care about the size for subscripting
                // Create a fresh type variable to represent any array size
                // Actually, we can't use a type variable for size since Type::Array uses usize
                // So we'll create an array type with a fresh size variable that will unify with any size
                // For now, we'll use 0 as a wildcard size that should unify with any concrete size
                // This is a hack - we need a better solution
                let array_type = Type::Array(Box::new(elem_var.clone()), ArraySize::Literal(0));
                self.constrain_eq(base_ty, array_type);
                
                // Return the element type
                Ok(elem_var)
            }

            AstNode::PathCall {
                path,
                method,
                args,
                type_args,
            } => {
                // Path call like Point::new(10, 20)
                // Try qualified name first: Type::method
                let qualified_name = format!("{}::{}", path.join("::"), method);

                // Get a clone of the function type before we start mutating self
                let func_ty = if let Some(ty) = self.functions.get(&qualified_name) {
                    ty.clone()
                } else if let Some(ty) = self.functions.get(method.as_str()) {
                    ty.clone()
                } else {
                    return Err(format!("Unknown function: {}", qualified_name));
                };

                // Check if this is a function type with parameters
                Ok(match func_ty {
                    Type::Function(param_types, return_ty) => {
                        // Check that we have the right number of arguments
                        if args.len() != param_types.len() {
                            return Err(format!(
                                "Wrong number of arguments: expected {}, got {}",
                                param_types.len(),
                                args.len()
                            ));
                        }

                        // Type check each argument against the corresponding parameter type
                        for (i, (arg, param_ty)) in args.iter().zip(param_types.iter()).enumerate()
                        {
                            let arg_ty = self.infer(arg)?;
                            self.constrain_eq(arg_ty, param_ty.clone());
                        }

                        // Return the function's return type
                        *return_ty
                    }
                    _ => {
                        // Not a function type, just return it (could be a constant)
                        func_ty
                    }
                })
            }

            AstNode::ImplBlock {
                concept, ty, body, ..
            } => {
                // Process functions in impl block to register them
                for func in body {
                    // Check if this is a Method node (signature) or FuncDef (implementation)
                    match func {
                        AstNode::Method {
                            name, params, ret, ..
                        } => {
                            // Parse the return type
                            let return_ty = self.parse_type_string(ret)?;

                            // Create function type from parameters
                            let mut param_types = Vec::new();
                            for (_, param_type_str) in params {
                                let param_ty = self.parse_type_string(param_type_str)?;
                                param_types.push(param_ty);
                            }

                            // Check if this is a static method (no self parameter)
                            let is_static = params.is_empty()
                                || !(params[0].1 == "&self"
                                    || params[0].1 == "&mut self"
                                    || params[0].1 == "self");

                            // Create the function type
                            let func_type = if param_types.is_empty() {
                                return_ty
                            } else {
                                Type::Function(param_types, Box::new(return_ty))
                            };

                            // Register with qualified name: Type::method_name for static methods
                            if is_static {
                                let qualified_name = format!("{}::{}", ty, name);
                                self.functions.insert(qualified_name, func_type);
                            }
                            // Instance methods will be handled differently
                        }
                        AstNode::FuncDef {
                            name, params, ret, async_, ..
                        } => {
                            // Parse the return type
                            let return_ty = self.parse_type_string(ret)?;

                            // Create function type from parameters
                            let mut param_types = Vec::new();
                            for (_, param_type_str) in params {
                                let param_ty = self.parse_type_string(param_type_str)?;
                                param_types.push(param_ty);
                            }

                            // Check if this is a static method (no self parameter)
                            let is_static = params.is_empty()
                                || !(params[0].1 == "&self"
                                    || params[0].1 == "&mut self"
                                    || params[0].1 == "self");

                            // Create the function type
                            // Use AsyncFunction for async functions
                            let func_type = if param_types.is_empty() {
                                return_ty
                            } else if *async_ {
                                Type::AsyncFunction(param_types, Box::new(return_ty))
                            } else {
                                Type::Function(param_types, Box::new(return_ty))
                            };

                            // Register with qualified name: Type::method_name for static methods
                            if is_static {
                                let qualified_name = format!("{}::{}", ty, name);
                                self.functions.insert(qualified_name, func_type);
                            }
                            // Instance methods will be handled differently
                        }
                        _ => {
                            // For other nodes, just infer them normally
                            self.infer(func)?;
                        }
                    }
                }
                // Impl blocks don't have a type, they're declarations
                // Return unit type
                Ok(Type::Tuple(vec![]))
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
                inner_ctx.constrain_eq(body_ty, return_type_var.clone());

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
                Ok(Type::Function(
                    final_param_types,
                    Box::new(final_return_type),
                ))
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
                self.constrain_eq(then_ty.clone(), else_ty.clone());

                // The if-let expression has the type of its branches
                Ok(then_ty)
            }

            AstNode::If { cond, then, else_ } => {
                // Type check the condition - must be boolean or integer (C-style)
                let cond_ty = self.infer(cond)?;
                // Allow bool or i64 (0 = false, non-zero = true)
                if !matches!(cond_ty, Type::Bool | Type::I64 | Type::I32 | Type::U64 | Type::U32) {
                    return Err("If condition must be boolean or integer".to_string());
                }

                // Compute branch type, treating last element specially
                let mut branch_type = |stmts: &[AstNode]| -> Result<Type, String> {
                    let len = stmts.len();
                    if len == 0 {
                        Ok(Type::Tuple(vec![]))
                    } else {
                        // Check all statements except the last
                        for stmt in &stmts[..len-1] {
                            self.infer(stmt)?;
                        }
                        // Last element determines the branch type
                        let last = &stmts[len-1];
                        match last {
                            AstNode::ExprStmt { expr } => self.infer(expr),
                            _ => self.infer(last),
                        }
                    }
                };

                let then_ty = branch_type(then)?;
                let else_ty = branch_type(else_)?;

                if else_.is_empty() {
                    // If there's no else branch, the if statement has unit type
                    // (This covers both statement usage and expression usage with missing else)
                    Ok(Type::Tuple(vec![]))
                } else {
                    // Both branches must have the same type
                    self.constrain_eq(then_ty.clone(), else_ty.clone());
                    // The if expression has the type of its branches
                    Ok(then_ty)
                }
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
                        self.constrain_eq(guard_ty, Type::Bool);
                    }

                    // Type check body
                    let body_ty = self.infer(&arm.body)?;
                    arm_types.push(body_ty);
                }

                // All arms must have the same type
                if let Some(first_ty) = arm_types.first() {
                    for other_ty in arm_types.iter().skip(1) {
                        self.constrain_eq(first_ty.clone(), other_ty.clone());
                    }
                    Ok(first_ty.clone())
                } else {
                    // Empty match - return unit type
                    Ok(Type::Tuple(vec![]))
                }
            }

            AstNode::Use { .. } => {
                // Use statements are processed by the resolver before type inference
                // They don't have a type themselves
                Ok(Type::Tuple(vec![])) // Unit type
            }

            AstNode::TypeAlias { name, ty, .. } => {
                // Parse the underlying type
                let underlying_ty = self.parse_type_string(ty)?;
                // Store the alias mapping
                self.types.insert(name.clone(), underlying_ty);
                // Type aliases themselves have unit type
                Ok(Type::Tuple(vec![]))
            }

            AstNode::Return(expr) => {
                // Type check the return expression
                let expr_ty = self.infer(expr)?;
                // Return statements have unit type (they don't produce a value)
                Ok(Type::Tuple(vec![]))
            }

            AstNode::ExprStmt { expr } => {
                // Type check the expression, but discard its type
                let _ = self.infer(expr)?;
                // Expression statements have unit type
                Ok(Type::Tuple(vec![]))
            }

            AstNode::While { cond, body } => {
                // Type check the condition - must be boolean
                let cond_ty = self.infer(cond)?;
                self.constrain_eq(cond_ty, Type::Bool);

                // Type check the body
                for stmt in body {
                    self.infer(stmt)?;
                }

                // While loops have unit type
                Ok(Type::Tuple(vec![]))
            }

            AstNode::Loop { body } => {
                // Type check the body
                for stmt in body {
                    self.infer(stmt)?;
                }

                // Loops have unit type
                Ok(Type::Tuple(vec![]))
            }

            AstNode::Break(expr_opt) => {
                // Type check the break expression if present
                if let Some(expr) = expr_opt {
                    self.infer(expr)?;
                }
                // Break statements have unit type (they don't produce a value)
                Ok(Type::Tuple(vec![]))
            }

            AstNode::Continue(expr_opt) => {
                // Type check the continue expression if present
                if let Some(expr) = expr_opt {
                    self.infer(expr)?;
                }
                // Continue statements have unit type (they don't produce a value)
                Ok(Type::Tuple(vec![]))
            }

            AstNode::For { pattern, expr, body } => {
                // Type check the expression - it should be a range
                let expr_ty = self.infer(expr)?;
                
                // Check that the expression is actually a Range type
                self.constrain_eq(expr_ty, Type::Range);
                
                // Check the pattern (loop variable)
                // For simple variable patterns, we need to infer the type from the range
                // For now, we'll use i64 as the default loop variable type
                // TODO: Infer actual type from range expression
                if let AstNode::Var(var_name) = &**pattern {
                    // Declare loop variable with i64 type for now
                    self.declare(var_name.clone(), Type::I64);
                } else {
                    // Complex pattern - for now, just check it
                    self.check_pattern(pattern, &Type::I64)?;
                }
                
                // Type check the body
                for stmt in body {
                    self.infer(stmt)?;
                }
                
                // For loops have unit type
                Ok(Type::Tuple(vec![]))
            }

            AstNode::Range { start, end, inclusive } => {
                // Type check the start and end expressions
                let start_ty = self.infer(start)?;
                let end_ty = self.infer(end)?;
                
                // Both start and end should be numeric types
                // For now, we'll accept any type and return Range
                // In the future, we should constrain them to be the same numeric type
                Ok(Type::Range)
            }

            AstNode::Block { body } => {
                // Type check block body and return type of last expression
                let len = body.len();
                if len == 0 {
                    // Empty block has unit type
                    Ok(Type::Tuple(vec![]))
                } else {
                    // Check all statements except the last
                    for stmt in &body[..len-1] {
                        self.infer(stmt)?;
                    }
                    // The last element determines the block's type
                    let last_node = &body[len-1];
                    match last_node {
                        AstNode::ExprStmt { expr } => {
                            // Expression statement: the block's value is the expression's type
                            let ty = self.infer(expr);
                            ty
                        }
                        _ => {
                            // Any other statement: block's type is the statement's type
                            let ty = self.infer(last_node);
                            ty
                        }
                    }
                }
            },

            _ => {
                // Default to error for unimplemented nodes
                return Err(format!("Type inference not implemented for: {:?}", node));
            }
        };

        let result_ty = ty?;
        self.last_type = Some(result_ty.clone());
        Ok(result_ty)
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

        for constraint in self.constraints.drain(..) {
            match constraint {
                Constraint::Equality(t1, t2) => {
                    if let Err(e) = self.substitution.unify(&t1, &t2) {
                        errors.push(e);
                    }
                }
                Constraint::Bound(ty, bound) => {
                    if !self.substitution.satisfies_bound(&ty, &bound) {
                        errors.push(UnifyError::MissingBound(ty, bound));
                    }
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Enter a new generic context
    pub fn enter_generic_scope(&mut self, type_params: Vec<TypeParam>) {
        let new_context = GenericContext {
            type_params,
            parent: Some(Box::new(self.generic_context.clone())),
        };
        self.generic_context = new_context;
    }

    /// Exit current generic context
    pub fn exit_generic_scope(&mut self) {
        if let Some(parent) = self.generic_context.parent.take() {
            self.generic_context = *parent;
        }
    }

    /// Infer type for generic function call
    pub fn infer_generic_call(
        &mut self,
        name: &str,
        type_args: &[Type],
        value_args: &[AstNode],
    ) -> Result<Type, String> {
        // Look up generic function
        let generic_ty = self
            .functions
            .get(name)
            .ok_or_else(|| format!("Unknown generic function: {}", name))?;

        // Instantiate with type arguments
        let instantiated_ty = self.substitution.instantiate_generic_with_bounds(
            generic_ty,
            type_args,
            &self.generic_context,
        )?;

        // Check if it's a function type
        match &instantiated_ty {
            Type::Function(param_types, ret_ty) => {
                // Check value arguments match parameter types
                if value_args.len() != param_types.len() {
                    return Err(format!(
                        "Wrong number of arguments: expected {}, got {}",
                        param_types.len(),
                        value_args.len()
                    ));
                }

                // Type check each argument
                for (i, (arg, param_ty)) in value_args.iter().zip(param_types.iter()).enumerate() {
                    let arg_ty = self.infer(arg)?;
                    self.constrain_eq(arg_ty, param_ty.clone());
                }

                Ok(*ret_ty.clone())
            }
            _ => Err(format!("{} is not a function", name)),
        }
    }

    /// Parse generic parameters from strings like "T: Clone + Copy"
    fn parse_generic_params(&self, generics: &[String]) -> Result<Vec<TypeParam>, String> {
        let mut type_params = Vec::new();

        for generic in generics {
            // Parse "T: Clone + Copy" or just "T"
            let parts: Vec<&str> = generic.split(':').map(|s| s.trim()).collect();

            if parts.is_empty() {
                return Err("Empty generic parameter".to_string());
            }

            let name = parts[0].to_string();
            let mut bounds = Vec::new();

            if parts.len() > 1 {
                // Parse bounds: "Clone + Copy + Debug"
                let bound_parts: Vec<&str> = parts[1].split('+').map(|s| s.trim()).collect();

                for bound_str in bound_parts {
                    match bound_str {
                        "Clone" => bounds.push(TraitBound::Clone),
                        "Copy" => bounds.push(TraitBound::Copy),
                        "Debug" => bounds.push(TraitBound::Debug),
                        "Default" => bounds.push(TraitBound::Default),
                        "PartialEq" => bounds.push(TraitBound::PartialEq),
                        "Eq" => bounds.push(TraitBound::Eq),
                        "PartialOrd" => bounds.push(TraitBound::PartialOrd),
                        "Ord" => bounds.push(TraitBound::Ord),
                        "Hash" => bounds.push(TraitBound::Hash),
                        "Future" => bounds.push(TraitBound::Future),
                        bound_str if bound_str.starts_with("Identity<") && bound_str.ends_with(">") => {
                            // Parse Identity<Read> or Identity<Read+Write>
                            let inner = &bound_str[9..bound_str.len()-1]; // Remove "Identity<" and ">"
                            let capabilities: Result<Vec<_>, _> = inner.split('+')
                                .map(|cap| cap.trim())
                                .map(|cap_str| {
                                    match cap_str.to_lowercase().as_str() {
                                        "read" => Ok(crate::middle::types::identity::CapabilityLevel::Read),
                                        "write" => Ok(crate::middle::types::identity::CapabilityLevel::Write),
                                        "execute" => Ok(crate::middle::types::identity::CapabilityLevel::Execute),
                                        "owned" => Ok(crate::middle::types::identity::CapabilityLevel::Owned),
                                        "immutable" => Ok(crate::middle::types::identity::CapabilityLevel::Immutable),
                                        _ => Err(format!("Unknown capability: {}", cap_str)),
                                    }
                                })
                                .collect();
                            
                            match capabilities {
                                Ok(caps) => bounds.push(TraitBound::Identity(caps)),
                                Err(e) => return Err(format!("Invalid identity constraint: {}", e)),
                            }
                        },
                        _ => return Err(format!("Unknown trait bound: {}", bound_str)),
                    }
                }
            }

            type_params.push(TypeParam { name, bounds, kind: Kind::Star });
        }

        Ok(type_params)
    }

    /// Register built-in generic types (Vec, Option, Result)
    pub fn register_builtin_generics(&mut self) {
        // Vec<T> - generic type with one type parameter
        let t_var = Type::Variable(TypeVar::fresh());
        let vec_ty = Type::Named("Vec".to_string(), vec![t_var.clone()]);

        // Option<T> - generic type with one type parameter
        let option_ty = Type::Named("Option".to_string(), vec![t_var.clone()]);

        // Result<T, E> - generic type with two type parameters
        let t_var2 = Type::Variable(TypeVar::fresh());
        let result_ty = Type::Named("Result".to_string(), vec![t_var.clone(), t_var2.clone()]);

        // Register them as function-like types that can be instantiated
        // For now, we'll store them as simple named types
        // In a full implementation, we'd store them in a separate generic types table
        self.functions.insert("Vec".to_string(), vec_ty.clone());
        self.functions
            .insert("Option".to_string(), option_ty.clone());
        self.functions
            .insert("Result".to_string(), result_ty.clone());

        // Also register them as types for type checking
        self.types.insert("Vec".to_string(), vec_ty);
        self.types.insert("Option".to_string(), option_ty);
        self.types.insert("Result".to_string(), result_ty);
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
            types: self.types.clone(),
            substitution: self.substitution.clone(),
            constraints: self.constraints.clone(),
            generic_context: self.generic_context.clone(),
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
            Type::Array(Box::new(Type::I32), crate::middle::types::ArraySize::Literal(10))
        );
        assert_eq!(
            ctx.parse_type_string("[bool; 5]").unwrap(),
            Type::Array(Box::new(Type::Bool), crate::middle::types::ArraySize::Literal(5))
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
            Type::Array(Box::new(Type::Tuple(vec![Type::I32, Type::Bool])), crate::middle::types::ArraySize::Literal(3))
        );

        // Test error cases
        // Note: "[i32; not_a_number]" is now valid as a const parameter
        // assert!(ctx.parse_type_string("[i32; not_a_number]").is_err());
        assert!(ctx.parse_type_string("[i32;").is_err());
        assert!(ctx.parse_type_string("(i32, bool").is_err());
    }

    #[test]
    fn test_static_method_type_checking() {
        let mut ctx = InferContext::new();

        // Create a struct definition
        let struct_def = AstNode::StructDef {
            name: "Point".to_string(),
            generics: Vec::new(),
            lifetimes: Vec::new(),
            fields: vec![
                ("x".to_string(), "i64".to_string()),
                ("y".to_string(), "i64".to_string()),
            ],
            attrs: Vec::new(),
            doc: "".to_string(),
            pub_: false,
            where_clauses: vec![],
        };

        // Create an impl block with a static method
        let impl_block = AstNode::ImplBlock {
            concept: "".to_string(),
            generics: Vec::new(),
            lifetimes: Vec::new(),
            ty: "Point".to_string(),
            body: vec![AstNode::Method {
                name: "new".to_string(),
                params: vec![
                    ("x".to_string(), "i64".to_string()),
                    ("y".to_string(), "i64".to_string()),
                ],
                ret: "Point".to_string(),
                generics: Vec::new(),
                lifetimes: Vec::new(),
                attrs: Vec::new(),
                doc: "".to_string(),
                where_clauses: Vec::new(),
                body: None,
            }],
            attrs: Vec::new(),
            doc: "".to_string(),
            where_clauses: vec![],
        };

        // Process struct definition
        ctx.infer(&struct_def).unwrap();

        // Process impl block (registers the static method)
        ctx.infer(&impl_block).unwrap();

        // Create a PathCall for Point::new(10, 20)
        let path_call = AstNode::PathCall {
            path: vec!["Point".to_string()],
            method: "new".to_string(),
            args: vec![AstNode::Lit(10), AstNode::Lit(20)],
            type_args: vec![],
        };

        // Type check the PathCall
        let result = ctx.infer(&path_call);
        assert!(result.is_ok(), "Type checking failed: {:?}", result.err());

        // The result should be a Point type
        // Note: We can't easily check the exact type since we don't have
        // a Type::Named constructor in the test, but we can verify it doesn't fail
    }
}
