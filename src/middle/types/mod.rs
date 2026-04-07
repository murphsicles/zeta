//! # Type System Foundation
//!
//! Algebraic type representation and unification for Zeta.
//! Replaces string-based types with proper type algebra.

use std::collections::HashMap;
use std::sync::atomic::{AtomicU32, Ordering};

// Re-export lifetime types
pub mod lifetime;
pub use lifetime::{Lifetime, LifetimeContext, LifetimeSubstitution, LifetimeVar};

// Re-export kind types
pub mod kind;
pub use kind::{Kind, KindContext, KindSubstitution, KindVar, TypeExpr};

// Re-export associated type types
pub mod associated;
pub use associated::{
    AssociatedType, AssociatedTypeBuilder, AssociatedTypeContext, AssociatedTypeProjection,
    GenericParam,
};

// Re-export type family types
pub mod family;
pub use family::{
    TypeFamily, TypeFamilyBuilder, TypeFamilyContext, TypeFamilyEquation,
    TypeFamilyEquationBuilder, TypeFamilyPattern, TypeFamilyConstraint,
};

/// Type variable for inference
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVar(pub u32);

impl TypeVar {
    /// Generate a fresh type variable
    pub fn fresh() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        TypeVar(COUNTER.fetch_add(1, Ordering::Relaxed))
    }
}

/// Mutability qualifier for references
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mutability {
    Immutable,
    Mutable,
}

/// Algebraic type representation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    // Primitive numeric types
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Usize,  // Platform-dependent unsigned integer (size of pointer)
    F32,
    F64,

    // Other primitives
    Bool,
    Char,
    Str,
    // Range type for for loops
    Range,

    // Compound types
    Array(Box<Type>, usize),              // [T; N]
    Slice(Box<Type>),                     // [T]
    DynamicArray(Box<Type>),              // [dynamic]T
    Tuple(Vec<Type>),                     // (T1, T2, ...)
    Ptr(Box<Type>, Mutability),           // *const T, *mut T
    Ref(Box<Type>, Lifetime, Mutability), // &'a T, &'a mut T

    // Named types (structs, enums, concepts)
    Named(String, Vec<Type>), // Name<T1, T2, ...>

    // Trait objects
    TraitObject(String), // dyn Trait

    // Function types
    Function(Vec<Type>, Box<Type>), // (T1, T2, ...) -> R
    // Async function types (returns a Future)
    AsyncFunction(Vec<Type>, Box<Type>), // async (T1, T2, ...) -> R

    // Type variables (for inference)
    Variable(TypeVar),

    // Type constructor (higher-kinded type)
    Constructor(String, Vec<Type>, Kind), // Name, type arguments, kind

    // Partially applied type constructor
    PartialApplication(Box<Type>, Vec<Type>), // Constructor, applied arguments

    // Error type (when inference fails)
    Error,
}

/// Trait bounds for generic type parameters
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TraitBound {
    Clone,
    Copy,
    Debug,
    Default,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Future,
    // Add more as needed
}

/// Generic type parameter with optional trait bounds and kind
#[derive(Debug, Clone)]
pub struct TypeParam {
    pub name: String,
    pub bounds: Vec<TraitBound>,
    pub kind: Kind, // Kind of this type parameter (default: *)
}

impl TypeParam {
    /// Create a new type parameter with default kind (*)
    pub fn new(name: String) -> Self {
        TypeParam {
            name,
            bounds: Vec::new(),
            kind: Kind::Star,
        }
    }
    
    /// Create a type parameter with specific kind
    pub fn with_kind(name: String, kind: Kind) -> Self {
        TypeParam {
            name,
            bounds: Vec::new(),
            kind,
        }
    }
    
    /// Add a trait bound
    pub fn with_bound(mut self, bound: TraitBound) -> Self {
        self.bounds.push(bound);
        self
    }
}

/// Context for generic type parameters (scoping)
#[derive(Debug, Clone)]
pub struct GenericContext {
    pub type_params: Vec<TypeParam>,
    pub parent: Option<Box<GenericContext>>,
}

impl GenericContext {
    /// Create empty generic context
    pub fn new() -> Self {
        GenericContext {
            type_params: Vec::new(),
            parent: None,
        }
    }
}

impl Default for GenericContext {
    fn default() -> Self {
        Self::new()
    }
}

impl GenericContext {
    /// Create context with parent
    pub fn with_parent(parent: GenericContext) -> Self {
        GenericContext {
            type_params: Vec::new(),
            parent: Some(Box::new(parent)),
        }
    }

    /// Find type parameter by name (searching up parent chain)
    pub fn find_type_param(&self, name: &str) -> Option<&TypeParam> {
        self.type_params
            .iter()
            .find(|p| p.name == name)
            .or_else(|| self.parent.as_ref().and_then(|p| p.find_type_param(name)))
    }

    /// Add a type parameter to this context
    pub fn add_type_param(&mut self, param: TypeParam) {
        self.type_params.push(param);
    }
}

impl Type {
    /// Parse a type from a string representation
    pub fn from_string(s: &str) -> Type {
        let s = s.trim();

        // Handle primitive types
        match s {
            "i8" => Type::I8,
            "i16" => Type::I16,
            "i32" => Type::I32,
            "i64" => Type::I64,
            "u8" => Type::U8,
            "u16" => Type::U16,
            "u32" => Type::U32,
            "u64" => Type::U64,
            "usize" => Type::Usize,
            "f32" => Type::F32,
            "f64" => Type::F64,
            "bool" => Type::Bool,
            "char" => Type::Char,
            "str" => Type::Str,
            _ => {
                // Check for &mut prefix (must check before & prefix)
                if let Some(rest) = s.strip_prefix("&mut ") {
                    let inner = Type::from_string(rest);
                    return Type::Ref(Box::new(inner), Lifetime::Static, Mutability::Mutable);
                }

                // Check for & prefix
                if let Some(rest) = s.strip_prefix("&") {
                    // Make sure we didn't match &mut (should have been caught above)
                    if !rest.starts_with("mut ") {
                        let inner = Type::from_string(rest);
                        return Type::Ref(Box::new(inner), Lifetime::Static, Mutability::Immutable);
                    }
                }

                // Check for array type: [T; N] or [dynamic]T
                if s.starts_with('[') && s.ends_with(']') {
                    let inner = &s[1..s.len() - 1]; // Remove brackets
                    
                    // Check for dynamic array: [dynamic]T
                    if let Some(type_part) = inner.strip_prefix("dynamic]") {
                        let inner_type = Type::from_string(type_part.trim());
                        return Type::DynamicArray(Box::new(inner_type));
                    }
                    
                    // Find the semicolon that separates type from size, handling nested brackets
                    let mut bracket_count = 0;
                    let mut split_pos = None;
                    
                    for (i, ch) in inner.chars().enumerate() {
                        match ch {
                            '[' => bracket_count += 1,
                            ']' => bracket_count -= 1,
                            ';' if bracket_count == 0 => {
                                split_pos = Some(i);
                                break;
                            }
                            _ => {}
                        }
                    }
                    
                    if let Some(pos) = split_pos {
                        // Array with size: [T; N]
                        let type_part = &inner[..pos];
                        let size_part = &inner[pos + 1..];
                        let inner_type = Type::from_string(type_part.trim());
                        if let Ok(size) = size_part.trim().parse::<usize>() {
                            return Type::Array(Box::new(inner_type), size);
                        }
                        // If size doesn't parse as usize, fall through to Named type
                    } else {
                        // Slice type: [T]
                        let inner_type = Type::from_string(inner.trim());
                        return Type::Slice(Box::new(inner_type));
                    }
                }

                // Check for tuple type: (T1, T2, T3)
                if s.starts_with('(') && s.ends_with(')') {
                    let inner = &s[1..s.len() - 1]; // Remove parentheses
                    if inner.is_empty() {
                        // Empty tuple: ()
                        return Type::Tuple(Vec::new());
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
                                    types.push(Type::from_string(current.trim()));
                                    current.clear();
                                }
                            }
                            _ => current.push(ch),
                        }
                    }

                    if !current.is_empty() {
                        types.push(Type::from_string(current.trim()));
                    }

                    return Type::Tuple(types);
                }

                // Check for Zeta's lt() syntax: lt(Result, i64)
                if s.starts_with("lt(") && s.ends_with(')') {
                    let inner = &s[3..s.len() - 1]; // Remove "lt(" and ")"
                    let parts: Vec<&str> = inner.split(',').map(|s| s.trim()).collect();
                    if parts.len() >= 2 {
                        let type_name = parts[0];
                        let type_args: Vec<Type> = parts[1..]
                            .iter()
                            .map(|arg| Type::from_string(arg))
                            .collect();
                        return Type::Named(type_name.to_string(), type_args);
                    }
                }

                // Check for pointer types: *const T, *mut T
                if let Some(inner) = s.strip_prefix("*const ") {
                    let inner_type = Type::from_string(inner);
                    return Type::Ptr(Box::new(inner_type), Mutability::Immutable);
                }

                if let Some(inner) = s.strip_prefix("*mut ") {
                    let inner_type = Type::from_string(inner);
                    return Type::Ptr(Box::new(inner_type), Mutability::Mutable);
                }

                // Check for trait objects: dyn Trait
                if let Some(trait_name) = s.strip_prefix("dyn ") {
                    return Type::TraitObject(trait_name.trim().to_string());
                }

                // Check for generic type: Vec<i32>, Option<T>, Result<T, E>, Box<dyn Error>
                // Look for < followed by > with content in between
                // We need to find the matching > for the first <
                let mut open_angle_pos = None;
                let mut close_angle_pos = None;
                let mut depth = 0;

                for (i, ch) in s.chars().enumerate() {
                    match ch {
                        '<' => {
                            if depth == 0 {
                                open_angle_pos = Some(i);
                            }
                            depth += 1;
                        }
                        '>' => {
                            depth -= 1;
                            if depth == 0 && open_angle_pos.is_some() {
                                close_angle_pos = Some(i);
                                break;
                            }
                        }
                        _ => {}
                    }
                }

                if let (Some(open_angle), Some(close_angle)) = (open_angle_pos, close_angle_pos) {
                    let type_name = &s[..open_angle];
                    let inner = &s[open_angle + 1..close_angle];

                    // Check if this is a trait object inside a generic
                    if inner.trim().starts_with("dyn ") {
                        // Handle trait object inside container
                        let trait_name = inner.trim()[4..].trim().to_string();
                        return Type::Named(
                            type_name.trim().to_string(),
                            vec![Type::TraitObject(trait_name)],
                        );
                    }

                    // Parse type arguments, handling nested generics and tuples
                    let mut args = Vec::new();
                    let mut current = String::new();
                    let mut bracket_depth = 0;
                    let mut paren_depth = 0;

                    for ch in inner.chars() {
                        match ch {
                            '<' => {
                                bracket_depth += 1;
                                current.push(ch);
                            }
                            '>' => {
                                bracket_depth -= 1;
                                current.push(ch);
                            }
                            '(' => {
                                paren_depth += 1;
                                current.push(ch);
                            }
                            ')' => {
                                paren_depth -= 1;
                                current.push(ch);
                            }
                            ',' if bracket_depth == 0 && paren_depth == 0 => {
                                if !current.is_empty() {
                                    args.push(Type::from_string(current.trim()));
                                    current.clear();
                                }
                            }
                            _ => current.push(ch),
                        }
                    }

                    if !current.is_empty() {
                        args.push(Type::from_string(current.trim()));
                    }

                    return Type::Named(type_name.trim().to_string(), args);
                }

                // Simple named type without generics
                Type::Named(s.to_string(), vec![])
            }
        }
    }

    /// Check if type contains any type variables
    pub fn contains_vars(&self) -> bool {
        match self {
            Type::Variable(_) => true,
            Type::Array(inner, _) => inner.contains_vars(),
            Type::Slice(inner) => inner.contains_vars(),
            Type::Tuple(types) => types.iter().any(|t| t.contains_vars()),
            Type::Ptr(inner, _) => inner.contains_vars(),
            Type::Ref(inner, lifetime, _) => inner.contains_vars() || lifetime.contains_vars(),
            Type::Named(_, args) => args.iter().any(|t| t.contains_vars()),
            Type::TraitObject(_) => false,
            Type::Function(params, ret) => {
                params.iter().any(|t| t.contains_vars()) || ret.contains_vars()
            }
            Type::AsyncFunction(params, ret) => {
                params.iter().any(|t| t.contains_vars()) || ret.contains_vars()
            }
            Type::Constructor(_, args, _) => args.iter().any(|t| t.contains_vars()),
            Type::PartialApplication(constructor, args) => {
                constructor.contains_vars() || args.iter().any(|t| t.contains_vars())
            }
            _ => false,
        }
    }

    /// Get display name for type
    pub fn display_name(&self) -> String {
        match self {
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::Usize => "usize".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Char => "char".to_string(),
            Type::Str => "str".to_string(),
            Type::Range => "Range".to_string(),
            Type::Array(inner, size) => format!("[{}; {}]", inner.display_name(), size),
            Type::Slice(inner) => format!("[{}]", inner.display_name()),
            Type::DynamicArray(inner) => format!("[dynamic]{}", inner.display_name()),
            Type::Tuple(types) => {
                let inner = types
                    .iter()
                    .map(|t| t.display_name())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", inner)
            }
            Type::Ptr(inner, Mutability::Immutable) => format!("*const {}", inner.display_name()),
            Type::Ptr(inner, Mutability::Mutable) => format!("*mut {}", inner.display_name()),
            Type::Ref(inner, lifetime, Mutability::Immutable) => {
                format!("&{} {}", lifetime.display_name(), inner.display_name())
            }
            Type::Ref(inner, lifetime, Mutability::Mutable) => {
                format!("&{} mut {}", lifetime.display_name(), inner.display_name())
            }
            Type::Named(name, args) => {
                if args.is_empty() {
                    name.clone()
                } else {
                    let args_str = args
                        .iter()
                        .map(|t| t.display_name())
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{}<{}>", name, args_str)
                }
            }
            Type::TraitObject(trait_name) => format!("dyn {}", trait_name),
            Type::Function(params, ret) => {
                let params_str = params
                    .iter()
                    .map(|t| t.display_name())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({}) -> {}", params_str, ret.display_name())
            }
            Type::AsyncFunction(params, ret) => {
                let params_str = params
                    .iter()
                    .map(|t| t.display_name())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("async ({}) -> {}", params_str, ret.display_name())
            }
            Type::Variable(var) => format!("T{}", var.0),
            Type::Constructor(name, args, kind) => {
                if args.is_empty() {
                    format!("{} : {}", name, kind.display_name())
                } else {
                    let args_str = args
                        .iter()
                        .map(|t| t.display_name())
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{}<{}> : {}", name, args_str, kind.display_name())
                }
            }
            Type::PartialApplication(constructor, args) => {
                let constructor_str = constructor.display_name();
                let args_str = args
                    .iter()
                    .map(|t| t.display_name())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}<{}>", constructor_str, args_str)
            }
            Type::Error => "<?>".to_string(),
        }
    }

    /// Get mangled name for type (used in codegen for monomorphization)
    pub fn mangled_name(&self) -> String {
        match self {
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::Usize => "usize".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Char => "char".to_string(),
            Type::Str => "str".to_string(),
            Type::Range => "Range".to_string(),
            Type::Array(inner, size) => format!("Array_{}_{}", inner.mangled_name(), size),
            Type::Slice(inner) => format!("Slice_{}", inner.mangled_name()),
            Type::DynamicArray(inner) => format!("DynamicArray_{}", inner.mangled_name()),
            Type::Tuple(types) => {
                let mut name = "Tuple".to_string();
                for ty in types {
                    name.push('_');
                    name.push_str(&ty.mangled_name());
                }
                name
            }
            Type::Ptr(inner, mutability) => {
                let mut_str = match mutability {
                    Mutability::Immutable => "const",
                    Mutability::Mutable => "mut",
                };
                format!("Ptr_{}_{}", mut_str, inner.mangled_name())
            }
            Type::Ref(inner, lifetime, mutability) => {
                let mut_str = match mutability {
                    Mutability::Immutable => "immut",
                    Mutability::Mutable => "mut",
                };
                format!(
                    "Ref_{}_{}_{}",
                    lifetime.mangled_name(),
                    inner.mangled_name(),
                    mut_str
                )
            }
            Type::Named(name, args) => {
                if args.is_empty() {
                    name.clone()
                } else {
                    let mut mangled = name.clone();
                    mangled.push('_');
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            mangled.push('_');
                        }
                        mangled.push_str(&arg.mangled_name());
                    }
                    mangled
                }
            }
            Type::TraitObject(trait_name) => {
                format!("TraitObject_{}", trait_name.replace("::", "_"))
            }
            Type::Function(params, ret) => {
                let mut name = "Fn".to_string();
                for param in params {
                    name.push('_');
                    name.push_str(&param.mangled_name());
                }
                name.push_str("_to_");
                name.push_str(&ret.mangled_name());
                name
            }
            Type::Variable(var) => format!("Var_{}", var.0),
            Type::Error => "Error".to_string(),
            Type::AsyncFunction(params, ret) => {
                let param_str = params
                    .iter()
                    .map(|p| p.mangled_name())
                    .collect::<Vec<_>>()
                    .join("_");
                format!("AsyncFunction_{}_{}", param_str, ret.mangled_name())
            }
            Type::Constructor(name, args, kind) => {
                let mut mangled = format!("Constructor_{}_kind_{}", name, kind.mangled_name());
                if !args.is_empty() {
                    mangled.push('_');
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            mangled.push('_');
                        }
                        mangled.push_str(&arg.mangled_name());
                    }
                }
                mangled
            }
            Type::PartialApplication(constructor, args) => {
                let mut mangled = format!("PartialApp_{}", constructor.mangled_name());
                if !args.is_empty() {
                    mangled.push('_');
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            mangled.push('_');
                        }
                        mangled.push_str(&arg.mangled_name());
                    }
                }
                mangled
            }
        }
    }

    /// Instantiate a generic type by substituting type variables with concrete types
    /// This is used when we have a generic type like `Result<T, E>` and want to
    /// instantiate it as `Result<i64, String>` by providing `[i64, String]` as arguments
    pub fn instantiate_generic(&self, type_args: &[Type]) -> Result<Type, String> {
        match self {
            Type::Named(name, generic_params) => {
                // Check if we have the right number of type arguments
                if generic_params.len() != type_args.len() {
                    return Err(format!(
                        "Wrong number of type arguments for {}: expected {}, got {}",
                        name,
                        generic_params.len(),
                        type_args.len()
                    ));
                }

                // Create a substitution mapping from type variables to concrete types
                let mut substitution = Substitution::new();

                // For each generic parameter, if it's a type variable, map it to the corresponding type argument
                for (param, arg) in generic_params.iter().zip(type_args.iter()) {
                    if let Type::Variable(var) = param {
                        substitution.mapping.insert(var.clone(), arg.clone());
                    } else {
                        // If the parameter is not a simple type variable, we need to recursively
                        // substitute within it (for nested generics like Vec<Option<T>>)
                        // For now, we'll handle simple cases
                        return Err(format!(
                            "Complex generic parameter not yet supported: {}",
                            param.display_name()
                        ));
                    }
                }

                // Apply the substitution to create the instantiated type
                let instantiated =
                    substitution.apply(&Type::Named(name.clone(), generic_params.clone()));
                Ok(instantiated)
            }

            // For other types, we might need to recursively instantiate
            Type::Array(inner, size) => {
                let instantiated_inner = inner.instantiate_generic(type_args)?;
                Ok(Type::Array(Box::new(instantiated_inner), *size))
            }

            Type::Slice(inner) => {
                let instantiated_inner = inner.instantiate_generic(type_args)?;
                Ok(Type::Slice(Box::new(instantiated_inner)))
            }

            Type::Tuple(types) => {
                let instantiated_types: Result<Vec<Type>, String> = types
                    .iter()
                    .map(|t| t.instantiate_generic(type_args))
                    .collect();
                Ok(Type::Tuple(instantiated_types?))
            }

            Type::Ptr(inner, mutability) => {
                let instantiated_inner = inner.instantiate_generic(type_args)?;
                Ok(Type::Ptr(Box::new(instantiated_inner), *mutability))
            }

            Type::Ref(inner, lifetime, mutability) => {
                let instantiated_inner = inner.instantiate_generic(type_args)?;
                // For now, keep the same lifetime (lifetimes don't get instantiated from type_args)
                Ok(Type::Ref(
                    Box::new(instantiated_inner),
                    lifetime.clone(),
                    *mutability,
                ))
            }

            Type::Function(params, ret) => {
                let instantiated_params: Result<Vec<Type>, String> = params
                    .iter()
                    .map(|p| p.instantiate_generic(type_args))
                    .collect();
                let instantiated_ret = ret.instantiate_generic(type_args)?;
                Ok(Type::Function(
                    instantiated_params?,
                    Box::new(instantiated_ret),
                ))
            }

            // Type variables get replaced if they're in the substitution
            Type::Variable(var) => {
                // For a standalone type variable, we need to check if it should be replaced
                // This would require tracking which type variables are bound to this generic
                // For now, we'll return an error for unbound type variables
                Err(format!(
                    "Unbound type variable T{} in generic instantiation",
                    var.0
                ))
            }

            // Primitive types and error type remain unchanged
            _ => Ok(self.clone()),
        }
    }
}

/// Simple derive attribute handler
/// This is a basic implementation that recognizes common derive attributes
pub fn handle_derive_attribute(attr: &str, type_name: &str) -> Result<Vec<String>, String> {
    if !attr.starts_with("derive(") || !attr.ends_with(')') {
        return Err(format!("Not a derive attribute: {}", attr));
    }

    let content = &attr[7..attr.len() - 1]; // Remove "derive(" and ")"
    let traits: Vec<&str> = content.split(',').map(|s| s.trim()).collect();

    let mut implementations = Vec::new();

    for trait_name in traits {
        match trait_name {
            "Copy" => {
                implementations.push(format!("impl Copy for {} {{}}", type_name));
            }
            "Clone" => {
                implementations.push(format!(
                    "impl Clone for {} {{
    fn clone(&self) -> Self {{
        *self
    }}
}}",
                    type_name
                ));
            }
            "Debug" => {
                implementations.push(format!(
                    "impl Debug for {} {{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {{
        write!(f, \"{{:?}}\", self)
    }}
}}",
                    type_name
                ));
            }
            "PartialEq" => {
                implementations.push(format!(
                    "impl PartialEq for {} {{
    fn eq(&self, other: &Self) -> bool {{
        // Default implementation - would need field-by-field comparison
        true
    }}
}}",
                    type_name
                ));
            }
            "Eq" => {
                implementations.push("// Eq is a marker trait with no methods".to_string());
            }
            _ => {
                return Err(format!("Unsupported derive trait: {}", trait_name));
            }
        }
    }

    Ok(implementations)
}

/// Substitution mapping type variables to types
#[derive(Debug, Clone, Default)]
pub struct Substitution {
    pub mapping: HashMap<TypeVar, Type>,
}

impl Substitution {
    /// Create empty substitution
    pub fn new() -> Self {
        Substitution {
            mapping: HashMap::new(),
        }
    }

    /// Apply substitution to a type
    pub fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Variable(var) => self
                .mapping
                .get(var)
                .cloned()
                .unwrap_or(Type::Variable(var.clone())),
            Type::Array(inner, size) => Type::Array(Box::new(self.apply(inner)), *size),
            Type::Slice(inner) => Type::Slice(Box::new(self.apply(inner))),
            Type::Tuple(types) => Type::Tuple(types.iter().map(|t| self.apply(t)).collect()),
            Type::Ptr(inner, mutability) => Type::Ptr(Box::new(self.apply(inner)), *mutability),
            Type::Ref(inner, lifetime, mutability) => {
                Type::Ref(Box::new(self.apply(inner)), lifetime.clone(), *mutability)
            }
            Type::Named(name, args) => {
                Type::Named(name.clone(), args.iter().map(|t| self.apply(t)).collect())
            }
            Type::Function(params, ret) => Type::Function(
                params.iter().map(|p| self.apply(p)).collect(),
                Box::new(self.apply(ret)),
            ),
            _ => ty.clone(),
        }
    }

    /// Check if a type variable occurs in a type
    fn occurs_check(&self, var: &TypeVar, ty: &Type) -> bool {
        let ty = self.apply(ty);
        match &ty {
            Type::Variable(v) => v == var,
            Type::Array(inner, _) => self.occurs_check(var, inner),
            Type::Slice(inner) => self.occurs_check(var, inner),
            Type::Tuple(types) => types.iter().any(|t| self.occurs_check(var, t)),
            Type::Ptr(inner, _) => self.occurs_check(var, inner),
            Type::Ref(inner, _, _) => self.occurs_check(var, inner),
            Type::Named(_, args) => args.iter().any(|t| self.occurs_check(var, t)),
            Type::Function(params, ret) => {
                params.iter().any(|p| self.occurs_check(var, p)) || self.occurs_check(var, ret)
            }
            Type::AsyncFunction(params, ret) => {
                params.iter().any(|p| self.occurs_check(var, p)) || self.occurs_check(var, ret)
            }
            _ => false,
        }
    }
}

/// Unification errors
#[derive(Debug, Clone)]
pub enum UnifyError {
    Mismatch(Type, Type),
    OccursCheck(TypeVar, Type),
    ArityMismatch(usize, usize),
    MissingBound(Type, TraitBound), // Type doesn't satisfy trait bound
}

impl std::fmt::Display for UnifyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnifyError::Mismatch(t1, t2) => {
                // Provide more helpful error messages for common type mismatches
                let t1_name = t1.display_name();
                let t2_name = t2.display_name();

                // Check for common mismatches
                match (&t1, &t2) {
                    (Type::Str, Type::I64) | (Type::I64, Type::Str) => {
                        write!(
                            f,
                            "Cannot use string as integer: expected {}, found {}",
                            t1_name, t2_name
                        )
                    }
                    (Type::Bool, Type::I64) | (Type::I64, Type::Bool) => {
                        write!(
                            f,
                            "Cannot use boolean as integer: expected {}, found {}",
                            t1_name, t2_name
                        )
                    }
                    (Type::F64, Type::I64) | (Type::I64, Type::F64) => {
                        write!(
                            f,
                            "Cannot mix integer and floating-point: expected {}, found {}",
                            t1_name, t2_name
                        )
                    }
                    _ => {
                        write!(f, "Type mismatch: expected {}, found {}", t1_name, t2_name)
                    }
                }
            }
            UnifyError::OccursCheck(var, ty) => write!(
                f,
                "Occurs check failed: T{} occurs in {}",
                var.0,
                ty.display_name()
            ),
            UnifyError::ArityMismatch(expected, actual) => write!(
                f,
                "Arity mismatch: expected {} type arguments, got {}",
                expected, actual
            ),
            UnifyError::MissingBound(ty, bound) => write!(
                f,
                "Missing trait bound: {} doesn't implement {:?}",
                ty.display_name(),
                bound
            ),
        }
    }
}

impl Substitution {
    /// Unify two types, updating substitution
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> Result<(), UnifyError> {
        let t1 = self.apply(t1);
        let t2 = self.apply(t2);

        match (&t1, &t2) {
            // Same primitive types
            (Type::I8, Type::I8) => Ok(()),
            (Type::I16, Type::I16) => Ok(()),
            (Type::I32, Type::I32) => Ok(()),
            (Type::I64, Type::I64) => Ok(()),
            (Type::U8, Type::U8) => Ok(()),
            (Type::U16, Type::U16) => Ok(()),
            (Type::U32, Type::U32) => Ok(()),
            (Type::U64, Type::U64) => Ok(()),
            (Type::Usize, Type::Usize) => Ok(()),
            (Type::F32, Type::F32) => Ok(()),
            (Type::F64, Type::F64) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),
            (Type::Char, Type::Char) => Ok(()),
            (Type::Str, Type::Str) => Ok(()),
            (Type::Range, Type::Range) => Ok(()),

            // Note: No implicit numeric coercions in unification
            // i32 and i64 are distinct types
            // Coercions would be handled separately in type checking
            
            // Special case for PrimeZeta compatibility: allow i64 to unify with unsigned integers
            // This is unsafe but needed for v0.3.26 compatibility and Murphy's Sieve algorithm
            (Type::I64, Type::U8) | (Type::U8, Type::I64) => {
                // Allow unification between i64 and u8 (for array element comparisons)
                Ok(())
            }
            (Type::I64, Type::U16) | (Type::U16, Type::I64) => {
                // Allow unification between i64 and u16
                Ok(())
            }
            (Type::I64, Type::U32) | (Type::U32, Type::I64) => {
                // Allow unification between i64 and u32
                Ok(())
            }
            (Type::I64, Type::U64) | (Type::U64, Type::I64) => {
                // Allow unification between i64 and u64
                // In a real implementation, we would check bounds
                Ok(())
            }
            (Type::I64, Type::Usize) | (Type::Usize, Type::I64) => {
                // Allow unification between i64 and usize
                Ok(())
            }

            // Type variable cases
            (Type::Variable(a), Type::Variable(b)) if a == b => Ok(()),
            (Type::Variable(a), _) => {
                if self.occurs_check(a, &t2) {
                    Err(UnifyError::OccursCheck(a.clone(), t2))
                } else {
                    self.mapping.insert(a.clone(), t2);
                    Ok(())
                }
            }
            (_, Type::Variable(_b)) => self.unify(&t2, &t1),

            // Array types
            (Type::Array(inner1, size1), Type::Array(inner2, size2)) => {
                if size1 != size2 {
                    return Err(UnifyError::Mismatch(t1, t2));
                }
                self.unify(inner1, inner2)
            }

            // Dynamic array types
            (Type::DynamicArray(inner1), Type::DynamicArray(inner2)) => {
                self.unify(inner1, inner2)
            }

            // Tuple types
            (Type::Tuple(types1), Type::Tuple(types2)) => {
                if types1.len() != types2.len() {
                    return Err(UnifyError::ArityMismatch(types1.len(), types2.len()));
                }
                for (t1, t2) in types1.iter().zip(types2) {
                    self.unify(t1, t2)?;
                }
                Ok(())
            }

            // Function types
            (Type::Function(params1, ret1), Type::Function(params2, ret2)) => {
                if params1.len() != params2.len() {
                    return Err(UnifyError::ArityMismatch(params1.len(), params2.len()));
                }
                for (p1, p2) in params1.iter().zip(params2) {
                    self.unify(p1, p2)?;
                }
                self.unify(ret1, ret2)
            }

            // Reference types
            (
                Type::Ref(inner1, lifetime1, mutability1),
                Type::Ref(inner2, lifetime2, mutability2),
            ) => {
                // Mutability must match
                if mutability1 != mutability2 {
                    return Err(UnifyError::Mismatch(t1, t2));
                }
                // Lifetimes must be compatible
                // For now, we'll require exact match (simplified)
                if lifetime1 != lifetime2 {
                    // In a real implementation, we'd check lifetime subtyping
                    // For now, we'll allow unification if one is a variable
                    if let (Lifetime::Variable(_), _) | (_, Lifetime::Variable(_)) =
                        (lifetime1, lifetime2)
                    {
                        // Allow unification with lifetime variables
                    } else {
                        return Err(UnifyError::Mismatch(t1, t2));
                    }
                }
                // Inner types must unify
                self.unify(inner1, inner2)
            }

            // Named types (structs, enums, concepts)
            (Type::Named(name1, args1), Type::Named(name2, args2)) => {
                // Type names must match OR we have a variant relationship
                if name1 != name2 {
                    // Check for variant relationships (e.g., Some -> Option)
                    match (name1.as_str(), name2.as_str()) {
                        // Option variants
                        ("Some", "Option") | ("Option", "Some")
                            if args1.len() == 1 && args2.len() == 1 =>
                        {
                            // Some<T> unifies with Option<T>
                            return self.unify(&args1[0], &args2[0]);
                        }
                        ("None", "Option") | ("Option", "None") => {
                            // None unifies with Option<T> for any T
                            // None has no type arguments, Option has one
                            if name1 == "None" && args1.is_empty() && args2.len() == 1 {
                                // None can unify with Option<T> for any T
                                return Ok(());
                            } else if name2 == "None" && args2.is_empty() && args1.len() == 1 {
                                // Option<T> can unify with None
                                return Ok(());
                            }
                        }
                        // Result variants
                        ("Ok", "Result") | ("Result", "Ok")
                            if args1.len() == 2 && args2.len() == 2 =>
                        {
                            // Ok<T, E> unifies with Result<T, E>
                            self.unify(&args1[0], &args2[0])?;
                            return self.unify(&args1[1], &args2[1]);
                        }
                        ("Err", "Result") | ("Result", "Err")
                            if args1.len() == 2 && args2.len() == 2 =>
                        {
                            // Err<T, E> unifies with Result<T, E>
                            self.unify(&args1[0], &args2[0])?;
                            return self.unify(&args1[1], &args2[1]);
                        }
                        _ => {} // No variant relationship
                    }

                    // No variant relationship found
                    return Err(UnifyError::Mismatch(t1, t2));
                }

                // Same type name - check arity
                if args1.len() != args2.len() {
                    return Err(UnifyError::ArityMismatch(args1.len(), args2.len()));
                }

                // Unify each type argument
                for (arg1, arg2) in args1.iter().zip(args2) {
                    self.unify(arg1, arg2)?;
                }
                Ok(())
            }

            // Mismatch
            _ => Err(UnifyError::Mismatch(t1, t2)),
        }
    }

    /// Helper function to collect type variables from a type
    fn collect_type_vars(ty: &Type, type_vars: &mut std::collections::HashSet<TypeVar>) {
        match ty {
            Type::Variable(var) => {
                type_vars.insert(var.clone());
            }
            Type::Array(inner, _) => {
                Self::collect_type_vars(inner, type_vars);
            }
            Type::Slice(inner) => {
                Self::collect_type_vars(inner, type_vars);
            }
            Type::DynamicArray(inner) => {
                Self::collect_type_vars(inner, type_vars);
            }
            Type::Tuple(types) => {
                for t in types {
                    Self::collect_type_vars(t, type_vars);
                }
            }
            Type::Ptr(inner, _) => {
                Self::collect_type_vars(inner, type_vars);
            }
            Type::Ref(inner, _, _) => {
                Self::collect_type_vars(inner, type_vars);
            }
            Type::Named(_, args) => {
                for arg in args {
                    Self::collect_type_vars(arg, type_vars);
                }
            }
            Type::Function(params, ret) => {
                for param in params {
                    Self::collect_type_vars(param, type_vars);
                }
                Self::collect_type_vars(ret, type_vars);
            }
            _ => {} // Primitive types don't contain type variables
        }
    }

    /// Improved generic instantiation with bounds checking
    #[allow(clippy::only_used_in_recursion)]
    pub fn instantiate_generic_with_bounds(
        &self,
        generic_ty: &Type,
        type_args: &[Type],
        context: &GenericContext,
    ) -> Result<Type, String> {
        match generic_ty {
            Type::Named(name, generic_params) => {
                // Check arity
                if generic_params.len() != type_args.len() {
                    return Err(format!(
                        "Wrong number of type arguments for {}: expected {}, got {}",
                        name,
                        generic_params.len(),
                        type_args.len()
                    ));
                }

                // Create substitution mapping
                let mut substitution = Substitution::new();

                // Map type parameters to arguments
                for (param, arg) in generic_params.iter().zip(type_args.iter()) {
                    if let Type::Variable(var) = param {
                        // TODO: Check bounds when we have proper TypeVar -> TypeParam mapping
                        // For now, just substitute
                        substitution.mapping.insert(var.clone(), arg.clone());
                    } else {
                        // Complex parameter - recursively instantiate
                        let instantiated_param =
                            self.instantiate_generic_with_bounds(param, type_args, context)?;
                        // For now, require exact match for non-variable parameters
                        if &instantiated_param != arg {
                            return Err(format!(
                                "Type argument mismatch: expected {}, got {}",
                                instantiated_param.display_name(),
                                arg.display_name()
                            ));
                        }
                    }
                }

                // Apply substitution
                Ok(substitution.apply(generic_ty))
            }

            // Handle other type constructors recursively
            Type::Array(inner, size) => {
                let instantiated_inner =
                    self.instantiate_generic_with_bounds(inner, type_args, context)?;
                Ok(Type::Array(Box::new(instantiated_inner), *size))
            }

            Type::Slice(inner) => {
                let instantiated_inner =
                    self.instantiate_generic_with_bounds(inner, type_args, context)?;
                Ok(Type::Slice(Box::new(instantiated_inner)))
            }

            Type::Tuple(types) => {
                let instantiated_types: Result<Vec<Type>, String> = types
                    .iter()
                    .map(|t| self.instantiate_generic_with_bounds(t, type_args, context))
                    .collect();
                Ok(Type::Tuple(instantiated_types?))
            }

            Type::Ref(inner, lifetime, mutability) => {
                let instantiated_inner =
                    self.instantiate_generic_with_bounds(inner, type_args, context)?;
                Ok(Type::Ref(
                    Box::new(instantiated_inner),
                    lifetime.clone(),
                    *mutability,
                ))
            }

            Type::Function(params, ret) => {
                // For function types, we need to handle type variables differently
                // First, collect all unique type variables in the function type
                let mut type_vars = std::collections::HashSet::new();
                for param in params {
                    Self::collect_type_vars(param, &mut type_vars);
                }
                Self::collect_type_vars(ret, &mut type_vars);

                // Check if we have the right number of type arguments
                let type_vars_count = type_vars.len();
                if type_args.len() != type_vars_count {
                    return Err(format!(
                        "Wrong number of type arguments for function: expected {}, got {}",
                        type_vars_count,
                        type_args.len()
                    ));
                }

                // Create a substitution mapping type variables to type arguments
                let mut substitution = self.clone();
                let type_vars_vec: Vec<TypeVar> = type_vars.into_iter().collect();
                for (type_var, type_arg) in type_vars_vec.iter().zip(type_args.iter()) {
                    substitution
                        .mapping
                        .insert(type_var.clone(), type_arg.clone());
                }

                // Apply substitution to parameters and return type
                let instantiated_params: Vec<Type> =
                    params.iter().map(|p| substitution.apply(p)).collect();
                let instantiated_ret = substitution.apply(ret);

                Ok(Type::Function(
                    instantiated_params,
                    Box::new(instantiated_ret),
                ))
            }

            // Type variables get replaced via substitution
            Type::Variable(var) => {
                if let Some(ty) = self.mapping.get(var) {
                    Ok(ty.clone())
                } else {
                    // Unbound type variable remains
                    Ok(Type::Variable(var.clone()))
                }
            }

            // Primitive types remain unchanged
            _ => Ok(generic_ty.clone()),
        }
    }

    /// Check if a type satisfies a trait bound
    pub fn satisfies_bound(&self, ty: &Type, bound: &TraitBound) -> bool {
        let ty = self.apply(ty);

        match bound {
            TraitBound::Copy => self.is_copy(&ty),
            TraitBound::Clone => self.is_clone(&ty),
            TraitBound::Debug => self.is_debug(&ty),
            TraitBound::Default => self.is_default(&ty),
            TraitBound::PartialEq => self.is_partial_eq(&ty),
            TraitBound::Eq => self.is_eq(&ty),
            TraitBound::PartialOrd => self.is_partial_ord(&ty),
            TraitBound::Ord => self.is_ord(&ty),
            TraitBound::Hash => self.is_hash(&ty),
            TraitBound::Future => false, // TODO: Implement Future trait check
        }
    }

    /// Helper methods for trait checks
    fn is_copy(&self, ty: &Type) -> bool {
        match ty {
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::Usize
            | Type::F32
            | Type::F64
            | Type::Bool
            | Type::Char => true,

            Type::Ref(_, _, Mutability::Immutable) => true, // Shared references are Copy

            Type::Tuple(types) => types.iter().all(|t| self.is_copy(t)),

            Type::Named(name, args) => {
                // Check if this is a known Copy type
                match name.as_str() {
                    "Option" if args.len() == 1 => self.is_copy(&args[0]),
                    "Result" if args.len() == 2 => self.is_copy(&args[0]) && self.is_copy(&args[1]),
                    _ => false,
                }
            }

            _ => false,
        }
    }

    fn is_clone(&self, ty: &Type) -> bool {
        // For now, assume all types are Clone
        // In a real implementation, we'd track which types implement Clone
        true
    }

    fn is_debug(&self, ty: &Type) -> bool {
        // Assume all types are Debug for now
        true
    }

    fn is_default(&self, ty: &Type) -> bool {
        match ty {
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::Usize
            | Type::F32
            | Type::F64
            | Type::Bool
            | Type::Char => true,

            Type::Tuple(types) => types.iter().all(|t| self.is_default(t)),

            Type::Named(name, args) => match name.as_str() {
                "Option" if args.len() == 1 => self.is_default(&args[0]),
                "Vec" if args.len() == 1 => self.is_default(&args[0]),
                _ => false,
            },

            _ => false,
        }
    }

    fn is_partial_eq(&self, ty: &Type) -> bool {
        // Most types are PartialEq
        !matches!(ty, Type::Error)
    }

    fn is_eq(&self, ty: &Type) -> bool {
        self.is_partial_eq(ty) // For now, same as PartialEq
    }

    fn is_partial_ord(&self, ty: &Type) -> bool {
        matches!(
            ty,
            Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::Usize
                | Type::F32
                | Type::F64
                | Type::Bool
                | Type::Char
        )
    }

    fn is_ord(&self, ty: &Type) -> bool {
        self.is_partial_ord(ty) // For now, same as PartialOrd
    }

    fn is_hash(&self, ty: &Type) -> bool {
        // Assume all types are Hash for now
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_display() {
        assert_eq!(Type::I64.display_name(), "i64");
        assert_eq!(Type::Bool.display_name(), "bool");
        assert_eq!(Type::Str.display_name(), "str");

        let array = Type::Array(Box::new(Type::I32), 10);
        assert_eq!(array.display_name(), "[i32; 10]");

        let tuple = Type::Tuple(vec![Type::I32, Type::Bool]);
        assert_eq!(tuple.display_name(), "(i32, bool)");

        let func = Type::Function(vec![Type::I32, Type::I32], Box::new(Type::I32));
        assert_eq!(func.display_name(), "(i32, i32) -> i32");
    }

    #[test]
    fn test_unify_primitives() {
        let mut subst = Substitution::new();
        assert!(subst.unify(&Type::I64, &Type::I64).is_ok());
        assert!(subst.unify(&Type::I64, &Type::Bool).is_err());
    }

    #[test]
    fn test_unify_variables() {
        let mut subst = Substitution::new();
        let a = Type::Variable(TypeVar::fresh());
        let b = Type::Variable(TypeVar::fresh());

        // a = i64
        assert!(subst.unify(&a, &Type::I64).is_ok());
        assert_eq!(subst.apply(&a), Type::I64);

        // b = a (which is now i64)
        assert!(subst.unify(&b, &a).is_ok());
        assert_eq!(subst.apply(&b), Type::I64);
    }

    #[test]
    fn test_occurs_check() {
        let mut subst = Substitution::new();
        let a = Type::Variable(TypeVar::fresh());
        let list = Type::Named("List".to_string(), vec![a.clone()]);

        // a = List<a> should fail occurs check
        assert!(subst.unify(&a, &list).is_err());
    }

    #[test]
    fn test_unify_functions() {
        let mut subst = Substitution::new();
        let a = Type::Variable(TypeVar::fresh());

        // (i32) -> T  unified with  (i32) -> i64
        let func1 = Type::Function(vec![Type::I32], Box::new(a.clone()));
        let func2 = Type::Function(vec![Type::I32], Box::new(Type::I64));

        assert!(subst.unify(&func1, &func2).is_ok());
        assert_eq!(subst.apply(&a), Type::I64);
    }

    #[test]
    fn test_trait_bounds() {
        let subst = Substitution::new();

        // i32 should satisfy Copy, Clone, Debug
        assert!(subst.satisfies_bound(&Type::I32, &TraitBound::Copy));
        assert!(subst.satisfies_bound(&Type::I32, &TraitBound::Clone));
        assert!(subst.satisfies_bound(&Type::I32, &TraitBound::Debug));

        // &i32 should be Copy (shared reference)
        let ref_i32 = Type::Ref(Box::new(Type::I32), Lifetime::Static, Mutability::Immutable);
        assert!(subst.satisfies_bound(&ref_i32, &TraitBound::Copy));

        // &mut i32 should NOT be Copy
        let mut_ref_i32 = Type::Ref(Box::new(Type::I32), Lifetime::Static, Mutability::Mutable);
        assert!(!subst.satisfies_bound(&mut_ref_i32, &TraitBound::Copy));

        // Tuple of Copy types should be Copy
        let tuple = Type::Tuple(vec![Type::I32, Type::Bool]);
        assert!(subst.satisfies_bound(&tuple, &TraitBound::Copy));
    }

    #[test]
    fn test_generic_context() {
        let mut context = GenericContext::new();

        // Add a type parameter with bounds
        let type_param = TypeParam {
            name: "T".to_string(),
            bounds: vec![TraitBound::Clone, TraitBound::Debug],
            kind: Kind::Star,
        };
        context.add_type_param(type_param);

        // Should find the type parameter
        assert!(context.find_type_param("T").is_some());
        let found = context.find_type_param("T").unwrap();
        assert_eq!(found.name, "T");
        assert_eq!(found.bounds.len(), 2);
        assert!(found.bounds.contains(&TraitBound::Clone));
        assert!(found.bounds.contains(&TraitBound::Debug));

        // Should not find non-existent parameter
        assert!(context.find_type_param("U").is_none());
    }

    #[test]
    fn test_generic_instantiation_with_bounds() {
        let context = GenericContext::new();
        let subst = Substitution::new();

        // Create a generic type Vec<T>
        let t_var = Type::Variable(TypeVar::fresh());
        let vec_t = Type::Named("Vec".to_string(), vec![t_var.clone()]);

        // Instantiate with i32
        let result = subst.instantiate_generic_with_bounds(&vec_t, &[Type::I32], &context);
        if let Err(e) = &result {
            eprintln!("Error: {}", e);
        }
        assert!(result.is_ok(), "Result: {:?}", result);
        let vec_i32 = result.unwrap();

        // Should be Vec<i32>
        if let Type::Named(name, args) = vec_i32 {
            assert_eq!(name, "Vec");
            assert_eq!(args.len(), 1);
            assert_eq!(args[0], Type::I32);
        } else {
            panic!("Expected Named type");
        }

        // Test with simple generic: Option<T>
        let u_var = Type::Variable(TypeVar::fresh());
        let option_t = Type::Named("Option".to_string(), vec![u_var.clone()]);
        let result = subst.instantiate_generic_with_bounds(&option_t, &[Type::Bool], &context);
        assert!(result.is_ok());
        let option_bool = result.unwrap();

        // Should be Option<bool>
        if let Type::Named(name, args) = option_bool {
            assert_eq!(name, "Option");
            assert_eq!(args.len(), 1);
            assert_eq!(args[0], Type::Bool);
        } else {
            panic!("Expected Named type");
        }
    }

    #[test]
    fn test_generic_instantiation_error_cases() {
        let context = GenericContext::new();
        let subst = Substitution::new();

        // Create a generic type Result<T, E>
        let t_var = Type::Variable(TypeVar::fresh());
        let e_var = Type::Variable(TypeVar::fresh());
        let result_te = Type::Named("Result".to_string(), vec![t_var, e_var]);

        // Wrong number of type arguments
        let result = subst.instantiate_generic_with_bounds(&result_te, &[Type::I32], &context);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(error.contains("Wrong number of type arguments"));

        // Correct number of arguments
        let result =
            subst.instantiate_generic_with_bounds(&result_te, &[Type::I32, Type::Bool], &context);
        assert!(result.is_ok());
        let result_ib = result.unwrap();

        // Should be Result<i32, bool>
        if let Type::Named(name, args) = result_ib {
            assert_eq!(name, "Result");
            assert_eq!(args.len(), 2);
            assert_eq!(args[0], Type::I32);
            assert_eq!(args[1], Type::Bool);
        } else {
            panic!("Expected Named type");
        }
    }
}
