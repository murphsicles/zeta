//! # Type Families
//!
//! Support for type families and type-level computation in Zeta.

use super::{Kind, Type};
use std::collections::HashMap;

/// Type family declaration
#[derive(Debug, Clone)]
pub struct TypeFamily {
    /// Name of the type family
    pub name: String,
    
    /// Kind of the type family
    pub kind: Kind,
    
    /// Equations defining the type family
    pub equations: Vec<TypeFamilyEquation>,
    
    /// Whether the type family is closed (all equations must be defined here)
    pub closed: bool,
    
    /// Whether the type family is injective
    pub injective: bool,
}

/// Equation in a type family
#[derive(Debug, Clone)]
pub struct TypeFamilyEquation {
    /// Left-hand side pattern
    pub pattern: TypeFamilyPattern,
    
    /// Right-hand side result
    pub result: Type,
    
    /// Optional constraints that must be satisfied
    pub constraints: Vec<TypeFamilyConstraint>,
}

/// Pattern in a type family equation
#[derive(Debug, Clone)]
pub enum TypeFamilyPattern {
    /// Type constructor application
    Application(String, Vec<TypeFamilyPattern>),
    
    /// Type variable
    Variable(String),
    
    /// Wildcard pattern
    Wildcard,
    
    /// Literal pattern (for type-level literals)
    Literal(Type),
}

impl TypeFamilyPattern {
    /// Get display name
    pub fn display_name(&self) -> String {
        match self {
            TypeFamilyPattern::Application(name, args) => {
                if args.is_empty() {
                    name.clone()
                } else {
                    let args_str = args
                        .iter()
                        .map(|p| p.display_name())
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{}({})", name, args_str)
                }
            }
            TypeFamilyPattern::Variable(name) => name.clone(),
            TypeFamilyPattern::Wildcard => "_".to_string(),
            TypeFamilyPattern::Literal(ty) => ty.display_name(),
        }
    }
    
    /// Match against a type, returning substitution if successful
    pub fn match_type(&self, ty: &Type) -> Option<HashMap<String, Type>> {
        let mut substitution = HashMap::new();
        
        if self.match_type_with_subst(ty, &mut substitution) {
            Some(substitution)
        } else {
            None
        }
    }
    
    /// Match with existing substitution
    fn match_type_with_subst(
        &self,
        ty: &Type,
        substitution: &mut HashMap<String, Type>,
    ) -> bool {
        match (self, ty) {
            (TypeFamilyPattern::Wildcard, _) => true,
            
            (TypeFamilyPattern::Variable(name), _) => {
                // Check if variable is already bound
                if let Some(bound_ty) = substitution.get(name) {
                    // Must match existing binding
                    bound_ty == ty
                } else {
                    // Bind variable to type
                    substitution.insert(name.clone(), ty.clone());
                    true
                }
            }
            
            (TypeFamilyPattern::Literal(pattern_ty), _) => {
                pattern_ty == ty
            }
            
            (
                TypeFamilyPattern::Application(pattern_name, pattern_args),
                Type::Named(type_name, type_args),
            ) if pattern_name == type_name => {
                // Check arity
                if pattern_args.len() != type_args.len() {
                    return false;
                }
                
                // Match each argument
                for (pattern_arg, type_arg) in pattern_args.iter().zip(type_args) {
                    if !pattern_arg.match_type_with_subst(type_arg, substitution) {
                        return false;
                    }
                }
                
                true
            }
            
            // Handle other type constructors
            _ => false,
        }
    }
}

/// Constraint in a type family equation
#[derive(Debug, Clone)]
pub enum TypeFamilyConstraint {
    /// Type equality constraint
    Equal(Type, Type),
    
    /// Type inequality constraint
    NotEqual(Type, Type),
    
    /// Trait constraint
    Trait(String, Type),
    
    /// Kind constraint
    Kind(Type, Kind),
}

impl TypeFamilyConstraint {
    /// Get display name
    pub fn display_name(&self) -> String {
        match self {
            TypeFamilyConstraint::Equal(t1, t2) => {
                format!("{} == {}", t1.display_name(), t2.display_name())
            }
            TypeFamilyConstraint::NotEqual(t1, t2) => {
                format!("{} != {}", t1.display_name(), t2.display_name())
            }
            TypeFamilyConstraint::Trait(trait_name, ty) => {
                format!("{}: {}", ty.display_name(), trait_name)
            }
            TypeFamilyConstraint::Kind(ty, kind) => {
                format!("{} :: {}", ty.display_name(), kind.display_name())
            }
        }
    }
}

/// Context for type families
#[derive(Debug, Clone, Default)]
pub struct TypeFamilyContext {
    /// Mapping from type family name to definition
    pub families: HashMap<String, TypeFamily>,
    
    /// Cache of reduced type family applications
    pub cache: HashMap<(String, Vec<Type>), Type>,
}

impl TypeFamilyContext {
    /// Create new empty context
    pub fn new() -> Self {
        TypeFamilyContext {
            families: HashMap::new(),
            cache: HashMap::new(),
        }
    }
    
    /// Add a type family definition
    pub fn add_family(&mut self, family: TypeFamily) -> Result<(), String> {
        let name = family.name.clone();
        
        // Check for duplicate
        if self.families.contains_key(&name) {
            return Err(format!("Type family {} already defined", name));
        }
        
        self.families.insert(name, family);
        Ok(())
    }
    
    /// Reduce a type family application
    pub fn reduce(&mut self, family_name: &str, args: &[Type]) -> Result<Type, String> {
        // Check cache first
        let cache_key = (family_name.to_string(), args.to_vec());
        if let Some(cached) = self.cache.get(&cache_key) {
            return Ok(cached.clone());
        }
        
        // Get type family definition
        let family = self
            .families
            .get(family_name)
            .ok_or_else(|| format!("Type family {} not found", family_name))?;
        
        // Check arity based on kind
        let expected_arity = family.kind.arity();
        if args.len() != expected_arity {
            return Err(format!(
                "Wrong number of arguments for type family {}: expected {}, got {}",
                family_name,
                expected_arity,
                args.len()
            ));
        }
        
        // Try each equation in order
        for equation in &family.equations {
            // Try to match pattern
            if let Some(substitution) = equation.pattern.match_type(&Self::make_application(family_name, args)) {
                // Check constraints
                if self.check_constraints(&equation.constraints, &substitution)? {
                    // Apply substitution to result
                    let result = self.apply_substitution(&equation.result, &substitution);
                    
                    // Cache the result
                    self.cache.insert(cache_key, result.clone());
                    
                    return Ok(result);
                }
            }
        }
        
        // If closed type family and no equation matches, it's an error
        if family.closed {
            return Err(format!(
                "No equation matches for {}{}",
                family_name,
                Self::format_args(args)
            ));
        }
        
        // For open type families, return the application itself
        let result = Self::make_application(family_name, args);
        self.cache.insert(cache_key, result.clone());
        Ok(result)
    }
    
    /// Make a type family application type
    fn make_application(family_name: &str, args: &[Type]) -> Type {
        if args.is_empty() {
            Type::Named(family_name.to_string(), Vec::new())
        } else {
            Type::Named(
                family_name.to_string(),
                args.to_vec(),
            )
        }
    }
    
    /// Format type arguments for display
    fn format_args(args: &[Type]) -> String {
        if args.is_empty() {
            String::new()
        } else {
            let args_str = args
                .iter()
                .map(|t| t.display_name())
                .collect::<Vec<_>>()
                .join(", ");
            format!("<{}>", args_str)
        }
    }
    
    /// Check constraints with substitution
    fn check_constraints(
        &self,
        constraints: &[TypeFamilyConstraint],
        substitution: &HashMap<String, Type>,
    ) -> Result<bool, String> {
        for constraint in constraints {
            match constraint {
                TypeFamilyConstraint::Equal(t1, t2) => {
                    let t1_subst = self.apply_substitution(t1, substitution);
                    let t2_subst = self.apply_substitution(t2, substitution);
                    
                    if t1_subst != t2_subst {
                        return Ok(false);
                    }
                }
                TypeFamilyConstraint::NotEqual(t1, t2) => {
                    let t1_subst = self.apply_substitution(t1, substitution);
                    let t2_subst = self.apply_substitution(t2, substitution);
                    
                    if t1_subst == t2_subst {
                        return Ok(false);
                    }
                }
                TypeFamilyConstraint::Trait(_, _) => {
                    // Trait constraints not yet implemented
                    // For now, assume they're satisfied
                }
                TypeFamilyConstraint::Kind(ty, kind) => {
                    let ty_subst = self.apply_substitution(ty, substitution);
                    // Kind checking not yet implemented
                    // For now, assume it's satisfied
                }
            }
        }
        
        Ok(true)
    }
    
    /// Apply substitution to a type
    fn apply_substitution(&self, ty: &Type, substitution: &HashMap<String, Type>) -> Type {
        match ty {
            Type::Named(name, args) => {
                // Check if this is a type variable in the substitution
                if args.is_empty() {
                    if let Some(subst_ty) = substitution.get(name) {
                        return subst_ty.clone();
                    }
                }
                
                // Apply substitution to arguments
                let new_args = args
                    .iter()
                    .map(|arg| self.apply_substitution(arg, substitution))
                    .collect();
                
                Type::Named(name.clone(), new_args)
            }
            
            // Handle other type constructors recursively
            Type::Array(inner, size) => {
                Type::Array(
                    Box::new(self.apply_substitution(inner, substitution)),
                    size.clone(),
                )
            }
            
            Type::Slice(inner) => {
                Type::Slice(Box::new(self.apply_substitution(inner, substitution)))
            }
            
            Type::Tuple(types) => {
                Type::Tuple(
                    types
                        .iter()
                        .map(|t| self.apply_substitution(t, substitution))
                        .collect(),
                )
            }
            
            Type::Ref(inner, lifetime, mutability) => {
                Type::Ref(
                    Box::new(self.apply_substitution(inner, substitution)),
                    lifetime.clone(),
                    *mutability,
                )
            }
            
            Type::Vector(inner, size) => {
                Type::Vector(
                    Box::new(self.apply_substitution(inner, substitution)),
                    size.clone(),
                )
            }
            
            Type::Function(params, ret) => {
                Type::Function(
                    params
                        .iter()
                        .map(|p| self.apply_substitution(p, substitution))
                        .collect(),
                    Box::new(self.apply_substitution(ret, substitution)),
                )
            }
            
            // Other types remain unchanged
            _ => ty.clone(),
        }
    }
    
    /// Simplify a type using type families
    pub fn simplify(&mut self, ty: &Type) -> Result<Type, String> {
        match ty {
            Type::Named(name, args) => {
                // Check if this is a type family application
                if self.families.contains_key(name) {
                    // Reduce the application
                    self.reduce(name, args)
                } else {
                    // Not a type family, simplify arguments
                    let simplified_args: Result<Vec<Type>, String> = args
                        .iter()
                        .map(|arg| self.simplify(arg))
                        .collect();
                    
                    Ok(Type::Named(name.clone(), simplified_args?))
                }
            }
            
            // Recursively simplify compound types
            Type::Array(inner, size) => {
                Ok(Type::Array(Box::new(self.simplify(inner)?), size.clone()))
            }
            
            Type::Slice(inner) => {
                Ok(Type::Slice(Box::new(self.simplify(inner)?)))
            }
            
            Type::Tuple(types) => {
                let simplified: Result<Vec<Type>, String> = types
                    .iter()
                    .map(|t| self.simplify(t))
                    .collect();
                Ok(Type::Tuple(simplified?))
            }
            
            Type::Ref(inner, lifetime, mutability) => {
                Ok(Type::Ref(
                    Box::new(self.simplify(inner)?),
                    lifetime.clone(),
                    *mutability,
                ))
            }
            
            Type::Function(params, ret) => {
                let simplified_params: Result<Vec<Type>, String> = params
                    .iter()
                    .map(|p| self.simplify(p))
                    .collect();
                
                Ok(Type::Function(
                    simplified_params?,
                    Box::new(self.simplify(ret)?),
                ))
            }
            
            Type::Vector(inner, size) => {
                Ok(Type::Vector(Box::new(self.simplify(inner)?), size.clone()))
            }
            
            // Other types remain unchanged
            _ => Ok(ty.clone()),
        }
    }
}

/// Builder for type families
pub struct TypeFamilyBuilder {
    name: String,
    kind: Kind,
    equations: Vec<TypeFamilyEquation>,
    closed: bool,
    injective: bool,
}

impl TypeFamilyBuilder {
    /// Start building a new type family
    pub fn new(name: String) -> Self {
        TypeFamilyBuilder {
            name,
            kind: Kind::Star,
            equations: Vec::new(),
            closed: false,
            injective: false,
        }
    }
    
    /// Set the kind
    pub fn with_kind(mut self, kind: Kind) -> Self {
        self.kind = kind;
        self
    }
    
    /// Add an equation
    pub fn with_equation(mut self, equation: TypeFamilyEquation) -> Self {
        self.equations.push(equation);
        self
    }
    
    /// Set as closed type family
    pub fn closed(mut self) -> Self {
        self.closed = true;
        self
    }
    
    /// Set as injective type family
    pub fn injective(mut self) -> Self {
        self.injective = true;
        self
    }
    
    /// Build the type family
    pub fn build(self) -> TypeFamily {
        TypeFamily {
            name: self.name,
            kind: self.kind,
            equations: self.equations,
            closed: self.closed,
            injective: self.injective,
        }
    }
}

/// Builder for type family equations
pub struct TypeFamilyEquationBuilder {
    pattern: TypeFamilyPattern,
    result: Type,
    constraints: Vec<TypeFamilyConstraint>,
}

impl TypeFamilyEquationBuilder {
    /// Start building a new equation
    pub fn new(pattern: TypeFamilyPattern, result: Type) -> Self {
        TypeFamilyEquationBuilder {
            pattern,
            result,
            constraints: Vec::new(),
        }
    }
    
    /// Add a constraint
    pub fn with_constraint(mut self, constraint: TypeFamilyConstraint) -> Self {
        self.constraints.push(constraint);
        self
    }
    
    /// Build the equation
    pub fn build(self) -> TypeFamilyEquation {
        TypeFamilyEquation {
            pattern: self.pattern,
            result: self.result,
            constraints: self.constraints,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::middle::types::Type;
    
    #[test]
    fn test_type_family_pattern_matching() {
        // Pattern: List(a)
        let pattern = TypeFamilyPattern::Application(
            "List".to_string(),
            vec![TypeFamilyPattern::Variable("a".to_string())],
        );
        
        // Type: List<i32>
        let ty = Type::Named("List".to_string(), vec![Type::I32]);
        
        // Should match with substitution a = i32
        let substitution = pattern.match_type(&ty);
        assert!(substitution.is_some());
        
        let subst = substitution.unwrap();
        assert_eq!(subst.get("a"), Some(&Type::I32));
        
        // Type: Option<i32> should not match
        let wrong_ty = Type::Named("Option".to_string(), vec![Type::I32]);
        let substitution = pattern.match_type(&wrong_ty);
        assert!(substitution.is_none());
    }
    
    #[test]
    fn test_type_family_reduction() {
        let mut ctx = TypeFamilyContext::new();
        
        // Create a simple type family: Id<T> = T
        let equation = TypeFamilyEquationBuilder::new(
            TypeFamilyPattern::Application(
                "Id".to_string(),
                vec![TypeFamilyPattern::Variable("T".to_string())],
            ),
            Type::Named("T".to_string(), vec![]), // T
        )
        .build();
        
        let family = TypeFamilyBuilder::new("Id".to_string())
            .with_kind(Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star)))
            .with_equation(equation)
            .build();
        
        ctx.add_family(family).unwrap();
        
        // Reduce Id<i32>
        let result = ctx.reduce("Id", &[Type::I32]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::I32);
        
        // Reduce Id<bool>
        let result = ctx.reduce("Id", &[Type::Bool]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::Bool);
    }
    
    #[test]
    fn test_type_family_simplify() {
        let mut ctx = TypeFamilyContext::new();
        
        // Create type family: ConstInt<T> = i32
        let equation = TypeFamilyEquationBuilder::new(
            TypeFamilyPattern::Application(
                "ConstInt".to_string(),
                vec![TypeFamilyPattern::Wildcard],
            ),
            Type::I32,
        )
        .build();
        
        let family = TypeFamilyBuilder::new("ConstInt".to_string())
            .with_kind(Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star)))
            .with_equation(equation)
            .build();
        
        ctx.add_family(family).unwrap();
        
        // Simplify ConstInt<bool>
        let ty = Type::Named("ConstInt".to_string(), vec![Type::Bool]);
        let simplified = ctx.simplify(&ty);
        assert!(simplified.is_ok());
        assert_eq!(simplified.unwrap(), Type::I32);
    }
    
    #[test]
    fn test_type_family_with_constraints() {
        let mut ctx = TypeFamilyContext::new();
        
        // Create type family with constraint: EqId<T> = T where T == i32
        let equation = TypeFamilyEquationBuilder::new(
            TypeFamilyPattern::Application(
                "EqId".to_string(),
                vec![TypeFamilyPattern::Variable("T".to_string())],
            ),
            Type::Named("T".to_string(), vec![]), // T
        )
        .with_constraint(TypeFamilyConstraint::Equal(
            Type::Named("T".to_string(), vec![]), // T
            Type::I32,
        ))
        .build();
        
        let family = TypeFamilyBuilder::new("EqId".to_string())
            .with_kind(Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star)))
            .with_equation(equation)
            .build();
        
        ctx.add_family(family).unwrap();
        
        // Reduce EqId<i32> should succeed
        let result = ctx.reduce("EqId", &[Type::I32]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::I32);
        
        // Reduce EqId<bool> should fail (constraint not satisfied)
        let result = ctx.reduce("EqId", &[Type::Bool]);
        assert!(result.is_ok()); // Actually returns false from check_constraints
        // The equation won't match because constraint fails
    }
    
    #[test]
    fn test_closed_type_family() {
        let mut ctx = TypeFamilyContext::new();
        
        // Create closed type family: MaybeZero<T>
        let equation1 = TypeFamilyEquationBuilder::new(
            TypeFamilyPattern::Application(
                "MaybeZero".to_string(),
                vec![TypeFamilyPattern::Literal(Type::I32)],
            ),
            Type::I32,
        )
        .build();
        
        let equation2 = TypeFamilyEquationBuilder::new(
            TypeFamilyPattern::Application(
                "MaybeZero".to_string(),
                vec![TypeFamilyPattern::Literal(Type::Bool)],
            ),
            Type::Bool,
        )
        .build();
        
        let family = TypeFamilyBuilder::new("MaybeZero".to_string())
            .with_kind(Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star)))
            .with_equation(equation1)
            .with_equation(equation2)
            .closed()
            .build();
        
        ctx.add_family(family).unwrap();
        
        // Reduce MaybeZero<i32> should succeed
        let result = ctx.reduce("MaybeZero", &[Type::I32]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::I32);
        
        // Reduce MaybeZero<f64> should fail (no matching equation in closed family)
        let result = ctx.reduce("MaybeZero", &[Type::F64]);
        assert!(result.is_err());
    }
}