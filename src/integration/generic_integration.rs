//! Generic Type System Integration Bridge
//!
//! This module provides the integration layer between parser, type checker,
//! and codegen for generic type support. It coordinates:
//! 1. Enhanced AST with GenericParam instead of Vec<String>
//! 2. Type context propagation for generic parameters
//! 3. Constraint collection and solving
//! 4. Monomorphization coordination
//! 5. Unified error reporting

use std::collections::HashMap;
use std::sync::Arc;

use crate::frontend::ast::AstNode;
use crate::middle::types::lifetime::{Lifetime, LifetimeVar};
use crate::middle::types::{Substitution, Type, TypeVar};

/// Enhanced generic parameter representation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericParam {
    /// Type parameter with optional trait bounds: `T`, `T: Debug`, `T: Debug + Clone`
    Type {
        name: String,
        bounds: Vec<String>, // Simplified for now
    },
    /// Lifetime parameter: `'a`, `'static`
    Lifetime { name: String },
    /// Const parameter: `const N: usize`
    Const { name: String, ty: Type },
}

/// Where clause for additional bounds
#[derive(Debug, Clone)]
pub struct WhereClause {
    pub param_name: String,
    pub bounds: Vec<String>,
}

/// Generic function representation for monomorphization
#[derive(Debug, Clone)]
pub struct GenericFunction {
    pub name: String,
    pub generic_params: Vec<GenericParam>,
    pub param_types: Vec<Type>,
    pub return_type: Type,
    pub body: Arc<AstNode>, // AST with type variables

    // Cache of concrete instantiations
    pub instantiations: HashMap<Vec<Type>, ConcreteFunction>,
}

/// Concrete function after monomorphization
#[derive(Debug, Clone)]
pub struct ConcreteFunction {
    pub generic_fn: Arc<GenericFunction>,
    pub type_args: Vec<Type>,
    pub param_types: Vec<Type>, // With type_args substituted
    pub return_type: Type,      // With type_args substituted
                                // MIR and machine code would be added here
}

/// Type context for generic parameter tracking
#[derive(Debug, Clone)]
pub struct TypeContext {
    /// Variables in current scope
    pub variables: HashMap<String, Type>,

    /// Generic parameters in scope
    pub generic_params: HashMap<String, GenericParam>,

    /// Current substitutions for type variables
    pub substitution: Substitution,

    /// Lifetime context
    pub lifetimes: HashMap<String, Lifetime>,

    /// Parent context (for nested scopes)
    pub parent: Option<Arc<TypeContext>>,
}

impl Default for TypeContext {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeContext {
    /// Create a new root type context
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            generic_params: HashMap::new(),
            substitution: Substitution::new(),
            lifetimes: HashMap::new(),
            parent: None,
        }
    }

    /// Enter a new scope (e.g., function body)
    pub fn enter_scope(&self) -> Self {
        Self {
            variables: HashMap::new(),
            generic_params: self.generic_params.clone(),
            substitution: self.substitution.clone(),
            lifetimes: self.lifetimes.clone(),
            parent: Some(Arc::new(self.clone())),
        }
    }

    /// Add generic parameters to the current scope
    pub fn add_generic_params(&mut self, params: &[GenericParam]) {
        for param in params {
            match param {
                GenericParam::Type { name, bounds } => {
                    // Create a fresh type variable for this generic parameter
                    let ty_var = Type::Variable(TypeVar::fresh());
                    self.generic_params.insert(name.clone(), param.clone());
                    self.variables.insert(name.clone(), ty_var);
                }
                GenericParam::Lifetime { name } => {
                    // Create a fresh lifetime variable
                    let lt_var = LifetimeVar::fresh();
                    let lifetime = Lifetime::Variable(lt_var);
                    self.lifetimes.insert(name.clone(), lifetime);
                }
                GenericParam::Const { name, ty } => {
                    // Const parameters are handled differently
                    self.variables.insert(name.clone(), ty.clone());
                }
            }
        }
    }

    /// Look up a type, resolving generic parameters
    pub fn lookup_type(&self, name: &str) -> Option<Type> {
        // Check if it's a generic parameter
        if let Some(param) = self.generic_params.get(name) {
            match param {
                GenericParam::Type { .. } => {
                    // Return the type variable for this generic parameter
                    self.variables.get(name).cloned()
                }
                _ => None,
            }
        } else {
            // Check current variables
            self.variables.get(name).cloned().or_else(|| {
                // Check parent scope
                self.parent.as_ref().and_then(|p| p.lookup_type(name))
            })
        }
    }

    /// Apply substitution to a type
    pub fn apply_substitution(&self, ty: &Type) -> Type {
        self.substitution.apply(ty)
    }
}

/// Constraint for type inference
#[derive(Debug, Clone)]
pub enum Constraint {
    /// Type equality: T = U
    Equality(Type, Type),

    /// Trait bound: T: Debug
    TraitBound(Type, String),

    /// Lifetime bound: 'a: 'b
    LifetimeBound(Lifetime, Lifetime),

    /// Generic instantiation: F<T> = ConcreteType
    Instantiation(Type, Vec<Type>, Type),
}

/// Constraint set for collection and solving
#[derive(Debug, Clone)]
pub struct ConstraintSet {
    pub constraints: Vec<Constraint>,
    pub unsolved: Vec<Constraint>,
}

impl Default for ConstraintSet {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstraintSet {
    pub fn new() -> Self {
        Self {
            constraints: Vec::new(),
            unsolved: Vec::new(),
        }
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint.clone());
        self.unsolved.push(constraint);
    }
}

/// Main integration bridge struct
pub struct GenericIntegration {
    /// Type context stack
    pub type_context: TypeContext,

    /// Constraint set for current inference
    pub constraints: ConstraintSet,

    /// Cache of generic functions
    pub generic_functions: HashMap<String, Arc<GenericFunction>>,

    /// Cache of concrete functions
    pub concrete_functions: HashMap<(String, Vec<Type>), ConcreteFunction>,

    /// Error collector
    pub errors: Vec<IntegrationError>,
}

/// Unified error type for integration
#[derive(Debug, Clone)]
pub enum IntegrationError {
    /// Type checking error
    TypeError(String),

    /// Parser error for generic syntax
    ParseError(String),

    /// Monomorphization error
    MonomorphizationError(String),

    /// Integration coordination error
    CoordinationError(String),
}

impl Default for GenericIntegration {
    fn default() -> Self {
        Self::new()
    }
}

impl GenericIntegration {
    /// Create a new integration bridge
    pub fn new() -> Self {
        Self {
            type_context: TypeContext::new(),
            constraints: ConstraintSet::new(),
            generic_functions: HashMap::new(),
            concrete_functions: HashMap::new(),
            errors: Vec::new(),
        }
    }

    /// Register a generic function
    pub fn register_generic_function(&mut self, generic_fn: GenericFunction) {
        let name = generic_fn.name.clone();
        self.generic_functions.insert(name, Arc::new(generic_fn));
    }

    /// Get or create a concrete function from generic function
    pub fn get_concrete_function(
        &mut self,
        generic_name: &str,
        type_args: &[Type],
    ) -> Result<ConcreteFunction, IntegrationError> {
        let key = (generic_name.to_string(), type_args.to_vec());

        // Check cache first
        if let Some(concrete) = self.concrete_functions.get(&key) {
            return Ok(concrete.clone());
        }

        // Get generic function
        let generic_fn = self.generic_functions.get(generic_name).ok_or_else(|| {
            IntegrationError::MonomorphizationError(format!(
                "Unknown generic function: {}",
                generic_name
            ))
        })?;

        // Check arity
        if type_args.len() != generic_fn.generic_params.len() {
            return Err(IntegrationError::MonomorphizationError(format!(
                "Type argument arity mismatch: expected {}, got {}",
                generic_fn.generic_params.len(),
                type_args.len()
            )));
        }

        // Create substitution map
        let mut substitution = Substitution::new();
        for (param, arg) in generic_fn.generic_params.iter().zip(type_args) {
            if let GenericParam::Type { name, .. } = param {
                // Create a type variable for this generic parameter
                // We need to track the mapping between param names and type vars
                // For now, we'll create a fresh variable and assume it maps to the arg
                let type_var = TypeVar::fresh();
                substitution.mapping.insert(type_var, arg.clone());
            }
        }

        // Apply substitution to get concrete types
        let param_types: Vec<Type> = generic_fn
            .param_types
            .iter()
            .map(|ty| substitution.apply(ty))
            .collect();
        let return_type = substitution.apply(&generic_fn.return_type);

        // Create concrete function
        let concrete = ConcreteFunction {
            generic_fn: generic_fn.clone(),
            type_args: type_args.to_vec(),
            param_types,
            return_type,
        };

        // Cache it
        self.concrete_functions.insert(key, concrete.clone());

        Ok(concrete)
    }

    /// Add an error to the error collection
    pub fn add_error(&mut self, error: IntegrationError) {
        self.errors.push(error);
    }

    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Get all errors
    pub fn get_errors(&self) -> Vec<IntegrationError> {
        self.errors.clone()
    }

    /// Clear errors
    pub fn clear_errors(&mut self) {
        self.errors.clear();
    }
}

/// Conversion utilities between old and new representations
pub mod conversion {
    use super::*;

    /// Convert old Vec<String> generics to Vec<GenericParam>
    pub fn convert_generics(old_generics: &[String]) -> Vec<GenericParam> {
        old_generics
            .iter()
            .map(|s| {
                // Simple conversion - assumes all are type parameters without bounds
                GenericParam::Type {
                    name: s.clone(),
                    bounds: Vec::new(),
                }
            })
            .collect()
    }

    /// Convert old string type to Type with generic context
    pub fn convert_type_string(
        type_str: &str,
        context: &TypeContext,
    ) -> Result<Type, IntegrationError> {
        // Simple implementation - uses existing string_to_type
        // In full implementation, would use enhanced parser
        // For now, delegate to existing conversion
        // This is a placeholder
        Ok(Type::Named(type_str.to_string(), Vec::new()))
    }
}
