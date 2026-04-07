//! # Generic Associated Types (GATs)
//!
//! Support for generic associated types in traits and implementations.

use super::{Kind, Type};
use std::collections::HashMap;

/// Generic associated type declaration
#[derive(Debug, Clone)]
pub struct AssociatedType {
    /// Name of the associated type
    pub name: String,
    
    /// Generic parameters (type and lifetime)
    pub generics: Vec<GenericParam>,
    
    /// Trait bounds on the associated type
    pub bounds: Vec<AssocTraitBound>,
    
    /// Kind of the associated type
    pub kind: Kind,
    
    /// Optional default type
    pub default: Option<Type>,
}

/// Generic parameter for associated types (can be type or lifetime)
#[derive(Debug, Clone)]
pub enum GenericParam {
    /// Type parameter with optional kind
    Type {
        name: String,
        kind: Kind,
        bounds: Vec<AssocTraitBound>,
    },
    
    /// Lifetime parameter
    Lifetime {
        name: String,
    },
}

impl GenericParam {
    /// Get display name
    pub fn display_name(&self) -> String {
        match self {
            GenericParam::Type { name, .. } => name.clone(),
            GenericParam::Lifetime { name } => format!("'{}", name),
        }
    }
    
    /// Check if this is a type parameter
    pub fn is_type(&self) -> bool {
        matches!(self, GenericParam::Type { .. })
    }
    
    /// Check if this is a lifetime parameter
    pub fn is_lifetime(&self) -> bool {
        matches!(self, GenericParam::Lifetime { .. })
    }
}

/// Associated trait bound for associated types
#[derive(Debug, Clone)]
pub enum AssocTraitBound {
    /// Regular trait bound
    Trait(String, Vec<Type>),
    
    /// Lifetime bound
    Lifetime(String),
    
    /// Higher-ranked trait bound (for<'a> ...)
    HigherRanked(Vec<GenericParam>, Box<AssocTraitBound>),
}

impl AssocTraitBound {
    /// Get display name
    pub fn display_name(&self) -> String {
        match self {
            AssocTraitBound::Trait(name, args) => {
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
            AssocTraitBound::Lifetime(name) => format!("'{}", name),
            AssocTraitBound::HigherRanked(params, bound) => {
                let params_str = params
                    .iter()
                    .map(|p| p.display_name())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("for<{}> {}", params_str, bound.display_name())
            }
        }
    }
}

/// Associated type projection (T::Item<Args>)
#[derive(Debug, Clone)]
pub struct AssociatedTypeProjection {
    /// The base type (implementing the trait)
    pub base: Type,
    
    /// Name of the associated type
    pub assoc_name: String,
    
    /// Type arguments for the associated type
    pub type_args: Vec<Type>,
    
    /// Lifetime arguments for the associated type
    pub lifetime_args: Vec<String>,
}

impl AssociatedTypeProjection {
    /// Create a new associated type projection
    pub fn new(base: Type, assoc_name: String) -> Self {
        AssociatedTypeProjection {
            base,
            assoc_name,
            type_args: Vec::new(),
            lifetime_args: Vec::new(),
        }
    }
    
    /// Add type argument
    pub fn with_type_arg(mut self, arg: Type) -> Self {
        self.type_args.push(arg);
        self
    }
    
    /// Add lifetime argument
    pub fn with_lifetime_arg(mut self, arg: String) -> Self {
        self.lifetime_args.push(arg);
        self
    }
    
    /// Get display name
    pub fn display_name(&self) -> String {
        let base_str = self.base.display_name();
        let mut args = Vec::new();
        
        // Add lifetime arguments
        for lifetime in &self.lifetime_args {
            args.push(format!("'{}", lifetime));
        }
        
        // Add type arguments
        for ty in &self.type_args {
            args.push(ty.display_name());
        }
        
        if args.is_empty() {
            format!("{}::{}", base_str, self.assoc_name)
        } else {
            format!("{}::{}<{}>", base_str, self.assoc_name, args.join(", "))
        }
    }
}

/// Context for tracking associated types
#[derive(Debug, Clone, Default)]
pub struct AssociatedTypeContext {
    /// Mapping from (trait_name, assoc_name) to associated type definition
    pub definitions: HashMap<(String, String), AssociatedType>,
    
    /// Mapping from (impl_type, trait_name, assoc_name) to concrete type
    pub implementations: HashMap<(Type, String, String), Type>,
}

impl AssociatedTypeContext {
    /// Create new empty context
    pub fn new() -> Self {
        AssociatedTypeContext {
            definitions: HashMap::new(),
            implementations: HashMap::new(),
        }
    }
    
    /// Add an associated type definition
    pub fn add_definition(
        &mut self,
        trait_name: String,
        assoc_type: AssociatedType,
    ) {
        self.definitions.insert((trait_name, assoc_type.name.clone()), assoc_type);
    }
    
    /// Get an associated type definition
    pub fn get_definition(&self, trait_name: &str, assoc_name: &str) -> Option<&AssociatedType> {
        self.definitions.get(&(trait_name.to_string(), assoc_name.to_string()))
    }
    
    /// Add an associated type implementation
    pub fn add_implementation(
        &mut self,
        impl_type: Type,
        trait_name: String,
        assoc_name: String,
        concrete_type: Type,
    ) {
        self.implementations.insert(
            (impl_type, trait_name, assoc_name),
            concrete_type,
        );
    }
    
    /// Get an associated type implementation
    pub fn get_implementation(
        &self,
        impl_type: &Type,
        trait_name: &str,
        assoc_name: &str,
    ) -> Option<&Type> {
        self.implementations.get(&(impl_type.clone(), trait_name.to_string(), assoc_name.to_string()))
    }
    
    /// Normalize an associated type projection
    pub fn normalize_projection(&self, projection: &AssociatedTypeProjection) -> Result<Type, String> {
        // First, check if the base type implements any traits with this associated type
        // For now, we'll use a simplified approach
        
        // Look for the trait that defines this associated type
        // In a real implementation, we would need to know which trait we're projecting from
        
        // For testing, we'll return a placeholder
        Ok(Type::Named(
            format!("{}::{}", projection.base.display_name(), projection.assoc_name),
            projection.type_args.clone(),
        ))
    }
    
    /// Check if a type satisfies an associated type bound
    pub fn check_bound(
        &self,
        ty: &Type,
        bound: &AssocTraitBound,
    ) -> Result<(), String> {
        match bound {
            AssocTraitBound::Trait(trait_name, args) => {
                // Check if type implements the trait
                // Simplified for now
                Ok(())
            }
            AssocTraitBound::Lifetime(_) => {
                // Lifetime bounds are handled separately
                Ok(())
            }
            AssocTraitBound::HigherRanked(params, inner_bound) => {
                // For higher-ranked bounds, we need to check for all possible instantiations
                // Simplified for now
                self.check_bound(ty, inner_bound)
            }
        }
    }
}

/// Helper for building associated types
pub struct AssociatedTypeBuilder {
    name: String,
    generics: Vec<GenericParam>,
    bounds: Vec<AssocTraitBound>,
    kind: Kind,
    default: Option<Type>,
}

impl AssociatedTypeBuilder {
    /// Start building a new associated type
    pub fn new(name: String) -> Self {
        AssociatedTypeBuilder {
            name,
            generics: Vec::new(),
            bounds: Vec::new(),
            kind: Kind::Star,
            default: None,
        }
    }
    
    /// Add a type parameter
    pub fn with_type_param(mut self, name: String, kind: Kind) -> Self {
        self.generics.push(GenericParam::Type {
            name,
            kind,
            bounds: Vec::new(),
        });
        self
    }
    
    /// Add a lifetime parameter
    pub fn with_lifetime_param(mut self, name: String) -> Self {
        self.generics.push(GenericParam::Lifetime { name });
        self
    }
    
    /// Add a trait bound
    pub fn with_bound(mut self, bound: AssocTraitBound) -> Self {
        self.bounds.push(bound);
        self
    }
    
    /// Set the kind
    pub fn with_kind(mut self, kind: Kind) -> Self {
        self.kind = kind;
        self
    }
    
    /// Set the default type
    pub fn with_default(mut self, default: Type) -> Self {
        self.default = Some(default);
        self
    }
    
    /// Build the associated type
    pub fn build(self) -> AssociatedType {
        AssociatedType {
            name: self.name,
            generics: self.generics,
            bounds: self.bounds,
            kind: self.kind,
            default: self.default,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::middle::types::Type;
    
    #[test]
    fn test_associated_type_builder() {
        let assoc_type = AssociatedTypeBuilder::new("Item".to_string())
            .with_lifetime_param("a".to_string())
            .with_type_param("T".to_string(), Kind::Star)
            .with_bound(AssocTraitBound::Trait("Clone".to_string(), Vec::new()))
            .with_kind(Kind::Star)
            .build();
        
        assert_eq!(assoc_type.name, "Item");
        assert_eq!(assoc_type.generics.len(), 2);
        assert_eq!(assoc_type.bounds.len(), 1);
        assert_eq!(assoc_type.kind, Kind::Star);
        assert!(assoc_type.default.is_none());
    }
    
    #[test]
    fn test_associated_type_projection() {
        let base_type = Type::Named("Iterator".to_string(), Vec::new());
        let projection = AssociatedTypeProjection::new(base_type, "Item".to_string())
            .with_lifetime_arg("a".to_string())
            .with_type_arg(Type::I32);
        
        assert_eq!(projection.display_name(), "Iterator::Item<'a, i32>");
    }
    
    #[test]
    fn test_associated_type_context() {
        let mut ctx = AssociatedTypeContext::new();
        
        // Add a definition
        let assoc_type = AssociatedTypeBuilder::new("Item".to_string())
            .with_lifetime_param("a".to_string())
            .build();
        
        ctx.add_definition("Iterator".to_string(), assoc_type);
        
        // Should find the definition
        assert!(ctx.get_definition("Iterator", "Item").is_some());
        
        // Should not find non-existent definition
        assert!(ctx.get_definition("Iterator", "Unknown").is_none());
        
        // Add an implementation
        let impl_type = Type::Named("Counter".to_string(), Vec::new());
        let concrete_type = Type::Ref(
            Box::new(Type::I32),
            super::super::lifetime::Lifetime::Named("a".to_string()),
            super::super::Mutability::Immutable,
        );
        
        ctx.add_implementation(
            impl_type.clone(),
            "Iterator".to_string(),
            "Item".to_string(),
            concrete_type.clone(),
        );
        
        // Should find the implementation
        let found = ctx.get_implementation(&impl_type, "Iterator", "Item");
        assert!(found.is_some());
        assert_eq!(found.unwrap(), &concrete_type);
    }
    
    #[test]
    fn test_trait_bound_display() {
        // Simple trait bound
        let bound = AssocTraitBound::Trait("Clone".to_string(), Vec::new());
        assert_eq!(bound.display_name(), "Clone");
        
        // Trait bound with arguments
        let bound = AssocTraitBound::Trait(
            "Iterator".to_string(),
            vec![Type::I32],
        );
        assert_eq!(bound.display_name(), "Iterator<i32>");
        
        // Lifetime bound
        let bound = AssocTraitBound::Lifetime("a".to_string());
        assert_eq!(bound.display_name(), "'a");
        
        // Higher-ranked bound
        let inner = AssocTraitBound::Trait("Clone".to_string(), Vec::new());
        let bound = AssocTraitBound::HigherRanked(
            vec![GenericParam::Lifetime { name: "a".to_string() }],
            Box::new(inner),
        );
        assert_eq!(bound.display_name(), "for<'a> Clone");
    }
}