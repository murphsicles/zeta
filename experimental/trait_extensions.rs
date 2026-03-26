// trait_extensions.rs - Enhanced trait/concept system for Zeta v0.3.5
// File: C:\Users\mummy\OneDrive\Documents\DarkFactory\zeta-0.3.4\src\trait_extensions.rs
// Purpose: Add associated types, default methods, supertraits to Zeta's concept system

use std::collections::HashMap;

/// Enhanced concept definition with associated types and default methods
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnhancedConceptDef {
    pub name: String,
    pub generics: Vec<String>,
    pub associated_types: Vec<AssociatedType>,
    pub methods: Vec<EnhancedMethod>,
    pub supertraits: Vec<String>,
    pub doc: String,
}

/// Associated type definition within a concept
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssociatedType {
    pub name: String,
    pub bounds: Vec<String>,  // Trait bounds on the associated type
    pub default: Option<String>,  // Default type (if any)
    pub doc: String,
}

/// Enhanced method with default implementation support
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnhancedMethod {
    pub name: String,
    pub params: Vec<(String, String)>,  // (name, type)
    pub ret: String,
    pub generics: Vec<String>,
    pub default_body: Option<Vec<crate::frontend::ast::AstNode>>,  // Default implementation
    pub doc: String,
}

/// Enhanced implementation block with associated type mappings
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnhancedImplBlock {
    pub concept: String,
    pub generics: Vec<String>,
    pub ty: String,
    pub associated_type_mappings: HashMap<String, String>,  // Maps associated type names to concrete types
    pub method_implementations: Vec<MethodImplementation>,
    pub doc: String,
}

/// Method implementation in an impl block
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodImplementation {
    pub method_name: String,
    pub body: Vec<crate::frontend::ast::AstNode>,
    pub doc: String,
}

/// Trait system extensions manager
pub struct TraitSystemExtensions {
    pub concepts: HashMap<String, EnhancedConceptDef>,
    pub implementations: HashMap<String, Vec<EnhancedImplBlock>>,
}

impl TraitSystemExtensions {
    /// Create a new trait system extensions manager
    pub fn new() -> Self {
        Self {
            concepts: HashMap::new(),
            implementations: HashMap::new(),
        }
    }
    
    /// Register an enhanced concept definition
    pub fn register_concept(&mut self, concept: EnhancedConceptDef) -> Result<(), String> {
        let name = concept.name.clone();
        
        // Validate concept
        self.validate_concept(&concept)?;
        
        // Check for duplicates
        if self.concepts.contains_key(&name) {
            return Err(format!("Concept '{}' already defined", name));
        }
        
        self.concepts.insert(name, concept);
        Ok(())
    }
    
    /// Register an enhanced implementation block
    pub fn register_implementation(&mut self, impl_block: EnhancedImplBlock) -> Result<(), String> {
        let concept_name = impl_block.concept.clone();
        
        // Validate implementation
        self.validate_implementation(&impl_block)?;
        
        // Get or create entry for this concept
        let impls = self.implementations.entry(concept_name).or_insert_with(Vec::new);
        impls.push(impl_block);
        
        Ok(())
    }
    
    /// Validate a concept definition
    fn validate_concept(&self, concept: &EnhancedConceptDef) -> Result<(), String> {
        // Check for duplicate associated type names
        let mut seen_types = std::collections::HashSet::new();
        for assoc_type in &concept.associated_types {
            if seen_types.contains(&assoc_type.name) {
                return Err(format!("Duplicate associated type '{}' in concept '{}'", 
                    assoc_type.name, concept.name));
            }
            seen_types.insert(assoc_type.name.clone());
        }
        
        // Check for duplicate method names
        let mut seen_methods = std::collections::HashSet::new();
        for method in &concept.methods {
            if seen_methods.contains(&method.name) {
                return Err(format!("Duplicate method '{}' in concept '{}'", 
                    method.name, concept.name));
            }
            seen_methods.insert(method.name.clone());
        }
        
        // Validate supertraits exist (if they've been registered)
        for supertrait in &concept.supertraits {
            if !self.concepts.contains_key(supertrait) {
                // Note: This is just a warning for now since supertraits might be defined later
                println!("Warning: Supertrait '{}' not yet defined for concept '{}'", 
                    supertrait, concept.name);
            }
        }
        
        Ok(())
    }
    
    /// Validate an implementation block
    fn validate_implementation(&self, impl_block: &EnhancedImplBlock) -> Result<(), String> {
        // Check if concept exists
        let concept = match self.concepts.get(&impl_block.concept) {
            Some(c) => c,
            None => return Err(format!("Concept '{}' not defined", impl_block.concept)),
        };
        
        // Check that all required associated types are mapped
        for assoc_type in &concept.associated_types {
            if !impl_block.associated_type_mappings.contains_key(&assoc_type.name) {
                // Check if there's a default
                if assoc_type.default.is_none() {
                    return Err(format!("Missing mapping for associated type '{}' in implementation of '{}' for '{}'", 
                        assoc_type.name, concept.name, impl_block.ty));
                }
            }
        }
        
        // Check that all required methods are implemented (or have defaults)
        for method in &concept.methods {
            let is_implemented = impl_block.method_implementations
                .iter()
                .any(|impl_method| impl_method.method_name == method.name);
            
            if !is_implemented && method.default_body.is_none() {
                return Err(format!("Missing implementation for method '{}' in implementation of '{}' for '{}'", 
                    method.name, concept.name, impl_block.ty));
            }
        }
        
        Ok(())
    }
    
    /// Get the concrete type for an associated type in a specific implementation
    pub fn get_associated_type(&self, concept: &str, implementor: &str, assoc_type: &str) -> Option<String> {
        if let Some(impls) = self.implementations.get(concept) {
            for impl_block in impls {
                if impl_block.ty == implementor {
                    return impl_block.associated_type_mappings.get(assoc_type).cloned();
                }
            }
        }
        None
    }
    
    /// Check if a type implements a concept
    pub fn type_implements_concept(&self, ty: &str, concept: &str) -> bool {
        if let Some(impls) = self.implementations.get(concept) {
            impls.iter().any(|impl_block| impl_block.ty == ty)
        } else {
            false
        }
    }
    
    /// Get all concepts implemented by a type
    pub fn get_concepts_for_type(&self, ty: &str) -> Vec<String> {
        let mut concepts = Vec::new();
        
        for (concept_name, impls) in &self.implementations {
            if impls.iter().any(|impl_block| impl_block.ty == ty) {
                concepts.push(concept_name.clone());
            }
        }
        
        concepts
    }
    
    /// Generate enhanced concept from basic AST concept
    pub fn enhance_concept_from_ast(&self, ast_concept: &crate::frontend::ast::AstNode) -> Result<EnhancedConceptDef, String> {
        match ast_concept {
            crate::frontend::ast::AstNode::ConceptDef { name, generics, methods, doc } => {
                // Convert methods to enhanced methods (without default bodies for now)
                let enhanced_methods = methods.iter()
                    .filter_map(|node| {
                        if let crate::frontend::ast::AstNode::Method { name, params, ret, generics, doc } = node {
                            Some(EnhancedMethod {
                                name: name.clone(),
                                params: params.clone(),
                                ret: ret.clone(),
                                generics: generics.clone(),
                                default_body: None,  // Basic AST doesn't have default bodies
                                doc: doc.clone(),
                            })
                        } else {
                            None
                        }
                    })
                    .collect();
                
                Ok(EnhancedConceptDef {
                    name: name.clone(),
                    generics: generics.clone(),
                    associated_types: Vec::new(),  // Basic AST doesn't have associated types
                    methods: enhanced_methods,
                    supertraits: Vec::new(),  // Basic AST doesn't have supertraits
                    doc: doc.clone(),
                })
            }
            _ => Err("Not a concept AST node".to_string()),
        }
    }
}

/// Parser extensions for enhanced trait syntax
pub mod parser_extensions {
    use nom::IResult;
    
    /// Parse associated type definition: `type Name = Type;` or `type Name: Bound;`
    pub fn parse_associated_type(input: &str) -> IResult<&str, super::AssociatedType> {
        use nom::bytes::complete::tag;
        use nom::character::complete::{alpha1, multispace0};
        use nom::sequence::{delimited, tuple};
        
        let (input, _) = tag("type")(input)?;
        let (input, _) = multispace0(input)?;
        let (input, name) = alpha1(input)?;
        
        // TODO: Implement full parsing for bounds and defaults
        // For now, return a simple associated type
        
        Ok((input, super::AssociatedType {
            name: name.to_string(),
            bounds: Vec::new(),
            default: None,
            doc: String::new(),
        }))
    }
    
    /// Parse supertrait list: `: Trait1 + Trait2`
    pub fn parse_supertraits(input: &str) -> IResult<&str, Vec<String>> {
        use nom::bytes::complete::tag;
        use nom::character::complete::{alpha1, multispace0};
        use nom::multi::separated_list1;
        use nom::sequence::{preceded, tuple};
        
        let (input, _) = tag(":")(input)?;
        let (input, _) = multispace0(input)?;
        
        let (input, traits) = separated_list1(
            tuple((multispace0, tag("+"), multispace0)),
            alpha1
        )(input)?;
        
        Ok((input, traits.iter().map(|s| s.to_string()).collect()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_enhanced_concept_creation() {
        let concept = EnhancedConceptDef {
            name: "Addable".to_string(),
            generics: vec!["Rhs".to_string()],
            associated_types: vec![
                AssociatedType {
                    name: "Output".to_string(),
                    bounds: vec!["Addable".to_string()],
                    default: Some("Self".to_string()),
                    doc: "Result type of addition".to_string(),
                }
            ],
            methods: vec![
                EnhancedMethod {
                    name: "add".to_string(),
                    params: vec![
                        ("self".to_string(), "Self".to_string()),
                        ("rhs".to_string(), "Rhs".to_string()),
                    ],
                    ret: "Self::Output".to_string(),
                    generics: Vec::new(),
                    default_body: None,
                    doc: "Add two values".to_string(),
                }
            ],
            supertraits: vec!["Copy".to_string(), "Debug".to_string()],
            doc: "Things that can be added".to_string(),
        };
        
        assert_eq!(concept.name, "Addable");
        assert_eq!(concept.associated_types.len(), 1);
        assert_eq!(concept.methods.len(), 1);
        assert_eq!(concept.supertraits.len(), 2);
    }
    
    #[test]
    fn test_trait_system_registration() {
        let mut system = TraitSystemExtensions::new();
        
        let concept = EnhancedConceptDef {
            name: "Display".to_string(),
            generics: Vec::new(),
            associated_types: Vec::new(),
            methods: vec![
                EnhancedMethod {
                    name: "fmt".to_string(),
                    params: vec![
                        ("self".to_string(), "Self".to_string()),
                        ("f".to_string(), "Formatter".to_string()),
                    ],
                    ret: "Result".to_string(),
                    generics: Vec::new(),
                    default_body: None,
                    doc: "Format the value".to_string(),
                }
            ],
            supertraits: Vec::new(),
            doc: "Things that can be formatted".to_string(),
        };
        
        assert!(system.register_concept(concept).is_ok());
        assert!(system.concepts.contains_key("Display"));
    }
    
    #[test]
    fn test_implementation_validation() {
        let mut system = TraitSystemExtensions::new();
        
        // First register a concept with an associated type
        let concept = EnhancedConceptDef {
            name: "Iterator".to_string(),
            generics: Vec::new(),
            associated_types: vec![
                AssociatedType {
                    name: "Item".to_string(),
                    bounds: Vec::new(),
                    default: None,  // No default - must be specified in impl
                    doc: "Type of items yielded".to_string(),
                }
            ],
            methods: vec![
                EnhancedMethod {
                    name: "next".to_string(),
                    params: vec![("self".to_string(), "&mut Self".to_string())],
                    ret: "Option<Self::Item>".to_string(),
                    generics: Vec::new(),
                    default_body: None,
                    doc: "Get the next item".to_string(),
                }
            ],
            supertraits: Vec::new(),
            doc: "Things that can be iterated".to_string(),
        };
        
        assert!(system.register_concept(concept).is_ok());
        
        // Try to register an implementation without mapping the associated type
        let mut type_mappings = HashMap::new();
        type_mappings.insert("Item".to_string(), "i32".to_string());
        
        let impl_block = EnhancedImplBlock {
            concept: "Iterator".to_string(),
            generics: Vec::new(),
            ty: "Range".to_string(),
            associated_type_mappings: type_mappings,
            method_implementations: vec![
                MethodImplementation {
                    method_name: "next".to_string(),
                    body: Vec::new(),  // Empty for test
                    doc: "Get next value".to_string(),
                }
            ],
            doc: "Iterator for ranges".to_string(),
        };
        
        assert!(system.register_implementation(impl_block).is_ok());
    }
    
    #[test]
    fn test_type_implements_concept() {
        let mut system = TraitSystemExtensions::new();
        
        // Register a concept
        let concept = EnhancedConceptDef {
            name: "Debug".to_string(),
            generics: Vec::new(),
            associated_types: Vec::new(),
            methods: vec![EnhancedMethod {
                name: "fmt".to_string(),
                params: vec![
                    ("self".to_string(), "Self".to_string()),
                    ("f".to_string(), "Formatter".to_string()),
                ],
                ret: "Result".to_string(),
                generics: Vec::new(),
                default_body: None,
                doc: "Format".to_string(),
            }],
            supertraits: Vec::new(),
            doc: "Debug formatting".to_string(),
        };
        
        system.register_concept(concept).unwrap();
        
        // Register an implementation
        let impl_block = EnhancedImplBlock {
            concept: "Debug".to_string(),
            generics: Vec::new(),
            ty: "i32".to_string(),
            associated_type_mappings: HashMap::new(),
            method_implementations: vec![MethodImplementation {
                method_name: "fmt".to_string(),
                body: Vec::new(),
                doc: "Format i32".to_string(),
            }],
            doc: "Debug for i32".to_string(),
        };
        
        system.register_implementation(impl_block).unwrap();
        
        // Test
        assert!(system.type_implements_concept("i32", "Debug"));
        assert!(!system.type_implements_concept("f64", "Debug"));
    }
}
