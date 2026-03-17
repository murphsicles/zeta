// trait_system_test.rs - Comprehensive test for enhanced trait system
// File: C:\Users\mummy\OneDrive\Documents\DarkFactory\zeta-0.3.4\src\trait_system_test.rs
// Purpose: Test associated types, default methods, supertraits

#[cfg(test)]
mod tests {
    use crate::trait_extensions::{TraitSystemExtensions, EnhancedConceptDef, AssociatedType, EnhancedMethod, EnhancedImplBlock};
    use std::collections::HashMap;
    
    #[test]
    fn test_iterator_concept_with_associated_type() {
        let mut system = TraitSystemExtensions::new();
        
        // Create Iterator concept with associated type
        let iterator_concept = EnhancedConceptDef {
            name: "Iterator".to_string(),
            generics: Vec::new(),
            associated_types: vec![
                AssociatedType {
                    name: "Item".to_string(),
                    bounds: Vec::new(),
                    default: None,  // Must be specified in implementations
                    doc: "Type of items yielded".to_string(),
                }
            ],
            methods: vec![
                EnhancedMethod {
                    name: "next".to_string(),
                    params: vec![
                        ("self".to_string(), "&mut Self".to_string()),
                    ],
                    ret: "Option<Self::Item>".to_string(),
                    generics: Vec::new(),
                    default_body: None,
                    doc: "Get the next item".to_string(),
                }
            ],
            supertraits: Vec::new(),
            doc: "Things that can be iterated".to_string(),
        };
        
        // Register the concept
        assert!(system.register_concept(iterator_concept).is_ok());
        
        // Create implementation for Range<i32>
        let mut type_mappings = HashMap::new();
        type_mappings.insert("Item".to_string(), "i32".to_string());
        
        let range_impl = EnhancedImplBlock {
            concept: "Iterator".to_string(),
            generics: Vec::new(),
            ty: "Range<i32>".to_string(),
            associated_type_mappings: type_mappings,
            method_implementations: vec![
                crate::trait_extensions::MethodImplementation {
                    method_name: "next".to_string(),
                    body: Vec::new(),  // Simplified for test
                    doc: "Get next value from range".to_string(),
                }
            ],
            doc: "Iterator for integer ranges".to_string(),
        };
        
        // Register the implementation
        assert!(system.register_implementation(range_impl).is_ok());
        
        // Verify the implementation
        assert!(system.type_implements_concept("Range<i32>", "Iterator"));
        
        // Check associated type mapping
        let item_type = system.get_associated_type("Iterator", "Range<i32>", "Item");
        assert_eq!(item_type, Some("i32".to_string()));
    }
    
    #[test]
    fn test_concept_with_default_method() {
        let mut system = TraitSystemExtensions::new();
        
        // Create a concept with a default method
        let default_concept = EnhancedConceptDef {
            name: "Default".to_string(),
            generics: Vec::new(),
            associated_types: Vec::new(),
            methods: vec![
                EnhancedMethod {
                    name: "default".to_string(),
                    params: Vec::new(),
                    ret: "Self".to_string(),
                    generics: Vec::new(),
                    default_body: Some(Vec::new()),  // Has default implementation
                    doc: "Create default value".to_string(),
                }
            ],
            supertraits: Vec::new(),
            doc: "Types with default values".to_string(),
        };
        
        assert!(system.register_concept(default_concept).is_ok());
        
        // Implementation can omit the method since it has a default
        let impl_block = EnhancedImplBlock {
            concept: "Default".to_string(),
            generics: Vec::new(),
            ty: "MyType".to_string(),
            associated_type_mappings: HashMap::new(),
            method_implementations: Vec::new(),  // Empty - using default
            doc: "Default for MyType".to_string(),
        };
        
        // This should succeed even without method implementation
        assert!(system.register_implementation(impl_block).is_ok());
    }
    
    #[test]
    fn test_supertrait_inheritance() {
        let mut system = TraitSystemExtensions::new();
        
        // Create base trait
        let base_concept = EnhancedConceptDef {
            name: "Debug".to_string(),
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
                    doc: "Format for debugging".to_string(),
                }
            ],
            supertraits: Vec::new(),
            doc: "Debug formatting".to_string(),
        };
        
        assert!(system.register_concept(base_concept).is_ok());
        
        // Create derived trait with supertrait
        let derived_concept = EnhancedConceptDef {
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
                    doc: "Format for display".to_string(),
                }
            ],
            supertraits: vec!["Debug".to_string()],  // Display requires Debug
            doc: "Display formatting".to_string(),
        };
        
        assert!(system.register_concept(derived_concept).is_ok());
        
        // When implementing Display, we must also implement Debug
        // (This would be enforced by the type checker)
        println!("Supertrait relationship established: Display : Debug");
    }
    
    #[test]
    fn test_multiple_concepts_for_type() {
        let mut system = TraitSystemExtensions::new();
        
        // Register multiple concepts
        let debug_concept = EnhancedConceptDef {
            name: "Debug".to_string(),
            generics: Vec::new(),
            associated_types: Vec::new(),
            methods: vec![EnhancedMethod {
                name: "fmt".to_string(),
                params: vec![("self".to_string(), "Self".to_string()), ("f".to_string(), "Formatter".to_string())],
                ret: "Result".to_string(),
                generics: Vec::new(),
                default_body: None,
                doc: "Format".to_string(),
            }],
            supertraits: Vec::new(),
            doc: "Debug".to_string(),
        };
        
        let clone_concept = EnhancedConceptDef {
            name: "Clone".to_string(),
            generics: Vec::new(),
            associated_types: Vec::new(),
            methods: vec![EnhancedMethod {
                name: "clone".to_string(),
                params: vec![("self".to_string(), "Self".to_string())],
                ret: "Self".to_string(),
                generics: Vec::new(),
                default_body: None,
                doc: "Clone".to_string(),
            }],
            supertraits: Vec::new(),
            doc: "Clone".to_string(),
        };
        
        system.register_concept(debug_concept).unwrap();
        system.register_concept(clone_concept).unwrap();
        
        // Register implementations for i32
        let debug_impl = EnhancedImplBlock {
            concept: "Debug".to_string(),
            generics: Vec::new(),
            ty: "i32".to_string(),
            associated_type_mappings: HashMap::new(),
            method_implementations: vec![crate::trait_extensions::MethodImplementation {
                method_name: "fmt".to_string(),
                body: Vec::new(),
                doc: "Format i32".to_string(),
            }],
            doc: "Debug for i32".to_string(),
        };
        
        let clone_impl = EnhancedImplBlock {
            concept: "Clone".to_string(),
            generics: Vec::new(),
            ty: "i32".to_string(),
            associated_type_mappings: HashMap::new(),
            method_implementations: vec![crate::trait_extensions::MethodImplementation {
                method_name: "clone".to_string(),
                body: Vec::new(),
                doc: "Clone i32".to_string(),
            }],
            doc: "Clone for i32".to_string(),
        };
        
        system.register_implementation(debug_impl).unwrap();
        system.register_implementation(clone_impl).unwrap();
        
        // Test
        let concepts = system.get_concepts_for_type("i32");
        assert!(concepts.contains(&"Debug".to_string()));
        assert!(concepts.contains(&"Clone".to_string()));
        assert_eq!(concepts.len(), 2);
    }
    
    #[test]
    fn test_concept_validation() {
        let mut system = TraitSystemExtensions::new();
        
        // Try to register a concept with duplicate method names
        let bad_concept = EnhancedConceptDef {
            name: "Bad".to_string(),
            generics: Vec::new(),
            associated_types: Vec::new(),
            methods: vec![
                EnhancedMethod {
                    name: "method".to_string(),
                    params: Vec::new(),
                    ret: "()".to_string(),
                    generics: Vec::new(),
                    default_body: None,
                    doc: "First".to_string(),
                },
                EnhancedMethod {
                    name: "method".to_string(),  // Duplicate!
                    params: Vec::new(),
                    ret: "()".to_string(),
                    generics: Vec::new(),
                    default_body: None,
                    doc: "Second".to_string(),
                },
            ],
            supertraits: Vec::new(),
            doc: "Bad concept".to_string(),
        };
        
        // This should fail validation
        let result = system.register_concept(bad_concept);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Duplicate method"));
    }
    
    #[test]
    fn test_implementation_validation() {
        let mut system = TraitSystemExtensions::new();
        
        // Register a concept with required method
        let concept = EnhancedConceptDef {
            name: "Required".to_string(),
            generics: Vec::new(),
            associated_types: Vec::new(),
            methods: vec![EnhancedMethod {
                name: "required".to_string(),
                params: Vec::new(),
                ret: "()".to_string(),
                generics: Vec::new(),
                default_body: None,  // No default - must be implemented
                doc: "Required method".to_string(),
            }],
            supertraits: Vec::new(),
            doc: "Concept with required method".to_string(),
        };
        
        system.register_concept(concept).unwrap();
        
        // Try to register implementation without the required method
        let bad_impl = EnhancedImplBlock {
            concept: "Required".to_string(),
            generics: Vec::new(),
            ty: "MyType".to_string(),
            associated_type_mappings: HashMap::new(),
            method_implementations: Vec::new(),  // Missing required method!
            doc: "Bad implementation".to_string(),
        };
        
        // This should fail
        let result = system.register_implementation(bad_impl);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Missing implementation"));
    }
}