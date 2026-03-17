// advanced_generics_test.rs - Comprehensive tests for advanced generics
// File: C:\Users\mummy\OneDrive\Documents\DarkFactory\zeta-0.3.4\src\advanced_generics_test.rs
// Purpose: Test const generics, HRTB, and enhanced generic system

#[cfg(test)]
mod tests {
    use crate::advanced_generics::{
        GenericSystemExtensions, ConstEvaluator, HrtbAnalyzer,
        GenericParam, GenericParams, ConstValue, WhereClause, WhereBound,
        HigherRankedTraitBound, EnhancedFuncSig,
    };
    
    #[test]
    fn test_complex_generic_parsing() {
        let system = GenericSystemExtensions::new();
        
        // Test complex generic parameter list
        let input = "T: Debug + Clone, const N: usize = 42, 'a, U";
        let params = system.parse_generic_params(input).unwrap();
        
        assert_eq!(params.params.len(), 4);
        
        // Check first parameter: T: Debug + Clone
        if let GenericParam::Type { name, bounds } = &params.params[0] {
            assert_eq!(name, "T");
            assert_eq!(bounds, &["Debug".to_string(), "Clone".to_string()]);
        } else {
            panic!("Expected type parameter with bounds");
        }
        
        // Check second parameter: const N: usize = 42
        if let GenericParam::Const { name, ty, default } = &params.params[1] {
            assert_eq!(name, "N");
            assert_eq!(ty, "usize");
            assert_eq!(default, &Some(ConstValue::Integer(42)));
        } else {
            panic!("Expected const generic");
        }
        
        // Check third parameter: 'a
        if let GenericParam::Lifetime { name } = &params.params[2] {
            assert_eq!(name, "'a");
        } else {
            panic!("Expected lifetime parameter");
        }
        
        // Check fourth parameter: U
        if let GenericParam::Type { name, bounds } = &params.params[3] {
            assert_eq!(name, "U");
            assert!(bounds.is_empty());
        } else {
            panic!("Expected simple type parameter");
        }
    }
    
    #[test]
    fn test_const_generic_arrays() {
        let system = GenericSystemExtensions::new();
        
        // Test creating const generic array types
        let array1 = system.create_const_array_type("i32", "N");
        assert_eq!(array1, "[i32; N]");
        
        let array2 = system.create_const_array_type("T", "SIZE");
        assert_eq!(array2, "[T; SIZE]");
        
        // Test with actual constant value
        let array3 = system.create_const_array_type("u8", "256");
        assert_eq!(array3, "[u8; 256]");
    }
    
    #[test]
    fn test_const_value_range_validation() {
        let system = GenericSystemExtensions::new();
        
        // Test valid ranges
        assert!(system.validate_const_value("u8", &ConstValue::Integer(0)).is_ok());
        assert!(system.validate_const_value("u8", &ConstValue::Integer(255)).is_ok());
        assert!(system.validate_const_value("i8", &ConstValue::Integer(-128)).is_ok());
        assert!(system.validate_const_value("i8", &ConstValue::Integer(127)).is_ok());
        assert!(system.validate_const_value("usize", &ConstValue::Integer(1000)).is_ok());
        assert!(system.validate_const_value("bool", &ConstValue::Boolean(true)).is_ok());
        assert!(system.validate_const_value("bool", &ConstValue::Boolean(false)).is_ok());
        assert!(system.validate_const_value("char", &ConstValue::Char('a')).is_ok());
        
        // Test invalid ranges
        assert!(system.validate_const_value("u8", &ConstValue::Integer(256)).is_err());
        assert!(system.validate_const_value("u8", &ConstValue::Integer(-1)).is_err());
        assert!(system.validate_const_value("i8", &ConstValue::Integer(-129)).is_err());
        assert!(system.validate_const_value("i8", &ConstValue::Integer(128)).is_err());
        assert!(system.validate_const_value("bool", &ConstValue::Integer(1)).is_err());
        assert!(system.validate_const_value("char", &ConstValue::Boolean(true)).is_err());
    }
    
    #[test]
    fn test_hrtb_complex_cases() {
        let analyzer = HrtbAnalyzer;
        
        // Test simple HRTB
        let hrtb1 = analyzer.parse_hrtb("for<'a> Fn(&'a T) -> &'a U").unwrap();
        assert_eq!(hrtb1.lifetimes, vec!["'a".to_string()]);
        assert_eq!(hrtb1.bound, "Fn(&'a T) -> &'a U");
        
        // Test multiple lifetimes
        let hrtb2 = analyzer.parse_hrtb("for<'a, 'b> Fn(&'a T, &'b U) -> &'a T").unwrap();
        assert_eq!(hrtb2.lifetimes, vec!["'a".to_string(), "'b".to_string()]);
        assert_eq!(hrtb2.bound, "Fn(&'a T, &'b U) -> &'a T");
        
        // Test HRTB detection
        assert!(analyzer.is_hrtb("for<'a> Fn(&'a T)"));
        assert!(analyzer.is_hrtb("for<'a, 'b> FnMut(&'a T, &'b U)"));
        assert!(!analyzer.is_hrtb("T: Debug"));
        assert!(!analyzer.is_hrtb("Fn(T) -> U"));
        
        // Test HRTB application
        let applied = analyzer.apply_hrtb(&hrtb1, "MyFunc");
        assert_eq!(applied, "Fn(&'a T) -> &'a U MyFunc");
    }
    
    #[test]
    fn test_where_clause_construction() {
        // Build a complex where clause
        let where_clause = WhereClause {
            bounds: vec![
                WhereBound::TypeBound {
                    ty: "T".to_string(),
                    bounds: vec!["Debug".to_string(), "Clone".to_string()],
                },
                WhereBound::TypeBound {
                    ty: "U".to_string(),
                    bounds: vec!["Send".to_string(), "Sync".to_string()],
                },
                WhereBound::LifetimeBound {
                    lifetime: "'a".to_string(),
                    bound: "'static".to_string(),
                },
                WhereBound::ConstEquality {
                    name: "N".to_string(),
                    value: ConstValue::Integer(42),
                },
            ],
        };
        
        assert_eq!(where_clause.bounds.len(), 4);
        
        // Verify each bound
        if let WhereBound::TypeBound { ty, bounds } = &where_clause.bounds[0] {
            assert_eq!(ty, "T");
            assert_eq!(bounds, &["Debug".to_string(), "Clone".to_string()]);
        }
        
        if let WhereBound::ConstEquality { name, value } = &where_clause.bounds[3] {
            assert_eq!(name, "N");
            assert_eq!(value, &ConstValue::Integer(42));
        }
    }
    
    #[test]
    fn test_enhanced_function_signature() {
        // Create an enhanced function signature with advanced generics
        let generics = GenericParams {
            params: vec![
                GenericParam::Type {
                    name: "T".to_string(),
                    bounds: vec!["Debug".to_string()],
                },
                GenericParam::Const {
                    name: "N".to_string(),
                    ty: "usize".to_string(),
                    default: Some(ConstValue::Integer(10)),
                },
                GenericParam::Lifetime {
                    name: "'a".to_string(),
                },
            ],
            where_clause: Some(WhereClause {
                bounds: vec![
                    WhereBound::TypeBound {
                        ty: "T".to_string(),
                        bounds: vec!["Clone".to_string()],
                    },
                ],
            }),
        };
        
        let func_sig = EnhancedFuncSig {
            name: "process_array".to_string(),
            generics,
            params: vec![
                ("arr".to_string(), "[T; N]".to_string()),
                ("lifetime".to_string(), "&'a str".to_string()),
            ],
            ret: "Vec<T>".to_string(),
        };
        
        assert_eq!(func_sig.name, "process_array");
        assert_eq!(func_sig.params.len(), 2);
        assert_eq!(func_sig.ret, "Vec<T>");
        assert_eq!(func_sig.generics.params.len(), 3);
        assert!(func_sig.generics.where_clause.is_some());
    }
    
    #[test]
    fn test_const_generic_type_generation() {
        let system = GenericSystemExtensions::new();
        
        // Test generating types with const generic values
        assert_eq!(
            system.generate_const_generic_type("usize", &ConstValue::Integer(42)).unwrap(),
            "42"
        );
        
        assert_eq!(
            system.generate_const_generic_type("bool", &ConstValue::Boolean(true)).unwrap(),
            "true"
        );
        
        assert_eq!(
            system.generate_const_generic_type("char", &ConstValue::Char('x')).unwrap(),
            "'x'"
        );
        
        // Test error cases
        assert!(system.generate_const_generic_type("usize", &ConstValue::Boolean(true)).is_err());
        assert!(system.generate_const_generic_type("bool", &ConstValue::Integer(1)).is_err());
    }
    
    #[test]
    fn test_const_evaluator_edge_cases() {
        let evaluator = ConstEvaluator;
        
        // Test various integer formats
        assert_eq!(evaluator.evaluate("0").unwrap(), ConstValue::Integer(0));
        assert_eq!(evaluator.evaluate("-42").unwrap(), ConstValue::Integer(-42));
        assert_eq!(evaluator.evaluate("1000000").unwrap(), ConstValue::Integer(1000000));
        
        // Test character escapes (simplified)
        assert_eq!(evaluator.evaluate("'\\n'").unwrap(), ConstValue::Char('\n'));
        
        // Test string with escapes
        assert_eq!(
            evaluator.evaluate("\"hello\\nworld\"").unwrap(),
            ConstValue::String("hello\nworld".to_string())
        );
        
        // Test error cases
        assert!(evaluator.evaluate("not_a_constant").is_err());
        assert!(evaluator.evaluate("").is_err());
    }
    
    #[test]
    fn test_valid_const_types() {
        let evaluator = ConstEvaluator;
        
        // Valid const generic types
        assert!(evaluator.is_valid_const_type("usize"));
        assert!(evaluator.is_valid_const_type("isize"));
        assert!(evaluator.is_valid_const_type("u8"));
        assert!(evaluator.is_valid_const_type("i32"));
        assert!(evaluator.is_valid_const_type("bool"));
        assert!(evaluator.is_valid_const_type("char"));
        
        // Invalid const generic types
        assert!(!evaluator.is_valid_const_type("String"));
        assert!(!evaluator.is_valid_const_type("Vec<T>"));
        assert!(!evaluator.is_valid_const_type("&str"));
        assert!(!evaluator.is_valid_const_type("f32"));  // Floats not supported as const generics
        assert!(!evaluator.is_valid_const_type("f64"));
    }
    
    #[test]
    fn test_generic_system_integration() {
        let system = GenericSystemExtensions::new();
        let analyzer = HrtbAnalyzer;
        
        // Test a complete scenario: parsing, validation, and type generation
        
        // 1. Parse complex generic parameters
        let params = system.parse_generic_params(
            "T: Debug, const SIZE: usize = 1024, 'lifetime, U: Clone + Send"
        ).unwrap();
        
        // 2. Validate const generic values
        if let GenericParam::Const { ty, default, .. } = &params.params[1] {
            assert_eq!(ty, "usize");
            if let Some(value) = default {
                assert!(system.validate_const_value(ty, value).is_ok());
            }
        }
        
        // 3. Check for HRTB in bounds
        let hrtb_bound = "for<'a> Fn(&'a [T; SIZE]) -> &'a T";
        assert!(analyzer.is_hrtb(hrtb_bound));
        
        let hrtb = analyzer.parse_hrtb(hrtb_bound).unwrap();
        assert_eq!(hrtb.lifetimes, vec!["'a".to_string()]);
        
        // 4. Create array type using const generic
        let array_type = system.create_const_array_type("T", "SIZE");
        assert_eq!(array_type, "[T; SIZE]");
        
        // 5. Test the complete system works together
        println!("Generic system integration test passed!");
        println!("  Parsed {} generic parameters", params.params.len());
        println!("  HRTB detected and parsed: {}", hrtb_bound);
        println!("  Array type created: {}", array_type);
    }
}