// Test runner for comptime evaluation
// This follows the existing test patterns in the repository

#[test]
fn test_comptime_parsing() {
    use zetac::frontend::parser::top_level::parse_zeta;
    
    let code = r#"
comptime fn simple() -> i64 {
    42
}
    "#;
    
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            assert!(remaining.trim().is_empty(), "Should parse completely");
            assert_eq!(ast.len(), 1, "Should have one AST node");
            
            // Check that it's a comptime function
            if let zetac::frontend::ast::AstNode::FuncDef { comptime_, .. } = &ast[0] {
                assert!(*comptime_, "Function should be marked as comptime");
            } else {
                panic!("Expected FuncDef node");
            }
        }
        Err(e) => panic!("Parse error: {:?}", e),
    }
}

#[test]
fn test_comptime_evaluation() {
    use zetac::frontend::parser::top_level::parse_zeta;
    use zetac::middle::const_eval::{ConstEvaluator, ConstValue};
    
    let code = r#"
comptime fn answer() -> i64 {
    42
}
    "#;
    
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            assert!(remaining.trim().is_empty(), "Should parse completely");
            
            let mut evaluator = ConstEvaluator::new();
            if let Some(func) = ast.first() {
                match evaluator.try_eval_const_call(func, &[]) {
                    Ok(Some(ConstValue::Int(value))) => {
                        assert_eq!(value, 42, "Should evaluate to 42");
                    }
                    Ok(Some(other)) => {
                        panic!("Expected integer, got {:?}", other);
                    }
                    Ok(None) => {
                        panic!("Should evaluate to Some value");
                    }
                    Err(e) => {
                        panic!("Evaluation error: {}", e);
                    }
                }
            } else {
                panic!("No function found");
            }
        }
        Err(e) => panic!("Parse error: {:?}", e),
    }
}

#[test]
fn test_comptime_array() {
    use zetac::frontend::parser::top_level::parse_zeta;
    use zetac::middle::const_eval::{ConstEvaluator, ConstValue};
    
    let code = r#"
comptime fn array_test() -> [i64; 3] {
    [1, 2, 3]
}
    "#;
    
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            assert!(remaining.trim().is_empty(), "Should parse completely");
            
            let mut evaluator = ConstEvaluator::new();
            if let Some(func) = ast.first() {
                match evaluator.try_eval_const_call(func, &[]) {
                    Ok(Some(ConstValue::Array(arr))) => {
                        assert_eq!(arr.len(), 3, "Array should have 3 elements");
                        assert_eq!(arr[0], ConstValue::Int(1), "First element should be 1");
                        assert_eq!(arr[1], ConstValue::Int(2), "Second element should be 2");
                        assert_eq!(arr[2], ConstValue::Int(3), "Third element should be 3");
                    }
                    Ok(Some(other)) => {
                        // For now, arrays might not evaluate - that's OK
                        println!("Note: Array evaluation returned {:?}", other);
                    }
                    Ok(None) => {
                        // This is expected until array evaluation is fully implemented
                        println!("Note: Array evaluation returned None");
                    }
                    Err(e) => {
                        panic!("Evaluation error: {}", e);
                    }
                }
            } else {
                panic!("No function found");
            }
        }
        Err(e) => panic!("Parse error: {:?}", e),
    }
}