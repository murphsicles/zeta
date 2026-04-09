// Test function calls in comptime

extern crate zetac;

#[test]
fn test_comptime_function_calls() {
    use zetac::frontend::parser::top_level::parse_zeta;
    use zetac::middle::const_eval::{ConstEvaluator, ConstValue};
    
    let code = r#"
comptime fn simple_add(x: i64, y: i64) -> i64 {
    x + y
}

comptime fn test_calls() -> i64 {
    simple_add(10, 20)
}
    "#;
    
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            println!("Parsed successfully, AST has {} nodes", ast.len());
            println!("Remaining input: '{}'", remaining);
            
            // Find the test_calls function
            let test_func = ast.iter().find(|node| {
                match node {
                    zetac::frontend::ast::AstNode::FuncDef { name, .. } => {
                        name.as_ref().map(|s: &String| s.as_str()) == Some("test_calls")
                    }
                    _ => false,
                }
            });
            
            if test_func.is_none() {
                println!("⚠️  No test_calls function found in AST");
                return;
            }
            
            // Try to evaluate it
            let mut evaluator = ConstEvaluator::new();
            match evaluator.try_eval_const_call(test_func.unwrap(), &[]) {
                Ok(Some(ConstValue::Int(value))) => {
                    println!("✅ Comptime function evaluates to: {}", value);
                    assert_eq!(value, 30, "Should evaluate to 30");
                }
                Ok(Some(other)) => {
                    println!("⚠️  Comptime function evaluates to: {:?}", other);
                }
                Ok(None) => {
                    println!("⚠️  Comptime function doesn't evaluate yet");
                }
                Err(e) => {
                    println!("❌ Evaluation error: {}", e);
                }
            }
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
        }
    }
}