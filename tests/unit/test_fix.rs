extern crate zetac;

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::const_eval::{ConstEvaluator, ConstValue};

fn main() {
    let code = r#"
comptime fn add(x: i64, y: i64) -> i64 {
    x + y
}

comptime fn test() -> i64 {
    add(10, 20)
}
    "#;
    
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            println!("Parsed successfully, AST has {} nodes", ast.len());
            
            // Create evaluator and register all functions
            let mut evaluator = ConstEvaluator::new();
            
            // Register all const/comptime functions
            for node in &ast {
                if let zetac::frontend::ast::AstNode::FuncDef {
                    ref name,
                    const_,
                    comptime_,
                    ..
                } = *node {
                    if const_ || comptime_ {
                        println!("Registering function: {}", name);
                        evaluator.register_function(name.clone(), node.clone());
                    }
                }
            }
            
            // Find the test function
            let test_func = ast.iter().find(|node| {
                match node {
                    zetac::frontend::ast::AstNode::FuncDef { name, .. } => {
                        name.as_ref().map(|n| n.as_str()) == Some("test")
                    }
                    _ => false,
                }
            });
            
            if let Some(func) = test_func {
                match evaluator.try_eval_const_call(func, &[]) {
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
            } else {
                println!("⚠️  No test function found");
            }
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
        }
    }
}