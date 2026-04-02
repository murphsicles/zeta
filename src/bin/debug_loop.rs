//! Debug loop

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::const_eval::{ConstEvaluator, ConstValue};
use zetac::AstNode;

fn main() {
    let code = r#"
comptime fn test_loop() -> i64 {
    var sum = 0
    for i in 0..3 {
        sum = sum + i
    }
    sum
}
    "#;
    
    println!("Parsing: {}", code);
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                println!("AST: {:#?}", ast);
                
                let mut evaluator = ConstEvaluator::new();
                if let Some(func) = ast.first() {
                    match evaluator.try_eval_const_call(func, &[]) {
                        Ok(Some(ConstValue::Int(value))) => {
                            println!("✅ Evaluates to: {}", value);
                        }
                        Ok(Some(other)) => {
                            println!("❌ Evaluates to: {:?}", other);
                        }
                        Ok(None) => {
                            println!("❌ Does not evaluate (returns None)");
                        }
                        Err(e) => {
                            println!("❌ Evaluation error: {}", e);
                        }
                    }
                }
            } else {
                println!("❌ Partial parse");
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }
}