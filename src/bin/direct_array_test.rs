// Direct test of array syntax parsing
use zetac::frontend::parser::expr::parse_expr;
use zetac::frontend::ast::AstNode;

fn test_parse(input: &str, expected_variant: &str) {
    println!("Testing: '{}'", input);
    match parse_expr(input) {
        Ok((remaining, ast)) => {
            if !remaining.is_empty() {
                println!("  ⚠️  Did not consume all input: '{}'", remaining);
            }
            println!("  ✅ Parsed successfully");
            
            // Check which variant we got
            match &ast {
                AstNode::ArrayLit(_) => {
                    if expected_variant == "ArrayLit" {
                        println!("  ✅ Correct AST variant: ArrayLit");
                    } else {
                        println!("  ❌ Wrong AST variant. Expected {}, got ArrayLit", expected_variant);
                    }
                }
                AstNode::ArrayRepeat { value, size } => {
                    if expected_variant == "ArrayRepeat" {
                        println!("  ✅ Correct AST variant: ArrayRepeat");
                        println!("    Value: {:?}", value);
                        println!("    Size: {:?}", size);
                    } else {
                        println!("  ❌ Wrong AST variant. Expected {}, got ArrayRepeat", expected_variant);
                    }
                }
                _ => {
                    println!("  ❌ Unexpected AST variant: {:?}", ast);
                }
            }
        }
        Err(e) => {
            println!("  ❌ Parse error: {:?}", e);
        }
    }
    println!();
}

fn main() {
    println!("=== Direct Array Syntax Test ===\n");
    
    // Test array repeat syntax
    test_parse("[0; 4]", "ArrayRepeat");
    test_parse("[true; 100]", "ArrayRepeat");
    test_parse("[0; SIZE]", "ArrayRepeat");
    
    // Test regular array syntax
    test_parse("[1, 2, 3]", "ArrayLit");
    test_parse("[]", "ArrayLit");
    test_parse("[true, false, true]", "ArrayLit");
    
    // Test multi-dimensional
    test_parse("[[0; 4]; 3]", "ArrayRepeat"); // Outer should be ArrayRepeat
    
    println!("=== Test Complete ===");
}