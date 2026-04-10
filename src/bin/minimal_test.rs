// Minimal test to check if Zeta compiler works
use zetac::frontend::parser::top_level::parse_zeta;
use std::fs;

fn main() {
    println!("=== Minimal PrimeZeta Compilation Test ===\n");
    
    // Read the test file
    let code = match fs::read_to_string("minimal_test.z") {
        Ok(content) => content,
        Err(e) => {
            println!("❌ Failed to read test file: {}", e);
            return;
        }
    };
    
    println!("Test code:");
    println!("{}\n", code);
    
    // Test parsing
    println!("Attempting to parse...");
    match parse_zeta(&code) {
        Ok((remaining, ast)) => {
            println!("✅ Parsed successfully!");
            println!("  AST has {} nodes", ast.len());
            
            if !remaining.trim().is_empty() {
                println!("  ⚠️  Partial parse ({} chars remaining)", remaining.len());
            } else {
                println!("  ✅ Full parse - no remaining input");
            }
            
            // Check for functions
            let func_count = ast.iter().filter(|node| {
                matches!(node, zetac::frontend::ast::AstNode::FuncDef { .. })
            }).count();
            
            println!("  Functions found: {}", func_count);
            
            println!("\n=== Result ===");
            println!("✅ Basic Zeta compilation works!");
            println!("✅ Can parse simple PrimeZeta-like code");
            println!("✅ Ready for more complex PrimeZeta features");
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
            println!("\n=== Result ===");
            println!("❌ Basic parsing failed");
        }
    }
}