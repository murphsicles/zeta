use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    let code = std::fs::read_to_string("test_stdlib_import.z").unwrap();
    
    println!("=== Testing stdlib import parsing ===");
    match parse_zeta(&code) {
        Ok((remaining, ast)) => {
            println!("✅ Parsed successfully");
            println!("AST has {} nodes", ast.len());
            
            // Check for use statements
            let use_statements: Vec<_> = ast.iter()
                .filter(|node| {
                    match node {
                        zetac::frontend::ast::AstNode::Use { .. } => true,
                        _ => false,
                    }
                })
                .collect();
            
            println!("Found {} use statements", use_statements.len());
            
            for use_stmt in use_statements {
                if let zetac::frontend::ast::AstNode::Use { path } = use_stmt {
                    println!("  use {}::...", path[0]);
                }
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }
}