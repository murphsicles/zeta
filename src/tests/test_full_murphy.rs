use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    println!("Testing full Murphy's Sieve parsing...");
    
    // Test with a simplified version that includes const and function
    let code = r#"
const MAX_LIMIT: usize = 1000000;

fn murphy_sieve_true(limit: usize) -> usize {
    if limit > MAX_LIMIT {
        return 0;
    }
    
    if limit < 2 {
        return 0;
    }
    
    return 42; // Simplified
}
"#;
    
    println!("Code:\n{}", code);
    
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            println!("\nSuccess! AST has {} top-level nodes", ast.len());
            
            for (i, node) in ast.iter().enumerate() {
                match node {
                    zetac::frontend::ast::AstNode::ConstDef { name, .. } => {
                        println!("  Node {}: ConstDef '{}'", i, name);
                    }
                    zetac::frontend::ast::AstNode::FuncDef { name, .. } => {
                        println!("  Node {}: FuncDef '{}'", i, name);
                    }
                    _ => {
                        println!("  Node {}: Other: {:?}", i, node);
                    }
                }
            }
            
            if !remaining.trim().is_empty() {
                println!("\nRemaining input: '{}'", remaining);
            } else {
                println!("\nAll input consumed");
            }
        }
        Err(e) => {
            println!("\nParse error: {:?}", e);
        }
    }
}