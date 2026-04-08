// Temporary fix for resolver.rs - check the structure
use std::fs;

fn main() {
    let content = fs::read_to_string("src/middle/resolver/resolver.rs").unwrap();
    
    // Count braces
    let mut brace_count = 0;
    let mut in_use_match = false;
    let mut lines: Vec<String> = vec![];
    
    for (i, line) in content.lines().enumerate() {
        lines.push(line.to_string());
        
        if line.contains("AstNode::Use { path, alias, is_glob } =>") {
            println!("Found Use match at line {}", i + 1);
            in_use_match = true;
        }
        
        if in_use_match {
            brace_count += line.matches('{').count();
            brace_count -= line.matches('}').count();
            
            if brace_count == 0 && line.trim().starts_with('}') && line.trim() != "}" {
                println!("Possible issue at line {}: {}", i + 1, line);
            }
        }
    }
    
    println!("Final brace count in Use match: {}", brace_count);
    
    // Write fixed version
    if brace_count != 0 {
        println!("Need to fix brace mismatch");
        // For now, just add closing braces at the end
        let mut fixed_content = content.clone();
        for _ in 0..brace_count {
            fixed_content.push_str("\n}\n");
        }
        fs::write("src/middle/resolver/resolver.rs.fixed", fixed_content).unwrap();
        println!("Wrote fixed version to resolver.rs.fixed");
    }
}