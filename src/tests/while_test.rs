#[cfg(test)]
mod tests {
    use zetac::frontend::parser::top_level::parse_zeta;
    
    #[test]
    fn test_while_simple() {
        let code = "while i < 10 { i = i + 1 }";
        println!("Testing: {}", code);
        
        match parse_zeta(code) {
            Ok((remaining, asts)) => {
                assert!(remaining.is_empty(), "Should consume all input, but remaining: '{}'", remaining);
                assert!(!asts.is_empty(), "Should parse at least one AST node");
                println!("✓ Parsed successfully: {:?}", asts);
            }
            Err(e) => {
                panic!("Parse error: {:?}", e);
            }
        }
    }
    
    #[test]
    fn test_while_complex_condition() {
        let code = "while x > 0 && x < 100 { x = x - 1 }";
        println!("Testing: {}", code);
        
        match parse_zeta(code) {
            Ok((remaining, asts)) => {
                assert!(remaining.is_empty(), "Should consume all input, but remaining: '{}'", remaining);
                assert!(!asts.is_empty(), "Should parse at least one AST node");
                println!("✓ Parsed successfully");
            }
            Err(e) => {
                panic!("Parse error: {:?}", e);
            }
        }
    }
    
    #[test]
    fn test_while_nested() {
        let code = "while i < 10 { while j < 5 { j = j + 1 } i = i + 1 }";
        println!("Testing: {}", code);
        
        match parse_zeta(code) {
            Ok((remaining, asts)) => {
                assert!(remaining.is_empty(), "Should consume all input, but remaining: '{}'", remaining);
                assert!(!asts.is_empty(), "Should parse at least one AST node");
                println!("✓ Parsed successfully");
            }
            Err(e) => {
                panic!("Parse error: {:?}", e);
            }
        }
    }
}