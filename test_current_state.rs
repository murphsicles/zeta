use zetac::frontend::parser::expr::parse_expr;
use zetac::frontend::parser::stmt::parse_stmt;

fn test_parser(input: &str, parser_name: &str) {
    println!("\n=== Testing {}: '{}' ===", parser_name, input);
    
    let result = if parser_name == "expr" {
        parse_expr(input)
    } else {
        parse_stmt(input)
    };
    
    match result {
        Ok((remaining, ast)) => {
            println!("✓ Success!");
            println!("  Remaining: '{}'", remaining);
            println!("  AST: {:?}", ast);
        }
        Err(e) => {
            println!("✗ Failed: {:?}", e);
        }
    }
}

fn main() {
    println!("=== Testing Current Zeta Parser State ===");
    
    // Test identifier parsing (ASCII)
    test_parser("my_var", "expr");
    test_parser("_private", "expr");
    test_parser("var123", "expr");
    
    // Test Unicode identifiers (should fail currently)
    test_parser("π", "expr");
    test_parser("変数", "expr");
    test_parser("标识符", "expr");
    test_parser("α", "expr");
    
    // Test operators
    test_parser("a + b", "expr");
    test_parser("a - b", "expr");
    test_parser("a * b", "expr");
    test_parser("a / b", "expr");
    test_parser("a % b", "expr");
    
    // Test comparison operators
    test_parser("a == b", "expr");
    test_parser("a != b", "expr");
    test_parser("a < b", "expr");
    test_parser("a > b", "expr");
    test_parser("a <= b", "expr");
    test_parser("a >= b", "expr");
    
    // Test logical operators
    test_parser("a && b", "expr");
    test_parser("a || b", "expr");
    test_parser("!a", "expr");
    
    // Test bitwise operators (should fail currently)
    test_parser("a & b", "expr");
    test_parser("a | b", "expr");
    test_parser("a ^ b", "expr");
    test_parser("~a", "expr");
    test_parser("a << b", "expr");
    test_parser("a >> b", "expr");
    
    // Test assignment operators
    test_parser("a = b", "stmt");
    test_parser("a += b", "stmt");  // Should fail
    test_parser("a -= b", "stmt");  // Should fail
    test_parser("a *= b", "stmt");  // Should fail
    test_parser("a /= b", "stmt");  // Should fail
    test_parser("a %= b", "stmt");  // Should fail
    test_parser("a &= b", "stmt");  // Should fail
    test_parser("a |= b", "stmt");  // Should fail
    test_parser("a ^= b", "stmt");  // Should fail
    test_parser("a <<= b", "stmt"); // Should fail
    test_parser("a >>= b", "stmt"); // Should fail
    
    // Test string escapes
    test_parser(r#""hello\nworld""#, "expr");
    test_parser(r#""tab\there""#, "expr");
    test_parser(r#""quote\"here""#, "expr");
    test_parser(r#""backslash\\here""#, "expr");
    test_parser(r#""unicode\u{1F600}""#, "expr");
    test_parser(r#""hex\x7F""#, "expr");
    
    // Test float literals
    test_parser("3.14", "expr");
    test_parser("1.23e-4", "expr");
    test_parser("123.", "expr");
    test_parser(".456", "expr");  // Might fail
    
    println!("\n=== Test Complete ===");
}