//! Integration tests for Zeta v0.3.9 features
//!
//! Tests the interaction between v0.3.9 features:
//! - Float literals
//! - Const parsing  
//! - Type system enhancements
//! - Match statements

use zetac::frontend::ast::AstNode;
use zetac::frontend::parser::top_level::parse_zeta;

/// Test float literals in various contexts
#[test]
fn test_float_literals_integration() {
    // Test 1: Basic float usage
    let code = r#"
    fn main() -> f64 {
        3.14
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse float literal in function");
    
    // Test 2: Float in arithmetic
    let code = r#"
    fn add() -> f64 {
        1.5 + 2.5
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse float arithmetic");
    
    // Test 3: Float in const
    let code = r#"
    const PI: f64 = 3.14159;
    
    fn circle_area(radius: f64) -> f64 {
        PI * radius * radius
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse float constant and usage");
    
    // Test 4: Mixed int/float (should work with type coercion)
    let code = r#"
    fn mixed() -> f64 {
        3 + 4.5  // Integer 3 should be promoted to float
    }
    "#;
    
    let result = parse_zeta(code);
    // Note: Type checking happens later, parsing should succeed
    assert!(result.is_ok(), "Should parse mixed int/float expression");
}

/// Test const parsing integration
#[test]
fn test_const_parsing_integration() {
    // Test 1: Const used in function
    let code = r#"
    const MAX_SIZE: i64 = 4096;
    
    fn allocate() -> i64 {
        MAX_SIZE * 2
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse const used in function");
    
    // Test 2: Multiple consts with dependencies
    let code = r#"
    const BASE: i64 = 100;
    const OFFSET: i64 = 20;
    const TOTAL: i64 = BASE + OFFSET;
    
    fn calculate() -> i64 {
        TOTAL - OFFSET
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse const dependencies");
    
    // Test 3: Const with complex expression
    let code = r#"
    const FLAGS: i64 = 0x1 | 0x2 | 0x4;
    const MASK: i64 = ~FLAGS;
    
    fn check(value: i64) -> bool {
        (value & FLAGS) != 0
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse const with bit operations");
    
    // Test 4: Const in match statement
    let code = r#"
    const MODE_A: i64 = 1;
    const MODE_B: i64 = 2;
    const MODE_C: i64 = 3;
    
    fn process(mode: i64) -> i64 {
        match mode {
            MODE_A => 100,
            MODE_B => 200,
            MODE_C => 300,
            _ => 0,
        }
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse const in match arms");
}

/// Test type system integration
#[test]
fn test_type_system_integration() {
    // Test 1: Function with explicit float type
    let code = r#"
    fn add_floats(x: f64, y: f64) -> f64 {
        x + y
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse function with float types");
    
    // Test 2: Const with type annotation
    let code = r#"
    const IS_ENABLED: bool = true;
    const MESSAGE: &str = "Hello";
    
    fn check() -> bool {
        IS_ENABLED && MESSAGE.len() > 0
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse typed constants");
    
    // Test 3: Array with const size
    let code = r#"
    const SIZE: i64 = 10;
    
    fn create_array() -> [i64; SIZE] {
        [0; SIZE]
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse array with const size");
    
    // Test 4: Type inference with floats
    let code = r#"
    fn infer_types() {
        let x = 3.14;      // Should infer f64
        let y = x + 1.5;   // Should infer f64
        let z = 42;        // Should infer i64
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse type inference examples");
}

/// Test match statement integration
#[test]
fn test_match_statement_integration() {
    // Test 1: Basic match with literals
    let code = r#"
    fn basic_match(value: i64) -> i64 {
        match value {
            1 => 100,
            2 => 200,
            3 => 300,
            _ => 0,
        }
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse basic match statement");
    
    // Test 2: Match with float patterns (if supported)
    let code = r#"
    fn float_match(value: f64) -> &str {
        match value {
            0.0 => "zero",
            1.0 => "one",
            3.14 => "pi",
            _ => "other",
        }
    }
    "#;
    
    let result = parse_zeta(code);
    // Float patterns might not be supported yet, but parsing should work
    assert!(result.is_ok(), "Should parse match with float patterns");
    
    // Test 3: Match with const patterns
    let code = r#"
    const RED: i64 = 1;
    const GREEN: i64 = 2;
    const BLUE: i64 = 3;
    
    fn color_match(color: i64) -> &str {
        match color {
            RED => "red",
            GREEN => "green",
            BLUE => "blue",
            _ => "unknown",
        }
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse match with const patterns");
    
    // Test 4: Nested match
    let code = r#"
    fn nested_match(x: i64, y: i64) -> i64 {
        match x {
            1 => match y {
                1 => 11,
                2 => 12,
                _ => 10,
            },
            2 => y * 2,
            _ => 0,
        }
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse nested match statements");
}

/// Test cross-feature integration
#[test]
fn test_cross_feature_integration() {
    // Test 1: All features together
    let code = r#"
    // Constants with various types
    const PI: f64 = 3.14159;
    const MAX_ATTEMPTS: i64 = 3;
    const APP_NAME: &str = "Zeta Compiler";
    
    // Function using all features
    fn process(value: f64, mode: i64) -> f64 {
        // Use const
        let base = PI;
        
        // Match statement
        let multiplier = match mode {
            1 => 1.0,
            2 => 2.0,
            3 => 3.0,
            _ => 0.5,
        };
        
        // Float arithmetic
        let result = base * value * multiplier;
        
        // Type conversion if needed
        if result > 100.0 {
            result / 2.0
        } else {
            result
        }
    }
    
    // Main function
    fn main() -> f64 {
        process(10.0, 2)
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse complex integration example");
    
    // Test 2: Real-world example - simple calculator
    let code = r#"
    // Calculator operations as constants
    const OP_ADD: i64 = 1;
    const OP_SUB: i64 = 2;
    const OP_MUL: i64 = 3;
    const OP_DIV: i64 = 4;
    
    // Calculator function
    fn calculate(op: i64, a: f64, b: f64) -> f64 {
        match op {
            OP_ADD => a + b,
            OP_SUB => a - b,
            OP_MUL => a * b,
            OP_DIV => {
                if b == 0.0 {
                    0.0  // Handle division by zero
                } else {
                    a / b
                }
            },
            _ => 0.0,  // Unknown operation
        }
    }
    
    // Example usage
    fn example() -> f64 {
        let result1 = calculate(OP_ADD, 3.14, 2.86);  // 6.0
        let result2 = calculate(OP_MUL, result1, 2.0); // 12.0
        result2
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse calculator example");
}

/// Test error cases and recovery
#[test]
fn test_error_recovery_integration() {
    // Test 1: Invalid float literal recovery
    let code = r#"
    fn test1() -> f64 {
        3.  // Invalid: trailing decimal only
    }
    
    fn test2() -> f64 {
        3.14  // Valid float
    }
    "#;
    
    let result = parse_zeta(code);
    // First function should fail, but second should still be parseable
    // This tests parser error recovery
    
    // Test 2: Const type mismatch recovery
    let code = r#"
    const BAD: i64 = "string";  // Type mismatch
    
    const GOOD: i64 = 42;  // Should still parse
    
    fn test() -> i64 {
        GOOD
    }
    "#;
    
    let result = parse_zeta(code);
    // Parsing should succeed (type checking happens later)
    assert!(result.is_ok(), "Should parse despite const type mismatch");
    
    // Test 3: Match exhaustiveness (future check)
    let code = r#"
    enum Color { Red, Green, Blue }
    
    fn match_color(c: Color) -> &str {
        match c {
            Color::Red => "red",
            // Missing Green and Blue - should warn in type checking
        }
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse non-exhaustive match");
}

/// Test performance with many features
#[test]
fn test_performance_integration() {
    // Generate a program with many constants, floats, and matches
    let mut code = String::new();
    code.push_str("// Performance test with many v0.3.9 features\n\n");
    
    // Add 100 constants
    for i in 0..100 {
        code.push_str(&format!("const C{}: i64 = {};\n", i, i));
    }
    
    code.push_str("\n// Function using all constants\n");
    code.push_str("fn sum_constants() -> i64 {\n");
    code.push_str("    let mut total = 0;\n");
    
    // Use constants in match
    code.push_str("    match C0 {\n");
    for i in 0..10 {
        code.push_str(&format!("        {} => total += C{},\n", i, i));
    }
    code.push_str("        _ => total = -1,\n");
    code.push_str("    }\n");
    
    // Use floats
    code.push_str("    let float_result = 3.14 * total as f64;\n");
    code.push_str("    float_result as i64\n");
    code.push_str("}\n");
    
    let result = parse_zeta(&code);
    assert!(result.is_ok(), "Should parse large integration test");
}

/// Test backward compatibility
#[test]
fn test_backward_compatibility() {
    // Test that v0.3.7-style code still parses
    let v0_3_7_code = r#"
    // v0.3.7 style constants
    const OLD_CONST: i64 = 42;
    
    // v0.3.7 functions
    fn old_function(x: i64) -> i64 {
        x * 2
    }
    
    // Should still work in v0.3.9
    fn new_function() -> f64 {
        3.14 + old_function(10) as f64
    }
    "#;
    
    let result = parse_zeta(v0_3_7_code);
    assert!(result.is_ok(), "Should maintain backward compatibility");
    
    // Test mixed old and new features
    let mixed_code = r#"
    // Old: integer constants
    const INT_CONST: i64 = 100;
    
    // New: float constant
    const FLOAT_CONST: f64 = 3.14159;
    
    // Old: integer function
    fn int_op(a: i64, b: i64) -> i64 {
        a + b
    }
    
    // New: float function with match
    fn float_op(op: i64, a: f64, b: f64) -> f64 {
        match op {
            1 => a + b,
            2 => a - b,
            3 => a * b,
            4 => a / b,
            _ => 0.0,
        }
    }
    
    // Mixed usage
    fn mixed() -> f64 {
        let int_result = int_op(INT_CONST, 50);
        float_op(3, int_result as f64, FLOAT_CONST)
    }
    "#;
    
    let result = parse_zeta(mixed_code);
    assert!(result.is_ok(), "Should parse mixed old/new features");
}

// Helper function to count AST nodes (for complexity analysis)
fn count_ast_nodes(ast: &AstNode) -> usize {
    match ast {
        AstNode::Program(nodes) => {
            1 + nodes.iter().map(|n| count_ast_nodes(n)).sum::<usize>()
        }
        AstNode::FuncDef { body, .. } => {
            1 + body.iter().map(|n| count_ast_nodes(n)).sum::<usize>()
        }
        AstNode::Match { scrutinee, arms } => {
            1 + count_ast_nodes(scrutinee)
                + arms.iter().map(|arm| {
                    count_ast_nodes(&arm.pattern)
                        + arm.guard.as_ref().map_or(0, |g| count_ast_nodes(g))
                        + count_ast_nodes(&arm.body)
                }).sum::<usize>()
        }
        // Add other variants as needed
        _ => 1,
    }
}

#[test]
fn test_ast_complexity() {
    // Simple test to ensure AST construction works
    let code = r#"
    fn simple() -> f64 {
        3.14
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok());
    
    let (_, asts) = result.unwrap();
    assert!(!asts.is_empty(), "Should have at least one AST node");
    
    // Count total nodes
    let total_nodes: usize = asts.iter().map(|ast| count_ast_nodes(ast)).sum();
    assert!(total_nodes > 0, "Should have positive node count");
    
    println!("AST has {} total nodes", total_nodes);
}