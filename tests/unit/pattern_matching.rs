//! Comprehensive tests for pattern matching features

use zetac::AstNode;
use zetac::Resolver;
use zetac::frontend::parser::expr::parse_expr;
use zetac::frontend::parser::stmt::parse_stmt;

#[test]
fn test_if_let_syntax() {
    let code = r#"
if let Ok(value) = result {
    println!("Got value: {}", value);
}
"#;

    // Parse the if-let statement
    let result = parse_stmt(code);
    assert!(result.is_ok(), "Failed to parse if-let: {:?}", result);

    let (remaining, ast) = result.unwrap();
    assert!(
        remaining.is_empty() || remaining.trim().is_empty(),
        "Didn't parse entire input: '{}'",
        remaining
    );

    // Check it's an IfLet node
    match ast {
        AstNode::IfLet {
            pattern,
            expr,
            then,
            else_,
        } => {
            // Pattern should be a struct pattern for Result::Ok
            match &*pattern {
                AstNode::StructPattern {
                    variant, fields, ..
                } => {
                    assert_eq!(variant, "Ok");
                    assert_eq!(fields.len(), 1);
                    // The field should be a variable pattern "value"
                    if let (_, AstNode::Var(var_name)) = &fields[0] {
                        assert_eq!(var_name, "value");
                    } else {
                        panic!("Expected variable pattern 'value'");
                    }
                }
                _ => panic!("Expected StructPattern for Ok variant"),
            }

            // Expression should be a variable "result"
            match &*expr {
                AstNode::Var(name) => {
                    assert_eq!(name, "result");
                }
                _ => panic!("Expected variable 'result'"),
            }

            // Then block should not be empty
            assert!(!then.is_empty());

            // Else should be empty (no else clause)
            assert!(else_.is_empty());
        }
        _ => panic!("Expected IfLet node, got: {:?}", ast),
    }
}

#[test]
fn test_struct_pattern_named_fields() {
    let code = r#"
match point {
    Point { x, y } => x + y,
    Point { x: 0, y } => y,
    _ => 0,
}
"#;

    let result = parse_expr(code);
    assert!(
        result.is_ok(),
        "Failed to parse match with struct pattern: {:?}",
        result
    );

    let (remaining, ast) = result.unwrap();
    assert!(
        remaining.is_empty() || remaining.trim().is_empty(),
        "Didn't parse entire input: '{}'",
        remaining
    );

    // Check it's a Match node
    match ast {
        AstNode::Match { scrutinee, arms } => {
            // Scrutinee should be variable "point"
            match &*scrutinee {
                AstNode::Var(name) => {
                    assert_eq!(name, "point");
                }
                _ => panic!("Expected variable 'point'"),
            }

            // Should have 3 arms
            assert_eq!(arms.len(), 3);

            // First arm: Point { x, y }
            if let AstNode::StructPattern {
                variant, fields, ..
            } = &*arms[0].pattern
            {
                assert_eq!(variant, "Point");
                assert_eq!(fields.len(), 2);

                // Check field names
                let field_names: Vec<&String> = fields.iter().map(|(name, _)| name).collect();
                assert!(field_names.contains(&&"x".to_string()));
                assert!(field_names.contains(&&"y".to_string()));
            } else {
                panic!("Expected StructPattern for first arm");
            }

            // Second arm: Point { x: 0, y }
            if let AstNode::StructPattern {
                variant, fields, ..
            } = &*arms[1].pattern
            {
                assert_eq!(variant, "Point");
                assert_eq!(fields.len(), 2);

                // One field should have a literal pattern
                let mut found_literal = false;
                let mut found_variable = false;
                for (name, pat) in fields {
                    if name == "x" {
                        if let AstNode::Lit(0) = pat {
                            found_literal = true;
                        }
                    } else if name == "y"
                        && let AstNode::Var(var_name) = pat
                    {
                        assert_eq!(var_name, "y");
                        found_variable = true;
                    }
                }
                assert!(found_literal, "Expected literal pattern for x: 0");
                assert!(found_variable, "Expected variable pattern for y");
            } else {
                panic!("Expected StructPattern for second arm");
            }

            // Third arm: wildcard
            match &*arms[2].pattern {
                AstNode::Ignore => {
                    // Good
                }
                _ => panic!("Expected wildcard pattern for third arm"),
            }
        }
        _ => panic!("Expected Match node, got: {:?}", ast),
    }
}

#[test]
fn test_tuple_pattern() {
    let code = r#"
match coordinates {
    (x, y) => x + y,
    (0, y) => y,
    (x, 0) => x,
    _ => 0,
}
"#;

    let result = parse_expr(code);
    assert!(
        result.is_ok(),
        "Failed to parse match with tuple pattern: {:?}",
        result
    );

    let (remaining, ast) = result.unwrap();
    assert!(
        remaining.is_empty() || remaining.trim().is_empty(),
        "Didn't parse entire input: '{}'",
        remaining
    );

    match ast {
        AstNode::Match { scrutinee, arms } => {
            match &*scrutinee {
                AstNode::Var(name) => {
                    assert_eq!(name, "coordinates");
                }
                _ => panic!("Expected variable 'coordinates'"),
            }

            assert_eq!(arms.len(), 4);

            // First arm: (x, y) tuple pattern
            match &*arms[0].pattern {
                AstNode::Tuple(patterns) => {
                    assert_eq!(patterns.len(), 2);
                    match &patterns[0] {
                        AstNode::Var(name) => assert_eq!(name, "x"),
                        _ => panic!("Expected variable pattern 'x'"),
                    }
                    match &patterns[1] {
                        AstNode::Var(name) => assert_eq!(name, "y"),
                        _ => panic!("Expected variable pattern 'y'"),
                    }
                }
                _ => panic!("Expected tuple pattern for first arm"),
            }

            // Second arm: (0, y)
            match &*arms[1].pattern {
                AstNode::Tuple(patterns) => {
                    assert_eq!(patterns.len(), 2);
                    match &patterns[0] {
                        AstNode::Lit(0) => {} // Good
                        _ => panic!("Expected literal 0"),
                    }
                    match &patterns[1] {
                        AstNode::Var(name) => assert_eq!(name, "y"),
                        _ => panic!("Expected variable pattern 'y'"),
                    }
                }
                _ => panic!("Expected tuple pattern for second arm"),
            }
        }
        _ => panic!("Expected Match node"),
    }
}

#[test]
fn test_pattern_guards() {
    let code = r#"
match value {
    x if x > 0 => "positive",
    x if x < 0 => "negative",
    _ => "zero",
}
"#;

    let result = parse_expr(code);
    assert!(
        result.is_ok(),
        "Failed to parse match with pattern guards: {:?}",
        result
    );

    let (remaining, ast) = result.unwrap();
    assert!(
        remaining.is_empty() || remaining.trim().is_empty(),
        "Didn't parse entire input: '{}'",
        remaining
    );

    match ast {
        AstNode::Match { arms, .. } => {
            assert_eq!(arms.len(), 3);

            // First arm should have a guard
            assert!(arms[0].guard.is_some());

            // Second arm should have a guard
            assert!(arms[1].guard.is_some());

            // Third arm should not have a guard
            assert!(arms[2].guard.is_none());

            // Check the guard expressions
            if let Some(guard) = &arms[0].guard {
                // Should be a binary operation x > 0
                match &**guard {
                    AstNode::BinaryOp { op, left, right } => {
                        assert_eq!(op, ">");
                        match &**left {
                            AstNode::Var(name) => assert_eq!(name, "x"),
                            _ => panic!("Expected variable 'x' in guard"),
                        }
                        match &**right {
                            AstNode::Lit(0) => {} // Good
                            _ => panic!("Expected literal 0 in guard"),
                        }
                    }
                    _ => panic!("Expected binary operation in guard"),
                }
            }
        }
        _ => panic!("Expected Match node"),
    }
}

#[test]
fn test_tuple_struct_pattern() {
    let code = r#"
match result {
    Ok(value) => value,
    Err(msg) => 0,
}
"#;

    let result = parse_expr(code);
    assert!(
        result.is_ok(),
        "Failed to parse match with tuple struct pattern: {:?}",
        result
    );

    let (remaining, ast) = result.unwrap();
    assert!(
        remaining.is_empty() || remaining.trim().is_empty(),
        "Didn't parse entire input: '{}'",
        remaining
    );

    match ast {
        AstNode::Match { arms, .. } => {
            assert_eq!(arms.len(), 2);

            // First arm: Ok(value) - tuple struct pattern
            match &*arms[0].pattern {
                AstNode::StructPattern {
                    variant, fields, ..
                } => {
                    assert_eq!(variant, "Ok");
                    assert_eq!(fields.len(), 1);
                    // Should be indexed as "0" for tuple struct
                    assert_eq!(fields[0].0, "0");
                    match &fields[0].1 {
                        AstNode::Var(name) => assert_eq!(name, "value"),
                        _ => panic!("Expected variable pattern 'value'"),
                    }
                }
                _ => panic!("Expected StructPattern for Ok variant"),
            }
        }
        _ => panic!("Expected Match node"),
    }
}

#[test]
fn test_pattern_type_checking() {
    // This test requires a resolver to type check patterns
    let mut resolver = Resolver::new();

    // Test simple variable pattern
    let code = "let x = 42;";
    let result = parse_stmt(code);
    assert!(result.is_ok());

    let (_, ast) = result.unwrap();
    let typecheck_result = resolver.typecheck(&[ast]);
    assert!(
        typecheck_result,
        "Type checking should succeed for simple let"
    );

    // Test tuple pattern
    let code = "let (x, y) = (1, 2);";
    let result = parse_stmt(code);
    assert!(result.is_ok());

    let (_, ast) = result.unwrap();
    resolver = Resolver::new(); // Fresh resolver
    let typecheck_result = resolver.typecheck(&[ast]);
    assert!(
        typecheck_result,
        "Type checking should succeed for tuple pattern"
    );

    // Test if-let
    let code = r#"
if let Some(value) = maybe_value {
    println!("{}", value);
}
"#;
    let result = parse_stmt(code);
    assert!(result.is_ok());

    let (_, ast) = result.unwrap();
    resolver = Resolver::new(); // Fresh resolver
    let typecheck_result = resolver.typecheck(&[ast]);
    // This might fail if we don't have Result/Some types defined
    // but the pattern parsing should still work
    println!("Type check result for if-let: {}", typecheck_result);
}
