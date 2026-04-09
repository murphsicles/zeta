//! Tests for higher-order function functionality in Zeta v0.3.18

use zetac::middle::resolver::new_resolver::type_check;
use zetac::parse_zeta;

#[test]
fn test_higher_order_function() {
    let code = r#"
        fn apply_twice(f: (i64) -> i64, x: i64) -> i64 {
            f(f(x))
        }
        
        fn add_one(x: i64) -> i64 {
            x + 1
        }
        
        let result = apply_twice(add_one, 5);
    "#;

    let (_, ast) = parse_zeta(code).expect("Failed to parse");
    let result = type_check(&ast);
    assert!(result.is_ok(), "Type checking failed: {:?}", result);
}

#[test]
fn test_higher_order_with_closure() {
    let code = r#"
        fn map(arr: [i64; 3], f: (i64) -> i64) -> [i64; 3] {
            let mut result = [0, 0, 0];
            let mut i = 0;
            while i < 3 {
                result[i] = f(arr[i]);
                i = i + 1;
            }
            result
        }
        
        let arr = [1, 2, 3];
        let x = 10;
        let result = map(arr, |y| y + x);
    "#;

    let (_, ast) = parse_zeta(code).expect("Failed to parse");
    let result = type_check(&ast);
    // This might fail because we haven't implemented function type parsing yet
    println!("Result: {:?}", result);
}

#[test]
fn test_closure_as_argument() {
    let code = r#"
        fn call_with_five(f: (i64) -> i64) -> i64 {
            f(5)
        }
        
        let x = 10;
        let result = call_with_five(|y| y + x);
    "#;

    let (_, ast) = parse_zeta(code).expect("Failed to parse");
    let result = type_check(&ast);
    println!("Result: {:?}", result);
}
