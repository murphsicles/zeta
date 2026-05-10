//! Tests for standard library functions with closures in Zeta v0.3.18

use zetac::middle::resolver::new_resolver::type_check;
use zetac::parse_zeta;

#[test]
fn test_map_function() {
    let code = r#"
        // Simple map function for arrays
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
        let doubled = map(arr, |x| x * 2);
    "#;

    let (_, ast) = parse_zeta(code).expect("Failed to parse");
    let result = type_check(&ast);
    assert!(result.is_ok(), "Type checking failed: {:?}", result);
}

#[test]
fn test_filter_function() {
    let code = r#"
        // Simple filter function that returns array of same size
        // (In real implementation, we'd need dynamic arrays or slices)
        fn filter(arr: [i64; 3], f: (i64) -> bool) -> [i64; 3] {
            let mut result = [0, 0, 0];
            let mut j = 0;
            let mut i = 0;
            while i < 3 {
                if f(arr[i]) {
                    result[j] = arr[i];
                    j = j + 1;
                }
                i = i + 1;
            }
            result
        }
        
        let arr = [1, 2, 3, 4, 5];
        let evens = filter(arr, |x| x % 2 == 0);
    "#;

    let (_, ast) = parse_zeta(code).expect("Failed to parse");
    let result = type_check(&ast);
    println!("Result: {:?}", result);
}

#[test]
fn test_fold_function() {
    let code = r#"
        // Fold/reduce function
        fn fold(arr: [i64; 3], init: i64, f: (i64, i64) -> i64) -> i64 {
            let mut result = init;
            let mut i = 0;
            while i < 3 {
                result = f(result, arr[i]);
                i = i + 1;
            }
            result
        }
        
        let arr = [1, 2, 3, 4, 5];
        let sum = fold(arr, 0, |acc, x| acc + x);
    "#;

    let (_, ast) = parse_zeta(code).expect("Failed to parse");
    let result = type_check(&ast);
    println!("Result: {:?}", result);
}

#[test]
fn test_closure_capturing_in_stdlib() {
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
        
        let multiplier = 3;
        let arr = [1, 2, 3];
        let tripled = map(arr, |x| x * multiplier);
    "#;

    let (_, ast) = parse_zeta(code).expect("Failed to parse");
    let result = type_check(&ast);
    assert!(result.is_ok(), "Type checking failed: {:?}", result);
}
