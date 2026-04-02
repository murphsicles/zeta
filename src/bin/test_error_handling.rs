use zetac::compile_and_run_zeta;

fn main() {
    // Test 1: Simple Option test
    let code1 = r#"
        fn main() -> i64 {
            let x = Option::Some(42);
            match x {
                Option::Some(val) => val,
                Option::None => 0,
            }
        }
    "#;
    
    match compile_and_run_zeta(code1) {
        Ok(result) => println!("Test 1 passed: {}", result),
        Err(e) => println!("Test 1 failed: {}", e),
    }
    
    // Test 2: Simple Result test
    let code2 = r#"
        fn main() -> i64 {
            let x = Result::Ok(42);
            match x {
                Result::Ok(val) => val,
                Result::Err(err) => err,
            }
        }
    "#;
    
    match compile_and_run_zeta(code2) {
        Ok(result) => println!("Test 2 passed: {}", result),
        Err(e) => println!("Test 2 failed: {}", e),
    }
    
    // Test 3: Try operator (this might fail)
    let code3 = r#"
        fn might_fail(succeed: bool) -> Result<i64, i64> {
            if succeed {
                Result::Ok(42)
            } else {
                Result::Err(99)
            }
        }
        
        fn test_try() -> Result<i64, i64> {
            let x = might_fail(true)?;
            Result::Ok(x)
        }
        
        fn main() -> i64 {
            match test_try() {
                Result::Ok(val) => val,
                Result::Err(err) => err,
            }
        }
    "#;
    
    match compile_and_run_zeta(code3) {
        Ok(result) => println!("Test 3 passed: {}", result),
        Err(e) => println!("Test 3 failed: {}", e),
    }
}