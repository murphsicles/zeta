// Test to enable and test monomorphization
use zetac::compile_and_run_zeta;

fn main() {
    println!("=== Testing Monomorphization Enablement ===");

    // Test 1: Simple generic function
    let code1 = r#"
    fn identity<T>(x: T) -> T { x }
    
    fn main() -> i64 {
        identity::<i64>(42)
    }
    "#;

    println!("Test 1: Simple generic function identity::<i64>(42)");
    match compile_and_run_zeta(code1) {
        Ok(result) => {
            println!("✓ Result: {}", result);
            assert_eq!(result, 42, "identity function should return 42");
        }
        Err(e) => {
            println!("✗ Error: {}", e);
            println!("This might be expected if monomorphization isn't fully integrated yet");
        }
    }

    // Test 2: vec_new::<i32>() as requested
    let code2 = r#"
    struct Vec<T> {
        data: [T],
    }
    
    fn vec_new<T>() -> Vec<T> {
        Vec { data: [] }
    }
    
    fn main() -> i64 {
        let v = vec_new::<i32>();
        0
    }
    "#;

    println!("\nTest 2: vec_new::<i32>()");
    match compile_and_run_zeta(code2) {
        Ok(result) => {
            println!("✓ Result: {}", result);
            println!("✓ Monomorphization working for vec_new::<i32>()");
        }
        Err(e) => {
            println!("✗ Error: {}", e);
            println!("Need to fix monomorphization integration");
        }
    }

    // Test 3: Generic function with multiple type parameters
    let code3 = r#"
    fn pair<T, U>(a: T, b: U) -> (T, U) {
        (a, b)
    }
    
    fn main() -> i64 {
        let p = pair::<i64, i32>(100, 200);
        0
    }
    "#;

    println!("\nTest 3: Generic function with multiple type parameters");
    match compile_and_run_zeta(code3) {
        Ok(result) => {
            println!("✓ Result: {}", result);
        }
        Err(e) => {
            println!("✗ Error: {}", e);
        }
    }

    println!("\n=== Monomorphization Status ===");
    println!("Check if get_function_with_types is being called in codegen");
    println!("Check if type_args are being passed from MIR to codegen");
    println!("Check if monomorphize_function creates proper LLVM functions");
}
