// Test for nested if destination initialization bug
fn test_nested_if() -> i64 {
    let x: i64;
    if true {
        if false {
            x = 42;
        }
        // x might be uninitialized here if inner condition is false
    } else {
        x = 0;
    }
    x  // This might read uninitialized memory
}

fn main() {
    let result = test_nested_if();
    println!("Result: {}", result);
}