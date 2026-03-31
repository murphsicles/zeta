// Test type argument propagation
fn identity<T>(x: T) -> T {
    x
}

fn main() {
    // Test with explicit type arguments
    let x = identity::<i64>(42);
    println!("Result: {}", x);

    // Test with Vec::new
    let _v = Vec::<i32>::new();
    println!("Vec created");
}
