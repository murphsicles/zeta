// Simple test for type argument propagation
fn identity<T>(x: T) -> T {
    x
}

fn main() {
    // Test with explicit type arguments
    let x = identity::<i64>(42);
    println!("Result: {}", x);
}
