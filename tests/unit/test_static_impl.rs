// Test static method implementation
fn main() {
    // Simple struct definition
    struct Point {
        x: i64,
        y: i64,
    }

    // Impl block with static method
    impl Point {
        // Static constructor
        fn new(x: i64, y: i64) -> Point {
            Point { x, y }
        }

        // Instance method
        fn sum(&self) -> i64 {
            self.x + self.y
        }
    }

    // Test static method call
    let p = Point::new(10, 20);
    println!("Point created: ({}, {})", p.x, p.y);

    // Test instance method call
    let total = p.sum();
    println!("Sum: {}", total);
}
