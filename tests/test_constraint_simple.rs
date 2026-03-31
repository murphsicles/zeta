fn main() {
    println!("Testing constraint system");
    
    // Create a simple test
    let code = r#"
    fn main() -> i64 {
        let x: i64 = 42;
        x
    }
    "#;
    
    println!("Code: {}", code);
}