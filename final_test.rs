fn main() {
    let tests = vec![
        "fn no_space(x:i64)->i64{x}",
        "fn space_before_colon(x :i64)->i64{x}",
        "fn space_after_colon(x: i64)->i64{x}",
        "fn space_both(x : i64)->i64{x}",
        "fn multiple_spaces(x   :   i64) -> i64 { x }",
    ];
    
    for test in tests {
        println!("Testing: {}", test);
        // In a real test, we'd parse it here
        println!("  Should work with proper parser");
    }
}