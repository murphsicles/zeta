use src::frontend::parser::parser::parse_type;

fn main() {
    println!("Testing SIMD type parsing...");
    
    let test_cases = vec![
        "i32x4",
        "Vector<i32, 4>",
        "u64x8",
        "f32x4",
    ];
    
    for input in test_cases {
        match parse_type(input) {
            Ok((remaining, result)) => {
                if remaining.is_empty() {
                    println!("✓ {} -> {}", input, result);
                } else {
                    println!("✗ {}: didn't consume all input, remaining: '{}'", input, remaining);
                }
            }
            Err(e) => {
                println!("✗ {}: parse error: {:?}", input, e);
            }
        }
    }
}