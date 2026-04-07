use zetac::frontend::parser::parser::parse_simd_type;

fn main() {
    println!("Testing SIMD type parsing...");
    
    // Test u64x8
    let test1 = "u64x8";
    match parse_simd_type(test1) {
        Ok((remaining, result)) => {
            println!("Test 1 (u64x8): Success!");
            println!("  Result: {}", result);
            println!("  Remaining: '{}'", remaining);
        }
        Err(e) => {
            println!("Test 1 (u64x8): Failed: {:?}", e);
        }
    }
    
    // Test Vector<u64, 8>
    let test2 = "Vector<u64, 8>";
    match parse_simd_type(test2) {
        Ok((remaining, result)) => {
            println!("Test 2 (Vector<u64, 8>): Success!");
            println!("  Result: {}", result);
            println!("  Remaining: '{}'", remaining);
        }
        Err(e) => {
            println!("Test 2 (Vector<u64, 8>): Failed: {:?}", e);
        }
    }
    
    // Test with whitespace
    let test3 = "Vector < u64 , 8 >";
    match parse_simd_type(test3) {
        Ok((remaining, result)) => {
            println!("Test 3 (Vector < u64 , 8 >): Success!");
            println!("  Result: {}", result);
            println!("  Remaining: '{}'", remaining);
        }
        Err(e) => {
            println!("Test 3 (Vector < u64 , 8 >): Failed: {:?}", e);
        }
    }
}