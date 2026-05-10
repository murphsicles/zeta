use src::frontend::parser::parser::parse_type;
use src::frontend::parser::top_level::parse_func;

fn main() {
    // Test 1: parse array type directly
    let test1 = "[u64; 10]";
    match parse_type(test1) {
        Ok((remaining, result)) => {
            println!("Test 1 - Direct array type parsing:");
            println!("  Input: {}", test1);
            println!("  Result: {}", result);
            println!("  Remaining: '{}'", remaining);
        }
        Err(e) => {
            println!("Test 1 FAILED: {:?}", e);
        }
    }

    // Test 2: parse function with array return type
    let test2 = "fn test() -> [u64; 10] { return [0; 10] }";
    match parse_func(test2) {
        Ok((remaining, result)) => {
            println!("\nTest 2 - Function with array return:");
            println!("  Input: {}", test2);
            println!("  Result: {:?}", result);
            println!("  Remaining: '{}'", remaining);
        }
        Err(e) => {
            println!("\nTest 2 FAILED: {:?}", e);
        }
    }

    // Test 3: parse comptime function with array return
    let test3 = "comptime fn generate_residues() -> [u64; NUM_RESIDUES]";
    match parse_func(test3) {
        Ok((remaining, result)) => {
            println!("\nTest 3 - Comptime function with array return:");
            println!("  Input: {}", test3);
            println!("  Result: {:?}", result);
            println!("  Remaining: '{}'", remaining);
        }
        Err(e) => {
            println!("\nTest 3 FAILED: {:?}", e);
        }
    }
}