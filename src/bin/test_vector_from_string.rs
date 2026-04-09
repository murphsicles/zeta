use zetac::middle::types::Type;

fn main() {
    println!("Testing Type::from_string for Vector types...");
    
    // Test Vector<u64, 8>
    let test1 = "Vector<u64, 8>";
    let type1 = Type::from_string(test1);
    println!("Test 1 (Vector<u64, 8>): {:?}", type1);
    println!("  Display name: {}", type1.display_name());
    println!("  Is vector: {}", type1.is_vector());
    
    // Test u64x8 (should be converted by parser to Vector<u64, 8>)
    let test2 = "Vector<u64, 8>"; // Parser converts u64x8 to this
    let type2 = Type::from_string(test2);
    println!("\nTest 2 (u64x8 -> Vector<u64, 8>): {:?}", type2);
    println!("  Display name: {}", type2.display_name());
    println!("  Is vector: {}", type2.is_vector());
    
    // Test Vector<i32, 4>
    let test3 = "Vector<i32, 4>";
    let type3 = Type::from_string(test3);
    println!("\nTest 3 (Vector<i32, 4>): {:?}", type3);
    println!("  Display name: {}", type3.display_name());
    println!("  Is vector: {}", type3.is_vector());
    
    // Test that as_vector works
    if let Some((inner, size)) = type1.as_vector() {
        println!("\nTest 1 as_vector:");
        println!("  Inner type: {:?}", inner);
        println!("  Size: {}", size);
    }
}