// Test just declaring a SIMD type
fn main() {
    // Declare a variable with SIMD type
    let x: u64x8;
    
    // Try to assign to it
    x = simd_splat_u64x8(0);
    
    println!("SIMD test");
}