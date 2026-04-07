// Minimal SIMD test to reproduce crash
fn main() {
    // Try to create a SIMD vector
    let v: Vector<f32, 4> = Vector::new(1.0, 2.0, 3.0, 4.0);
    println!("SIMD vector: {:?}", v);
    
    // Try to use SIMD in an array
    let arr: [Vector<f32, 4>; 2] = [
        Vector::new(1.0, 2.0, 3.0, 4.0),
        Vector::new(5.0, 6.0, 7.0, 8.0)
    ];
    println!("SIMD array: {:?}", arr);
}