// Test SIMD code generation
use zetac::backend::codegen::{LLVMCodegen, SimdCodegen};
use inkwell::context::Context;

fn test_simd_type_conversion() {
    let context = Context::create();
    let codegen = LLVMCodegen::new(&context, "test_module");
    
    // Test vector type conversion
    let i32_type = zetac::middle::types::Type::I32;
    let vec_type = zetac::middle::types::Type::Vector(Box::new(i32_type), 4);
    
    let llvm_type = codegen.type_to_llvm_type(&vec_type);
    println!("Vector<i32, 4> -> LLVM type: {:?}", llvm_type);
    
    // Test SIMD codegen
    let builder = context.create_builder();
    let simd_codegen = SimdCodegen::new(&context, &builder);
    
    // Test splat creation
    let i32_llvm_type = context.i32_type();
    let splat_value = i32_llvm_type.const_int(5, true);
    let splat_vector = simd_codegen.create_splat(i32_llvm_type, splat_value, 4);
    println!("Splat vector created: {:?}", splat_vector);
    
    // Test vector addition
    let vec1 = i32_llvm_type.vec_type(4).const_splat(i32_llvm_type.const_int(5, true));
    let vec2 = i32_llvm_type.vec_type(4).const_splat(i32_llvm_type.const_int(3, true));
    let sum = simd_codegen.vector_add(vec1, vec2, "sum");
    println!("Vector addition result: {:?}", sum);
}

fn main() {
    println!("Testing SIMD code generation...");
    test_simd_type_conversion();
    println!("Test completed!");
}