//! SIMD test suite for Zeta
//!
//! Tests AVX-512 SIMD operations and type system integration.

use zeta::middle::types::simd::{SimdTypeInfo, aliases};
use zeta::middle::types::Type;

#[test]
fn test_simd_type_info_creation() {
    // Test integer SIMD types
    assert!(SimdTypeInfo::new(Type::I32, 4).is_some());
    assert!(SimdTypeInfo::new(Type::I32, 8).is_some());
    assert!(SimdTypeInfo::new(Type::I32, 16).is_some());
    
    // Test floating-point SIMD types
    assert!(SimdTypeInfo::new(Type::F32, 4).is_some());
    assert!(SimdTypeInfo::new(Type::F32, 8).is_some());
    assert!(SimdTypeInfo::new(Type::F32, 16).is_some());
    
    // Test unsupported types
    assert!(SimdTypeInfo::new(Type::Bool, 4).is_none()); // Bool not supported
    assert!(SimdTypeInfo::new(Type::I32, 3).is_none()); // Not power of two
    assert!(SimdTypeInfo::new(Type::I32, 128).is_none()); // Too many lanes
}

#[test]
fn test_simd_type_aliases() {
    // Test that type aliases create correct types
    let f32x4 = aliases::f32x4();
    let f32x8 = aliases::f32x8();
    let f32x16 = aliases::f32x16();
    
    let f64x4 = aliases::f64x4();
    let f64x8 = aliases::f64x8();
    
    let i32x8 = aliases::i32x8();
    let i32x16 = aliases::i32x16();
    
    let i64x4 = aliases::i64x4();
    let i64x8 = aliases::i64x8();
    
    // Verify types are vectors
    match f32x4 {
        Type::Vector(element_type, size) => {
            assert_eq!(*element_type, Type::F32);
            match size {
                zeta::middle::types::ArraySize::Literal(n) => assert_eq!(n, 4),
                _ => panic!("Expected literal size"),
            }
        }
        _ => panic!("Expected vector type"),
    }
    
    match f32x16 {
        Type::Vector(element_type, size) => {
            assert_eq!(*element_type, Type::F32);
            match size {
                zeta::middle::types::ArraySize::Literal(n) => assert_eq!(n, 16),
                _ => panic!("Expected literal size"),
            }
        }
        _ => panic!("Expected vector type"),
    }
}

#[test]
fn test_simd_type_info_properties() {
    // Test 4-element i32 vector (128 bits)
    let i32x4 = SimdTypeInfo::new(Type::I32, 4).unwrap();
    assert_eq!(i32x4.element_type, Type::I32);
    assert_eq!(i32x4.lane_count, 4);
    assert_eq!(i32x4.bit_width, 128); // 4 * 32 = 128 bits
    assert!(!i32x4.requires_avx512()); // 128-bit doesn't require AVX-512
    
    // Test 8-element f64 vector (512 bits)
    let f64x8 = SimdTypeInfo::new(Type::F64, 8).unwrap();
    assert_eq!(f64x8.element_type, Type::F64);
    assert_eq!(f64x8.lane_count, 8);
    assert_eq!(f64x8.bit_width, 512); // 8 * 64 = 512 bits
    assert!(f64x8.requires_avx512()); // 512-bit requires AVX-512
    
    // Test 16-element f32 vector (512 bits)
    let f32x16 = SimdTypeInfo::new(Type::F32, 16).unwrap();
    assert_eq!(f32x16.element_type, Type::F32);
    assert_eq!(f32x16.lane_count, 16);
    assert_eq!(f32x16.bit_width, 512); // 16 * 32 = 512 bits
    assert!(f32x16.requires_avx512()); // 512-bit requires AVX-512
}

#[test]
fn test_simd_type_conversion() {
    // Test conversion between SimdTypeInfo and Type
    let i32x8_info = SimdTypeInfo::new(Type::I32, 8).unwrap();
    let i32x8_type = i32x8_info.to_type();
    
    // Convert back
    let converted_info = SimdTypeInfo::from_type(&i32x8_type).unwrap();
    assert_eq!(converted_info.element_type, Type::I32);
    assert_eq!(converted_info.lane_count, 8);
    assert_eq!(converted_info.bit_width, 256);
    
    // Test with alias
    let f32x4_type = aliases::f32x4();
    let f32x4_info = SimdTypeInfo::from_type(&f32x4_type).unwrap();
    assert_eq!(f32x4_info.element_type, Type::F32);
    assert_eq!(f32x4_info.lane_count, 4);
    assert_eq!(f32x4_info.bit_width, 128);
}

#[test]
fn test_simd_operations_support() {
    use zeta::middle::types::simd::operations::*;
    
    // Test integer SIMD type
    let i32x8 = SimdTypeInfo::new(Type::I32, 8).unwrap();
    assert!(i32x8.supports_add());
    assert!(i32x8.supports_sub());
    assert!(i32x8.supports_mul());
    assert!(!i32x8.supports_div()); // Integer division not supported
    
    assert!(i32x8.supports_and());
    assert!(i32x8.supports_or());
    assert!(i32x8.supports_xor());
    assert!(i32x8.supports_shift());
    
    assert!(i32x8.supports_eq());
    assert!(i32x8.supports_ne());
    assert!(i32x8.supports_gt());
    assert!(i32x8.supports_lt());
    
    assert!(i32x8.supports_hadd());
    assert!(i32x8.supports_hmin());
    assert!(i32x8.supports_hmax());
    
    // Test floating-point SIMD type
    let f32x8 = SimdTypeInfo::new(Type::F32, 8).unwrap();
    assert!(f32x8.supports_add());
    assert!(f32x8.supports_sub());
    assert!(f32x8.supports_mul());
    assert!(f32x8.supports_div()); // Floating-point division supported
    
    assert!(!f32x8.supports_and()); // Bitwise ops not supported for floats
    assert!(!f32x8.supports_or());
    assert!(!f32x8.supports_xor());
    assert!(!f32x8.supports_shift());
    
    assert!(f32x8.supports_eq());
    assert!(f32x8.supports_ne());
    assert!(f32x8.supports_gt());
    assert!(f32x8.supports_lt());
    
    assert!(f32x8.supports_hadd());
    assert!(f32x8.supports_hmin());
    assert!(f32x8.supports_hmax());
}

#[test]
fn test_simd_memory_operations() {
    use zeta::middle::types::simd::memory::*;
    
    // Test memory operation support
    let i32x4 = SimdTypeInfo::new(Type::I32, 4).unwrap();
    let f64x8 = SimdTypeInfo::new(Type::F64, 8).unwrap();
    
    assert!(supports_aligned_load(&i32x4));
    assert!(supports_unaligned_load(&i32x4));
    assert!(!supports_gather(&i32x4)); // 128-bit doesn't support gather
    assert!(!supports_scatter(&i32x4)); // 128-bit doesn't support scatter
    
    assert!(supports_aligned_load(&f64x8));
    assert!(supports_unaligned_load(&f64x8));
    assert!(supports_gather(&f64x8)); // 512-bit supports gather
    assert!(supports_scatter(&f64x8)); // 512-bit supports scatter
    
    // Test alignment recommendations
    assert_eq!(recommended_alignment(&i32x4), Alignment::Element);
    assert_eq!(recommended_alignment(&f64x8), Alignment::Vector);
}

#[test]
fn test_simd_compatibility() {
    use zeta::middle::types::simd::compatibility::*;
    
    // Test type compatibility
    let i32x4_a = SimdTypeInfo::new(Type::I32, 4).unwrap();
    let i32x4_b = SimdTypeInfo::new(Type::I32, 4).unwrap();
    let i32x8 = SimdTypeInfo::new(Type::I32, 8).unwrap();
    let f32x4 = SimdTypeInfo::new(Type::F32, 4).unwrap();
    
    assert!(are_compatible(&i32x4_a, &i32x4_b));
    assert!(!are_compatible(&i32x4_a, &i32x8));
    assert!(!are_compatible(&i32x4_a, &f32x4));
    
    // Test type casting
    assert!(can_cast(&i32x4_a, &i32x4_b)); // Same type
    assert!(can_cast(&i32x4_a, &f32x4)); // i32 -> f32
    assert!(can_cast(&f32x4, &i32x4_a)); // f32 -> i32 (with truncation)
    
    // Test common type
    assert_eq!(common_type(&i32x4_a, &i32x4_b), Some(i32x4_a.clone()));
    assert_eq!(common_type(&i32x4_a, &i32x8), None); // Different lane counts
    assert_eq!(common_type(&i32x4_a, &f32x4), None); // Different element types
}

#[test]
fn test_simd_mask() {
    use zeta::middle::types::simd::SimdMask;
    
    // Test mask creation
    let mask_4 = SimdMask::new(4);
    assert_eq!(mask_4.lane_count, 4);
    assert_eq!(mask_4.bit_width, 8); // 4 lanes -> 8-bit mask
    
    let mask_8 = SimdMask::new(8);
    assert_eq!(mask_8.lane_count, 8);
    assert_eq!(mask_8.bit_width, 8); // 8 lanes -> 8-bit mask
    
    let mask_16 = SimdMask::new(16);
    assert_eq!(mask_16.lane_count, 16);
    assert_eq!(mask_16.bit_width, 16); // 16 lanes -> 16-bit mask
    
    let mask_32 = SimdMask::new(32);
    assert_eq!(mask_32.lane_count, 32);
    assert_eq!(mask_32.bit_width, 32); // 32 lanes -> 32-bit mask
    
    let mask_64 = SimdMask::new(64);
    assert_eq!(mask_64.lane_count, 64);
    assert_eq!(mask_64.bit_width, 64); // 64 lanes -> 64-bit mask
    
    // Test mask for SIMD type
    let i32x8_info = SimdTypeInfo::new(Type::I32, 8).unwrap();
    let mask_for_i32x8 = SimdMask::for_simd_type(&i32x8_info);
    assert_eq!(mask_for_i32x8.lane_count, 8);
    assert_eq!(mask_for_i32x8.bit_width, 8);
}

#[test]
fn test_simd_fallback_compatibility() {
    use zeta::middle::types::simd::fallback;
    
    // Test fallback type creation
    let compatible_i32x16 = fallback::create_compatible_type(Type::I32, 16);
    let compatible_f64x8 = fallback::create_compatible_type(Type::F64, 8);
    
    // These should create valid types even if AVX-512 is not available
    match compatible_i32x16 {
        Type::Vector(element_type, size) => {
            assert_eq!(*element_type, Type::I32);
            match size {
                zeta::middle::types::ArraySize::Literal(n) => {
                    // Should be <= 16 based on available hardware
                    assert!(n <= 16);
                }
                _ => panic!("Expected literal size"),
            }
        }
        _ => panic!("Expected vector type"),
    }
    
    match compatible_f64x8 {
        Type::Vector(element_type, size) => {
            assert_eq!(*element_type, Type::F64);
            match size {
                zeta::middle::types::ArraySize::Literal(n) => {
                    // Should be <= 8 based on available hardware
                    assert!(n <= 8);
                }
                _ => panic!("Expected literal size"),
            }
        }
        _ => panic!("Expected vector type"),
    }
}

// Integration test with code generation
#[test]
fn test_simd_codegen_integration() {
    use zeta::backend::codegen::simd::SimdCodegen;
    use inkwell::context::Context;
    use inkwell::builder::Builder;
    
    // Create LLVM context and builder
    let context = Context::create();
    let module = context.create_module("simd_test");
    let builder = context.create_builder();
    
    // Create SIMD codegen instance
    let simd_codegen = SimdCodegen::new(&context, &builder);
    
    // Test type conversion
    let i32_type = context.i32_type();
    let i32x4_type = i32_type.vec_type(4);
    
    // Test splat creation
    let splat_value = i32_type.const_int(42, true);
    let splat_vector = simd_codegen.create_splat(i32x4_type, splat_value.into(), 4);
    
    // Verify vector type
    assert_eq!(splat_vector.get_type().get_size(), 4);
    
    // Test arithmetic operations
    let a = i32_type.const_int(10, true);
    let b = i32_type.const_int(20, true);
    let vec_a = simd_codegen.create_splat(i32x4_type, a.into(), 4);
    let vec_b = simd_codegen.create_splat(i32x4_type, b.into(), 4);
    
    let vec_add = simd_codegen.vector_add(vec_a, vec_b, "test_add");
    let vec_sub = simd_codegen.vector_sub(vec_a, vec_b, "test_sub");
    let vec_mul = simd_codegen.vector_mul(vec_a, vec_b, "test_mul");
    
    // Verify operations produce vectors
    assert_eq!(vec_add.get_type().get_size(), 4);
    assert_eq!(vec_sub.get_type().get_size(), 4);
    assert_eq!(vec_mul.get_type().get_size(), 4);
    
    // Test reduction
    let reduce_add = simd_codegen.vector_reduce_add(vec_a, "test_reduce_add");
    let reduce_min = simd_codegen.vector_reduce_min(vec_a, "test_reduce_min");
    let reduce_max = simd_codegen.vector_reduce_max(vec_a, "test_reduce_max");
    
    // Verify reductions produce scalars
    assert!(!reduce_add.is_vector_value());
    assert!(!reduce_min.is_vector_value());
    assert!(!reduce_max.is_vector_value());
}

// Performance test placeholder
#[test]
fn test_simd_performance_characteristics() {
    // This test documents expected performance characteristics
    // Actual performance testing would require running benchmarks
    
    // Expected speedups:
    // - 4x speedup for 128-bit operations (SSE/AVX)
    // - 8x speedup for 256-bit operations (AVX2)
    // - 16x speedup for 512-bit operations (AVX-512)
    
    // Note: Actual speedup depends on:
    // 1. Memory alignment
    // 2. Data dependencies
    // 3. CPU microarchitecture
    // 4. Compiler optimizations
    
    println!("SIMD performance test placeholder");
    println!("Expected speedups:");
    println!("  - 128-bit vectors: 3-4x");
    println!("  - 256-bit vectors: 6-8x");
    println!("  - 512-bit vectors: 12-16x");
    println!("");
    println!("Note: These are theoretical maximums.");
    println!("Real-world speedups depend on many factors.");
}

// Test error cases
#[test]
fn test_simd_error_cases() {
    // Test invalid SIMD types
    assert!(SimdTypeInfo::new(Type::Bool, 4).is_none());
    assert!(SimdTypeInfo::new(Type::Str, 4).is_none());
    assert!(SimdTypeInfo::new(Type::Char, 4).is_none());
    
    // Test invalid lane counts
    assert!(SimdTypeInfo::new(Type::I32, 0).is_none());
    assert!(SimdTypeInfo::new(Type::I32, 3).is_none()); // Not power of two
    assert!(SimdTypeInfo::new(Type::I32,