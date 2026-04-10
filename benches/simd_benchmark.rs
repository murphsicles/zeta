//! SIMD benchmark for Zeta
//!
//! Measures performance improvements from SIMD vectorization.

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use zeta::middle::types::simd::{SimdTypeInfo, aliases};
use zeta::middle::types::Type;

/// Benchmark scalar addition
fn bench_scalar_add(c: &mut Criterion) {
    c.bench_function("scalar_add_1000", |b| {
        b.iter(|| {
            let mut sum = 0i32;
            for i in 0..1000 {
                sum = black_box(sum.wrapping_add(i as i32));
            }
            black_box(sum);
        })
    });
}

/// Benchmark SIMD addition (conceptual - would need actual SIMD code)
fn bench_simd_add(c: &mut Criterion) {
    c.bench_function("simd_add_1000", |b| {
        b.iter(|| {
            // This is a placeholder for actual SIMD addition
            // In reality, this would use AVX-512 intrinsics
            let mut sum = [0i32; 16]; // 512-bit vector (16 x i32)
            for i in 0..(1000 / 16) {
                for j in 0..16 {
                    sum[j] = black_box(sum[j].wrapping_add((i * 16 + j) as i32));
                }
            }
            black_box(sum);
        })
    });
}

/// Benchmark SIMD type creation and validation
fn bench_simd_type_creation(c: &mut Criterion) {
    c.bench_function("simd_type_info_creation", |b| {
        b.iter(|| {
            for _ in 0..1000 {
                let info = SimdTypeInfo::new(black_box(Type::I32), black_box(8));
                black_box(info);
            }
        })
    });
    
    c.bench_function("simd_type_alias_creation", |b| {
        b.iter(|| {
            for _ in 0..1000 {
                let ty = aliases::f32x16();
                black_box(ty);
            }
        })
    });
}

/// Benchmark SIMD compatibility checks
fn bench_simd_compatibility(c: &mut Criterion) {
    use zeta::middle::types::simd::compatibility::*;
    
    let i32x8 = SimdTypeInfo::new(Type::I32, 8).unwrap();
    let f32x8 = SimdTypeInfo::new(Type::F32, 8).unwrap();
    let i32x16 = SimdTypeInfo::new(Type::I32, 16).unwrap();
    
    c.bench_function("simd_type_compatibility", |b| {
        b.iter(|| {
            for _ in 0..1000 {
                let compatible = are_compatible(black_box(&i32x8), black_box(&i32x8));
                black_box(compatible);
            }
        })
    });
    
    c.bench_function("simd_type_casting", |b| {
        b.iter(|| {
            for _ in 0..1000 {
                let can_cast = can_cast(black_box(&i32x8), black_box(&f32x8));
                black_box(can_cast);
            }
        })
    });
}

/// Benchmark SIMD memory operation analysis
fn bench_simd_memory_analysis(c: &mut Criterion) {
    use zeta::middle::types::simd::memory::*;
    
    let i32x4 = SimdTypeInfo::new(Type::I32, 4).unwrap();
    let f64x8 = SimdTypeInfo::new(Type::F64, 8).unwrap();
    
    c.bench_function("simd_memory_support", |b| {
        b.iter(|| {
            for _ in 0..1000 {
                let supports_gather = supports_gather(black_box(&f64x8));
                let supports_scatter = supports_scatter(black_box(&f64x8));
                let alignment = recommended_alignment(black_box(&i32x4));
                black_box((supports_gather, supports_scatter, alignment));
            }
        })
    });
}

/// Benchmark SIMD operation trait checks
fn bench_simd_operation_traits(c: &mut Criterion) {
    use zeta::middle::types::simd::operations::*;
    
    let i32x8 = SimdTypeInfo::new(Type::I32, 8).unwrap();
    let f32x8 = SimdTypeInfo::new(Type::F32, 8).unwrap();
    
    c.bench_function("simd_arithmetic_traits", |b| {
        b.iter(|| {
            for _ in 0..1000 {
                let supports_add = i32x8.supports_add();
                let supports_div = f32x8.supports_div();
                let supports_mul = i32x8.supports_mul();
                black_box((supports_add, supports_div, supports_mul));
            }
        })
    });
    
    c.bench_function("simd_bitwise_traits", |b| {
        b.iter(|| {
            for _ in 0..1000 {
                let supports_and = i32x8.supports_and();
                let supports_or = i32x8.supports_or();
                let supports_xor = i32x8.supports_xor();
                black_box((supports_and, supports_or, supports_xor));
            }
        })
    });
}

/// Benchmark SIMD mask creation
fn bench_simd_mask(c: &mut Criterion) {
    use zeta::middle::types::simd::SimdMask;
    
    c.bench_function("simd_mask_creation", |b| {
        b.iter(|| {
            for i in 1..=64 {
                let mask = SimdMask::new(black_box(i));
                black_box(mask);
            }
        })
    });
    
    c.bench_function("simd_mask_for_type", |b| {
        let i32x8 = SimdTypeInfo::new(Type::I32, 8).unwrap();
        let f32x16 = SimdTypeInfo::new(Type::F32, 16).unwrap();
        
        b.iter(|| {
            for _ in 0..1000 {
                let mask1 = SimdMask::for_simd_type(black_box(&i32x8));
                let mask2 = SimdMask::for_simd_type(black_box(&f32x16));
                black_box((mask1, mask2));
            }
        })
    });
}

/// Benchmark fallback compatibility
fn bench_simd_fallback(c: &mut Criterion) {
    use zeta::middle::types::simd::fallback;
    
    c.bench_function("simd_fallback_type", |b| {
        b.iter(|| {
            for _ in 0..1000 {
                let compatible_i32 = fallback::create_compatible_type(
                    black_box(Type::I32),
                    black_box(16)
                );
                let compatible_f64 = fallback::create_compatible_type(
                    black_box(Type::F64),
                    black_box(8)
                );
                black_box((compatible_i32, compatible_f64));
            }
        })
    });
}

/// Benchmark SIMD type conversion
fn bench_simd_type_conversion(c: &mut Criterion) {
    let i32x8_type = aliases::i32x8();
    let f32x16_type = aliases::f32x16();
    
    c.bench_function("simd_type_to_info", |b| {
        b.iter(|| {
            for _ in 0..1000 {
                let info1 = SimdTypeInfo::from_type(black_box(&i32x8_type));
                let info2 = SimdTypeInfo::from_type(black_box(&f32x16_type));
                black_box((info1, info2));
            }
        })
    });
    
    c.bench_function("simd_info_to_type", |b| {
        let i32x8_info = SimdTypeInfo::new(Type::I32, 8).unwrap();
        let f32x16_info = SimdTypeInfo::new(Type::F32, 16).unwrap();
        
        b.iter(|| {
            for _ in 0..1000 {
                let type1 = i32x8_info.to_type();
                let type2 = f32x16_info.to_type();
                black_box((type1, type2));
            }
        })
    });
}

// Create benchmark groups
criterion_group!(
    name = simd_benches;
    config = Criterion::default()
        .sample_size(100)
        .warm_up_time(std::time::Duration::from_secs(1))
        .measurement_time(std::time::Duration::from_secs(3));
    targets = 
        bench_scalar_add,
        bench_simd_add,
        bench_simd_type_creation,
        bench_simd_compatibility,
        bench_simd_memory_analysis,
        bench_simd_operation_traits,
        bench_simd_mask,
        bench_simd_fallback,
        bench_simd_type_conversion
);

criterion_main!(simd_benches);