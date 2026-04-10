//! Identity generics performance benchmarks for Zeta
//!
//! Measures:
//! 1. Identity-constrained generic functions vs regular functions
//! 2. Overhead of identity capability checking
//! 3. Performance of identity conversion functions
//! 4. Memory usage for identity types

use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;
use zetac::frontend::parser::top_level::parse_zeta;

/// Test Zeta source code for identity generics benchmarking
const IDENTITY_GENERICS_SOURCE: &str = r#"
// Identity generics benchmark
// Compare identity-constrained functions vs regular functions

// Regular function (no identity constraints)
fn regular_process(data: str) -> i64 {
    // Simple processing
    return 100;
}

// Identity-constrained function with read capability
fn identity_read_only<T: Identity<Read>>(data: T) -> i64 {
    // Can read from data but not modify
    return 100;
}

// Identity-constrained function with read+write capabilities
fn identity_read_write<T: Identity<Read+Write>>(data: T) -> i64 {
    // Can both read and write to data
    return 100;
}

// Function with multiple identity constraints
fn identity_multiple<T: Identity<Read+Write+Owned>>(data: T) -> i64 {
    // Has full ownership capabilities
    return 100;
}

// Main function for benchmarking
fn main() -> i64 {
    // Test data
    let regular_str: str = "test string";
    let read_only_str = read_only_string("test string");
    let read_write_str = read_write_string("test string");
    let owned_str = owned_string("test string");
    
    // Call regular function
    let result1 = regular_process(regular_str);
    
    // Call identity-constrained functions
    let result2 = identity_read_only(read_only_str);
    let result3 = identity_read_write(read_write_str);
    let result4 = identity_multiple(owned_str);
    
    // Return combined result
    return result1 + result2 + result3 + result4;
}
"#;

/// Test Zeta source code for identity conversion benchmarking
const IDENTITY_CONVERSION_SOURCE: &str = r#"
// Identity conversion benchmark
// Measure overhead of identity conversion functions

fn main() -> i64 {
    let mut total = 0;
    
    // Benchmark identity conversions
    for i in 0..100 {
        let regular_str: str = "test string " + i.to_string();
        
        // Convert to different identity types
        let read_only = read_only_string(regular_str);
        let read_write = read_write_string(regular_str);
        let owned = owned_string(regular_str);
        
        // Use the converted values
        total = total + i;
    }
    
    return total;
}
"#;

/// Test Zeta source code for identity struct benchmarking
const IDENTITY_STRUCT_SOURCE: &str = r#"
// Identity struct benchmark
// Measure performance of identity-constrained structs

// Regular struct
struct RegularContainer {
    data: str
}

impl RegularContainer {
    fn new(data: str) -> RegularContainer {
        RegularContainer { data: data }
    }
    
    fn process(&self) -> i64 {
        return 50;
    }
}

// Identity-constrained struct
struct IdentityContainer<T: Identity<Read>> {
    data: T
}

impl<T: Identity<Read>> IdentityContainer<T> {
    fn new(data: T) -> IdentityContainer<T> {
        IdentityContainer { data: data }
    }
    
    fn process(&self) -> i64 {
        return 50;
    }
}

fn main() -> i64 {
    // Test with regular struct
    let regular_str: str = "regular data";
    let regular_container = RegularContainer::new(regular_str);
    let result1 = regular_container.process();
    
    // Test with identity-constrained struct
    let identity_str = read_only_string("identity data" as str);
    let identity_container = IdentityContainer::new(identity_str);
    let result2 = identity_container.process();
    
    return result1 + result2;
}
"#;

/// Benchmark: Identity-constrained generic functions vs regular functions
fn bench_identity_vs_regular(c: &mut Criterion) {
    let mut group = c.benchmark_group("identity_vs_regular");
    
    // Parse all sources first
    let (_, identity_asts) = parse_zeta(IDENTITY_GENERICS_SOURCE).unwrap();
    let (_, conversion_asts) = parse_zeta(IDENTITY_CONVERSION_SOURCE).unwrap();
    let (_, struct_asts) = parse_zeta(IDENTITY_STRUCT_SOURCE).unwrap();
    
    // Benchmark identity generics compilation
    group.bench_function("compile_identity_generics", |b| {
        b.iter(|| {
            let mut resolver = zetac::middle::resolver::resolver::Resolver::new();
            for ast in &identity_asts {
                resolver.register(ast.clone());
            }
            let ok = resolver.typecheck(&identity_asts);
            let _ = black_box(ok);
        });
    });
    
    // Benchmark identity conversion compilation
    group.bench_function("compile_identity_conversion", |b| {
        b.iter(|| {
            let mut resolver = zetac::middle::resolver::resolver::Resolver::new();
            for ast in &conversion_asts {
                resolver.register(ast.clone());
            }
            let ok = resolver.typecheck(&conversion_asts);
            let _ = black_box(ok);
        });
    });
    
    // Benchmark identity struct compilation
    group.bench_function("compile_identity_struct", |b| {
        b.iter(|| {
            let mut resolver = zetac::middle::resolver::resolver::Resolver::new();
            for ast in &struct_asts {
                resolver.register(ast.clone());
            }
            let ok = resolver.typecheck(&struct_asts);
            let _ = black_box(ok);
        });
    });
    
    group.finish();
}

/// Benchmark: Parse speed for identity generics code
fn bench_parse_identity(c: &mut Criterion) {
    let mut group = c.benchmark_group("parse_identity");
    
    group.bench_function("parse_identity_generics", |b| {
        b.iter(|| {
            let result = parse_zeta(IDENTITY_GENERICS_SOURCE);
            let _ = black_box(result);
        });
    });
    
    group.bench_function("parse_identity_conversion", |b| {
        b.iter(|| {
            let result = parse_zeta(IDENTITY_CONVERSION_SOURCE);
            let _ = black_box(result);
        });
    });
    
    group.bench_function("parse_identity_struct", |b| {
        b.iter(|| {
            let result = parse_zeta(IDENTITY_STRUCT_SOURCE);
            let _ = black_box(result);
        });
    });
    
    group.finish();
}

/// Benchmark: Type checking speed for identity generics
fn bench_typecheck_identity(c: &mut Criterion) {
    c.bench_function("typecheck_identity_generics", |b| {
        b.iter(|| {
            let (_, asts) = parse_zeta(IDENTITY_GENERICS_SOURCE).unwrap();
            let mut resolver = zetac::middle::resolver::resolver::Resolver::new();
            for ast in &asts {
                resolver.register(ast.clone());
            }
            let ok = resolver.typecheck(&asts);
            let _ = black_box(ok);
        });
    });
}

criterion_group!(
    benches,
    bench_identity_vs_regular,
    bench_parse_identity,
    bench_typecheck_identity
);

criterion_main!(benches);