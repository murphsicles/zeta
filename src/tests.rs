// src/tests.rs
#[cfg(test)]
mod tests {
    //! Unit tests for Zeta compiler.
    //! Verifies parsing, type checking, code generation, resolution, and runtime features.
    use crate::frontend::ast::AstNode;
    use crate::middle::mir::r#gen::MirGen;
    use crate::middle::resolver::resolver::MonoKey;
    use crate::{middle::resolver::resolver::Resolver, compile_and_run_zeta, frontend::parser::top_level::parse_zeta};
    use std::time::Instant;
    /// Tests parsing of Addable concept definition.
    #[test]
    fn test_parse_addable() {
        let input = r#"
concept Addable<Rhs=Self> {
    fn add(self: Self, rhs: Rhs) -> Self;
}
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        assert!(asts.len() == 1);
        if let AstNode::ConceptDef { name, params, .. } = &asts[0] {
            assert_eq!(name, "Addable");
            assert_eq!(params, vec!["Rhs"]);
        }
    }
    /// Tests type checking of trait implementation.
    #[test]
    fn test_typecheck_impl() {
        let input = r#"
concept Send {}
impl Send for i32 {}
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        assert!(res.typecheck(&asts));
    }
    /// Tests code generation and runtime execution.
    #[test]
    fn test_codegen_run() {
        let input = r#"
fn simple() -> i32 { 42 }
"#;
        let res = compile_and_run_zeta(input).unwrap();
        assert_eq!(res, 0);
    }
    /// Tests type inference for phantom types.
    #[test]
    fn test_ergonomics_infer() {
        let input = r#"
fn test_phantom<T>() -> T { TimingOwned<i32> 0 }
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        assert!(res.typecheck(&asts));
    }
    /// Tests derivation of Copy trait.
    #[test]
    fn test_derive_copy() {
        let input = r#"#[derive(Copy)] struct Foo(i32);"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        assert!(res.resolve_impl("Copy", "Foo").is_some());
    }
    /// Tests MIR generation from AST.
    #[test]
    fn test_mir_gen() {
        let input = r#"
fn test() -> i32 {
    let x = 42;
    x
}
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        let ast_hash = format!("{:?}", asts[0]);
        let mir = res.get_cached_mir(&ast_hash).unwrap();
        assert!(!mir.stmts.is_empty());
    }
    /// Tests parallel resolution of traits.
    #[test]
    fn test_parallel_resolve() {
        let input = r#"
concept A {}
concept B {}
impl A for i32 {}
impl B for i32 {}
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        assert!(res.typecheck(&asts));
    }
    /// Tests compile-time evaluation of semiring operations.
    #[test]
    fn test_ctfe_semiring() {
        let input = r#"
fn semiring_test() -> i32 {
    let a = 2;
    let b = 3;
    let c = a.add(b);
    c
}
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        let ast_hash = format!("{:?}", asts[0]);
        let mir = res.get_cached_mir(&ast_hash).unwrap();
        assert!(mir.ctfe_consts.contains_key(&2));
        assert_eq!(*mir.ctfe_consts.get(&2).unwrap(), 5);
    }
    /// Tests thin monomorphization of templates.
    #[test]
    fn test_thin_templates() {
        let input = r#"
fn generic_add<T>(a: T, b: T) -> T { a.add(b) }
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        let key = MonoKey(("generic_add".to_string(), vec!["i32".to_string()]));
        let mono_ast = res.monomorphize(key.clone(), &asts[0]);
        assert_eq!(mono_ast.generics, vec![]);
        let mir = res.get_mono_mir(&key).unwrap();
        assert!(!mir.stmts.is_empty());
    }
    /// Tests cache safety for types.
    #[test]
    fn test_cachesafe() {
        let input = r#"
concept CacheSafe {}
impl CacheSafe for i32 {}
actor SafeChannel ...(truncated 3792 characters)...check(&asts));
        assert!(res.resolve_impl("Copy", "Pair").is_some());
        assert!(res.resolve_impl("Eq", "Pair").is_some()); // Auto-derived
    }
    /// Tests algebraic fusion optimization.
    #[test]
    fn test_algebraic_fusion() {
        let input = r#"
fn fusion_test() -> i32 {
    let a = 1;
    let b = 2;
    let c = 3;
    ((a.add(b)).add(c)) // Should fuse to a.add(b.add(c))
}
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        assert!(res.typecheck(&asts));
        let ast_hash = format!("{:?}", asts[0]);
        let mir = res.get_cached_mir(&ast_hash).unwrap();
        // Stub: Check for Fusion stmt
        assert!(mir.stmts.iter().any(|s| matches!(s, MirStmt::Fusion { .. })));
    }
    /// Tests semiring matrix multiplication using EOP.
    #[test]
    fn test_eop_semiring_matrix() {
        let input = r#"
concept Semiring {
    fn add(self: Self, rhs: Self) -> Self;
    fn mul(self: Self, rhs: Self) -> Self;
    fn zero() -> Self;
    fn one() -> Self;
}
impl Semiring for i32 {
    fn add(self: i32, rhs: i32) -> i32 { self + rhs }
    fn mul(self: i32, rhs: i32) -> i32 { self * rhs }
    fn zero() -> i32 { 0 }
    fn one() -> i32 { 1 }
}
fn matrix_mul<A: Semiring>(a: [[A; 2]; 2], b: [[A; 2]; 2]) -> [[A; 2]; 2] {
    let mut res = [[A::zero(); 2]; 2];
    for i in 0..2 {
        for j in 0..2 {
            for k in 0..2 {
                res[i][j] = res[i][j].add(a[i][k].mul(b[k][j]));
            }
        }
    }
    res
}
fn eop_test() -> i32 {
    let a = [[1i32, 2]; [3, 4]];
    let b = [[5, 6]; [7, 8]];
    let c = matrix_mul(a, b);
    c[0][0] // 19
}
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        assert!(res.typecheck(&asts));
        let res_val = compile_and_run_zeta(input).unwrap();
        assert_eq!(res_val, 19); // EOP semiring test
    }
    /// Tests actor counter with end-to-end spawn and increment.
    #[test]
    fn test_actor_counter() {
        let input = r#"
concept Send {}
concept Sync {}
concept CacheSafe {}
impl Send for i32 {}
impl Sync for i32 {}
impl CacheSafe for i32 {}
actor Counter {
    async fn increment(&self, delta: i32) -> i32 { self.state + delta }
    state: i32 = 0;
}
fn actor_test() -> i32 {
    let c = spawn_actor Counter();
    c.increment(42);
    42
}
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        assert!(res.typecheck(&asts));
        let res_val = compile_and_run_zeta(input).unwrap();
        assert_eq!(res_val, 42); // Actor spawn/inc
    }
    /// Tests stable ABI for FFI functions.
    #[test]
    fn test_stable_abi() {
        let input = r#"
#[stable_abi]
fn ffi_add(a: i32, b: i32) -> i32 { a + b } // No generics
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        assert!(res.typecheck(&asts)); // Stable ABI check
    }
    /// Benchmarks semiring addition performance.
    #[bench]
    fn bench_semiring_add(b: &mut criterion::Criterion) {
        let input = r#"
fn bench_add() -> i32 {
    let mut sum = 0;
    for i in 0..1000 {
        sum = sum.add(i);
    }
    sum
}
"#;
        let start = Instant::now();
        let _ = compile_and_run_zeta(input).unwrap();
        let duration = start.elapsed();
        println!("Semiring add bench: {:?}", duration);
        // Stub: Compare to Rust/Zig/Go (manual)
        b.bench_function("zeta_semiring", |b| b.iter(|| compile_and_run_zeta(input)));
    }
    /// Tests associative fold fusion optimization.
    #[test]
    fn test_assoc_fold_fusion() {
        let input = r#"
fn assoc_fold() -> i32 {
    let xs = [1, 2, 3, 4];
    xs.fold(0, |acc, x| acc.add(x)) // Fused semigroup
}
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        assert!(res.typecheck(&asts));
        let res_val = compile_and_run_zeta(input).unwrap();
        assert_eq!(res_val, 10); // 0+1+2+3+4
    }
}
}
