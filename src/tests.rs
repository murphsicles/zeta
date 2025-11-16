#[cfg(test)]
mod tests {
    use crate::ast::AstNode;
    use crate::mir::MirGen;
    use crate::resolver::MonoKey;
    use crate::{Resolver, compile_and_run_zeta, parse_zeta};
    use std::time::Instant;

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

    #[test]
    fn test_codegen_run() {
        let input = r#"
fn simple() -> i32 { 42 }
"#;
        let res = compile_and_run_zeta(input).unwrap();
        assert_eq!(res, 0);
    }

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

    #[test]
    fn test_cachesafe() {
        let input = r#"
concept CacheSafe {}
impl CacheSafe for i32 {}
actor SafeChannel {
    async fn send(&self, msg: i32) -> i32;
}
impl CacheSafe for SafeChannel {}
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        assert!(res.typecheck(&asts)); // Validates CacheSafe
    }

    #[test]
    fn test_affine_borrow() {
        let input = r#"
fn affine_test() -> i32 {
    let x = 42;
    let y = x; // Move: affine consume
    y // Use post-move: should fail
}
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        assert!(!res.typecheck(&asts)); // Affine violation
    }

    #[test]
    fn test_affine_ok() {
        let input = r#"
fn affine_ok() -> i32 {
    let x = 42;
    let y = x; // Move
    // No use of x after
    y
}
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        assert!(res.typecheck(&asts)); // Affine ok
    }

    #[test]
    fn test_speculative_leak() {
        let input = r#"
fn spec_leak() -> i32 {
    let secret = 42;
    if secret > 0 { // Spec branch
        secret // Leak: spec access
    } else { 0 }
}
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        assert!(!res.typecheck(&asts)); // Speculative leak
    }

    #[test]
    fn test_speculative_safe() {
        let input = r#"
fn spec_safe() -> i32 {
    let secret = TimingOwned<i32> 42; // Erase spec
    if secret > 0 { // Spec branch
        1
    } else { 0 }
}
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        assert!(res.typecheck(&asts)); // Spec safe with TimingOwned
    }

    #[test]
    fn test_structural_copy() {
        let input = r#"
struct Point { x: i32, y: i32 }
concept Copy {}
// No explicit impl
fn use_copy(p: Point) { p } // Should infer structural Copy
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        assert!(res.typecheck(&asts)); // Structural match
        assert!(res.resolve_impl("Copy", "Point").is_some()); // Hybrid resolution
    }

    #[test]
    fn test_nominal_eq() {
        let input = r#"
concept Eq {}
impl Eq for i32 {} // Nominal
fn eq_test(a: i32, b: i32) { a.eq(b) }
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        assert!(res.typecheck(&asts)); // Nominal impl
    }

    #[test]
    fn test_auto_derive_copy_eq() {
        let input = r#"
struct Pair { a: i32, b: i32 }
#[derive(Copy)] // Triggers auto Eq if fields Eq
fn use_pair(p: Pair) { p }
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts {
            res.register(ast.clone());
        }
        assert!(res.typecheck(&asts));
        assert!(res.resolve_impl("Copy", "Pair").is_some());
        assert!(res.resolve_impl("Eq", "Pair").is_some()); // Auto-derived
    }

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

    // EOP Algos: Semiring matrix multiply
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

    // Actor e2e: Counter increment
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

    // Stable ABI FFI test
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

    // Perf benchmark stub: Semiring vs baseline
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

    // EOP assoc fold fusion
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
