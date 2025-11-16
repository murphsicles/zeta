#[cfg(test)]
mod tests {
    use crate::ast::AstNode;
    use crate::mir::MirGen;
    use crate::resolver::MonoKey;
    use crate::{Resolver, compile_and_run_zeta, parse_zeta};

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
}
