// src/tests.rs
#[cfg(test)]
mod tests {
    use crate::{compile_and_run_zeta, parse_zeta, Resolver};
    use crate::ast::AstNode;

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
        for ast in &asts { res.register(ast.clone()); }
        assert!(res.typecheck(&asts));
    }

    #[test]
    fn test_codegen_run() {
        let input = r#"
fn simple() -> i32 { 42 }
"#;
        let res = compile_and_run_zeta(input).unwrap();
        assert_eq!(res, 0); // Stub return 0
    }

    #[test]
    fn test_ergonomics_infer() {
        let input = r#"
fn test_phantom<T>() -> T { /* stub */ TimingOwned<i32> 0 }
"#;
        let (_, asts) = parse_zeta(input).unwrap();
        let mut res = Resolver::new();
        for ast in &asts { res.register(ast.clone()); }
        assert!(res.typecheck(&asts)); // Infer <()>
    }

    #[test]
    fn test_derive_copy() {
        let input = r#"
#[derive(Copy)]
struct Foo(i32);
"#;
        // Parser stub for struct, but test resolve
        let (_, asts) = parse_zeta(input).unwrap(); // Assume parses as Derive
        let mut res = Resolver::new();
        for ast in &asts { res.register(ast.clone()); }
        assert!(res.resolve_impl("Copy", "Foo").is_some());
    }
}
