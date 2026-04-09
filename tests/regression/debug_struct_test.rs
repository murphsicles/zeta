// Simple test to debug struct field access
fn main() {
    let code = r#"
    struct Container<T> {
        value: T
    }
    
    fn main() -> i64 {
        let c = Container { value: 100 };
        c.value
    }
    "#;

    let result = zetac::compile_and_run_zeta(code);
    println!("Result: {:?}", result);

    // Also test a simpler case
    let simple_code = r#"
    struct Simple {
        x: i64
    }
    
    fn main() -> i64 {
        let s = Simple { x: 42 };
        s.x
    }
    "#;

    let simple_result = zetac::compile_and_run_zeta(simple_code);
    println!("Simple result: {:?}", simple_result);
}
