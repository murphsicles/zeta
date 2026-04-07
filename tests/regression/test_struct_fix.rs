use zetac::compile_and_run_zeta;

fn main() {
    let code = r#"
    struct Point {
        first: i64,
        second: i64,
    }

    fn main() -> i64 {
        let p = Point { first: 10, second: 20 };
        p.first + p.second
    }
    "#;

    let result = compile_and_run_zeta(code);
    match result {
        Ok(value) => {
            println!("Result: {}", value);
            if value == 30 {
                println!("✓ Test passed! Field access works correctly.");
            } else {
                println!("✗ Test failed! Expected 30, got {}", value);
            }
        }
        Err(e) => {
            println!("✗ Compilation failed: {}", e);
        }
    }
}