use zetac::compile_and_run_zeta;

fn main() {
    let code = r#"
fn main() -> i64 {
    42
}
"#;

    match compile_and_run_zeta(code) {
        Ok(result) => {
            println!("Result: {}", result);
        }
        Err(e) => {
            println!("Error: {}", e);
        }
    }
}