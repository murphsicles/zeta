// src/main.rs
use zeta::{compile_and_run_zeta, parse_zeta};

fn main() {
    let code = r#"
concept Addable<Rhs=Self> {
    fn add(self: Self, rhs: Rhs) -> Self;
}

impl Addable for i32 {
    fn add(self: i32, rhs: i32) -> i32;
}

fn use_add<T>(a: T, b: T) -> i32 where T: Addable {
    a.add(b);
}
"#;
    match compile_and_run_zeta(code) {
        Ok(res) => println!("Result: {}", res),
        Err(e) => eprintln!("Error: {}", e),
    }
}
