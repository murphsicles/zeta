// src/main.rs
use zeta::compile_and_run_zeta;

fn main() {
    let code = r#"
concept Addable<Rhs=Self> {
    fn add(self: Self, rhs: Rhs) -> Self;
}

impl Addable for i32 {
    fn add(self: i32, rhs: i32) -> i32;
}

impl Addable<i32> for Vec<i32> {
    fn add(self: Vec<i32>, rhs: i32) -> Vec<i32>;
}

fn use_vec_add(v: Vec<i32>, x: i32) -> i32 where Vec<i32>: Addable<i32> {
    let res = v.add(x);
    42  // Placeholder return
}
"#;
    match compile_and_run_zeta(code) {
        Ok(res) => println!("Result: {}", res),
        Err(e) => eprintln!("Error: {}", e),
    }
}
