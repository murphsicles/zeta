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

actor Counter {
    async fn increment(&self, delta: i32) -> i32;
}

fn use_actor() -> i32 {
    let counter = spawn_actor Counter();
    counter.increment(5);
    42
}
"#;
    match compile_and_run_zeta(code) {
        Ok(res) => println!("Result: {}", res),
        Err(e) => eprintln!("Error: {}", e),
    }
}
