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

fn use_std() -> i32 {
    let url = "https://example.com";
    let resp = std::net::http_get(url);
    let now = std::datetime::now();
    let v = Vec<i32>[1, 2];
    v.add(3);
    42
}
"#;
    match compile_and_run_zeta(code) {
        Ok(res) => println!("Result: {}", res),
        Err(e) => eprintln!("Error: {}", e),
    }
}
