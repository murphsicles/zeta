// src/main.rs
use std::env;
use std::fs;
use zeta::{compile_and_run_zeta, Plan};

fn main() {
    println!("Zeta Status: {}", Plan::status());

    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        let input_file = &args[1];
        let input = fs::read_to_string(input_file).expect("Failed to read file");
        match compile_and_run_zeta(&input) {
            Ok(res) => println!("Result: {}", res),
            Err(e) => eprintln!("Error: {}", e),
        }
    } else {
        let code = r#"
#[derive(Copy)]
fn use_vec_add<Rhs=Self>() -> i32 {
    let v: Vec<i32> = Vec<i32>[1, 2];
    v.add(3);
    42
}

concept Addable<Rhs=Self> {
    fn add(self: Self, rhs: Rhs) -> Self;
}

impl Addable for i32 {
    fn add(self: i32, rhs: i32) -> i32;
}

impl Addable<i32> for Vec<i32> {
    fn add(self: Vec<i32>, rhs: i32) -> Vec<i32>;
}

concept Send {}
concept Sync {}
concept CacheSafe {}

impl Send for i32 {}
impl Sync for i32 {}
impl CacheSafe for i32 {}

actor Counter<T=Self> {
    async fn increment(&self, delta: i32) -> i32;
}

impl Send for Counter<i32> {}
impl Sync for Counter<i32> {}
impl CacheSafe for Counter<i32> {}

fn multi_bound<U: Send + Sync>() -> U {
    TimingOwned<i32> 0
}

fn semiring_ctfe() -> i32 {
    let a = 2;
    let b = 3;
    a.add(b)
}

fn generic_add<T>(a: T, b: T) -> T {
    a.add(b)
}

fn use_std_embed() -> i64 {
    let url = "https://example.com";
    let resp = std::net::http_get(url);
    let now = std::datetime::now();
    now
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
}
