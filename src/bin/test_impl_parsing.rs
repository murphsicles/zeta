//! Test impl block parsing

use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    println!("=== Testing Impl Block Parsing ===\n");

    let test_cases = vec![
        (
            "Simple impl",
            r#"
        impl Point {
            fn new(x: i64, y: i64) -> Point {
                Point { x, y }
            }
        }
        "#,
        ),
        (
            "Impl for generic type",
            r#"
        impl Option<i64> {
            fn unwrap(self) -> i64 {
                self.value
            }
        }
        "#,
        ),
        (
            "Generic impl",
            r#"
        impl<T> Option<T> {
            fn unwrap(self) -> T {
                self.value
            }
        }
        "#,
        ),
        (
            "Impl with where clause",
            r#"
        impl<T> Option<T> where T: Clone {
            fn clone(self) -> T {
                self.value.clone()
            }
        }
        "#,
        ),
        (
            "Trait impl",
            r#"
        impl Clone for Point {
            fn clone(&self) -> Point {
                Point { x: self.x, y: self.y }
            }
        }
        "#,
        ),
    ];

    for (name, code) in test_cases {
        let result = parse_zeta(code);
        match result {
            Ok((remaining, ast)) => {
                if remaining.is_empty() {
                    println!("✅ {} - Parses completely", name);
                    println!("   AST: {:?}", ast);
                } else {
                    println!(
                        "⚠️  {} - Partial parse ({} chars remaining)",
                        name,
                        remaining.len()
                    );
                    let preview = if remaining.len() > 50 {
                        &remaining[..50]
                    } else {
                        remaining
                    };
                    println!("   Unparsed: '{}'", preview.replace("\n", "\\n"));
                }
            }
            Err(e) => {
                println!("❌ {} - Parse error: {:?}", name, e);
            }
        }
    }

    // Test what DOES parse
    println!("\n=== What Actually Parses ===");

    let working_cases = vec![
        (
            "Struct definition",
            r#"
        struct Point {
            x: i64,
            y: i64,
        }
        "#,
        ),
        (
            "Function in impl (standalone)",
            r#"
        fn unwrap(self) -> i64 {
            self.value
        }
        "#,
        ),
        (
            "Method call",
            r#"
        fn test() -> i64 {
            let p = Point { x: 1, y: 2 };
            p.x
        }
        "#,
        ),
    ];

    for (name, code) in working_cases {
        let result = parse_zeta(code);
        if result.is_ok() {
            println!("✅ {} - Parses", name);
        } else {
            println!("❌ {} - Doesn't parse", name);
        }
    }
}
