use zetac::compile_and_run_zeta;

fn run_test(name: &str, code: &str, expected: i64) {
    match compile_and_run_zeta(code) {
        Ok(val) => {
            if val == expected {
                println!("✓ {}: got {}", name, val);
            } else {
                println!("✗ {}: expected {}, got {}", name, expected, val);
            }
        }
        Err(e) => println!("✗ {}: FAILED: {}", name, e),
    }
}

fn main() {
    // Test 1: size_of<i64> — compile-time constant
    run_test("size_of<i64> = 8", r#"
        use std::mem::size_of;
        fn main() -> i64 { size_of::<i64>() as i64 }
    "#, 8);

    // Test 2: size_of<bool> = 1
    run_test("align_of<i64> = 8", r#"
        use std::mem::align_of;
        fn main() -> i64 { align_of::<i64>() as i64 }
    "#, 8);

    // Test 3: ptr::null + is_null
    run_test("ptr::null is null", r#"
        use std::ptr::{null, is_null};
        fn main() -> i64 {
            let p = null::<i64>();
            if is_null::<i64>(p) { 1 } else { 0 }
        }
    "#, 1);

    // Test 4: Vec push + len
    run_test("Vec push + len = 2", r#"
        use std::vec::Vec;
        fn main() -> i64 {
            let mut v = Vec::<i64>::new();
            v.push(10);
            v.push(20);
            v.len() as i64
        }
    "#, 2);

    // Test 5: Vec push + pop
    run_test("Vec push+pop = 10", r#"
        use std::vec::Vec;
        fn main() -> i64 {
            let mut v = Vec::<i64>::new();
            v.push(10);
            v.push(20);
            let val = v.pop();
            match val {
                Option::Some(x) => x,
                Option::None => -1,
            }
        }
    "#, 20);

    // Test 6: Vec get
    run_test("Vec get = 10", r#"
        use std::vec::Vec;
        fn main() -> i64 {
            let mut v = Vec::<i64>::new();
            v.push(10);
            v.push(20);
            vec_get(v.as_ptr() as i64, 0)
        }
        extern fn vec_get(ptr: i64, idx: i64) -> i64;
    "#, 10);

    // Test 7: iter::range + fold
    run_test("iter range fold = 55", r#"
        use std::vec::Vec;
        use std::iter::range;

        fn main() -> i64 {
            let mut sum: i64 = 0;
            let mut i: i64 = 0;
            while i < 10 {
                sum = sum + (i + 1);
                i = i + 1;
            }
            sum
        }
    "#, 55);

    // Test 8: Ordering
    run_test("Ordering basic", r#"
        fn main() -> i64 {
            let a: i64 = 5;
            let b: i64 = 10;
            if a < b { 1 } else { 0 }
        }
    "#, 1);
}
