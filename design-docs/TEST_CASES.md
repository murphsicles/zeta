# Test Cases for Generic Type System

## Category 1: Basic Generic Types

### Test 1.1: Vec<T>
```rust
fn main() {
    // Basic instantiation
    let v1: Vec<i32> = Vec::new();
    let v2: Vec<bool> = Vec::new();
    let v3: Vec<String> = Vec::new();
    
    // With initial values
    let v4 = vec![1, 2, 3];  // Should infer Vec<i32>
    let v5 = vec![true, false];  // Should infer Vec<bool>
}
```

### Test 1.2: Option<T>
```rust
fn main() {
    let o1: Option<i32> = Some(42);
    let o2: Option<bool> = None;
    let o3 = Some("hello");  // Should infer Option<&str>
    
    // Pattern matching
    match o1 {
        Some(x) => println!("Got {}", x),
        None => println!("Got nothing"),
    }
}
```

### Test 1.3: Result<T, E>
```rust
fn main() {
    let r1: Result<i32, String> = Ok(42);
    let r2: Result<(), &str> = Err("something went wrong");
    
    // Using question mark operator
    fn might_fail() -> Result<i32, String> {
        Ok(42)
    }
    
    let x = might_fail()?;  // Should propagate error
}
```

## Category 2: Generic Functions

### Test 2.1: Identity Function
```rust
fn identity<T>(x: T) -> T {
    x
}

fn main() {
    let a: i32 = identity(42);
    let b: &str = identity("hello");
    let c: bool = identity(true);
    
    // Type should be preserved
    let d = identity(3.14);  // Should be f64
}
```

### Test 2.2: Swap Function
```rust
fn swap<T>(a: T, b: T) -> (T, T) {
    (b, a)
}

fn main() {
    let (x, y) = swap(1, 2);  // (i32, i32)
    let (a, b) = swap("hello", "world");  // (&str, &str)
}
```

### Test 2.3: Generic with Multiple Type Parameters
```rust
fn pair<T, U>(first: T, second: U) -> (T, U) {
    (first, second)
}

fn main() {
    let p1 = pair(42, "hello");  // (i32, &str)
    let p2 = pair(true, 3.14);   // (bool, f64)
}
```

## Category 3: Trait Bounds

### Test 3.1: Clone Bound
```rust
fn duplicate<T: Clone>(x: T) -> (T, T) {
    (x.clone(), x)
}

fn main() {
    // i32 implements Clone
    let (a, b) = duplicate(42);
    
    // String implements Clone
    let s = String::from("hello");
    let (s1, s2) = duplicate(s);
    
    // &str also implements Clone (via Copy)
    let (t1, t2) = duplicate("world");
}
```

### Test 3.2: Copy Bound
```rust
fn copy_twice<T: Copy>(x: T) -> (T, T) {
    (x, x)  // Can copy because T: Copy
}

fn main() {
    // i32 is Copy
    let (a, b) = copy_twice(42);
    
    // bool is Copy
    let (c, d) = copy_twice(true);
    
    // &str is Copy (shared reference)
    let (e, f) = copy_twice("hello");
    
    // String is NOT Copy - should fail
    // let s = String::from("hello");
    // let (s1, s2) = copy_twice(s);  // ERROR
}
```

### Test 3.3: Multiple Bounds
```rust
fn clone_and_debug<T: Clone + std::fmt::Debug>(x: T) -> String {
    let cloned = x.clone();
    format!("{:?}", cloned)
}

fn main() {
    // i32 satisfies both bounds
    let s1 = clone_and_debug(42);
    
    // String satisfies both bounds
    let s2 = clone_and_debug(String::from("test"));
}
```

### Test 3.4: Where Clauses
```rust
fn complex_function<T, U>(x: T, y: U) -> T
where
    T: Clone + std::fmt::Debug,
    U: std::fmt::Display,
{
    println!("y = {}", y);
    x.clone()
}

fn main() {
    let result = complex_function(42, "hello");
}
```

## Category 4: Generic Structs and Enums

### Test 4.1: Generic Struct
```rust
struct Point<T> {
    x: T,
    y: T,
}

impl<T> Point<T> {
    fn new(x: T, y: T) -> Self {
        Point { x, y }
    }
    
    fn x(&self) -> &T {
        &self.x
    }
}

fn main() {
    let p1 = Point::new(10, 20);      // Point<i32>
    let p2 = Point::new(1.5, 2.5);    // Point<f64>
    let p3 = Point::new("a", "b");    // Point<&str>
}
```

### Test 4.2: Generic Enum
```rust
enum Result<T, E> {
    Ok(T),
    Err(E),
}

enum Option<T> {
    Some(T),
    None,
}

fn main() {
    let success: Result<i32, String> = Result::Ok(42);
    let failure: Result<(), &str> = Result::Err("failed");
    
    let some_value: Option<String> = Option::Some(String::from("hello"));
    let no_value: Option<i32> = Option::None;
}
```

### Test 4.3: Generic with Lifetime
```rust
struct Slice<'a, T> {
    data: &'a [T],
}

impl<'a, T> Slice<'a, T> {
    fn new(data: &'a [T]) -> Self {
        Slice { data }
    }
    
    fn first(&self) -> Option<&T> {
        self.data.first()
    }
}

fn main() {
    let arr = [1, 2, 3, 4, 5];
    let slice = Slice::new(&arr);
    let first = slice.first();  // Should be Option<&i32>
}
```

## Category 5: Reference Types with Generics

### Test 5.1: Generic References
```rust
fn largest<T: PartialOrd>(a: &T, b: &T) -> &T {
    if a > b { a } else { b }
}

fn main() {
    let x = 10;
    let y = 20;
    let max = largest(&x, &y);  // &i32
    
    let s1 = "hello";
    let s2 = "world";
    let longest = largest(&s1, &s2);  // &&str
}
```

### Test 5.2: Mutable Generic References
```rust
fn swap_values<T>(a: &mut T, b: &mut T) {
    std::mem::swap(a, b);
}

fn main() {
    let mut x = 10;
    let mut y = 20;
    swap_values(&mut x, &mut y);
    
    let mut s1 = String::from("hello");
    let mut s2 = String::from("world");
    swap_values(&mut s1, &mut s2);
}
```

### Test 5.3: Reference to Generic Type
```rust
fn process_slice<T>(slice: &[T]) -> usize {
    slice.len()
}

fn main() {
    let nums = [1, 2, 3];
    let len1 = process_slice(&nums);  // &[i32]
    
    let strings = ["a", "b", "c"];
    let len2 = process_slice(&strings);  // &[&str]
}
```

## Category 6: Nested Generics

### Test 6.1: Vec of Vec
```rust
fn main() {
    let matrix: Vec<Vec<i32>> = vec![
        vec![1, 2, 3],
        vec![4, 5, 6],
        vec![7, 8, 9],
    ];
    
    // Access nested element
    let element = matrix[1][2];  // Should be i32
}
```

### Test 6.2: Option of Result
```rust
fn might_return_error() -> Option<Result<i32, String>> {
    Some(Ok(42))
}

fn main() {
    let result = might_return_error();
    
    match result {
        Some(Ok(value)) => println!("Got {}", value),
        Some(Err(e)) => println!("Error: {}", e),
        None => println!("No result"),
    }
}
```

### Test 6.3: Complex Nested Type
```rust
use std::collections::HashMap;

fn main() {
    // HashMap with String keys and Vec<i32> values
    let mut map: HashMap<String, Vec<i32>> = HashMap::new();
    
    map.insert("even".to_string(), vec![2, 4, 6]);
    map.insert("odd".to_string(), vec![1, 3, 5]);
    
    // Get and modify a vector
    if let Some(numbers) = map.get_mut("even") {
        numbers.push(8);
    }
}
```

## Category 7: Error Cases (Should Fail)

### Test 7.1: Type Mismatch
```rust
fn main() {
    let v: Vec<i32> = Vec::<bool>::new();  // ERROR: expected Vec<i32>, found Vec<bool>
}
```

### Test 7.2: Missing Trait Bound
```rust
fn needs_clone<T>(x: T) -> T {
    x.clone()  // ERROR: T doesn't implement Clone
}

fn main() {
    needs_clone(42);
}
```

### Test 7.3: Wrong Number of Type Arguments
```rust
fn main() {
    let r: Result<i32> = Ok(42);  // ERROR: Result expects 2 type parameters
}
```

### Test 7.4: Move after Partial Move
```rust
struct Container<T> {
    value: T,
}

fn main() {
    let c = Container { value: String::from("hello") };
    let s = c.value;  // Moves value out of c
    // let x = c;  // ERROR: c partially moved
}
```

### Test 7.5: Lifetime Mismatch
```rust
fn longest<'a>(x: &'a str, y: &str) -> &'a str {
    x
}

fn main() {
    let s1 = String::from("short");
    let result;
    {
        let s2 = String::from("longer");
        result = longest(&s1, &s2);  // ERROR: s2 doesn't live long enough
    }
    println!("{}", result);
}
```

## Category 8: Advanced Features

### Test 8.1: Associated Types
```rust
trait Iterator {
    type Item;
    
    fn next(&mut self) -> Option<Self::Item>;
}

struct Range {
    current: i32,
    end: i32,
}

impl Iterator for Range {
    type Item = i32;
    
    fn next(&mut self) -> Option<Self::Item> {
        if self.current < self.end {
            let result = self.current;
            self.current += 1;
            Some(result)
        } else {
            None
        }
    }
}

fn main() {
    let mut range = Range { current: 0, end: 3 };
    while let Some(n) = range.next() {
        println!("{}", n);
    }
}
```

### Test 8.2: Generic Traits
```rust
trait Convert<T> {
    fn convert(self) -> T;
}

impl Convert<i32> for f64 {
    fn convert(self) -> i32 {
        self as i32
    }
}

impl Convert<f64> for i32 {
    fn convert(self) -> f64 {
        self as f64
    }
}

fn main() {
    let x: i32 = 3.14.convert();  // f64 -> i32
    let y: f64 = 42.convert();    // i32 -> f64
}
```

### Test 8.3: Const Generics
```rust
struct Array<T, const N: usize> {
    data: [T; N],
}

impl<T, const N: usize> Array<T, N> {
    fn new() -> Self where T: Default {
        Array {
            data: [(); N].map(|_| T::default()),
        }
    }
    
    fn len(&self) -> usize {
        N
    }
}

fn main() {
    let arr: Array<i32, 5> = Array::new();
    println!("Length: {}", arr.len());  // Should print 5
}
```

## Category 9: Integration Tests

### Test 9.1: Full Program with Generics
```rust
use std::fmt::Debug;

#[derive(Debug, Clone)]
struct Point<T> {
    x: T,
    y: T,
}

impl<T> Point<T> {
    fn new(x: T, y: T) -> Self {
        Point { x, y }
    }
}

fn print_points<T: Debug + Clone>(points: &[Point<T>]) {
    for point in points {
        println!("Point: {:?}", point);
    }
}

fn duplicate_point<T: Clone>(point: &Point<T>) -> Point<T> {
    point.clone()
}

fn main() {
    let points = vec![
        Point::new(1, 2),
        Point::new(3, 4),
        Point::new(5, 6),
    ];
    
    print_points(&points);
    
    let p = Point::new(10, 20);
    let p2 = duplicate_point(&p);
    
    println!("Original: {:?}", p);
    println!("Duplicate: {:?}", p2);
}
```

### Test 9.2: Generic Algorithm
```rust
fn binary_search<T: Ord>(arr: &[T], target: &T) -> Option<usize> {
    let mut left = 0;
    let mut right = arr.len();
    
    while left < right {
        let mid = left + (right - left) / 2;
        
        match arr[mid].cmp(target) {
            std::cmp::Ordering::Equal => return Some(mid),
            std::cmp::Ordering::Less => left = mid + 1,
            std::cmp::Ordering::Greater => right = mid,
        }
    }
    
    None
}

fn main() {
    let numbers = [1, 3, 5, 7, 9, 11];
    let target = 7;
    
    if let Some(index) = binary_search(&numbers, &target) {
        println!("Found {} at index {}", target, index);
    } else {
        println!("{} not found", target);
    }
    
    let strings = ["apple", "banana", "cherry", "date"];
    let target_str = "cherry";
    
    if let Some(index) = binary_search(&strings, &target_str) {
        println!("Found {} at index {}", target_str, index);
    }
}
```

## Test Execution Plan

### Phase 1: Basic Tests (20:00-20:30)
- Run tests 1.1 through 1.3 (basic generic types)
- Run tests 2.1 through 2.3 (generic functions)
- Fix any issues found

### Phase 2: Trait Bounds (20:30-21:00)
- Run tests 3.1 through 3.4 (trait bounds)
- Run tests 4.1 through 4.3 (generic structs/enums)
- Fix bound checking issues

### Phase 3: Advanced Features (21:00-21:30)
- Run tests 5.1 through 5.3 (reference types)
- Run tests 6.1 through 6.3 (nested generics)
- Run tests 7.1 through 7.5 (error cases - should fail)

### Phase 4: Integration (21:30-21:45)
- Run tests 8.1 through 8.3 (advanced features)
- Run tests 9.1 and 9.2 (integration tests)
- Final verification

### Success Criteria
- All tests in categories 1-6 should pass
- All tests in category 7 should fail with appropriate error messages
- Tests in categories 8-9 should pass or provide clear error messages if not implemented

## Notes for Test Implementation

1. **Parser Compatibility**: Ensure test code matches Zeta v0.5.0 syntax
2. **Error Messages**: Tests should check for specific error messages when failure is expected
3. **Type Inference**: Tests should verify type inference works correctly
4. **Performance**: Large generic types should not cause exponential blowup
5. **Memory**: No memory leaks in generic type handling