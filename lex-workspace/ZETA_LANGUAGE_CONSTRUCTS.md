# Zeta Language Constructs Reference
## By LEX - Zeta's Code Guru

## Introduction
This document provides a comprehensive reference of all Zeta language constructs with examples from v0.3.7 through v0.5.0. As Zeta's Code Guru, I've analyzed every construct to provide authoritative guidance on proper usage.

## 1. Basic Syntax Elements

### 1.1 Comments
```zeta
// Line comment - single line
/* Block comment - can span
   multiple lines */
/// Documentation comment for items
//! Module-level documentation
```

### 1.2 Identifiers
```zeta
// Valid identifiers
variable_name
_function
CONSTANT_VALUE
TypeName
mod123ule

// Invalid identifiers
123start    // Cannot start with digit
my-name     // Hyphens not allowed
type        // Reserved keyword
```

### 1.3 Keywords (v0.5.0 Complete Set)
```zeta
// v0.4.1 Core Keywords
fn concept impl for if else let mut return use struct enum type
unsafe loop break continue defer spawn

// v0.5.0 Additions
match const async await pub mod trait where
self Self dyn crate super

// Contextual Keywords
as async await break const continue crate dyn
else enum extern false fn for if impl in
let loop match mod move mut pub ref return
self Self static struct super trait true type
unsafe use where while yield
```

## 2. Type System

### 2.1 Primitive Types
```zeta
// Signed integers
i8 i16 i32 i64 i128 isize

// Unsigned integers  
u8 u16 u32 u64 u128 usize

// Floating point
f32 f64

// Other primitives
bool char str

// Special types
!          // Never type (diverging)
()         // Unit type
```

### 2.2 Compound Types
```zeta
// Arrays
[T]          // Slice
[T; N]       // Array with size
[mut T]      // Mutable slice

// Tuples
(T1, T2, T3) // Tuple
(T,)         // Single-element tuple

// References
&T           // Shared reference
&mut T       // Mutable reference

// Function pointers
fn(i32) -> i32
```

### 2.3 Generic Types (Syntax Evolution)

**v0.3.7 Syntax (Angle Brackets):**
```zeta
Result<i64>
HashMap<String, i64>
Vec<Box<dyn Error>>
```

**v0.5.0 Syntax (Parentheses):**
```zeta
lt(Result, i64)
lt(HashMap, String), i64)
lt(Vec, lt(Box, dyn Error))
```

**Translated Form (Type Aliases):**
```zeta
type lt_Result_i64 = i64;
type lt_HashMap_String_i64 = i64;
type lt_Vec_lt_Box_dyn_Error = i64;
```

## 3. Item Definitions

### 3.1 Functions
```zeta
// Basic function
fn add(a: i64, b: i64) -> i64 {
    a + b
}

// Generic function (v0.5.0 syntax)
fn identity<T>(value: T) -> T {
    value
}

// Const function
const fn square(x: i32) -> i32 {
    x * x
}

// Async function  
async fn fetch_data(url: &str) -> lt(Result, String) {
    // Async implementation
}

// Method
impl Point {
    fn distance(&self) -> f64 {
        (self.x * self.x + self.y * self.y) as f64
    }
}

// Single-line function (v0.5.0)
fn double(x: i64) -> i64 = x * 2
```

### 3.2 Concepts (Traits)
```zeta
// Basic concept
concept Display {
    fn fmt(&self, f: &mut Formatter) -> lt(Result, ());
}

// Generic concept
concept Add<Rhs = Self> {
    type Output;
    fn add(self, rhs: Rhs) -> Self::Output;
}

// Associated types
concept Iterator {
    type Item;
    fn next(&mut self) -> lt(Option, Self::Item);
}

// Default methods
concept Default {
    fn default() -> Self {
        // Default implementation
    }
}

// Supertraits
concept Clone: Sized {
    fn clone(&self) -> Self;
}
```

### 3.3 Implementations
```zeta
// Basic implementation
impl Display for Point {
    fn fmt(&self, f: &mut Formatter) -> lt(Result, ()) {
        write!(f, "({}, {})", self.x, self.y)
    }
}

// Generic implementation
impl<T> Display for lt(Vec, T)
where
    T: Display,
{
    fn fmt(&self, f: &mut Formatter) -> lt(Result, ()) {
        // Implementation
    }
}

// Blanket implementation
impl<T> Clone for T
where
    T: Copy,
{
    fn clone(&self) -> Self {
        *self
    }
}
```

### 3.4 Structs
```zeta
// Named-field struct
struct Point {
    x: i64,
    y: i64,
}

// Tuple struct
struct Color(i32, i32, i32);

// Unit struct
struct Marker;

// Generic struct
struct Pair<T, U> {
    first: T,
    second: U,
}

// Struct with lifetime
struct Ref<'a, T> {
    data: &'a T,
}
```

### 3.5 Enums
```zeta
// Simple enum
enum Direction {
    North,
    South,
    East,
    West,
}

// Enum with data
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}

// Generic enum
enum Option<T> {
    Some(T),
    None,
}
```

### 3.6 Type Aliases
```zeta
// Simple alias
type Meters = f64;

// Generic alias
type Result<T> = lt(std::result::Result, T), Error>;

// Function pointer alias
type Callback = fn(i32) -> i32;
```

## 4. Expressions

### 4.1 Literal Expressions
```zeta
// Integer literals
42           // i64
42i32        // i32
0xFF         // Hex
0o77         // Octal
0b1010       // Binary
1_000_000    // With separators

// Floating point
3.14         // f64
3.14f32      // f32
1.0e-10      // Scientific

// String literals
"hello"                 // &str
"multi\nline"          // Escape sequences
r#"raw "string"#"      // Raw string
b"bytes"               // Byte string

// Character literals
'a'                    // char
'\n'                   // Escape
'\u{1F600}'           // Unicode

// Boolean literals
true false

// Array literals
[1, 2, 3]              // Array literal
[]                     // Empty array

// Tuple literals
(1, "hello", true)     // Tuple
()                     // Unit
```

### 4.2 Operator Expressions
```zeta
// Arithmetic
a + b    a - b    a * b    a / b    a % b
-a       +a

// Bitwise
a & b    a | b    a ^ b    !a
a << n   a >> n

// Comparison
a == b   a != b   a < b    a > b    a <= b   a >= b

// Logical
a && b   a || b   !a

// Assignment
a = b    a += b   a -= b   a *= b   a /= b   a %= b
a &= b   a |= b   a ^= b   a <<= b  a >>= b

// Other operators
a as T            // Type cast
a.b               // Field access
a.method()        // Method call
a..b              // Range
a..=b             // Inclusive range
?                 // Error propagation (v0.5.0)
```

### 4.3 Control Flow Expressions

**If Expressions:**
```zeta
let result = if condition {
    value1
} else {
    value2
};
```

**Match Expressions (v0.5.0):**
```zeta
let result = match value {
    Pattern1 => expression1,
    Pattern2 if guard => expression2,
    _ => default_expression,
};
```

**Loop Expressions:**
```zeta
let result = loop {
    if condition { break value; }
};

let mut i = 0;
while i < 10 {
    i += 1;
}

for item in collection {
    process(item);
}
```

### 4.4 Other Expressions
```zeta
// Block expression
let x = {
    let y = compute();
    y * 2
};

// Return expression
return value;

// Await expression
let result = future.await;

// Try expression
let result = fallible_function()?;

// Array expression
let arr = [1, 2, 3, 4, 5];

// Tuple expression
let tuple = (1, "hello", true);

// Struct expression
let point = Point { x: 10, y: 20 };

// Enum variant
let message = Message::Write("hello".to_string());
```

## 5. Statements

### 5.1 Declaration Statements
```zeta
// Variable declaration
let x = 42;
let mut y = 0;

// Type annotation
let x: i32 = 42;
let mut y: &str = "hello";

// Pattern destructuring
let (a, b) = (1, 2);
let Point { x, y } = point;

// Ignore pattern
let _ = compute();

// Constant declaration
const MAX_SIZE: usize = 1024;

// Static declaration
static COUNTER: AtomicUsize = AtomicUsize::new(0);
```

### 5.2 Control Flow Statements
```zeta
// If statement
if condition {
    // then block
} else if other_condition {
    // else if block
} else {
    // else block
}

// Match statement
match value {
    Pattern1 => {
        // block 1
    }
    Pattern2 => {
        // block 2
    }
    _ => {
        // default block
    }
}

// Loop statements
loop {
    // infinite loop
    if condition { break; }
}

while condition {
    // while loop
}

for item in collection {
    // for loop
}

// Control flow
break;
continue;
return;
return value;
```

## 6. Patterns (v0.5.0)

### 6.1 Basic Patterns
```zeta
// Literal patterns
match value {
    0 => "zero",
    1 | 2 => "one or two",
    3..=5 => "three to five",
    _ => "other",
}

// Identifier patterns
match value {
    x => println("got {}", x),
    mut y => y = process(y),
    ref r => println("reference {}", r),
}

// Wildcard pattern
match value {
    _ => "anything",
    Some(_) => "some value",
}
```

### 6.2 Destructuring Patterns
```zeta
// Struct patterns
match point {
    Point { x: 0, y } => "on y-axis at {}", y,
    Point { x, y: 0 } => "on x-axis at {}", x,
    Point { x, y } => "at ({}, {})", x, y,
}

// Tuple patterns
match pair {
    (0, y) => "zero and {}", y,
    (x, 0) => "{} and zero", x,
    (x, y) => "{} and {}", x, y,
}

// Enum patterns
match message {
    Message::Quit => quit(),
    Message::Move { x, y } => move_to(x, y),
    Message::Write(text) => println("{}", text),
    Message::ChangeColor(r, g, b) => set_color(r, g, b),
}
```

### 6.3 Advanced Patterns
```zeta
// Rest patterns
match slice {
    [] => "empty",
    [first, ..] => "starts with {}", first,
    [.., last] => "ends with {}", last,
    [first, .., last] => "first {}, last {}", first, last,
}

// Reference patterns
match reference {
    &value => println("deref {}", value),
    ref mut r => *r = new_value,
}

// Pattern guards
match value {
    Some(x) if x > 0 => "positive",
    Some(x) if x < 0 => "negative",
    Some(0) => "zero",
    None => "none",
}

// @ patterns
match value {
    x @ 1..=10 => "between 1 and 10: {}", x,
    x @ 11..=20 => "between 11 and 20: {}", x,
    _ => "other",
}
```

## 7. Modules and Imports

### 7.1 Module Definitions
```zeta
// Module declaration
mod collections;

// Inline module
mod math {
    pub fn add(a: i64, b: i64) -> i64 {
        a + b
    }
}

// Module hierarchy
mod outer {
    pub mod inner {
        pub fn function() {}
    }
}
```

### 7.2 Import Statements
```zeta
// Simple import
use std::collections::HashMap;

// Multiple imports
use std::{collections::HashMap, fs::File};

// Renaming
use std::io::Result as IoResult;

// Glob import
use std::collections::*;

// Self import
use self::submodule::function;

// Super import
use super::parent_function;
```

## 8. Attributes

### 8.1 Common Attributes
```zeta
// Function attributes
#[inline]
fn fast_function() {}

#[cold]
fn rarely_called() {}

#[test]
fn test_function() {
    assert_eq!(2 + 2, 4);
}

// Struct/Enum attributes
#[derive(Debug, Clone, Copy)]
struct Point {
    x: i32,
    y: i32,
}

#[repr(C)]
struct CStruct {
    field: i32,
}

// Module attributes
#![allow(unused_variables)]
#![feature(const_generics)]

#[cfg(target_os = "linux")]
mod linux_specific;
```

## 9. Built-in Macros and Intrinsics

### 9.1 Print Macros
```zeta
println!("Hello, {}!", name);
print!("No newline");
eprintln!("Error: {}", error);
format!("Formatted: {}", value);
```

### 9.2 Assertion Macros
```zeta
assert!(condition);
assert_eq!(left, right);
assert_ne!(left, right);
debug_assert!(condition);
unreachable!();
unimplemented!();
```

### 9.3 v0.5.0 Special Features
```zeta
// SIMD types
type f32x4 = __m128;
type i32x8 = __m256i;

// SIMD operations
fn simd_add(a: f32x4, b: f32x4) -> f32x4;
fn simd_mul(a: f32x4, b: f32x4) -> f32x4;

// Special attributes
#[vectorize]
fn dot_product(a: [f32], b: [f32]) -> f32 {
    // Auto-vectorized
}

#[stack_alloc]
fn temporary() -> [u8; 1024] {
    // Stack allocated
}
```

## 10. Code Examples by Version

### v0.3.7 Example (Simple Function)
```zeta
fn add(a: i64, b: i64) -> i64 {
    a + b
}

fn main() -> i64 {
    let x = 10;
    let y = 20;
    add(x, y)
}
```

### v0.4.1 Example (With Concepts)
```zeta
concept Add {
    fn add(self, other: Self) -> Self;
}

impl Add for i64 {
    fn add(self, other: i64) -> i64 {
        self + other
    }
}

struct Point {
    x: i64,
    y: i64,
}

impl Add for Point {
    fn add(self, other: Point) -> Point {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

fn main() -> i64 {
    let p1 = Point { x: 1, y: 2 };
    let p2 = Point { x: 3, y: 4 };
    let result = p1.add(p2);
    println("Result: ({}, {})", result.x, result.y);
    0
}
```

### v0.5.0 Example (Advanced Features)
```zeta
// Using new generic syntax
fn process_data<T>(data: T) -> lt(Result, T)
where
    T: Display + Clone,
{
    println("Processing: {}", data);
    Ok(data.clone())
}

// Pattern matching
fn describe_number(n: i32) -> &str {
    match n {
        0 => "zero",
        1..=9 => "single digit",
        n if n % 2 == 0 => "even",
        _ => "odd",
    }
}

// Async function
async fn fetch_and_process(url: &str) -> lt(Result, String) {
    let response = fetch(url).await?;
    process(response).await
}

fn main() -> i64 {
    match process_data(42) {
        Ok(value) => println("Success: {}", value),
        Err(e) => println("Error: {}", e),
    }
    0
}
```

## Conclusion

This reference provides a complete overview of Zeta language constructs across all versions. The evolution from v0.3.7 to v0.5.0 shows significant maturation, particularly in the type system, pattern matching, and async support.

As Zeta's Code Guru, I will continue to update this reference as the language evolves, ensuring it remains the definitive authority on Zeta syntax and