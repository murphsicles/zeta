# ZETA LANGUAGE SYNTAX REFERENCE — v0.5.0
**Version:** v0.5.0 (Production-Ready Era)
**Purpose:** Complete syntax reference for Zeta v0.5.0. The single source of truth for all Zeta syntax.
**Status:** PLANNED (Target: June 2026)

**Last Updated:** March 17, 2026
**Author:** Zeta Dark Factory

---

## 🎯 QUICK REFERENCE

### **Installation:**
```bash
# From crates.io (v0.3.5+ features)
cargo install zetac --version 0.3.5

# From source (v0.4.1 self-hosted)
./zeta compile src/main.z -o zeta_new
```

### **Hello World:**
```zeta
fn main() -> i64 {
    println("Hello, Zeta v0.5.0!")
    0
}
```

---

## 1. LEXICAL STRUCTURE

### **Comments**
```zeta
// Line comment
/* Block comment */
/// Documentation comment
//! Module documentation
```

### **Identifiers**
- Start with letter or `_`
- Alphanumeric + `_` after first character
- Case-sensitive

### **Keywords**
```zeta
// v0.4.1 Keywords
fn concept impl for if else let mut return use struct enum type
unsafe loop break continue defer spawn

// v0.5.0 New Keywords
match const async await pub mod trait where
self Self dyn crate super
```

### **Literals**
```zeta
// Integers
42          // i64
42i32       // i32
0xFF        // Hex
0o77        // Octal
0b1010      // Binary
1_000_000   // With separators

// Floating Point (v0.5.0)
3.14        // f64
3.14f32     // f32
1.0e-10     // Scientific

// Strings
"hello"                 // &str
"multi\nline"          // Escape sequences
r#"raw "string"#"      // Raw string
b"bytes"               // Byte string

// Characters
'a'                    // char
'\n'                   // Escape
'\u{1F600}'           // Unicode

// Booleans
true false

// Arrays
[1, 2, 3]              // Array literal
[]                     // Empty array

// Tuples
(1, "hello", true)     // Tuple
()                     // Unit
```

### **Whitespace**
- Spaces, tabs, newlines are insignificant except in strings
- Indentation is optional but recommended for readability

---

## 2. TYPES

### **Primitive Types**
```zeta
// Signed integers
i8 i16 i32 i64 i128 isize

// Unsigned integers
u8 u16 u32 u64 u128 usize

// Floating point
f32 f64

// Other primitives
bool char str

// Special
!          // Never type (diverging)
()         // Unit type
```

### **Compound Types**
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
*const T     // Raw const pointer
*mut T       // Raw mut pointer

// Function pointers
fn(i32) -> i32
```

### **User-Defined Types**
```zeta
// Structs
struct Point {
    x: i64,
    y: i64,
}

// Tuple structs
struct Color(i32, i32, i32);

// Unit structs
struct Marker;

// Enums
enum Option<T> {
    Some(T),
    None,
}

// Newtypes
struct Meters(f64);
struct Seconds(f64);
```

### **Generic Types**
```zeta
// Type parameters
struct Pair<T, U> {
    first: T,
    second: U,
}

// Const generics (v0.5.0)
struct Array<T, const N: usize> {
    data: [T; N],
}

// Lifetime parameters
struct Ref<'a, T> {
    data: &'a T,
}

// Where clauses
fn process<T>(value: T) -> T
where
    T: Display + Debug,
    T: Clone,
{
    value.clone()
}
```

---

## 3. ITEMS (TOP-LEVEL)

### **Modules**
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

### **Imports**
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

### **Functions**
```zeta
// Basic function
fn add(a: i64, b: i64) -> i64 {
    a + b
}

// Generic function
fn identity<T>(value: T) -> T {
    value
}

// Const function (v0.5.0)
const fn square(x: i32) -> i32 {
    x * x
}

// Async function (v0.5.0)
async fn fetch_data(url: &str) -> Result<String, Error> {
    // Async implementation
}

// Single-line function
fn double(x: i64) -> i64 = x * 2

// Method
impl Point {
    fn distance(&self) -> f64 {
        (self.x * self.x + self.y * self.y) as f64
    }
}
```

### **Concepts (Traits)**
```zeta
// Basic concept
concept Display {
    fn fmt(&self, f: &mut Formatter) -> Result;
}

// Generic concept
concept Add<Rhs = Self> {
    type Output;
    fn add(self, rhs: Rhs) -> Self::Output;
}

// Associated types (v0.5.0)
concept Iterator {
    type Item;
    fn next(&mut self) -> Option<Self::Item>;
}

// Default methods (v0.5.0)
concept Default {
    fn default() -> Self {
        // Default implementation
    }
}

// Supertraits (v0.5.0)
concept Clone: Sized {
    fn clone(&self) -> Self;
}

// Multiple supertraits
concept MyTrait: Display + Debug + Clone {}
```

### **Implementations**
```zeta
// Basic implementation
impl Display for Point {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

// Generic implementation
impl<T> Display for Vec<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut Formatter) -> Result {
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

### **Structs & Enums**
```zeta
// Struct with methods
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn area(&self) -> u32 {
        self.width * self.height
    }
}

// Enum with variants
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}

impl Message {
    fn process(&self) {
        match self {
            Message::Quit => quit(),
            Message::Move { x, y } => move_to(*x, *y),
            Message::Write(text) => println("{}", text),
            Message::ChangeColor(r, g, b) => set_color(*r, *g, *b),
        }
    }
}
```

### **Type Aliases**
```zeta
type Meters = f64;
type Result<T> = std::result::Result<T, Error>;
type Callback = fn(i32) -> i32;
```

---

## 4. EXPRESSIONS

### **Literals & Variables**
```zeta
42                 // Integer literal
"hello"            // String literal
true               // Boolean literal
x                  // Variable
self               // Self parameter
Self               // Self type
```

### **Operators**
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

// Other
a as T            // Type cast
a.b               // Field access
a.method()        // Method call
a..b              // Range
a..=b             // Inclusive range
```

### **Function & Method Calls**
```zeta
// Function call
function(arg1, arg2)

// Method call
object.method(arg1, arg2)

// Associated function
Type::function(arg1, arg2)

// Generic function
function::<i32>(arg)

// Method call with turbofish
object.method::<i32>(arg)
```

### **Control Flow Expressions**
```zeta
// If expression
let result = if condition {
    value1
} else {
    value2
};

// Match expression (v0.5.0)
let result = match value {
    Pattern1 => expression1,
    Pattern2 if guard => expression2,
    _ => default_expression,
};

// Loop expression
let result = loop {
    if condition { break value; }
};

// While loop expression
let mut i = 0;
while i < 10 {
    i += 1;
}

// For loop (v0.5.0)
for item in collection {
    process(item);
}
```

### **Other Expressions**
```zeta
// Block expression
let x = {
    let y = compute();
    y * 2
};

// Return expression
return value;

// Await expression (v0.5.0)
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

---

## 5. STATEMENTS

### **Declaration Statements**
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

### **Expression Statements**
```zeta
// Function call
println("Hello");

// Method call
object.process();

// Assignment
x = 42;

// Compound assignment
x += 1;
```

### **Control Flow Statements**
```zeta
// If statement
if condition {
    // then block
} else if other_condition {
    // else if block
} else {
    // else block
}

// Match statement (v0.5.0)
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

### **Other Statements**
```zeta
// Defer statement
defer cleanup();

// Unsafe block
unsafe {
    // unsafe operations
}

// Async block (v0.5.0)
async {
    // async operations
}
```

---

## 6. PATTERNS (v0.5.0)

### **Literal Patterns**
```zeta
match value {
    0 => "zero",
    1 | 2 => "one or two",
    3..=5 => "three to five",
    _ => "other",
}
```

### **Identifier Patterns**
```zeta
match value {
    x => println("got {}", x),
    mut y => y = process(y),
    ref r => println("reference {}", r),
}
```

### **Wildcard Pattern**
```zeta
match value {
    _ => "anything",
    Some(_) => "some value",
}
```

### **Rest Patterns**
```zeta
match slice {
    [] => "empty",
    [first, ..] => "starts with {}", first,
    [.., last] => "ends with {}", last,
    [first, .., last] => "first {}, last {}", first, last,
}
```

### **Struct Patterns**
```zeta
match point {
    Point { x: 0, y } => "on y-axis at {}", y,
    Point { x, y: 0 } => "on x-axis at {}", x,
    Point { x, y } => "at ({}, {})", x, y,
}
```

### **Tuple Patterns**
```zeta
match pair {
    (0, y) => "zero and {}", y,
    (x, 0) => "{} and zero", x,
    (x, y) => "{} and {}", x, y,
}
```

### **Enum Patterns**
```zeta
match message {
    Message::Quit => quit(),
    Message::Move { x, y } => move_to(x, y),
    Message::Write(text) => println("{}", text),
    Message::ChangeColor(r, g, b) => set_color(r, g, b),
}
```

### **Reference Patterns**
```zeta
match reference {
    &value => println("deref {}", value),
    ref mut r => *r = new_value,
}
```

### **Pattern Guards**
```zeta
match value {
    Some(x) if x > 0 => "positive",
    Some(x) if x < 0 => "negative",
    Some(0) => "zero",
    None => "none",
}
```

### **@ Patterns**
```zeta
match value {
    x @ 1..=10 => "between 1 and 10: {}", x,
    x @ 11..=20 => "between 11 and 20: {}", x,
    _ => "other",
}
```

---

## 7. ATTRIBUTES

### **Function Attributes**
```zeta
#[inline]
fn fast_function() {}

#[cold]
fn rarely_called() {}

#[test]
fn test_function() {
    assert_eq!(2 + 2, 4);
}

#[should_panic]
fn test_panic() {
    panic!("expected");
}
```

### **Struct/Enum Attributes**
```zeta
#[derive(Debug, Clone, Copy)]
struct Point {
    x: i32,
    y: i32,
}

#[repr(C)]
struct CStruct {
    field: i32,
}

#[non_exhaustive]
enum Status {
    Ok,
    Error,
}
```

### **Module Attributes**
```zeta
#![allow(unused_variables)]
#![feature(const_generics)]

#[cfg(target_os = "linux")]
mod linux_specific;

#[cfg(test)]
mod tests {
    // test code
}
```

### **v0.5.0 Special Attributes**
```zeta
#[vectorize]
fn dot_product(a: [f32], b: [f32]) -> f32 {
    // Auto-vectorized
}

#[stack_alloc]
fn temporary() -> [u8; 1024] {
    // Stack allocated
}

#[pgo_hot]
fn hot_path() {
    // Profile-guided optimization
}

#[lto_merge]
mod critical {
    // Link-time optimization
}
```

---

## 8. BUILT-IN INTRINSICS & MACROS

### **Print Macros**
```zeta
println!("Hello, {}!", name);
print!("No newline");
eprintln!("Error: {}", error);
format!("Formatted: {}", value);
```

### **Assertion Macros**
```zeta
assert!(condition);
assert_eq!(left, right);
assert_ne!(left, right);
debug_assert!(condition);
unreachable!();
unimplemented!();
```

### **Type Conversion**
```zeta
let x = 42i32 as i64;
let ptr = &value as *const i32;
let bytes = string.as_bytes();
```

### **v0.5.0 SIMD Intrinsics**
```zeta
// SIMD types
type f32x4 = __m128;
type i32x8 = __m256i;

// SIMD operations
fn simd_add(a: f32x4, b: f32x4) -> f32x4;
fn simd_mul(a: f32x4, b: f32x4) -> f32x4;
fn simd_shuffle(a: f32x4, b: f32x4) -> f32x4;
```

### **Memory Operations**
```zeta
// Allocation
let ptr = std::alloc::alloc(layout);
std::alloc::dealloc(ptr, layout);

// Memory operations
std::ptr::read(ptr);
std::ptr::write(ptr, value);
std::mem::swap(&mut a, &mut b);
std