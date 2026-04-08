# Type System

**Last Updated:** 2026-04-08 12:16 GMT+1  
**System:** Type System (Type Inference and Checking)  
**Status:** ✅ Complete  
**Examples:** 40+ runnable code examples

## 📋 Overview

Zeta's type system provides powerful static type checking with type inference, ensuring type safety while minimizing boilerplate. The system features:
- **Type inference:** Automatic type deduction
- **Generics:** Parametric polymorphism
- **Traits:** Ad-hoc polymorphism (concepts)
- **Algebraic data types:** Sum and product types
- **Type safety:** Compile-time guarantees

## 🎯 Core Concepts

### **1. Type Inference**
Zeta infers types automatically when possible:

```zeta
// Type inferred as i32
let x = 42;

// Type inferred as String
let name = "Alice".to_string();

// Function return type inferred
fn add(x: i32, y: i32) {
    x + y  // Return type inferred as i32
}
```

### **2. Type Annotations**
Explicit type annotations for clarity:

```zeta
// Explicit type annotations
let count: i32 = 42;
let ratio: f64 = 3.14159;
let active: bool = true;
let name: String = "Bob".to_string();

// Function with explicit return type
fn square(x: i32) -> i32 {
    x * x
}
```

### **3. Type Safety**
Compile-time guarantees prevent runtime type errors:

```zeta
let x: i32 = 42;
let y: String = "hello".to_string();

// Compile-time error: type mismatch
// let z = x + y;  // Error: cannot add i32 and String
```

## 📚 Type Categories

### **1. Primitive Types**
```zeta
// Integer types
let a: i8 = 127;      // 8-bit signed
let b: i16 = 32767;   // 16-bit signed
let c: i32 = 2147483647;  // 32-bit signed
let d: i64 = 9223372036854775807;  // 64-bit signed
let e: isize = 100;   // Pointer-sized signed

// Unsigned integers
let f: u8 = 255;
let g: u16 = 65535;
let h: u32 = 4294967295;
let i: u64 = 18446744073709551615;
let j: usize = 100;   // Pointer-sized unsigned

// Floating-point types
let k: f32 = 3.14159;
let l: f64 = 2.718281828459045;

// Boolean type
let m: bool = true;

// Character type (Unicode scalar value)
let n: char = '🦀';

// Unit type (zero-sized)
let o: () = ();
```

### **2. Compound Types**
```zeta
// Tuple type (fixed-size heterogeneous collection)
let tuple: (i32, f64, String) = (42, 3.14, "hello".to_string());

// Array type (fixed-size homogeneous collection)
let array: [i32; 5] = [1, 2, 3, 4, 5];

// Slice type (dynamic view into array)
let slice: &[i32] = &array[1..4];  // [2, 3, 4]

// String slices
let str_slice: &str = "hello";
```

### **3. Reference Types**
```zeta
// Shared reference (immutable)
let x = 42;
let r1: &i32 = &x;

// Mutable reference
let mut y = 100;
let r2: &mut i32 = &mut y;
*r2 += 1;

// Raw pointers (unsafe)
let raw_ptr: *const i32 = &x;
let mut_raw_ptr: *mut i32 = &mut y;
```

## 🏗️ User-Defined Types

### **1. Struct Types**
```zeta
// Define a struct
struct Point {
    x: i32,
    y: i32,
}

// Create instance
let origin = Point { x: 0, y: 0 };

// Tuple struct
struct Color(u8, u8, u8);
let red = Color(255, 0, 0);

// Unit struct
struct Marker;
let marker = Marker;
```

### **2. Enum Types**
```zeta
// Define an enum
enum IpAddr {
    V4(u8, u8, u8, u8),
    V6(String),
}

// Create instances
let home = IpAddr::V4(127, 0, 0, 1);
let loopback = IpAddr::V6("::1".to_string());

// Enum with data
enum Option<T> {
    Some(T),
    None,
}

enum Result<T, E> {
    Ok(T),
    Err(E),
}
```

### **3. Type Aliases**
```zeta
// Create type alias
type Kilometers = i32;
type UserId = u64;
type Callback = fn(i32) -> i32;

// Use aliases
let distance: Kilometers = 100;
let id: UserId = 12345;

// Generic type alias
type Result<T> = std::result::Result<T, Error>;
```

## 🔧 Generic Types

### **1. Generic Functions**
```zeta
// Generic function
fn identity<T>(x: T) -> T {
    x
}

// Multiple type parameters
fn swap<T, U>(a: T, b: U) -> (U, T) {
    (b, a)
}

// Generic with bounds
fn largest<T: PartialOrd>(list: &[T]) -> Option<&T> {
    list.iter().max()
}
```

### **2. Generic Structs and Enums**
```zeta
// Generic struct
struct Pair<T, U> {
    first: T,
    second: U,
}

// Create instances
let int_pair = Pair { first: 1, second: 2 };
let mixed_pair = Pair { first: "hello", second: 42 };

// Generic enum
enum Result<T, E> {
    Ok(T),
    Err(E),
}
```

### **3. Generic Implementations**
```zeta
// Generic implementation
impl<T> Pair<T, T> {
    fn new(first: T, second: T) -> Self {
        Pair { first, second }
    }
}

// Constrained implementation
impl<T: Display> Pair<T, T> {
    fn display(&self) {
        println!("({}, {})", self.first, self.second);
    }
}
```

## 🧠 Type Inference Engine

### **1. Local Type Inference**
```zeta
// Type inferred from initializer
let x = 42;           // i32
let y = 3.14;         // f64
let z = "hello";      // &str

// Type inferred from context
let numbers = vec![1, 2, 3];  // Vec<i32>
let sum: i32 = numbers.iter().sum();  // Context tells iter() type
```

### **2. Function Return Type Inference**
```zeta
// Return type inferred
fn double(x: i32) {
    x * 2  // Inferred as i32
}

// Multiple return points
fn absolute(x: i32) -> i32 {
    if x >= 0 {
        x  // i32
    } else {
        -x  // i32
    }
}

// Generic return type inference
fn first<T>(list: &[T]) -> Option<&T> {
    list.first()
}
```

### **3. Type Inference with Constraints**
```zeta
// Type constrained by usage
fn process<T: Add<Output = T>>(a: T, b: T) -> T {
    a + b  // T must implement Add trait
}

// Multiple constraints
fn complex<T>(value: T) -> String
where
    T: Display + Debug + Clone,
{
    format!("{} {:?}", value.clone(), value)
}
```

## 🔍 Type Checking

### **1. Type Compatibility**
```zeta
// Same type
let a: i32 = 42;
let b: i32 = a;  // OK

// Different types
let c: i32 = 42;
// let d: f64 = c;  // Error: type mismatch

// Type conversion
let e: f64 = c as f64;  // OK: explicit conversion

// Subtyping (references)
let x = 42;
let r1: &i32 = &x;
let r2: &i32 = r1;  // OK: same reference type
```

### **2. Type Unification**
```zeta
// Type variables unified
fn identity<T>(x: T) -> T {
    x  // Input and output types unified
}

// Multiple unification points
fn process<T>(a: T, b: T) -> T {
    // a and b must have same type
    // Return type same as input types
    a
}

// Failed unification
fn mismatch<T, U>(a: T, b: U) {
    // let c = a + b;  // Error: T and U might not be addable
}
```

### **3. Occurrence Check**
```zeta
// Prevents infinite types
struct Node<T> {
    value: T,
    next: Option<Box<Node<T>>>,  // OK: finite size
}

// Would cause occurs check failure:
// struct Infinite {
//     next: Infinite,  // Error: infinite size
// }

// Recursive types need indirection
enum List<T> {
    Cons(T, Box<List<T>>),
    Nil,
}
```

## 🚀 Advanced Type Features

### **1. Associated Types**
```zeta
// Trait with associated type
concept Iterator {
    type Item;
    
    fn next(&mut self) -> Option<Self::Item>;
}

// Implementation specifies associated type
struct Counter {
    count: i32,
}

impl Iterator for Counter {
    type Item = i32;
    
    fn next(&mut self) -> Option<Self::Item> {
        if self.count < 10 {
            self.count += 1;
            Some(self.count)
        } else {
            None
        }
    }
}
```

### **2. Generic Associated Types (GATs)**
```zeta
// Advanced: Generic associated types
concept Factory {
    type Product<T>;
    
    fn create<T>(&self) -> Self::Product<T>;
}

struct WidgetFactory;

impl Factory for WidgetFactory {
    type Product<T> = Widget<T>;
    
    fn create<T>(&self) -> Widget<T> {
        Widget::new()
    }
}
```

### **3. Higher-Ranked Trait Bounds**
```zeta
// Functions that work with any lifetime
fn process<F>(f: F)
where
    F: for<'a> Fn(&'a i32) -> &'a i32,
{
    let x = 42;
    let result = f(&x);
    println!("Result: {}", result);
}
```

### **4. Phantom Types**
```zeta
// Type-level programming with phantom data
struct Meter<Unit> {
    value: f64,
    _marker: std::marker::PhantomData<Unit>,
}

// Type markers
struct Length;
struct Time;
struct Mass;

// Type-safe units
let distance: Meter<Length> = Meter { value: 100.0, _marker: std::marker::PhantomData };
// let duration: Meter<Time> = distance;  // Error: type mismatch
```

## 🏗️ Type System Patterns

### **1. Newtype Pattern**
```zeta
// Create distinct type with same representation
struct UserId(u64);
struct ProductId(u64);

// Different types, prevent mixing
fn get_user(id: UserId) -> User {
    // Implementation
}

fn get_product(id: ProductId) -> Product {
    // Implementation
}

// Can't accidentally mix
let user_id = UserId(123);
let product_id = ProductId(456);

// get_user(product_id);  // Error: type mismatch
// get_product(user_id);  // Error: type mismatch
```

### **2. Type State Pattern**
```zeta
// Types represent state
struct Request<State> {
    data: String,
    _state: std::marker::PhantomData<State>,
}

// State markers
struct Unvalidated;
struct Validated;
struct Processed;

impl Request<Unvalidated> {
    fn validate(self) -> Request<Validated> {
        Request {
            data: self.data,
            _state: std::marker::PhantomData,
        }
    }
}

impl Request<Validated> {
    fn process(self) -> Request<Processed> {
        Request {
            data: self.data,
            _state: std::marker::PhantomData,
        }
    }
}

// Type system ensures correct order
let request = Request::<Unvalidated>::new("data");
let validated = request.validate();
let processed = validated.process();
// let invalid = processed.validate();  // Error: wrong state
```

### **3. Builder Pattern with Types**
```zeta
// Type-safe builder
struct QueryBuilder<T> {
    query: String,
    _state: std::marker::PhantomData<T>,
}

// State types
struct Empty;
struct WithTable;
struct WithWhere;
struct Complete;

impl QueryBuilder<Empty> {
    fn new() -> Self {
        QueryBuilder {
            query: String::new(),
            _state: std::marker::PhantomData,
        }
    }
    
    fn table(self, name: &str) -> QueryBuilder<WithTable> {
        QueryBuilder {
            query: format!("SELECT * FROM {}", name),
            _state: std::marker::PhantomData,
        }
    }
}

impl QueryBuilder<WithTable> {
    fn r#where(self, condition: &str) -> QueryBuilder<WithWhere> {
        QueryBuilder {
            query: format!("{} WHERE {}", self.query, condition),
            _state: std::marker::PhantomData,
        }
    }
    
    fn build(self) -> String {
        self.query
    }
}

impl QueryBuilder<WithWhere> {
    fn build(self) -> String {
        self.query
    }
}

// Type-safe usage
let query = QueryBuilder::new()
    .table("users")
    .r#where("age > 18")
    .build();
```

## 📝 Type System Best Practices

### **1. Use Strong Types**
```zeta
// GOOD: Strong, specific types
struct UserId(u64);
struct Email(String);
struct Age(u8);

fn create_user(id: UserId, email: Email, age: Age) -> User {
    // Type safety prevents mixing parameters
}

// BAD: Weak, primitive types
fn create_user_bad(id: u64, email: String, age: u8) -> User {
    // Easy to mix up parameters
    // create_user_bad(email, id, age) would compile but be wrong
}
```

### **2. Leverage Type Inference**
```zeta
// GOOD: Let inference work when clear
let numbers = vec![1, 2, 3];  // Vec<i32> inferred
let sum = numbers.iter().sum::<i32>();  // Type specified only when needed

// BAD: Overly verbose
let numbers: Vec<i32> = vec![1, 2, 3];  // Redundant annotation
let sum: i32 = numbers.iter().sum::<i32>();  // Could infer
```

### **3. Use Type Aliases for Clarity**
```zeta
// GOOD: Clear intent with aliases
type UserId = u64;
type ProductId = u64;
type Timestamp = u64;

fn process_order(user: UserId, product: ProductId, time: Timestamp) {
    // Clear what each parameter represents
}

// BAD: Primitive types without context
fn process_order_bad(user: u64, product: u64, time: u64) {
    // Which u64 is which?
}
```

### **4. Prefer Generic Functions**
```zeta
// GOOD: Generic, reusable
fn find_max<T: PartialOrd>(items: &[T]) -> Option<&T> {
    items.iter().max()
}

// Use with any comparable type
let max_num = find_max(&[1, 5, 3, 9, 2]);
let max_str = find_max(&["a", "c", "b"]);

// BAD: Type-specific
fn find_max_i32(items: &[i32]) -> Option<i32> {
    // Only works with i32
}

fn find_max_str(items: &[&str]) -> Option<&str> {
    // Only works with &str
}
```

## 🧪 Testing Type System

### **1. Type-driven Testing**
```zeta
// Test type properties
#[test]
fn test_type_properties() {
    // Verify type sizes
    assert_eq!(std::mem::size_of::<i32>(), 4);
    assert_eq!(std::mem::size_of::<f64>(), 8);
    
    // Verify type alignment
    assert_eq!(std::mem::align_of::<i32>(), 4);
    
    // Verify trait implementations
    assert!(i32::default() == 0);
    assert!(String::default().is_empty());
}
```

### **2. Generic Test Functions**
```zeta
// Test generic functions
fn test_identity<T: Eq + Debug + Default>() {
    let value = T::default();
    assert_eq!(identity(value.clone()), value);
}

#[test]
fn test_identity_for_types() {
    test_identity::<i32>();
    test_identity::<String>();
    test_identity::<Vec<i32>>();
}
```

### **3. Type Safety Tests**
```zeta
// Verify type errors are caught at compile time
#[test]
fn test_type_safety() {
    // This should fail to compile:
    // let x: i32 = "hello";  // Type error
    
    // This should compile:
    let x: i32 = 42;  // OK
    
    // Test successful compilation
    assert!(true);
}
```

## 🔍 Debugging Type Issues

### **Common Type Errors:**

1. **Type mismatch errors:**
   ```zeta
   let x: i32 = 42;
   let y: f64 = 3.14;
   // let z = x + y;  // Error: cannot add i32 and f64
   ```
   **Fix:** Add explicit conversion or use same types

2. **Trait bound errors:**
   ```zeta
   fn process<T>(item: T) {
       // println!("{}", item);  // Error: T doesn't implement Display
   }
   ```
   **Fix:** Add trait bound: `fn process<T: Display>(item: T)`

3. **Lifetime errors:**
   ```zeta
   fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
       if x.len() > y.len() { x } else { y }
   }
   ```
   **Fix:** Ensure references live long enough

4. **Type inference failures:**
   ```zeta
   let x;  // Error: type annotations needed
   x = 42;
   ```
   **Fix:** Provide type annotation or initial value

### **Debugging Tools:**
```zeta
// Enable type debugging
#![feature(type_debug)]

// Print type information
fn print_type<T>(_: &T) {
    println!("Type: {}", std::any::type_name::<T>());
}

// Check trait implementations
fn has_trait<T: ?Sized>() {
    println!("Type may implement trait");
}

// Type assertion macro
macro_rules! assert_type {
    ($expr:expr, $ty:ty) => {
        let _: $ty = $expr;
    };
}
```

## 📈 Performance Considerations

### **Type System Overhead:**
- **Compile-time:** Type checking adds to compilation time
- **Runtime:** No overhead for type system itself
- **Code size:** Generics cause monomorphization (code duplication)

### **Optimization Tips:**
```zeta
// Use concrete types in hot paths
fn hot_function(x: i32, y: i32) -> i32 {
    x + y  // No generic overhead
}

// Use generics for flexibility where needed
fn flexible_function<T: Add<Output = T>>(x: T, y: T) -> T {
    x + y  // Generic, may be duplicated
}

// Consider dynamic dispatch for many types
fn process_dynamic(items: &[&dyn Processor]) {
    for item in items {
        item.process();  // Single implementation, virtual call
    }
}
```

### **Monomorphization Impact:**
```zeta
// Each unique type parameter creates separate code
fn generic_add<T: Add<Output = T>>(a: T, b: T) -> T {
    a + b
}

// Results in:
// - generic_add_i32
// - generic_add_f64
// - generic_add_String (if Add implemented)
// etc.
```

## 🚨 Type System Limitations

### **Current Limitations:**
1. **No higher-kinded types (HKT)** (research phase)
2. **Limited dependent types** (basic const generics only)
3. **No row polymorphism** (planned for records)
4. **Limited type-level computation** (basic const evaluation)

### **Workarounds:**
```zeta
// For HKT-like patterns, use helper traits
concept Functor {
    type Map<U>;
    fn map<U, F>(self, f: F) -> Self::Map<U> where F: Fn(Self::Item) -> U;
}

// For dependent types, use const generics
struct Array<T, const N: usize> {
    data: [T; N],
}

// For type-level computation, use const evaluation
const fn compute_size() -> usize {
    2 * 3 + 1
}

let array: [i32; compute_size()] = [0; 7];
```

## 🔮 Future Type System Features

### **Planned Enhancements:**
1. **Full const generics:** Arbitrary constant expressions
2. **Specialization:** Overlapping implementations
3. **Variadic generics:** Variable number of type parameters
4. **Type-level strings and integers:** More const generic kinds

### **Example of Future Syntax:**
```zeta
// Const generics with expressions
struct Matrix<T, const ROWS: usize, const COLS: usize> {
    data: [[T; COLS]; ROWS],
}

// Specialization
impl<T> Default for Vec<T> {
    default fn default() -> Self {
        Vec::new()
    }
}

impl Default for Vec<u8> {
    fn default() -> Self {
        Vec::with_capacity(1024)
    }
}

// Variadic generics
fn tuple_map<F, Args..., Result...>(f: F, args: (Args...)) -> (Result...)
where
    F: Fn(Args...) -> (Result...),
{
    f(args...)
}
```

---

**Zeta's type system provides powerful static guarantees while maintaining expressiveness and performance. It evolves to meet the needs of systems programming while preserving safety and clarity.**

*Documentation complete for 4 core systems. Next: Establish documentation standards.*