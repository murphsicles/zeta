# Trait System (Concepts)
## Comprehensive Guide to Zeta's Polymorphism System

**Last Updated:** 2026-03-28  
**System Status:** Active Development (v0.3.9)

---

## 📋 Overview

Zeta's trait system (called "Concepts") provides:
- **Ad-hoc polymorphism** - Define behavior for existing types
- **Generic constraints** - Type parameters with requirements
- **Code reuse** - Shared implementations across types
- **Type safety** - Compile-time verification of capabilities
- **Zero-cost abstraction** - No runtime overhead

### Design Philosophy
> "Concepts express what types can do, not what they are. Capabilities over categories."

---

## 🏗️ Architecture

### Concept System Layers:

```
┌─────────────────────────────────────────┐
│          Concept Definitions            │
│  (Method signatures, generic params)    │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│          Implementation Blocks          │
│  (Provide implementations for types)    │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│          Trait Resolution               │
│  (Find implementations, handle bounds)  │
└─────────────────────────────────────────┐
                    │
                    ▼
┌─────────────────────────────────────────┐
│          Monomorphization               │
│  (Generate concrete implementations)    │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│          Specialization Cache           │
│  (Reuse generated implementations)      │
└─────────────────────────────────────────┘
```

---

## 📝 Concept Definition Syntax

### Basic Concept Definition:

```zeta
// Define a concept (trait)
concept Display {
    // Method signature (no body)
    fn format(&self) -> String;
    
    // Default method implementation
    fn display(&self) {
        println!("{}", self.format());
    }
}

// Generic concept
concept Add<Rhs = Self> {
    // Associated type (optional)
    type Output;
    
    // Required method
    fn add(&self, rhs: Rhs) -> Self::Output;
}

// Concept with multiple methods
concept Iterator {
    type Item;
    
    fn next(&mut self) -> Option<Self::Item>;
    fn size_hint(&self) -> (usize, Option<usize>);
    
    // Default method using required methods
    fn count(self) -> usize {
        let mut count = 0;
        while self.next().is_some() {
            count += 1;
        }
        count
    }
}
```

### Concept Inheritance:

```zeta
// Concept can require other concepts
concept Eq {
    fn eq(&self, other: &Self) -> bool;
}

// PartialEq inherits from Eq
concept PartialEq: Eq {
    fn partial_eq(&self, other: &Self) -> bool;
}

// Ord requires PartialOrd and Eq
concept Ord: PartialOrd + Eq {
    fn cmp(&self, other: &Self) -> Ordering;
}
```

### Generic Concepts with Bounds:

```zeta
// Concept with generic parameters and bounds
concept Convert<T: Display + Clone> {
    fn convert(&self) -> T;
}

// Multiple generic parameters
concept Map<K: Eq + Hash, V> {
    fn get(&self, key: &K) -> Option<&V>;
    fn insert(&mut self, key: K, value: V) -> Option<V>;
}

// Where clauses for complex bounds
concept Processor<T, U> 
where
    T: Serialize + Deserialize,
    U: From<T> + Into<String>,
{
    fn process(&self, input: T) -> U;
}
```

### Associated Types and Constants:

```zeta
concept Collection {
    // Associated type
    type Item;
    type Key;
    type Value;
    
    // Associated constant
    const MAX_SIZE: usize = 1024;
    
    // Generic associated type (GAT)
    type Iter<'a>: Iterator<Item = &'a Self::Item>
    where
        Self: 'a;
    
    fn iter(&self) -> Self::Iter<'_>;
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
```

---

## 🔧 Implementation Syntax

### Basic Implementation:

```zeta
// Implement concept for type
impl Display for Point {
    fn format(&self) -> String {
        format!("({}, {})", self.x, self.y)
    }
}

// Use default methods
let p = Point { x: 1, y: 2 };
p.display();  // Uses default implementation

// Generic implementation
impl<T: Display> Display for Vec<T> {
    fn format(&self) -> String {
        let items: Vec<String> = self.iter()
            .map(|item| item.format())
            .collect();
        format!("[{}]", items.join(", "))
    }
}
```

### Blanket Implementations:

```zeta
// Implement for all types satisfying bounds
impl<T: Display> Debug for T {
    fn debug(&self) -> String {
        format!("Debug: {}", self.format())
    }
}

// Implement for all references
impl<T: Display> Display for &T {
    fn format(&self) -> String {
        (**self).format()
    }
}

// Conditional implementation
impl<T, U> Add<U> for T
where
    T: Add<U, Output = T>,
    U: Copy,
{
    type Output = T;
    
    fn add(&self, rhs: U) -> T {
        *self + rhs
    }
}
```

### Implementation Inheritance:

```zeta
// When implementing Ord, must also implement PartialOrd and Eq
impl Ord for Point {
    fn cmp(&self, other: &Self) -> Ordering {
        // Implementation
    }
}

// PartialOrd implementation required by Ord
impl PartialOrd for Point {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// Eq implementation required by Ord
impl Eq for Point {}
```

### Specialized Implementations:

```zeta
// Default implementation
impl<T> Default for Vec<T> {
    fn default() -> Self {
        Vec::new()
    }
}

// Specialized implementation for specific type
impl Default for Vec<i32> {
    fn default() -> Self {
        Vec::with_capacity(10)
    }
}

// More specialized wins over less specialized
let v1: Vec<i32> = Default::default();  // Uses specialized
let v2: Vec<String> = Default::default(); // Uses generic
```

---

## 🎯 Using Concepts

### Generic Functions with Bounds:

```zeta
// Function requiring concept
fn print<T: Display>(item: T) {
    item.display();
}

// Multiple bounds
fn compare<T: Eq + Ord>(a: T, b: T) -> Ordering {
    a.cmp(&b)
}

// Where clause for complex bounds
fn process<T, U>(input: T) -> U
where
    T: Serialize,
    U: Deserialize + From<T>,
{
    // Implementation
}

// Impl Trait in argument position
fn display_item(item: impl Display) {
    item.display();
}

// Impl Trait in return position
fn get_displayable() -> impl Display {
    Point { x: 1, y: 2 }
}
```

### Trait Objects (Dynamic Dispatch):

```zeta
// Boxed trait object
fn process_displayable(item: Box<dyn Display>) {
    item.display();
}

// Reference trait object
fn print_all(items: &[&dyn Display]) {
    for item in items {
        item.display();
    }
}

// Trait object with associated types
fn process_iterator(iter: Box<dyn Iterator<Item = i32>>) {
    for item in iter {
        println!("{}", item);
    }
}
```

### Concept Bounds in Structs:

```zeta
// Generic struct with bounds
struct Container<T: Display> {
    item: T,
}

impl<T: Display> Container<T> {
    fn show(&self) {
        self.item.display();
    }
}

// Multiple bounds
struct Pair<T: Eq, U: Ord> {
    first: T,
    second: U,
}

// Where clause in struct
struct Processor<T, U>
where
    T: Serialize,
    U: Deserialize,
{
    input: T,
    output: U,
}
```

---

## 🔄 Trait Resolution

### Method Resolution Order:

1. **Inherent methods** (methods defined directly on type)
2. **Trait methods** (methods from implemented traits)
3. **Default trait methods** (if not overridden)

```zeta
struct Point { x: i32, y: i32 }

// Inherent method (highest priority)
impl Point {
    fn distance(&self) -> f64 {
        (self.x * self.x + self.y * self.y) as f64
    }
}

// Trait method
impl Display for Point {
    fn format(&self) -> String {
        format!("Point({}, {})", self.x, self.y)
    }
}

let p = Point { x: 3, y: 4 };
p.distance();   // Calls inherent method
p.format();     // Calls trait method
p.display();    // Calls default trait method
```

### Ambiguity Resolution:

```zeta
// Two traits with same method name
concept A {
    fn method(&self) -> i32;
}

concept B {
    fn method(&self) -> i32;
}

struct Data;

impl A for Data {
    fn method(&self) -> i32 { 1 }
}

impl B for Data {
    fn method(&self) -> i32 { 2 }
}

let d = Data;

// ERROR: Ambiguous method call
// d.method();

// SOLUTION: Disambiguate with fully qualified syntax
A::method(&d);  // Calls A's method
B::method(&d);  // Calls B's method

// Or bring into scope with alias
use A as A_trait;
A_trait::method(&d);
```

### Associated Type Resolution:

```zeta
concept Container {
    type Item;
    fn get(&self) -> Self::Item;
}

struct Box<T> {
    value: T,
}

impl<T> Container for Box<T> {
    type Item = T;
    
    fn get(&self) -> T {
        self.value
    }
}

// Type inference knows Item = i32
let b: Box<i32> = Box { value: 42 };
let x: i32 = b.get();  // Item resolved to i32
```

---

## 🏭 Monomorphization

### Generic Code Generation:

```zeta
// Generic function
fn add<T: Add>(a: T, b: T) -> T::Output {
    a.add(b)
}

// Generated specialized versions:
fn add_i32(a: i32, b: i32) -> i32 {
    a + b
}

fn add_f64(a: f64, b: f64) -> f64 {
    a + b
}

fn add_string(a: String, b: String) -> String {
    a + &b
}
```

### Specialization Cache:

```zeta
// Cache key: (concept, type, generic parameters)
let cache_key = (Add::id(), TypeId::of::<i32>(), vec![]);

// Check cache before generating
if let Some(impl) = specialization_cache.get(&cache_key) {
    reuse_implementation(impl);
} else {
    let impl = generate_implementation();
    specialization_cache.insert(cache_key, impl);
    use_implementation(impl);
}
```

### Inline Expansion:

```zeta
// Small methods are inlined
impl Add for i32 {
    #[inline]
    fn add(&self, rhs: i32) -> i32 {
        *self + rhs
    }
}

// After inlining:
let result = 5.add(3);
// Becomes:
let result = 5 + 3;
```

---

## 🧪 Testing Concepts

### Mock Implementations:

```zeta
// Test concept implementation
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_display_implementation() {
        struct TestStruct(i32);
        
        impl Display for TestStruct {
            fn format(&self) -> String {
                format!("Test({})", self.0)
            }
        }
        
        let t = TestStruct(42);
        assert_eq!(t.format(), "Test(42)");
    }
}
```

### Property-based Testing:

```zeta
// Test concept laws
#[cfg(test)]
mod property_tests {
    use super::*;
    use proptest::prelude::*;
    
    proptest! {
        #[test]
        fn test_add_commutative(a: i32, b: i32) {
            // a + b == b + a
            assert_eq!(a.add(b), b.add(a));
        }
        
        #[test]
        fn test_add_associative(a: i32, b: i32, c: i32) {
            // (a + b) + c == a + (b + c)
            assert_eq!(a.add(b).add(c), a.add(b.add(c)));
        }
    }
}
```

### Integration Testing:

```zeta
// Test concept usage in real scenarios
#[cfg(test)]
mod integration_tests {
    use super::*;
    
    #[test]
    fn test_collection_operations() {
        let mut coll = MyCollection::new();
        
        // Test all Collection concept methods
        assert!(coll.is_empty());
        assert_eq!(coll.len(), 0);
        
        coll.insert(1, "hello");
        assert!(!coll.is_empty());
        assert_eq!(coll.len(), 1);
        assert_eq!(coll.get(&1), Some(&"hello"));
    }
}
```

---

## 🚀 Best Practices

### Concept Design:

1. **Single Responsibility**: Each concept should represent one capability
2. **Minimal Methods**: Include only essential methods
3. **Default Methods**: Provide sensible defaults when possible
4. **Documentation**: Document preconditions, postconditions, and invariants

### Implementation Guidelines:

```zeta
// GOOD: Clear, focused concepts
concept Read {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize>;
}

concept Write {
    fn write(&mut self, buf: &[u8]) -> Result<usize>;
    fn flush(&mut self) -> Result<()>;
}

// BAD: Kitchen sink concept
concept FileOperations {  // Too broad!
    fn read(&mut self, buf: &mut [u8]) -> Result<usize>;
    fn write(&mut self, buf: &[u8]) -> Result<usize>;
    fn seek(&mut self, pos: u64) -> Result<()>;
    fn metadata(&self) -> Result<Metadata>;
    // ... many more
}
```

### Performance Considerations:

```zeta
// Use static dispatch when possible
fn process<T: Processor>(item: T) {  // Monomorphized
    // Fast, no runtime overhead
}

// Use dynamic dispatch when needed
fn process_boxed(item: Box<dyn Processor>) {  // Virtual call
    // Flexible, but virtual call overhead
}

// Small methods should be inline
impl Add for i32 {
    #[inline(always)]  // Force inline for performance
    fn add(&self, rhs: i32) -> i32 {
        *self + rhs
    }
}
```

### Error Patterns to Avoid:

```zeta
// BAD: Overly restrictive bounds
fn process<T: Display + Debug + Clone + Serialize>(item: T) {
    // Requires too many capabilities
}

// BAD: Orphan rule violation
// Can't implement foreign trait for foreign type
impl std::fmt::Display for external::ExternalType {  // ERROR!
    // ...
}

// BAD: Conflicting implementations
impl<T> Display for Vec<T> { /* ... */ }
impl Display for Vec<i32> { /* ... */ }  // May conflict!

// SOLUTION: Use specialization or different approach
```

---

## 🔧 Advanced Features

### Higher-Rank Trait Bounds (HRTB):

```zeta
// For<'a> syntax for lifetime polymorphism
fn process<F>(f: F)
where
    F: for<'a> Fn(&'a str) -> &'a str,
{
    // Function works with any lifetime
}

// HRTB with concepts
conrait Processor {
    fn process<'a>(&self, input: &'a str) -> &'a str;
}

impl<T> Processor for T
where
    T: for<'a> Fn(&'a str) -> &'a str,
{
    fn process<'a>(&self, input: &'a str) -> &'a str {
        self(input)
    }
}
```

### Generic Associated Types (GATs):

```zeta
concept Factory {
    type Product<T>;
    
    fn create<T>(&self) -> Self::Product<T>;
}

struct MyFactory;

impl Factory for MyFactory {
    type Product<T> = Box<T>;
    
    fn create<T>(&self) -> Box<T> {
        Box::new(T::default())
    }
}
```

### Const Generics in Concepts:

```zeta
concept ArrayLike<const N: usize> {
    type Item;
    
    fn get(&self, index: usize) -> Option<&Self::Item>;
    fn len(&self) -> usize {
        N
    }
}

impl<T, const N: usize> ArrayLike<N> for [T; N] {
    type Item = T;
    
    fn get(&self, index: usize) -> Option<&T> {
        if index < N {
            Some(&self[index])
        } else {
            None
        }
    }
}
```

### Unsafe Traits:

```zeta
// Mark trait as unsafe to implement
unsafe concept Send {
    // No methods, just a marker
}

// Unsafe implementation requires unsafe block
unsafe impl Send for MyType {
    // Must guarantee thread safety
}

// Safe to use, unsafe to implement
fn spawn<T: Send + 'static>(task: T) {
    // Can send to another thread
}
```

---

## 🚨 Common Issues & Solutions

### "Trait not implemented" Error:

```zeta
struct Point { x: i32, y: i32 }

fn display_point(p: Point) {
    p