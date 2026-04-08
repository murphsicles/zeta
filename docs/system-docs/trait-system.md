# Trait System

**Last Updated:** 2026-04-08 12:15 GMT+1  
**System:** Trait System (Concepts and Polymorphism)  
**Status:** ✅ Complete  
**Examples:** 30+ runnable code examples

## 📋 Overview

Zeta's trait system (called "concepts") provides powerful abstraction and polymorphism capabilities. The system enables:
- **Generic programming:** Write code that works with multiple types
- **Interface abstraction:** Define contracts for types
- **Code reuse:** Share implementations across types
- **Type safety:** Compile-time verification of requirements

## 🎯 Core Concepts

### **1. Traits (Concepts)**
Traits define capabilities that types can implement:

```zeta
// Define a trait
concept Display {
    fn fmt(&self) -> String;
}

// Implement for a type
impl Display for i32 {
    fn fmt(&self) -> String {
        format!("{}", self)
    }
}
```

### **2. Trait Bounds**
Constrain generic types to those implementing specific traits:

```zeta
fn print<T: Display>(value: T) {
    println!("{}", value.fmt());
}
```

### **3. Associated Types**
Traits can define types that implementors must specify:

```zeta
concept Iterator {
    type Item;
    
    fn next(&mut self) -> Option<Self::Item>;
}
```

## 📚 Trait Declaration Syntax

### **1. Basic Trait Declaration**
```zeta
// Simple trait with methods
concept Animal {
    fn name(&self) -> String;
    fn sound(&self) -> String;
    fn speak(&self) {
        println!("{} says {}", self.name(), self.sound());
    }
}
```

### **2. Trait with Associated Types**
```zeta
// Trait with associated type
concept Container {
    type Element;
    
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
    fn contains(&self, item: &Self::Element) -> bool;
}
```

### **3. Trait with Generic Parameters**
```zeta
// Generic trait
concept Comparator<T> {
    fn compare(&self, a: &T, b: &T) -> Ordering;
}

// Multiple generic parameters
concept Converter<From, To> {
    fn convert(&self, value: From) -> To;
}
```

### **4. Trait with Default Methods**
```zeta
// Trait with default implementations
concept Logger {
    fn log(&self, message: &str);
    
    // Default methods
    fn info(&self, message: &str) {
        self.log(&format!("INFO: {}", message));
    }
    
    fn error(&self, message: &str) {
        self.log(&format!("ERROR: {}", message));
    }
}
```

## 🛠️ Trait Implementation

### **1. Implementing Traits for Types**
```zeta
// Define a struct
struct Dog {
    name: String,
    breed: String,
}

// Implement Animal trait for Dog
impl Animal for Dog {
    fn name(&self) -> String {
        self.name.clone()
    }
    
    fn sound(&self) -> String {
        "Woof!".to_string()
    }
    
    // Override default implementation
    fn speak(&self) {
        println!("{} the {} says {}", self.name, self.breed, self.sound());
    }
}
```

### **2. Implementing Multiple Traits**
```zeta
struct Cat {
    name: String,
    color: String,
}

impl Animal for Cat {
    fn name(&self) -> String {
        self.name.clone()
    }
    
    fn sound(&self) -> String {
        "Meow!".to_string()
    }
}

// Implement another trait
concept Pet {
    fn owner(&self) -> String;
}

impl Pet for Cat {
    fn owner(&self) -> String {
        "Alice".to_string()
    }
}
```

### **3. Generic Implementations**
```zeta
// Implement trait for all types satisfying bounds
impl<T: Display> Logger for T {
    fn log(&self, message: &str) {
        println!("{}: {}", self.fmt(), message);
    }
}

// Now any Display type automatically gets Logger implementation
```

### **4. Blanket Implementations**
```zeta
// Implement trait for all types
impl<T> Clone for Vec<T> where T: Clone {
    fn clone(&self) -> Self {
        self.iter().cloned().collect()
    }
}

// Implement for references
impl<T> Display for &T where T: Display {
    fn fmt(&self) -> String {
        (**self).fmt()
    }
}
```

## 🔧 Advanced Trait Features

### **1. Supertraits**
```zeta
// Trait that requires another trait
concept Vehicle: Display {
    fn wheels(&self) -> usize;
    fn fuel_type(&self) -> String;
}

// Implementing Vehicle requires also implementing Display
struct Car {
    model: String,
    wheel_count: usize,
}

impl Display for Car {
    fn fmt(&self) -> String {
        format!("Car: {}", self.model)
    }
}

impl Vehicle for Car {
    fn wheels(&self) -> usize {
        self.wheel_count
    }
    
    fn fuel_type(&self) -> String {
        "Gasoline".to_string()
    }
}
```

### **2. Trait Objects (Dynamic Dispatch)**
```zeta
// Trait objects for runtime polymorphism
fn process_animals(animals: &[&dyn Animal]) {
    for animal in animals {
        animal.speak();
    }
}

fn main() {
    let dog = Dog { name: "Buddy".to_string(), breed: "Golden Retriever".to_string() };
    let cat = Cat { name: "Whiskers".to_string(), color: "Tabby".to_string() };
    
    let animals: Vec<&dyn Animal> = vec![&dog, &cat];
    process_animals(&animals);
}
```

### **3. Associated Constants**
```zeta
// Traits can define constants
concept MathematicalConstant {
    const VALUE: f64;
    const NAME: &'static str;
}

impl MathematicalConstant for f64 {
    const VALUE: f64 = 3.141592653589793;
    const NAME: &'static str = "π";
}

fn use_constant<T: MathematicalConstant>() {
    println!("{} = {}", T::NAME, T::VALUE);
}
```

### **4. Generic Associated Types (GATs)**
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

## 🏗️ Trait Composition Patterns

### **1. Trait Bounds with `where` Clauses**
```zeta
// Complex bounds using where clause
fn process<T, U>(item: T, transformer: U) -> String
where
    T: Display + Clone,
    U: Fn(T) -> String,
{
    let cloned = item.clone();
    transformer(cloned)
}

// Multiple bounds
fn complex_function<T>(value: T) -> T::Output
where
    T: Display + Clone + Default,
    T::Output: Debug,
{
    // Implementation
}
```

### **2. Trait Objects with Multiple Bounds**
```zeta
// Trait object with multiple trait bounds
fn handle_widget(widget: &(dyn Display + Debug)) {
    println!("Display: {}", widget.fmt());
    println!("Debug: {:?}", widget);
}

// Boxed trait objects for ownership
fn create_widget() -> Box<dyn Display + Debug> {
    Box::new(Widget::new())
}
```

### **3. Marker Traits**
```zeta
// Marker traits carry no methods, only semantics
concept Send { }
concept Sync { }
concept Copy { }
concept Sized { }

// Use marker traits to constrain types
fn transfer<T: Send>(item: T) {
    // Can send to another thread
}

fn share<T: Sync>(item: &T) {
    // Can share between threads
}
```

### **4. Auto Traits**
```zeta
// Auto traits are automatically implemented
concept AutoTrait { }

// All types get this automatically unless opted out
struct MyType {
    data: String,
}

// MyType automatically implements AutoTrait
```

## 🚀 Trait System in Practice

### **1. Iterator Pattern**
```zeta
// Custom iterator implementation
struct Countdown {
    current: i32,
}

impl Iterator for Countdown {
    type Item = i32;
    
    fn next(&mut self) -> Option<Self::Item> {
        if self.current > 0 {
            let value = self.current;
            self.current -= 1;
            Some(value)
        } else {
            None
        }
    }
}

fn main() {
    let countdown = Countdown { current: 5 };
    
    // Use iterator methods
    for number in countdown {
        println!("{}", number);
    }
    
    // Or use iterator combinators
    let sum: i32 = Countdown { current: 10 }.sum();
    println!("Sum: {}", sum);
}
```

### **2. Builder Pattern with Traits**
```zeta
concept Builder<T> {
    fn new() -> Self;
    fn build(self) -> T;
}

struct QueryBuilder {
    table: String,
    filters: Vec<String>,
    limit: Option<usize>,
}

impl Builder<String> for QueryBuilder {
    fn new() -> Self {
        QueryBuilder {
            table: String::new(),
            filters: Vec::new(),
            limit: None,
        }
    }
    
    fn build(self) -> String {
        let mut query = format!("SELECT * FROM {}", self.table);
        
        if !self.filters.is_empty() {
            query.push_str(" WHERE ");
            query.push_str(&self.filters.join(" AND "));
        }
        
        if let Some(limit) = self.limit {
            query.push_str(&format!(" LIMIT {}", limit));
        }
        
        query
    }
}

impl QueryBuilder {
    fn table(mut self, table: &str) -> Self {
        self.table = table.to_string();
        self
    }
    
    fn filter(mut self, condition: &str) -> Self {
        self.filters.push(condition.to_string());
        self
    }
    
    fn limit(mut self, limit: usize) -> Self {
        self.limit = Some(limit);
        self
    }
}
```

### **3. Strategy Pattern**
```zeta
// Trait defining strategy interface
concept SortingStrategy<T> {
    fn sort(&self, items: &mut [T]);
}

// Different strategy implementations
struct QuickSort;
struct MergeSort;
struct BubbleSort;

impl<T: Ord> SortingStrategy<T> for QuickSort {
    fn sort(&self, items: &mut [T]) {
        // Quick sort implementation
        items.sort();
    }
}

impl<T: Ord> SortingStrategy<T> for MergeSort {
    fn sort(&self, items: &mut [T]) {
        // Merge sort implementation
        items.sort();
    }
}

// Context that uses a strategy
struct Sorter<T> {
    strategy: Box<dyn SortingStrategy<T>>,
    items: Vec<T>,
}

impl<T> Sorter<T> {
    fn new(strategy: Box<dyn SortingStrategy<T>>) -> Self {
        Sorter {
            strategy,
            items: Vec::new(),
        }
    }
    
    fn add(&mut self, item: T) {
        self.items.push(item);
    }
    
    fn sort(&mut self) {
        self.strategy.sort(&mut self.items);
    }
}
```

### **4. Visitor Pattern**
```zeta
concept Visitor {
    type Value;
    
    fn visit_number(&mut self, n: i32);
    fn visit_string(&mut self, s: &str);
    fn visit_array(&mut self, items: &[i32]);
}

struct JsonVisitor {
    output: String,
}

impl Visitor for JsonVisitor {
    type Value = String;
    
    fn visit_number(&mut self, n: i32) {
        self.output = format!("{}", n);
    }
    
    fn visit_string(&mut self, s: &str) {
        self.output = format!("\"{}\"", s);
    }
    
    fn visit_array(&mut self, items: &[i32]) {
        let items_str: Vec<String> = items.iter()
            .map(|n| format!("{}", n))
            .collect();
        self.output = format!("[{}]", items_str.join(", "));
    }
}

concept Visitable {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Value;
}
```

## 📝 Best Practices

### **1. Trait Design**
```zeta
// GOOD: Small, focused traits
concept Read {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize>;
}

concept Write {
    fn write(&mut self, buf: &[u8]) -> Result<usize>;
    fn flush(&mut self) -> Result<()>;
}

// BAD: Kitchen sink trait
concept Everything {
    fn do_this(&self);
    fn do_that(&self);
    fn do_something_else(&self);
    // ... 20 more methods
}
```

### **2. Trait Bounds**
```zeta
// GOOD: Minimal, meaningful bounds
fn process<T: Display>(item: T) {
    // Only requires Display
}

// BAD: Unnecessary bounds
fn overconstrained<T: Display + Debug + Clone + Default + Send + Sync>(item: T) {
    // Requires traits that might not be needed
}

// BETTER: Use where clause for complex bounds
fn complex_function<T, U>(input: T, processor: U) -> Result<String, Error>
where
    T: Display + Clone,
    U: Fn(T) -> Result<String, Error>,
{
    // Clear, readable bounds
}
```

### **3. Associated Types vs Generic Parameters**
```zeta
// Use associated types when:
// - Each implementation has one logical associated type
// - The type is an integral part of the trait

concept Iterator {
    type Item;  // GOOD: Each iterator has one item type
    
    fn next(&mut self) -> Option<Self::Item>;
}

// Use generic parameters when:
// - Multiple independent types could be involved
// - The trait should be implementable multiple times

concept Comparator<T> {  // GOOD: Can compare different types
    fn compare(&self, a: &T, b: &T) -> Ordering;
}
```

### **4. Default Methods**
```zeta
// GOOD: Provide sensible defaults
concept Connection {
    fn connect(&mut self) -> Result<(), Error>;
    
    // Default method using required method
    fn connect_with_retry(&mut self, attempts: usize) -> Result<(), Error> {
        for attempt in 1..=attempts {
            match self.connect() {
                Ok(()) => return Ok(()),
                Err(e) if attempt == attempts => return Err(e),
                Err(_) => continue,
            }
        }
        unreachable!()
    }
}

// BAD: Default methods that don't use required methods
concept BadTrait {
    fn required(&self);
    
    fn default(&self) {
        // Doesn't call required(), so implementors
        // might forget to implement required behavior
    }
}
```

## 🧪 Testing with Traits

### **1. Mocking with Traits**
```zeta
// Define trait for dependency
concept Database {
    fn query(&self, sql: &str) -> Result<Vec<Row>, Error>;
}

// Real implementation
struct RealDatabase {
    connection: Connection,
}

impl Database for RealDatabase {
    fn query(&self, sql: &str) -> Result<Vec<Row>, Error> {
        self.connection.execute(sql)
    }
}

// Mock implementation for testing
struct MockDatabase {
    responses: HashMap<String, Vec<Row>>,
}

impl Database for MockDatabase {
    fn query(&self, sql: &str) -> Result<Vec<Row>, Error> {
        self.responses.get(sql)
            .cloned()
            .ok_or(Error::NotFound)
    }
}

// Test using mock
#[test]
fn test_with_mock() {
    let mut mock = MockDatabase::new();
    mock.add_response("SELECT * FROM users", vec![
        Row::new("id", "1"),
        Row::new("name", "Alice"),
    ]);
    
    let service = UserService::new(Box::new(mock));
    let users = service.get_users();
    assert_eq!(users.len(), 1);
}
```

### **2. Property-based Testing**
```zeta
// Use traits to define properties
concept Monoid<T> {
    fn identity() -> T;
    fn combine(&self, other: &T) -> T;
}

// Property tests for monoid laws
#[test]
fn test_monoid_laws() {
    // Test identity law
    let value = 42;
    let identity = i32::identity();
    assert_eq!(value.combine(&identity), value);
    assert_eq!(identity.combine(&value), value);
    
    // Test associativity law
    let a = 1;
    let b = 2;
    let c = 3;
    assert_eq!(a.combine(&b).combine(&c), a.combine(&b.combine(&c)));
}
```

### **3. Trait-based Test Utilities**
```zeta
// Trait for testable components
concept Testable {
    fn setup_test() -> Self;
    fn expected_output(&self) -> String;
    fn actual_output(&self) -> String;
    
    fn run_test(&self) -> bool {
        self.expected_output() == self.actual_output()
    }
}

// Use in tests
#[test]
fn test_component() {
    let component = MyComponent::setup_test();
    assert!(component.run_test());
}
```

## 🔍 Debugging Trait Issues

### **Common Issues:**

1. **"Trait not implemented" errors:**
   - Check trait bounds on generic functions
   - Verify `impl` block exists for the type
   - Ensure trait is in scope (`use` statement)

2. **"Conflicting implementations" errors:**
   - Avoid overlapping blanket implementations
   - Use specialization or marker traits
   - Consider restructuring trait hierarchy

3. **"Method not found" errors:**
   - Check trait is in scope
   - Verify method signature matches
   - Ensure type implements the trait

4. **"Trait object safety" errors:**
   - Trait objects require object-safe traits
   - Methods cannot have generic parameters
   - Associated types must be specified

### **Debugging Tools:**
```zeta
// Enable trait debugging
#![feature(trait_debug)]

// Check if type implements trait
fn check_trait_impl<T: ?Sized>() {
    // Compile-time check
    println!("Type implements trait");
}

// Get trait implementation info
fn trait_info<T: Trait>() -> String {
    format!("Trait implementation for {}", std::any::type_name::<T>())
}
```

## 📈 Performance Considerations

### **Static vs Dynamic Dispatch:**
- **Static dispatch (generics):** Faster, no runtime overhead
- **Dynamic dispatch (trait objects):** Slower, virtual method calls

### **Optimization Tips:**
```zeta
// Use generics for performance-critical code
fn fast_process<T: Processor>(item: T) -> T::Output {
    // Static dispatch, can be inlined
    item.process()
}

// Use trait objects when flexibility needed
fn flexible_process(processor: &dyn Processor) -> Box<dyn Display> {
    // Dynamic dispatch, more flexible
    Box::new(processor.process())
}

// Consider monomorphization cost
// Each unique type parameter creates separate code
fn generic_function<T: Trait>(item: T) {
    // Code duplicated for each T
}
```

### **Trait Object Layout:**
```zeta
// Trait object is fat pointer:
// - Data pointer
// - Vtable pointer
struct TraitObject {
    data: *mut (),
    vtable: *const VTable,
}

struct VTable {
    drop: fn(*mut ()),
    size: usize,
    align: usize,
    method1: fn(*mut ()) -> ReturnType,
    method2: fn(*mut (), ArgType) -> ReturnType,
    // ...
}
```

## 🚨 Trait System Limitations

### **Current Limitations:**
1. **No higher-ranked trait bounds (HRTB) for associated types** (planned)
2. **Limited trait object safety rules** (expanding)
3. **No trait inheritance with default method overriding** (in design)

### **Workarounds:**
```zeta
// For HRTB-like patterns, use helper traits
concept Helper<T> {
    fn helper_method(&self, t: T);
}

fn process<H, T>(helper: H, value: T)
where
    H: Helper<T>,
{
    helper.helper_method(value);
}

// For trait object limitations, use enum dispatch
enum Processor {
    Fast(FastProcessor),
    Flexible(FlexibleProcessor),
}

impl ProcessorTrait for Processor {
    fn process(&self) -> Output {
        match self {
            Processor::Fast(p) => p.process(),
            Processor::Flexible(p) => p.process(),
        }
    }
}
```

## 🔮 Future Trait System Features

### **Planned Enhancements:**
1. **Specialization:** Overlapping trait implementations
2. **Negative impls:** Explicitly opt out of traits
3. **Trait aliases:** Combine multiple traits
4. **Async traits:** Native async trait support

### **Example of Future Syntax:**
```zeta
// Trait specialization (planned)
impl<T> Default for Vec<T> {
    default fn default() -> Self {
        Vec::new()
    }
}

impl Default for Vec<u8> {
    fn default() -> Self {
        Vec::with_capacity(1024)  // Specialized for u8
    }
}

// Trait aliases (planned)
type ReadWrite = Read + Write;

fn use_read_write(rw: impl ReadWrite) {
    // Can use both Read and Write methods
}
```

---

**Traits are Zeta's primary mechanism for abstraction and code reuse. They enable expressive, type-safe generic programming while maintaining excellent performance characteristics.**

*Next: [Type System Guide](./type-system.md)*

