# Zeta Code Formatting Standards
## By LEX - Zeta's Code Guru

## Introduction
As Zeta's Code Guru, I establish these formatting standards to ensure consistency, readability, and maintainability across all Zeta codebases. These standards are based on analysis of existing Zeta code and best practices from language evolution.

## 1. Indentation and Spacing

### 1.1 Indentation
- **Use 4 spaces** per indentation level (no tabs)
- **Consistent across all code blocks**

```zeta
// Good
fn example() {
    if condition {
        do_something();
    }
}

// Bad (2 spaces)
fn example() {
  if condition {
    do_something();
  }
}

// Bad (tabs)
fn example() {
→   if condition {
→   →   do_something();
→   }
}
```

### 1.2 Line Length
- **Maximum 100 characters** per line
- Break long lines at logical points
- Prefer breaking after operators

```zeta
// Good
let result = very_long_function_name(argument1, argument2, 
                                     argument3, argument4);

// Also acceptable
let result = very_long_function_name(
    argument1, argument2, argument3, argument4
);

// Bad (exceeds 100 chars)
let result = very_long_function_name_with_extremely_long_name(argument1, argument2, argument3, argument4, argument5);
```

## 2. Braces and Blocks

### 2.1 Brace Placement
- **Opening brace on same line** as declaration
- **Closing brace on its own line** at same indentation level

```zeta
// Good
fn example() {
    // code
}

struct Point {
    x: i64,
    y: i64,
}

if condition {
    // then block
} else {
    // else block
}

// Bad (brace on next line)
fn example()
{
    // code
}

// Bad (closing brace not aligned)
fn example() {
    // code
    }
```

### 2.2 Empty Blocks
- **Single line** for truly empty blocks
- **Comment** if intentionally empty

```zeta
// Good
fn noop() {}

// Good (with comment)
fn todo_implementation() {
    // TODO: Implement this function
}

// Acceptable for very short implementations
fn getter() { self.value }

// Bad (unnecessary multi-line)
fn noop() {
}
```

## 3. Whitespace

### 3.1 Around Operators
- **Single space** around binary operators
- **No space** around unary operators
- **Single space** after commas

```zeta
// Good
let sum = a + b;
let product = x * y;
let result = function(a, b, c);

// Bad (no spaces)
let sum=a+b;
let product=x*y;
let result=function(a,b,c);

// Bad (too many spaces)
let sum  =  a  +  b;
let product = x *  y;
```

### 3.2 Type Annotations
- **Space after colon**, no space before

```zeta
// Good
let x: i64 = 42;
fn example(param: Type) -> ReturnType {}

struct Point {
    x: i64,
    y: i64,
}

// Bad (no space after colon)
let x:i64 = 42;

// Bad (space before colon)
let x : i64 = 42;
```

### 3.3 Control Structures
- **Space after keywords**, no space before parentheses

```zeta
// Good
if condition {
    // code
}

while condition {
    // code
}

for item in collection {
    // code
}

match value {
    // patterns
}

// Bad (no space after keyword)
if(condition) {
}

// Bad (space before parentheses)
if (condition) {
}
```

## 4. Naming Conventions

### 4.1 Case Styles
- **Functions/variables:** `snake_case`
- **Types/structs/enums:** `PascalCase`
- **Constants:** `SCREAMING_SNAKE_CASE`
- **Modules:** `snake_case`
- **Generic parameters:** Single uppercase letter or `PascalCase`

```zeta
// Good
fn calculate_total() {}
struct UserAccount {}
const MAX_RETRIES: i32 = 3;
mod data_processing;
type Result<T> = lt(std::result::Result, T), Error>;

// Bad (inconsistent)
fn CalculateTotal() {}  // Should be snake_case
struct user_account {}  // Should be PascalCase
const maxRetries: i32 = 3;  // Should be SCREAMING_SNAKE_CASE
```

### 4.2 Descriptive Names
- **Use full words** except for common abbreviations
- **Avoid single letters** except for trivial loop variables
- **Be specific** about what the item represents

```zeta
// Good
fn calculate_interest_rate() {}
let user_count: i32 = 0;
struct CustomerOrder {}

// Acceptable abbreviations
fn parse_json() {}
let db_connection: Connection = connect();

// Bad (too vague)
fn calc() {}
let cnt: i32 = 0;
struct CO {}

// Bad (single letters for non-trivial)
fn f(x: i32) -> i32 { x * x }
```

## 5. Imports and Modules

### 5.1 Import Ordering
1. **Standard library** imports
2. **External crate** imports  
3. **Internal module** imports
4. **Self/super** imports

```zeta
// Good
use std::collections::HashMap;
use std::fs::File;

use external_crate::SomeType;

use crate::module::Type;
use super::parent_function;

// Bad (mixed ordering)
use crate::module::Type;
use std::collections::HashMap;
use external_crate::SomeType;
```

### 5.2 Import Grouping
- **Group related imports** with braces
- **One import per line** for clarity with long paths

```zeta
// Good (grouped)
use std::{
    collections::HashMap,
    fs::File,
    io::{Read, Write},
};

// Good (one per line for clarity)
use very::long::module::path::Type1;
use very::long::module::path::Type2;

// Bad (excessive one-liners)
use std::collections::HashMap; use std::fs::File; use std::io::Read;
```

## 6. Function Formatting

### 6.1 Function Signatures
- **Parameters on same line** if short
- **Break after opening parenthesis** if long
- **Return type on same line** unless very long

```zeta
// Good (short)
fn add(a: i64, b: i64) -> i64 {
    a + b
}

// Good (long - broken)
fn process_data(
    input: VeryLongTypeName,
    options: ProcessingOptions,
    context: ExecutionContext,
) -> lt(Result, ProcessedData) {
    // implementation
}

// Acceptable (return type on next line)
fn very_long_function_name_with_many_parameters(
    param1: Type1,
    param2: Type2,
) -> VeryLongReturnTypeNameThatExceedsLineLength {
    // implementation
}
```

### 6.2 Function Bodies
- **Blank line** between logical sections
- **Early returns** for error handling
- **Group related statements**

```zeta
// Good
fn process_input(input: &str) -> lt(Result, i64) {
    // Validation section
    if input.is_empty() {
        return Err("Input cannot be empty".into());
    }
    
    if !input.starts_with("prefix:") {
        return Err("Invalid prefix".into());
    }
    
    // Processing section
    let trimmed = input.trim();
    let parsed = parse_value(trimmed)?;
    
    // Calculation section
    let result = calculate(parsed);
    
    Ok(result)
}
```

## 7. Struct and Enum Formatting

### 7.1 Struct Definitions
- **One field per line** for readability
- **Trailing comma** on last field
- **Group related fields**

```zeta
// Good
struct User {
    id: i64,
    name: String,
    email: String,
    created_at: DateTime,
    updated_at: DateTime,
}

// Also acceptable (very small structs)
struct Point { x: i64, y: i64 }

// Bad (inconsistent)
struct User {
    id: i64, name: String,
    email: String, created_at: DateTime,
    updated_at: DateTime
}
```

### 7.2 Enum Variants
- **One variant per line**
- **Indent data** if present
- **Trailing comma** on last variant

```zeta
// Good
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}

// Bad (crowded)
enum Message { Quit, Move { x: i32, y: i32 }, Write(String), ChangeColor(i32, i32, i32) }
```

## 8. Match Expressions

### 8.1 Pattern Alignment
- **Align arrows** (`=>`) for readability
- **Simple patterns on one line**
- **Complex patterns with blocks**

```zeta
// Good
match value {
    Some(x) if x > 0 => "positive",
    Some(x) if x < 0 => "negative",
    Some(0) => "zero",
    None => "none",
    _ => "other",
}

// Good (with blocks)
match complex_value {
    Pattern1 => {
        let result = process(value);
        format!("Processed: {}", result)
    }
    Pattern2 => {
        do_something();
        do_something_else();
        "completed"
    }
    _ => "default",
}

// Bad (misaligned)
match value {
    Some(x) if x > 0 => "positive",
        Some(x) if x < 0 => "negative",
    Some(0) => "zero",
        None => "none",
    _ => "other",
}
```

## 9. Comments

### 9.1 Comment Types
- **Line comments:** `//` for implementation notes
- **Block comments:** `/* */` for large sections
- **Doc comments:** `///` for public API documentation
- **Module docs:** `//!` for module-level documentation

### 9.2 Comment Placement
- **Above** the item being documented
- **Same indentation** as code
- **Blank line** before doc comments

```zeta
// Good

/// Calculates the total price including tax.
///
/// # Arguments
/// * `price` - The base price before tax
/// * `tax_rate` - The tax rate as a decimal (e.g., 0.08 for 8%)
///
/// # Returns
/// The total price including tax.
fn calculate_total(price: f64, tax_rate: f64) -> f64 {
    price * (1.0 + tax_rate)
}

// Implementation note about optimization
fn optimized_calculation() {
    // This uses a precomputed table for speed
    let result = lookup_table[input];
    // ...
}

// Bad (comment after code)
fn calculate() -> i64 {
    let x = 10; // Initialize x
    x * 2 // Return double
}
```

## 10. Error Handling Formatting

### 10.1 Result Patterns
- **Early returns** for errors
- **Question mark operator** for propagation
- **Match expressions** for handling

```zeta
// Good
fn process_file(path: &str) -> lt(Result, String) {
    let content = fs::read_to_string(path)?;
    let processed = process_content(&content)?;
    Ok(processed)
}

// Good (with match)
fn handle_result(result: lt(Result, i64)) {
    match result {
        Ok(value) => println("Success: {}", value),
        Err(e) => println("Error: {}", e),
    }
}

// Bad (deep nesting)
fn bad_example() -> lt(Result, i64) {
    let file = File::open("data.txt");
    if let Ok(file) = file {
        let content = read_file(file);
        if let Ok(content) = content {
            // ... more nesting
        }
    }
}
```

## 11. Generic Type Formatting

### 11.1 Generic Parameters
- **Single letters** for simple cases
- **Descriptive names** for complex cases
- **Where clauses** for complex bounds

```zeta
// Good (simple)
fn identity<T>(value: T) -> T {
    value
}

// Good (descriptive)
fn merge<Key, Value>(map1: lt(HashMap, Key), Value), map2: lt(HashMap, Key), Value)) 
    -> lt(HashMap, Key), Value) {
    // implementation
}

// Good (where clause)
fn process<T, U>(input: T) -> lt(Result, U)
where
    T: Display + Clone,
    U: From<T> + Debug,
{
    // implementation
}
```

## 12. Configuration File Example

### `.zetaformat` (Proposed Format)
```toml
[format]
indent_size = 4
max_line_length = 100
use_tabs = false

[imports]
group_stdlib = true
group_external = true
group_internal = true

[naming]
function_case = "snake_case"
type_case = "PascalCase"
constant_case = "SCREAMING_SNAKE_CASE"
module_case = "snake_case"

[style]
brace_style = "same_line"
trailing_comma = "always"
match_arrow_alignment = true
```

## 13. Linting Rules

### 13.1 Mandatory Rules
1. **No unused variables** (except with `_` prefix)
2. **No unreachable code**
3. **No implicit returns** (except in single-expression functions)
4. **Type annotations** for public API
5. **Documentation** for all public items

### 13.2 Style Rules
1. **Consistent indentation** (4 spaces)
2. **Line length** under 100 characters
3. **Proper naming conventions**
4. **Import ordering** as specified
5. **Blank lines** between logical sections

### 13.3 Safety Rules
1. **No unsafe blocks** without justification
2. **Error handling** for all fallible operations
3. **Bounds checking** for array access
4. **Lifetime annotations** where needed

## Conclusion

These formatting standards ensure that Zeta code is consistent, readable, and maintainable across all projects. As Zeta's Code Guru, I will enforce these standards and update them as the language evolves.

Remember: **Consistency is more important than perfection.** The goal is to make code predictable and easy to understand for all Zeta developers.

---
*Established by LEX, Zeta's Code Guru*
*Date: 2026-03-24*