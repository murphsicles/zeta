# 📚 ZETA ACTUAL SYNTAX REFERENCE
## Based on AST Definition and Parser Implementation (v0.5.0)

**Source:** Analysis of `src/frontend/ast.z` and `src/frontend/parser/`  
**Status:** ACTUAL IMPLEMENTATION (not planned)  
**Date:** 2026-03-19

---

## 📖 HOW TO READ THIS DOCUMENT

- **`code`** - Actual Zeta syntax
- **`// comment`** - Explanation
- **`[optional]`** - Optional elements
- **`|`** - Alternatives
- **`*`** - Zero or more
- **`+`** - One or more
- **`?`** - Zero or one

---

## 1. 📝 PROGRAM STRUCTURE

### **Complete Program**
```
Program = TopLevelItem*
```

### **Top-Level Items**
```
TopLevelItem = 
    | FunctionDefinition
    | ConceptDefinition
    | ImplementationBlock
    | StructDefinition
    | EnumDefinition
    | ModuleDeclaration
    | UseDeclaration
    | ExternBlock
    | Attribute* Public? TopLevelItem  // Attributes and visibility
```

---

## 2. 🔤 LEXICAL STRUCTURE

### **Identifiers**
```
Identifier = Letter (Letter | Digit | '_')*
Letter = 'a'..'z' | 'A'..'Z'
Digit = '0'..'9'
```

### **Keywords**
```
// v0.5.0 Keywords (from parser)
fn concept impl for if else let mut return use struct enum type
unsafe loop break continue defer spawn match const async await
pub mod trait where self Self dyn crate super extern
```

### **Literals**
```
IntegerLiteral = Digit+ ('i8' | 'i16' | 'i32' | 'i64' | 'i128' | 'isize' | 'u8' | 'u16' | 'u32' | 'u64' | 'u128' | 'usize')?
FloatLiteral = Digit+ '.' Digit+ ('f32' | 'f64')?
StringLiteral = '"' (Escape | ~["\\])* '"'
CharLiteral = "'" (Escape | ~['\\]) "'"
BooleanLiteral = 'true' | 'false'
```

---

## 3. 🏗️ TYPE SYSTEM

### **Type Expressions**
```
Type = 
    | Identifier  // Simple type: i32, String, etc.
    | Type '[' ']'  // Slice: [T]
    | Type '[' Expr ']'  // Array: [T; N]
    | '&' 'mut'? Type  // Reference: &T, &mut T
    | '*' 'const'? 'mut'? Type  // Raw pointer: *const T, *mut T
    | '(' Type (',' Type)* ')'  // Tuple: (T1, T2, ...)
    | 'fn' '(' TypeList? ')' '->' Type  // Function pointer
    | Type '<' GenericArgs '>'  // Generic type: Vec<T>
```

### **Generic Parameters**
```
GenericParams = '<' GenericParam (',' GenericParam)* '>'
GenericParam = Identifier (':' Bounds)? ('=' Type)?
Bounds = Identifier ('+' Identifier)*
```

### **Where Clauses**
```
WhereClause = 'where' WhereBound (',' WhereBound)*
WhereBound = Type ':' Bounds
```

---

## 4. 🏛️ DECLARATIONS

### **Function Definition**
```
FunctionDefinition = 
    Attribute*
    'pub'?
    'fn' Identifier GenericParams?
    '(' Parameters? ')' ('->' Type)?
    (Block | '=' Expr ';')
    
Parameters = Parameter (',' Parameter)*
Parameter = ('mut'? Identifier ':' Type) | Pattern ':' Type
```

**Examples:**
```zeta
// Basic function
fn add(x: i32, y: i32) -> i32 {
    x + y
}

// Generic function
fn identity<T>(value: T) -> T {
    value
}

// Single-line function
fn double(x: i32) -> i32 = x * 2

// Public function with attributes
#[inline]
pub fn fast_compute() -> i64 {
    // implementation
}
```

### **Concept Definition (Traits)**
```
ConceptDefinition =
    Attribute*
    'pub'?
    'concept' Identifier GenericParams?
    (':' Supertraits)?
    '{' ConceptItem* '}'
    
Supertraits = Identifier ('+' Identifier)*
ConceptItem = 
    | FunctionDefinition
    | AssociatedType
    | ConstantDefinition
```

**Examples:**
```zeta
// Basic concept
concept Display {
    fn fmt(&self) -> String;
}

// Concept with associated type
concept Iterator {
    type Item;
    fn next(&mut self) -> Option<Self::Item>;
}

// Concept with supertraits
concept Clone: Sized {
    fn clone(&self) -> Self;
}

// Generic concept
concept Add<Rhs = Self> {
    type Output;
    fn add(self, rhs: Rhs) -> Self::Output;
}
```

### **Associated Type**
```
AssociatedType = 
    'type' Identifier (':' Bounds)? ('=' Type)? ';'
```

### **Struct Definition**
```
StructDefinition =
    Attribute*
    'pub'?
    'struct' Identifier GenericParams?
    (WhereClause)?
    ('{' Field* '}' | '(' TypeList ')' ';' | ';')
    
Field = Attribute* 'pub'? Identifier ':' Type ','
```

**Examples:**
```zeta
// Regular struct
struct Point {
    x: f64,
    y: f64,
}

// Tuple struct
struct Color(u8, u8, u8);

// Unit struct
struct Marker;

// Generic struct
struct Pair<T, U> {
    first: T,
    second: U,
}
```

### **Enum Definition**
```
EnumDefinition =
    Attribute*
    'pub'?
    'enum' Identifier GenericParams?
    (WhereClause)?
    '{' Variant (',' Variant)* ','? '}'
    
Variant = Identifier ('(' TypeList ')' | '{' Field* '}')?
```

**Examples:**
```zeta
// Simple enum
enum Option<T> {
    Some(T),
    None,
}

// Enum with struct variants
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}
```

### **Implementation Block**
```
ImplementationBlock =
    Attribute*
    'impl' GenericParams? (Identifier | Concept 'for' Type)
    (WhereClause)?
    '{' ImplementationItem* '}'
    
ImplementationItem = FunctionDefinition | ConstantDefinition
```

**Examples:**
```zeta
// Implementation for a type
impl Point {
    fn new(x: f64, y: f64) -> Point {
        Point { x, y }
    }
}

// Implementation of a concept
impl Display for Point {
    fn fmt(&self) -> String {
        format!("({}, {})", self.x, self.y)
    }
}

// Generic implementation
impl<T> Display for Vec<T>
where
    T: Display,
{
    fn fmt(&self) -> String {
        // implementation
    }
}
```

---

## 5. 🧩 EXPRESSIONS

### **Primary Expressions**
```
PrimaryExpr =
    | Literal
    | Identifier
    | '(' Expr ')'
    | BlockExpr
    | IfExpr
    | MatchExpr
    | LoopExpr
    | WhileExpr
    | ForExpr
```

### **Operator Precedence** (highest to lowest)
```
1. . [] () (field/method access, indexing, call)
2. - ! * & &mut (unary)
3. as (type cast)
4. * / % (multiplicative)
5. + - (additive)
6. << >> (shift)
7. & (bitwise AND)
8. ^ (bitwise XOR)
9. | (bitwise OR)
10. == != < <= > >= (comparison)
11. && (logical AND)
12. || (logical OR)
13. = += -= etc. (assignment)
14. , (sequence)
```

### **Function Call**
```
CallExpr = Expr '(' ArgList? ')'
ArgList = Expr (',' Expr)*
```

### **Method Call**
```
MethodCall = Expr '.' Identifier ('::' '<' GenericArgs '>')? '(' ArgList? ')'
```

### **Field Access**
```
FieldAccess = Expr '.' Identifier
```

### **Indexing**
```
IndexExpr = Expr '[' Expr ']'
```

### **Type Cast**
```
CastExpr = Expr 'as' Type
```

### **Block Expression**
```
BlockExpr = '{' Statement* Expr? '}'
```

### **If Expression**
```
IfExpr = 'if' Expr Block ('else' (IfExpr | Block))?
```

### **Match Expression** (v0.5.0)
```
MatchExpr = 'match' Expr '{' MatchArm* '}'
MatchArm = Pattern ('if' Expr)? '=>' (Block | Expr) ','
```

### **Patterns** (v0.5.0)
```
Pattern =
    | '_'  // Wildcard
    | Identifier ('@' Pattern)?  // Binding
    | Literal  // Literal pattern
    | RangePattern  // Range: 1..10, 1..=10
    | ReferencePattern  // &pattern, &mut pattern
    | StructPattern  // Point { x, y }
    | TuplePattern  // (a, b, c)
    | SlicePattern  // [a, b, .., c]
    | OrPattern  // Pattern1 | Pattern2
    
RangePattern = Expr ('..' | '..=') Expr
ReferencePattern = ('&' | '&' 'mut') Pattern
StructPattern = Identifier '{' FieldPattern (',' FieldPattern)* '..'? '}'
FieldPattern = Identifier (':' Pattern)?
TuplePattern = '(' Pattern (',' Pattern)* ')'
SlicePattern = '[' Pattern (',' Pattern)* ('..' Pattern?)? ']'
OrPattern = Pattern ('|' Pattern)+
```

### **Loop Expressions**
```
LoopExpr = ('label' ':')? 'loop' Block
WhileExpr = ('label' ':')? 'while' Expr Block
ForExpr = ('label' ':')? 'for' Pattern 'in' Expr Block
```

### **Break/Continue**
```
BreakExpr = 'break' Label? Expr?
ContinueExpr = 'continue' Label?
Label = Identifier
```

### **Return**
```
ReturnExpr = 'return' Expr?
```

---

## 6. 📦 STATEMENTS

### **Let Statement**
```
LetStatement = 'let' Pattern (':' Type)? '=' Expr ';'
```

**Examples:**
```zeta
let x = 42;
let y: i32 = 10;
let (a, b) = (1, 2);
let Point { x, y } = point;
```

### **Expression Statement**
```
ExprStatement = Expr ';'
```

### **Item Statement**
```
ItemStatement = TopLevelItem
```

---

## 7. 🧰 MODULES AND IMPORTS

### **Module Declaration**
```
ModuleDecl = 'mod' Identifier (';' | '{' TopLevelItem* '}')
```

### **Use Declaration**
```
UseDecl = 'use' UsePath ('as' Identifier)? ';'
UsePath = 
    | 'crate'? (Identifier '::')* (Identifier | '*')
    | 'self' (Identifier '::')* (Identifier | '*')
    | 'super' (Identifier '::')* (Identifier | '*')
```

**Examples:**
```zeta
use std::collections::HashMap;
use std::{collections::HashMap, fs::File};
use std::io::Result as IoResult;
use self::submodule::function;
use super::parent_function;
```

### **Extern Block**
```
ExternBlock = 'extern' StringLiteral? '{' ExternItem* '}'
ExternItem = 'fn' Identifier '(' Parameters? ')' ('->' Type)? ';'
```

**Example:**
```zeta
extern "C" {
    fn malloc(size: usize) -> *mut c_void;
    fn free(ptr: *mut c_void);
}
```

---

## 8. 🏷️ ATTRIBUTES

```
Attribute = '#' '[' AttributeItem ']'
AttributeItem = 
    | Identifier ('=' Literal)?
    | Identifier '(' AttributeArgs ')'
AttributeArgs = (Identifier '=')? Literal (',' (Identifier '=')? Literal)*
```

**Examples:**
```zeta
#[inline]
#[derive(Debug, Clone)]
#[cfg(target_os = "linux")]
#[test]
#[should_panic(expected = "division by zero")]
```

---

## 9. 🧪 COMPLETE EXAMPLES

### **Hello World**
```zeta
fn main() -> i32 {
    println("Hello, Zeta!");
    0
}
```

### **Generic Function with Concept**
```zeta
concept Addable {
    fn add(self, other: Self) -> Self;
}

fn sum<T: Addable>(items: &[T]) -> T {
    let mut total = items[0];
    for &item in &items[1..] {
        total = total.add(item);
    }
    total
}
```

### **Pattern Matching**
```zeta
enum Option<T> {
    Some(T),
    None,
}

fn unwrap_or<T>(opt: Option<T>, default: T) -> T {
    match opt {
        Option::Some(value) => value,
        Option::None => default,
    }
}
```

### **Iterator Implementation**
```zeta
concept Iterator {
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
            let value = self.current;
            self.current += 1;
            Some(value)
        } else {
            None
        }
    }
}
```

---

## 10. 🔍 SYNTAX SUMMARY TABLE

| Construct | Syntax | Example |
|-----------|--------|---------|
| Function | `fn name(params) -> type { ... }` | `fn add(x: i32, y: i32) -> i32` |
| Concept | `concept Name { ... }` | `concept Display { fn fmt(&self) }` |
| Struct | `struct Name { fields }` | `struct Point { x: f64, y: f64 }` |
| Enum | `enum Name { variants }` | `enum Option<T> { Some(T), None }` |
| Impl | `impl Type { ... }` | `impl Point { fn new() { ... } }` |
| Match | `match expr { arms }` | `match x { 1 => "one", _ => "other" }` |
| Let | `let pattern = expr` | `let (x, y) = (1, 2)` |
| Use | `use path` | `use std::collections::HashMap` |
| Mod | `mod name { ... }` | `mod math { pub fn add() { ... } }` |

---

## 📚 SOURCES & VALIDATION

This reference is derived from:
1. **`src/frontend/ast.z`** - Complete AST definition
2. **`src/frontend/parser/`** - Parser implementation
3. **Actual `.z` files** - Real Zeta code examples
4. **v0.3.6 compiler behavior** - What actually parses

**Note:** Some features (like `concept`, `match`) are defined in AST but may not appear in current `.z` files. This document represents the **full language specification** as defined in the compiler, not just what's currently used.

---

## 🚀 NEXT STEPS FOR COMPLETE DOCUMENTATION

1. **Formal Grammar** - Convert to BNF/EBNF
2. **Type System Rules** - Formal typing judgments  
3. **Operational Semantics** - How each construct evaluates
4. **Memory Model** - Ownership, borrowing, lifetimes
5. **ABI Specification** - Calling conventions, layout

*This document represents the **actual** Zeta v0.5.0 syntax, not planned or hypothetical features.*