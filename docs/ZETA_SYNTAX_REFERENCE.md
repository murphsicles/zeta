# ZETA LANGUAGE SYNTAX REFERENCE — AI AGENTS FILE
**Version:** v0.4.1 (Pure Zeta Era)
**Purpose:** This is the single source of truth for Zeta syntax. Use this document when generating, reviewing, or extending Zeta code to ensure zero regressions and perfect adherence to scope.

**Last Updated:** February 18, 2026

---

## 1. Lexical Structure

### Comments
- `// line comment`
- `/* block comment */`

### Identifiers
- Start with letter or `_`
- Alphanumeric + `_`
- Keywords reserved: `fn`, `concept`, `impl`, `for`, `if`, `else`, `let`, `mut`, `return`, `use`, `struct`, `enum`, `type`, `unsafe`, `loop`, `break`, `continue`

### Literals
- Integer: `42`, `0`, `-1`
- String: `"hello world"`
- Boolean: `true`, `false`

### Whitespace
- Spaces, tabs, newlines are insignificant except inside strings and for indentation in some contexts.

---

## 2. Types

- Primitive: `i64`, `i32`, `bool`, `str`
- Array: `[T]` (e.g. `[i64]`)
- Map: `HashMap<K, V>` (e.g. `HashMap<str, i64>`)
- Generic: `T`, `U`
- Self in impl: `Self`

---

## 3. Items (Top-Level)

### Use
```zeta
use zeta::frontend::ast::AstNode
use zeta::backend::codegen::LLVMCodegen
```

### Function
```zeta
fn name(params: [T]) -> RetType {
    // body
}
```

### Concept (Trait)
```zeta
concept Add {
    fn add(self, other: Self) -> Self;
}
```

### Impl
```zeta
impl Add for i64 {
    fn add(self, other: i64) -> i64 {
        self + other
    }
}
```

### Struct
```zeta
struct Point {
    x: i64,
    y: i64,
}
```

### Enum
```zeta
enum Color {
    Red,
    Green(i64),
    Blue,
}
```

---

## 4. Expressions

### Literal
- `42`
- `"hello"`
- `true`

### Variable
- `x`

### Method Call
- `40.add(2)`
- `self.add(other)`

### Binary Op
- `a + b`
- `a * b`
- `a == b`

### Format (println support)
- `println("Result: {}", result)`
- `format!("hello {}", name)`

### FString (if needed)
- `"hello {name}"`

---

## 5. Statements

### Let
```zeta
let x = 42
let mut y = 0
```

### Assign
```zeta
x = 10
map[key] = value
```

### If
```zeta
if cond {
    // then
} else {
    // else
}
```

### Return
```zeta
return 42
```

### Expr as statement
```zeta
println("hello")
```

---

## 6. Full Grammar Summary (for AI agents)

**TopLevelItem** = Use | Fn | Concept | Impl | Struct | Enum

**Fn** = "fn" Ident "(" ParamList ")" ("->" Type)? Block

**Concept** = "concept" Ident "{" MethodSig* "}"

**Impl** = "impl" Ident "for" Type "{" Fn* "}"

**Expr** = Lit | Var | Call | BinaryOp | FormatCall | IfExpr

**Call** = Expr "." Ident "(" ArgList ")" | Ident "(" ArgList ")"

**FormatCall** = "println" "(" StringLit "," Expr ")"

**Block** = "{" Stmt* "}"

**Stmt** = Let | Assign | If | Return | ExprStmt

---

## 7. Built-in Intrinsics (Runtime)

- `println(val: i64)`
- `fs::read_to_string(path: str) -> str`
- `std::env::args() -> [str]`

---

## 8. Rules for AI Agents (Strict)

1. Never use `Vec<T>` — use `[T]` for arrays
2. Never use `HashMap` without full qualification if not imported
3. Always include `concept` + `impl` for any method used
4. `println` is the only output function — use `println("text {}", var)`
5. All functions must return `i64` for main
6. No raw strings (`r#"..."#`) — use normal `"..."`
7. No macros except `format!` and `println!`
8. Use `use` for modules
9. Match exactly the AST nodes from parser.z

This file must be referenced in every response when generating Zeta code.

**This is the canonical syntax reference for Zeta v0.4.1+**
