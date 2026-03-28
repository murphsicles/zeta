# Error Handling System
## Comprehensive Guide to Zeta's Error Management

**Last Updated:** 2026-03-28  
**System Status:** Active Development (v0.3.9)

---

## 📋 Overview

Zeta's error handling system is designed for:
- **Compile-time safety** - Catch errors before execution
- **User-friendly diagnostics** - Clear, actionable error messages
- **Error recovery** - Continue parsing/compilation after errors
- **Runtime reliability** - Predictable failure modes

### Design Philosophy
> "Fail fast, fail informatively, recover gracefully when possible."

---

## 🏗️ Architecture

### Error Handling Layers:

```
┌─────────────────────────────────────────┐
│            User Interface               │
│  (Command Line, LSP, IDE Integration)  │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│       Error Reporting & Formatting      │
│  (Pretty printing, source highlighting) │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│          Error Classification           │
│  (Parse, Type, Borrow, Runtime errors)  │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│          Error Recovery Engine          │
│  (Skip invalid code, continue parsing)  │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│         Error Type Definitions          │
│  (Structured error data, error codes)   │
└─────────────────────────────────────────┘
```

---

## 📊 Error Categories

### 1. Parse Errors
**When:** During source code parsing  
**Causes:** Syntax errors, invalid tokens, mismatched delimiters  
**Recovery:** Skip to next valid statement/function

```rust
// Example parse errors:
fn main() {
    let x = 5  // Missing semicolon
    let y = "unclosed string
    if (x > 3 {  // Missing closing parenthesis
}
```

### 2. Type Errors
**When:** During type checking and inference  
**Causes:** Type mismatches, undefined functions, invalid operations  
**Recovery:** Mark expression as error type, continue checking

```rust
// Example type errors:
fn main() {
    let x: i32 = "hello";  // Type mismatch
    let y = x + "world";   // Invalid operation
    undefined_function();  // Undefined function
}
```

### 3. Borrow Checker Errors
**When:** During ownership analysis  
**Causes:** Multiple mutable borrows, use after move, dangling references  
**Recovery:** Reject program (cannot safely continue)

```rust
// Example borrow errors:
fn main() {
    let mut x = vec![1, 2, 3];
    let r1 = &mut x;
    let r2 = &mut x;  // Multiple mutable borrows
    
    let y = String::from("hello");
    let z = y;
    println!("{}", y);  // Use after move
}
```

### 4. Runtime Errors
**When:** During program execution  
**Causes:** Division by zero, out-of-bounds access, null dereference  
**Recovery:** Program termination or exception handling (if enabled)

```rust
// Example runtime errors:
fn main() {
    let x = 10 / 0;           // Division by zero
    let arr = [1, 2, 3];
    let y = arr[5];           // Out of bounds
    let ptr: *const i32 = std::ptr::null();
    unsafe { *ptr };          // Null dereference
}
```

---

## 🔧 Error Type Definitions

### Base Error Structure:

```rust
// src/error/mod.rs

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    Parse(ParseError),
    Type(TypeError),
    Borrow(BorrowError),
    Runtime(RuntimeError),
    Internal(InternalError),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub span: Span,           // Source location
    pub kind: ParseErrorKind,
    pub message: String,      // Human-readable message
    pub suggestion: Option<String>, // Suggested fix
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorKind {
    UnexpectedToken { expected: Vec<String>, found: String },
    UnclosedDelimiter { delimiter: char },
    MissingSemicolon,
    InvalidNumberLiteral,
    InvalidStringLiteral,
    UnknownKeyword,
    // ... more error kinds
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub file_id: FileId,
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Position {
    pub line: u32,
    pub column: u32,
    pub byte_offset: u32,
}
```

### Type Error Structure:

```rust
#[derive(Debug, Clone, PartialEq)]
pub struct TypeError {
    pub span: Span,
    pub kind: TypeErrorKind,
    pub context: TypeContext, // Additional type information
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeErrorKind {
    Mismatch { expected: Type, found: Type },
    UnboundVariable { name: String },
    UndefinedFunction { name: String },
    InvalidOperation { op: String, left: Type, right: Type },
    ArityMismatch { expected: usize, found: usize },
    // ... more type error kinds
}
```

---

## 🚀 Error Recovery System

### Parser Error Recovery:

```rust
// src/frontend/parser/error_recovery.rs

/// Skip to next recoverable point in source code
pub fn skip_to_recoverable(input: &str) -> IResult<&str, ()> {
    alt((
        // Skip to next semicolon
        value((), preceded(take_until(";"), tag(";"))),
        // Skip to next closing brace
        value((), preceded(take_until("}"), tag("}"))),
        // Skip to next function definition
        value((), preceded(take_until("fn "), tag("fn "))),
        // Skip to end of file as last resort
        value((), rest),
    ))(input)
}

/// Parse with error recovery
pub fn parse_with_recovery(input: &str) -> IResult<&str, (Vec<AstNode>, Vec<ParseError>)> {
    let mut errors = Vec::new();
    let mut remaining = input;
    let mut asts = Vec::new();
    
    while !remaining.is_empty() {
        match parse_function(remaining) {
            Ok((next, ast)) => {
                asts.push(ast);
                remaining = next;
            }
            Err(nom::Err::Error(e) | nom::Err::Failure(e)) => {
                // Record error
                let error = convert_nom_error(e, remaining);
                errors.push(error);
                
                // Skip to recoverable point
                match skip_to_recoverable(remaining) {
                    Ok((next, _)) => remaining = next,
                    Err(_) => break, // Cannot recover further
                }
            }
            Err(nom::Err::Incomplete(_)) => break,
        }
    }
    
    Ok((remaining, (asts, errors)))
}
```

### Type Checker Error Recovery:

```rust
// src/middle/resolver/error_recovery.rs

impl Resolver {
    pub fn typecheck_with_recovery(&mut self, asts: &[AstNode]) -> (bool, Vec<TypeError>) {
        let mut errors = Vec::new();
        let mut all_ok = true;
        
        for ast in asts {
            match self.typecheck_expr(ast) {
                Ok(_) => {}
                Err(err) => {
                    errors.push(err);
                    all_ok = false;
                    
                    // Mark expression as error type to continue checking
                    self.mark_as_error(ast);
                }
            }
        }
        
        (all_ok, errors)
    }
    
    fn mark_as_error(&mut self, ast: &AstNode) {
        // Insert error type into type environment
        // This allows checking to continue for other parts
        match ast {
            AstNode::Var(name) => {
                self.env.insert(name.clone(), Type::Error);
            }
            // ... handle other AST nodes
        }
    }
}
```

---

## 📝 Error Reporting

### Pretty Error Formatting:

```rust
// src/error/reporting.rs

pub fn format_error(error: &Error, source: &str) -> String {
    match error {
        Error::Parse(parse_error) => format_parse_error(parse_error, source),
        Error::Type(type_error) => format_type_error(type_error, source),
        // ... other error types
    }
}

fn format_parse_error(error: &ParseError, source: &str) -> String {
    let lines: Vec<&str> = source.lines().collect();
    let line_num = error.span.start.line as usize;
    
    format!(
        "Parse error at {}:{}: {}\n\n{}\n{}{}\n",
        error.span.start.line,
        error.span.start.column,
        error.message,
        lines.get(line_num - 1).unwrap_or(&""),
        " ".repeat(error.span.start.column as usize - 1),
        "^".repeat((error.span.end.column - error.span.start.column) as usize)
    )
}
```

### Example Error Output:

```
Parse error at 3:15: Missing semicolon

    let x = 5
              ^
Suggestion: Add a semicolon: `let x = 5;`
```

---

## 🛠️ Error Codes Reference

### Parse Error Codes (PXXX):

| Code | Description | Severity | Recovery |
|------|-------------|----------|----------|
| P001 | Unexpected token | Error | Skip token |
| P002 | Unclosed delimiter | Error | Insert closing delimiter |
| P003 | Missing semicolon | Warning | Auto-insert |
| P004 | Invalid number literal | Error | Skip literal |
| P005 | Invalid string literal | Error | Skip to next quote |
| P006 | Unknown keyword | Error | Skip keyword |

### Type Error Codes (TXXX):

| Code | Description | Severity | Recovery |
|------|-------------|----------|----------|
| T001 | Type mismatch | Error | Mark as error type |
| T002 | Unbound variable | Error | Suggest similar names |
| T003 | Undefined function | Error | Skip call |
| T004 | Invalid operation | Error | Mark as error type |
| T005 | Arity mismatch | Error | Skip call |
| T006 | Missing return | Warning | Add implicit return |

### Borrow Error Codes (BXXX):

| Code | Description | Severity | Recovery |
|------|-------------|----------|----------|
| B001 | Multiple mutable borrows | Error | Reject |
| B002 | Use after move | Error | Reject |
| B003 | Dangling reference | Error | Reject |
| B004 | Borrow escapes scope | Error | Reject |

### Runtime Error Codes (RXXX):

| Code | Description | Severity | Recovery |
|------|-------------|----------|----------|
| R001 | Division by zero | Fatal | Terminate |
| R002 | Out of bounds | Fatal | Terminate |
| R003 | Null dereference | Fatal | Terminate |
| R004 | Stack overflow | Fatal | Terminate |
| R005 | Heap exhaustion | Fatal | Terminate |

---

## 🔄 Error Recovery Procedures

### For Developers:

1. **Parse Error Recovery:**
```rust
// In your parser:
let result = parse_with_recovery(source_code);
match result {
    Ok((remaining, (asts, errors))) => {
        if !errors.is_empty() {
            report_errors(errors);
        }
        // Continue with asts (may be incomplete)
    }
    Err(_) => {
        // Complete failure
        report_fatal_error();
    }
}
```

2. **Type Error Recovery:**
```rust
// In your type checker:
let (success, errors) = resolver.typecheck_with_recovery(&asts);
if !success {
    report_type_errors(errors);
    // Continue with partial type information
}
```

3. **Borrow Checker Recovery:**
```rust
// Borrow errors are generally not recoverable
let result = borrow_checker.check(&asts);
if let Err(errors) = result {
    report_borrow_errors(errors);
    return Err("Borrow check failed");
}
```

### For Users:

1. **Parse Errors:**
   - Check for missing semicolons, parentheses, braces
   - Verify string literals are properly closed
   - Ensure keywords are spelled correctly

2. **Type Errors:**
   - Check variable declarations match usage
   - Verify function arguments match parameter types
   - Ensure operations are valid for the types involved

3. **Borrow Errors:**
   - Avoid multiple mutable references to same data
   - Don't use variables after moving them
   - Ensure references don't outlive their data

---

## 🧪 Testing Error Handling

### Unit Tests:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_error_recovery() {
        let source = r#"
            fn foo() {
                let x = 5  // Missing semicolon
                let y = 10;
            }
            
            fn bar() {
                println!("Hello");
            }
        "#;
        
        let result = parse_with_recovery(source);
        assert!(result.is_ok());
        
        let (_, (asts, errors)) = result.unwrap();
        assert_eq!(errors.len(), 1);  // One parse error
        assert_eq!(asts.len(), 2);    // Both functions parsed
    }
    
    #[test]
    fn test_type_error_detection() {
        let source = r#"
            fn main() {
                let x: i32 = "hello";  // Type mismatch
                let y = x + "world";   // Invalid operation
            }
        "#;
        
        let asts = parse(source).unwrap();
        let mut resolver = Resolver::new();
        let (success, errors) = resolver.typecheck_with_recovery(&asts);
        
        assert!(!success);
        assert_eq!(errors.len(), 2);  // Two type errors
    }
}
```

### Integration Tests:

```rust
#[test]
fn test_end_to_end_error_handling() {
    // Test complete pipeline with errors
    let source = TEST_SOURCE_WITH_ERRORS;
    
    // Parse with recovery
    let (asts, parse_errors) = parse_with_recovery(source);
    
    // Type check with recovery
    let mut resolver = Resolver::new();
    let (type_ok, type_errors) = resolver.typecheck_with_recovery(&asts);
    
    // Borrow check (no recovery)
    let borrow_result = borrow_checker.check(&asts);
    
    // Verify error reporting
    let all_errors = combine_errors(parse_errors, type_errors, borrow_result.err());
    assert!(!all_errors.is_empty());
    
    // Verify we can still generate code for valid parts
    if !asts.is_empty() {
        let mir = generate_mir(&asts);
        assert!(mir.is_some());
    }
}
```

---

## 🚨 Emergency Procedures

### Critical Errors (Cannot Continue):

1. **Internal Compiler Error:**
   - Log detailed diagnostic information
   - Save state for debugging
   - Exit gracefully with error code

2. **Memory Exhaustion:**
   - Free any allocated resources
   - Write minimal error message
   - Exit with out-of-memory code

3. **Corrupted Internal State:**
   - Attempt to save debugging information
   - Do not attempt to continue
   - Exit with internal error code

### Recovery Procedures:

```rust
fn handle_critical_error(error: InternalError) -> ! {
    // Log error details
    eprintln!("Internal compiler error: {:?}", error);
    
    // Save debugging information if possible
    if let Some(debug_info) = collect_debug_info() {
        save_debug_info(&debug_info);
    }
    
    // Exit with appropriate code
    std::process::exit(1);
}
```

---

## 📈 Performance Considerations

### Error Recovery Overhead:

1. **Parse Error Recovery:**
   - Minimal overhead when no errors
   - Linear scanning for recovery points
   - Memory proportional to error count

2. **Type Error Recovery:**
   - Additional type environment entries
   - Continued checking of erroneous code
   - May affect optimization decisions

3. **Error Reporting:**
   - Source code indexing for pretty printing
   - Error message formatting
   - Suggestion generation

### Optimization Strategies:

```rust
// Lazy error reporting - only format when needed
struct LazyError {
    data: ErrorData,
    formatted: OnceCell<String>,
}

impl LazyError {
    fn format(&self) -> &str {
        self.formatted.get_or_init(|| {
            format_error(&self.data)
        })
    }
}

// Batch error processing
fn process_errors_batch(errors: Vec<Error>) -> Vec<FormattedError> {
    errors.into_par_iter()  // Parallel processing
        .map(|e| format_error(&e))
        .collect()
}
```

---

## 🔮 Future Improvements

### Planned Enhancements:

1. **Better Error Suggestions:**
   - Machine learning for fix suggestions
   - Context-aware error correction
   - Automated quick fixes

2. **Interactive Error Handling:**
   - REPL with error correction
   - Live error highlighting
   - On-the-fly fix application

3. **Error Telemetry:**
   - Anonymous error reporting
   - Common error patterns analysis
   - Automatic fix generation

4. **Multilingual Error Messages:**
   - Localized error messages
   - Culture-specific examples
   - Accessibility improvements

### Research Areas:
- Formal verification of error recovery correctness
- Statistical analysis of error patterns
- AI-assisted error diagnosis
- Predictive error prevention

---

## 📚 Further Reading

### Related Documentation:
- [Module System](./module-system.md)
- [Type System](./type-system.md)
- [Parser System](./parser-system.md)
- [Tooling](./tooling.md)

### External Resources:
- [Rust Error Handling Guide](https://doc.rust-lang.org/book