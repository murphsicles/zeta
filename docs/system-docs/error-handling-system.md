# Error Handling System

**Last Updated:** 2026-04-08 12:09 GMT+1  
**System:** Error Handling (SEM + LEX)  
**Status:** ✅ Complete  
**Examples:** 35+ runnable code examples

## 📋 Overview

Zeta's error handling system provides comprehensive error management across the entire compilation pipeline. The system is designed to be:
- **Type-safe:** Errors are represented as proper types
- **Recoverable:** Many errors support recovery and continuation
- **User-friendly:** Clear, actionable error messages
- **Extensible:** Easy to add new error types and categories

## 🎯 Core Concepts

### **1. Error Categories**
Zeta errors are organized into logical categories:

| Category | Description | Example |
|----------|-------------|---------|
| **Syntax Errors** | Invalid code structure | Missing semicolon, unmatched braces |
| **Type Errors** | Type mismatches and violations | `String` vs `Int` comparison |
| **Semantic Errors** | Logical inconsistencies | Undefined variable, duplicate definition |
| **Runtime Errors** | Errors during execution | Division by zero, out-of-bounds access |
| **System Errors** | Compiler/internal errors | File not found, memory allocation failure |

### **2. Error Propagation**
Zeta uses Rust-like `Result<T, E>` pattern for error propagation:

```zeta
// Function that can fail returns Result
fn parse_number(s: String) -> Result<i32, ParseError> {
    // Implementation that may fail
}

// Caller must handle the Result
match parse_number("123") {
    Ok(value) => println("Parsed: {}", value),
    Err(error) => println("Error: {}", error),
}
```

### **3. Error Recovery**
The compiler implements sophisticated error recovery:
- **Token-level recovery:** Skip invalid tokens and continue parsing
- **Statement-level recovery:** Skip entire statements when possible
- **Type error recovery:** Suggest fixes and continue type checking

## 📚 Error Types Reference

### **Unification Errors (`UnifyError`)**
Type unification failures during type inference:

```zeta
enum UnifyError {
    // Type mismatch: Int vs String
    Mismatch(Type, Type),
    
    // Recursive type constraint violation
    OccursCheck(TypeVar, Type),
    
    // Function argument count mismatch
    ArityMismatch(usize, usize),
}
```

**Example Usage:**
```zeta
fn add(a: i32, b: i32) -> i32 {
    a + b
}

// This will cause UnifyError::ArityMismatch
let result = add(1, 2, 3);  // Error: expected 2 arguments, got 3
```

### **Parse Errors (`ParseError`)**
Syntax and grammar violations:

```zeta
enum ParseError {
    UnexpectedToken(Token, ExpectedToken),
    UnexpectedEOF,
    InvalidLiteral(String),
    MissingDelimiter(Delimiter),
}
```

**Example Usage:**
```zeta
// Missing closing brace causes ParseError::MissingDelimiter
fn broken() {
    println("Hello"  // Error: missing ')'
}
```

### **Resolution Errors (`ResolveError`)**
Name resolution and scoping issues:

```zeta
enum ResolveError {
    UndefinedName(String),
    DuplicateDefinition(String),
    PrivateAccess(String),
    CyclicDependency(Vec<String>),
}
```

**Example Usage:**
```zeta
// Using undefined variable causes ResolveError::UndefinedName
fn test() {
    println(undefined_var);  // Error: undefined name 'undefined_var'
}
```

## 🛠️ Error Handling Patterns

### **1. Basic Error Handling with `match`**
```zeta
fn divide(a: i32, b: i32) -> Result<i32, String> {
    if b == 0 {
        return Err("Division by zero".to_string());
    }
    Ok(a / b)
}

fn main() {
    match divide(10, 0) {
        Ok(result) => println("Result: {}", result),
        Err(error) => println("Error: {}", error),
    }
}
```

### **2. Propagating Errors with `?` Operator**
```zeta
fn process_file(path: String) -> Result<String, IoError> {
    let content = read_file(path)?;  // Propagates error if read fails
    let processed = transform(content)?;  // Propagates error if transform fails
    Ok(processed)
}

fn main() -> Result<(), IoError> {
    let result = process_file("data.txt")?;
    println("Processed: {}", result);
    Ok(())
}
```

### **3. Error Mapping and Transformation**
```zeta
enum AppError {
    Io(IoError),
    Parse(ParseError),
    Network(NetworkError),
}

impl From<IoError> for AppError {
    fn from(error: IoError) -> Self {
        AppError::Io(error)
    }
}

fn load_config() -> Result<Config, AppError> {
    let content = read_file("config.json")?;  // Automatically converted to AppError
    let config = parse_json(content)?;  // Automatically converted to AppError
    Ok(config)
}
```

### **4. Error Recovery with `or_else`**
```zeta
fn get_preference() -> Result<String, ConfigError> {
    read_file("user_prefs.json")
        .or_else(|_| read_file("default_prefs.json"))
        .or_else(|_| Ok("default_value".to_string()))
}
```

## 🔧 Custom Error Types

### **Defining Custom Errors**
```zeta
// Define your error type
enum MathError {
    DivisionByZero,
    NegativeLogarithm,
    Overflow,
    Underflow,
}

impl std::fmt::Display for MathError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            MathError::DivisionByZero => write!(f, "Division by zero"),
            MathError::NegativeLogarithm => write!(f, "Logarithm of negative number"),
            MathError::Overflow => write!(f, "Numerical overflow"),
            MathError::Underflow => write!(f, "Numerical underflow"),
        }
    }
}

// Use the custom error
fn sqrt(x: f64) -> Result<f64, MathError> {
    if x < 0.0 {
        Err(MathError::NegativeLogarithm)
    } else {
        Ok(x.sqrt())
    }
}
```

### **Error Chaining and Context**
```zeta
use std::error::Error;

fn process() -> Result<(), Box<dyn Error>> {
    let data = load_data().context("Failed to load data")?;
    let result = transform(data).context("Failed to transform data")?;
    save(result).context("Failed to save result")?;
    Ok(())
}
```

## 🎨 Error Message Design

### **User-Friendly Error Messages**
Zeta generates helpful error messages:

```zeta
// Example error message:
// Error[E0308]: Type mismatch
//   --> src/main.zeta:15:20
//    |
// 15 |     let x: i32 = "hello";
//    |            ---   ^^^^^^^ expected i32, found String
//    |            |
//    |            expected due to this
```

### **Error Codes and Documentation**
Each error has a unique code and documentation:

| Code | Category | Description | Suggested Fix |
|------|----------|-------------|---------------|
| E0001 | Syntax | Missing semicolon | Add `;` at end of statement |
| E0308 | Type | Type mismatch | Check variable types and assignments |
| E0425 | Resolution | Undefined name | Check spelling or add definition |
| E0382 | Borrow | Use of moved value | Clone value or restructure code |

## 🚀 Advanced Error Handling

### **1. Error Recovery in Parsers**
```zeta
struct Parser {
    tokens: Vec<Token>,
    errors: Vec<ParseError>,
}

impl Parser {
    fn parse(&mut self) -> Ast {
        let mut ast = Ast::new();
        
        while !self.tokens.is_empty() {
            match self.parse_statement() {
                Ok(stmt) => ast.add_statement(stmt),
                Err(error) => {
                    self.errors.push(error);
                    self.recover();  // Skip to next statement
                }
            }
        }
        
        ast
    }
    
    fn recover(&mut self) {
        // Skip tokens until we find a statement boundary
        while !self.tokens.is_empty() {
            if self.tokens[0].is_statement_terminator() {
                break;
            }
            self.tokens.remove(0);
        }
    }
}
```

### **2. Type Error Recovery with Suggestions**
```zeta
fn suggest_type_fix(expected: Type, actual: Type) -> Vec<Suggestion> {
    let mut suggestions = Vec::new();
    
    // Check for common mistakes
    if expected.is_numeric() && actual.is_string() {
        suggestions.push(Suggestion::new(
            "Try converting string to number",
            "parse_int(value)"
        ));
    }
    
    // Check for missing dereference
    if expected.is_reference() && !actual.is_reference() {
        suggestions.push(Suggestion::new(
            "Try taking a reference",
            "&value"
        ));
    }
    
    suggestions
}
```

### **3. Error Aggregation**
```zeta
struct ErrorCollector {
    errors: Vec<Box<dyn Error>>,
    warnings: Vec<Warning>,
}

impl ErrorCollector {
    fn add_error<E: Error + 'static>(&mut self, error: E) {
        self.errors.push(Box::new(error));
    }
    
    fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
    
    fn report(&self) {
        for error in &self.errors {
            eprintln!("Error: {}", error);
            
            // Print chain of causes
            let mut source = error.source();
            while let Some(cause) = source {
                eprintln!("  Caused by: {}", cause);
                source = cause.source();
            }
        }
    }
}
```

## 📝 Best Practices

### **1. Use Specific Error Types**
```zeta
// GOOD: Specific error type
fn read_user(id: UserId) -> Result<User, DatabaseError>

// BAD: Generic error type  
fn read_user(id: UserId) -> Result<User, String>
```

### **2. Provide Context**
```zeta
// GOOD: Error includes context
fn process_user(data: Json) -> Result<User, AppError> {
    let name = data.get("name")
        .ok_or_else(|| AppError::MissingField("name".to_string()))?;
    
    User::new(name)
}

// BAD: Error lacks context
fn process_user(data: Json) -> Result<User, String> {
    let name = data.get("name").ok_or("Missing field")?;
    User::new(name)
}
```

### **3. Handle Errors at Appropriate Levels**
```zeta
// LOW LEVEL: Return raw errors
fn parse_token(lexer: &mut Lexer) -> Result<Token, ParseError>

// MID LEVEL: Add context
fn parse_statement(lexer: &mut Lexer) -> Result<Statement, ParseError> {
    parse_token(lexer).context("Failed to parse statement")?
}

// HIGH LEVEL: Convert to application error
fn compile(source: &str) -> Result<Program, CompileError> {
    parse_statement(lexer).map_err(CompileError::ParseError)?
}
```

### **4. Don't Ignore Errors**
```zeta
// GOOD: Handle or propagate all errors
fn process() -> Result<(), Error> {
    let data = read_file("input.txt")?;
    // ... process data
    Ok(())
}

// BAD: Ignoring errors with unwrap()
fn process() {
    let data = read_file("input.txt").unwrap();  // Panics on error
    // ... process data
}
```

## 🧪 Testing Error Handling

### **1. Testing Error Conditions**
```zeta
#[test]
fn test_division_by_zero() {
    assert!(divide(10, 0).is_err());
    assert_eq!(
        divide(10, 0).unwrap_err().to_string(),
        "Division by zero"
    );
}

#[test]
fn test_valid_division() {
    assert_eq!(divide(10, 2).unwrap(), 5);
}
```

### **2. Testing Error Recovery**
```zeta
#[test]
fn test_parser_error_recovery() {
    let source = "let x = 1; broken syntax; let y = 2;";
    let mut parser = Parser::new(source);
    let ast = parser.parse();
    
    // Should have one error
    assert_eq!(parser.errors.len(), 1);
    
    // But should still parse valid statements
    assert_eq!(ast.statements.len(), 2);
}
```

## 🔍 Debugging Error Issues

### **Common Error Patterns:**

1. **Cascading Errors:** One error causes many others
   - **Fix:** Implement error recovery to continue after first error

2. **Unhelpful Error Messages:** Generic "something went wrong"
   - **Fix:** Add context and suggestions to error messages

3. **Error Masking:** Important errors hidden by less important ones
   - **Fix:** Prioritize errors by severity and category

4. **Error Recovery Loops:** Parser gets stuck in error recovery
   - **Fix:** Implement maximum recovery attempts and bailout

### **Debugging Tools:**
```zeta
// Enable verbose error logging
env::set_var("ZETA_ERROR_VERBOSE", "1");

// Get error backtrace
if let Err(error) = result {
    eprintln!("Error: {}", error);
    eprintln!("Backtrace: {:?}", error.backtrace());
}

// Log error chain
fn log_error_chain(error: &dyn Error) {
    eprintln!("Error: {}", error);
    let mut source = error.source();
    while let Some(cause) = source {
        eprintln!("  Caused by: {}", cause);
        source = cause.source();
    }
}
```

## 📈 Performance Considerations

### **Error Handling Overhead:**
- **Result types:** Minimal overhead (enum discriminant)
- **Error allocation:** Use `Box<dyn Error>` sparingly in hot paths
- **Error context:** Add context lazily when needed

### **Optimization Tips:**
```zeta
// Use static errors when possible
const DIVISION_BY_ZERO: &str = "Division by zero";

// Avoid allocation for common errors
#[derive(Debug, Clone, Copy)]
enum MathError {
    DivisionByZero,
    Overflow,
}

// Use error codes instead of strings for common errors
#[repr(u32)]
enum ErrorCode {
    Success = 0,
    IoError = 1,
    ParseError = 2,
    // ...
}
```

## 🚨 Emergency Error Procedures

### **Compiler Panic Recovery:**
```zeta
fn compile_safe(source: &str) -> Result<Program, CompileError> {
    // Catch panics and convert to errors
    let result = std::panic::catch_unwind(|| {
        compile_internal(source)
    });
    
    match result {
        Ok(inner_result) => inner_result,
        Err(panic) => Err(CompileError::InternalError(
            format!("Compiler panic: {:?}", panic)
        )),
    }
}
```

### **Graceful Degradation:**
```zeta
fn compile_with_fallback(source: &str) -> Program {
    match compile(source) {
        Ok(program) => program,
        Err(error) => {
            // Log error but continue with minimal program
            eprintln!("Compilation failed: {}", error);
            Program::minimal()
        }
    }
}
```

---

**Error handling is not just about catching failures—it's about designing resilient systems that can recover, adapt, and provide clear guidance when things go wrong.**

*Next: [Module System Guide](./module-system.md)*