# Type System
## Comprehensive Guide to Zeta's Type System

**Last Updated:** 2026-03-28  
**System Status:** Active Development (v0.3.9)

---

## 📋 Overview

Zeta's type system provides:
- **Static type checking** - Catch errors at compile time
- **Type inference** - Reduce annotation burden
- **Algebraic types** - Expressive type combinations
- **Generic programming** - Reusable code across types
- **Type safety** - Prevent runtime type errors
- **Zero-cost abstractions** - No runtime type information

### Design Philosophy
> "Types are proofs. The type checker is the theorem prover. Correct by construction."

---

## 🏗️ Architecture

### Type System Layers:

```
┌─────────────────────────────────────────┐
│          Type Representation            │
│  (Algebraic types, type variables)      │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│          Type Inference Engine          │
│  (Hindley-Milner, constraint collection)│
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│          Unification Algorithm          │
│  (Solve type equations, occurs check)   │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│          Type Environment               │
│  (Variable bindings, substitution)      │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│          Type Checking                  │
│  (Verify program against type rules)    │
└─────────────────────────────────────────┘
```

---

## 📊 Type Hierarchy

### Primitive Types:

```zeta
// Integer types (signed)
i8   // 8-bit signed integer (-128 to 127)
i16  // 16-bit signed integer
i32  // 32-bit signed integer
i64  // 64-bit signed integer

// Integer types (unsigned)
u8   // 8-bit unsigned integer (0 to 255)
u16  // 16-bit unsigned integer
u32  // 32-bit unsigned integer
u64  // 64-bit unsigned integer

// Floating-point types
f32  // 32-bit floating point (single precision)
f64  // 64-bit floating point (double precision)

// Other primitives
bool // Boolean (true or false)
char // Unicode scalar value (4 bytes)
str  // String slice (UTF-8 encoded)
```

### Compound Types:

```zeta
// Arrays - fixed size, homogeneous
[i32; 10]      // Array of 10 i32s
[[f64; 3]; 4]  // 4x3 matrix of f64s

// Slices - dynamic view into array
[i32]          // Slice of i32s
[&str]         // Slice of string slices

// Tuples - fixed size, heterogeneous
(i32, bool)    // Pair of i32 and bool
()             // Unit type (empty tuple)
(i32,)         // Singleton tuple (note comma)

// Pointers
*i32           // Raw pointer to i32
*const i32     // Immutable raw pointer
*mut i32       // Mutable raw pointer

// References
&i32           // Immutable reference to i32
&mut i32       // Mutable reference to i32
```

### Function Types:

```zeta
// Function that takes i32 and returns i32
(i32) -> i32

// Multiple parameters
(i32, i32) -> i32

// No parameters, returns unit
() -> ()

// Generic function type
<T>(T) -> T

// Higher-order function
((i32) -> i32) -> i32
```

### Named Types:

```zeta
// Struct types
Point          // Simple struct
Point<i32>     // Generic struct instantiated
Vec<String>    // Standard library vector

// Enum types
Option<i32>    // Optional i32
Result<i32, String> // Result with i32 success, String error

// Concept (trait) types
Display        // Display concept
Iterator<Item = i32> // Iterator yielding i32s
```

---

## 🎯 Type Representation

### Algebraic Type Definition:

```rust
// src/middle/types/mod.rs

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    // Primitive numeric types
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    
    // Other primitives
    Bool, Char, Str,
    
    // Compound types
    Array(Box<Type>, usize),    // [T; N]
    Slice(Box<Type>),           // [T]
    Tuple(Vec<Type>),           // (T1, T2, ...)
    Ptr(Box<Type>),             // *T
    Ref(Box<Type>, Mutability), // &T, &mut T
    
    // Named types
    Named(String, Vec<Type>),   // Name<T1, T2, ...>
    
    // Function types
    Function(Vec<Type>, Box<Type>), // (T1, T2, ...) -> R
    
    // Type variables (for inference)
    Variable(TypeVar),
    
    // Error type
    Error,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mutability {
    Immutable,
    Mutable,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVar(u32);

impl TypeVar {
    pub fn fresh() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        TypeVar(COUNTER.fetch_add(1, Ordering::Relaxed))
    }
}
```

### Type Operations:

```rust
impl Type {
    /// Check if type contains type variables
    pub fn contains_vars(&self) -> bool {
        match self {
            Type::Variable(_) => true,
            Type::Array(inner, _) => inner.contains_vars(),
            Type::Slice(inner) => inner.contains_vars(),
            Type::Tuple(types) => types.iter().any(|t| t.contains_vars()),
            Type::Ptr(inner) => inner.contains_vars(),
            Type::Ref(inner, _) => inner.contains_vars(),
            Type::Named(_, args) => args.iter().any(|t| t.contains_vars()),
            Type::Function(params, ret) => {
                params.iter().any(|t| t.contains_vars()) || ret.contains_vars()
            }
            _ => false,
        }
    }
    
    /// Get display name for type
    pub fn display_name(&self) -> String {
        match self {
            Type::I32 => "i32".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Array(inner, size) => format!("[{}; {}]", inner.display_name(), size),
            Type::Tuple(types) => {
                let inner = types.iter()
                    .map(|t| t.display_name())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", inner)
            }
            Type::Function(params, ret) => {
                let params_str = params.iter()
                    .map(|t| t.display_name())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({}) -> {}", params_str, ret.display_name())
            }
            Type::Variable(var) => format!("T{}", var.0),
            // ... other cases
        }
    }
}
```

---

## 🔍 Type Inference

### Hindley-Milner Type Inference:

```zeta
// Type inference examples
let x = 42;           // x: i32 (integer literal)
let y = 3.14;         // y: f64 (float literal)
let z = x + y;        // ERROR: i32 + f64 not allowed

let a = true;         // a: bool
let b = !a;           // b: bool

let c = "hello";      // c: &str
let d = c.len();      // d: usize (method return type)

// Function type inference
fn add(x, y) {        // x: ?T, y: ?T -> ?T
    x + y             // Constraint: ?T supports +
}

let sum = add(1, 2);  // sum: i32 (unified with i32)
```

### Constraint Collection:

```rust
// Type inference collects constraints
struct Infer {
    env: TypeEnv,
    constraints: Vec<(Type, Type)>,
}

impl Infer {
    fn infer_expr(&mut self, expr: &AstNode) -> Result<Type, TypeError> {
        match expr {
            AstNode::Lit(n) => {
                // Integer literal: infer appropriate integer type
                if *n >= i32::MIN as i64 && *n <= i32::MAX as i64 {
                    Ok(Type::I32)
                } else {
                    Ok(Type::I64)
                }
            }
            
            AstNode::BinaryOp { op, left, right } => {
                let left_ty = self.infer_expr(left)?;
                let right_ty = self.infer_expr(right)?;
                
                // Add constraint: left_ty == right_ty
                self.constraints.push((left_ty.clone(), right_ty.clone()));
                
                // Determine result type based on operator
                match op.as_str() {
                    "+" | "-" | "*" | "/" => Ok(left_ty),
                    "==" | "!=" => Ok(Type::Bool),
                    _ => Err(TypeError::UnknownOperator(op.clone())),
                }
            }
            
            AstNode::Var(name) => {
                self.env.lookup(name)
                    .ok_or_else(|| TypeError::UnboundVariable(name.clone()))
            }
            
            // ... other cases
        }
    }
}
```

### Type Environment:

```rust
struct TypeEnv {
    // Variable to type mapping
    variables: HashMap<String, Type>,
    
    // Type variable substitutions
    substitution: Substitution,
    
    // Current scope level
    scope_level: u32,
    
    // Parent environment (for nested scopes)
    parent: Option<Rc<TypeEnv>>,
}

impl TypeEnv {
    fn lookup(&self, name: &str) -> Option<Type> {
        // Check current scope
        if let Some(ty) = self.variables.get(name) {
            return Some(self.substitution.apply(ty));
        }
        
        // Check parent scopes
        if let Some(parent) = &self.parent {
            parent.lookup(name)
        } else {
            None
        }
    }
    
    fn insert(&mut self, name: String, ty: Type) {
        self.variables.insert(name, ty);
    }
}
```

---

## 🔗 Unification Algorithm

### Substitution and Unification:

```rust
#[derive(Debug, Clone, Default)]
pub struct Substitution {
    mapping: HashMap<TypeVar, Type>,
}

impl Substitution {
    /// Apply substitution to a type
    pub fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Variable(var) => self.mapping.get(var)
                .cloned()
                .unwrap_or(Type::Variable(var.clone())),
            Type::Array(inner, size) => Type::Array(Box::new(self.apply(inner)), *size),
            Type::Function(params, ret) => Type::Function(
                params.iter().map(|p| self.apply(p)).collect(),
                Box::new(self.apply(ret)),
            ),
            // ... other cases
            _ => ty.clone(),
        }
    }
    
    /// Unify two types
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> Result<(), UnifyError> {
        let t1 = self.apply(t1);
        let t2 = self.apply(t2);
        
        match (&t1, &t2) {
            // Same primitive type
            (Type::I64, Type::I64) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),
            
            // Type variable cases
            (Type::Variable(a), Type::Variable(b)) if a == b => Ok(()),
            (Type::Variable(a), _) => {
                if self.occurs_check(a, &t2) {
                    Err(UnifyError::OccursCheck(a.clone(), t2))
                } else {
                    self.mapping.insert(a.clone(), t2);
                    Ok(())
                }
            }
            (_, Type::Variable(b)) => self.unify(&t2, &t1),
            
            // Array types
            (Type::Array(inner1, size1), Type::Array(inner2, size2)) => {
                if size1 != size2 {
                    return Err(UnifyError::Mismatch(t1, t2));
                }
                self.unify(inner1, inner2)
            }
            
            // Function types
            (Type::Function(params1, ret1), Type::Function(params2, ret2)) => {
                if params1.len() != params2.len() {
                    return Err(UnifyError::ArityMismatch(params1.len(), params2.len()));
                }
                for (p1, p2) in params1.iter().zip(params2) {
                    self.unify(p1, p2)?;
                }
                self.unify(ret1, ret2)
            }
            
            // Mismatch
            _ => Err(UnifyError::Mismatch(t1, t2)),
        }
    }
    
    /// Occurs check prevents infinite types
    fn occurs_check(&self, var: &TypeVar, ty: &Type) -> bool {
        let ty = self.apply(ty);
        match &ty {
            Type::Variable(v) => v == var,
            Type::Array(inner, _) => self.occurs_check(var, inner),
            Type::Function(params, ret) => {
                params.iter().any(|p| self.occurs_check(var, p)) ||
                self.occurs_check(var, ret)
            }
            _ => false,
        }
    }
}
```

### Unification Examples:

```zeta
// Example 1: Simple unification
let x: ?T = 42;       // ?T = i32
let y: ?U = x;        // ?U = ?T = i32

// Example 2: Function unification
fn apply(f: (?A) -> ?B, x: ?A) -> ?B {
    f(x)
}

// Calling: apply(|x| x + 1, 42)
// Constraints:
//   ?A = i32 (from 42)
//   (?A) -> ?B = (i32) -> i32 (from lambda)
//   Result: ?B = i32

// Example 3: Generic function
fn identity<T>(x: T) -> T {
    x
}

// identity(42): T = i32
// identity("hello"): T = &str
```

---

## 🧪 Type Checking

### Expression Type Checking:

```rust
enum TypeError {
    Mismatch { expected: Type, found: Type },
    UnboundVariable(String),
    UndefinedFunction(String),
    InvalidOperation { op: String, left: Type, right: Type },
    ArityMismatch { expected: usize, found: usize },
}

impl Resolver {
    fn typecheck_expr(&mut self, expr: &AstNode) -> Result<Type, TypeError> {
        match expr {
            AstNode::Lit(n) => Ok(self.infer_literal_type(*n)),
            
            AstNode::BinaryOp { op, left, right } => {
                let left_ty = self.typecheck_expr(left)?;
                let right_ty = self.typecheck_expr(right)?;
                
                self.check_binary_op(op, &left_ty, &right_ty)
            }
            
            AstNode::Call { receiver, method, args, .. } => {
                let receiver_ty = if let Some(r) = receiver {
                    self.typecheck_expr(r)?
                } else {
                    // Static method call
                    Type::Named("Self".to_string(), Vec::new())
                };
                
                self.check_method_call(&receiver_ty, method, args)
            }
            
            AstNode::Var(name) => {
                self.env.lookup(name)
                    .ok_or_else(|| TypeError::UnboundVariable(name.clone()))
            }
            
            // ... other cases
        }
    }
    
    fn check_binary_op(&self, op: &str, left: &Type, right: &Type) -> Result<Type, TypeError> {
        match op {
            "+" | "-" | "*" | "/" => {
                // Numeric operations
                if self.is_numeric(left) && self.is_numeric(right) {
                    // Type promotion rules
                    Ok(self.promote_numeric(left, right))
                } else {
                    Err(TypeError::InvalidOperation {
                        op: op.to_string(),
                        left: left.clone(),
                        right: right.clone(),
                    })
                }
            }
            
            "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                // Comparison operations
                if left == right {
                    Ok(Type::Bool)
                } else {
                    Err(TypeError::Mismatch {
                        expected: left.clone(),
                        found: right.clone(),
                    })
                }
            }
            
            "&&" | "||" => {
                // Logical operations
                if *left == Type::Bool && *right == Type::Bool {
                    Ok(Type::Bool)
                } else {
                    Err(TypeError::InvalidOperation {
                        op: op.to_string(),
                        left: left.clone(),
                        right: right.clone(),
                    })
                }
            }
            
            _ => Err(TypeError::UnknownOperator(op.to_string())),
        }
    }
}
```

### Function Type Checking:

```rust
impl Resolver {
    fn typecheck_func(&mut self, func: &AstNode) -> Result<(), Vec<TypeError>> {
        if let AstNode::FuncDef { name, params, ret, body, .. } = func {
            // Enter new scope
            self.env.enter_scope();
            
            // Add parameters to environment
            for (param_name, param_type) in params {
                let ty = self.parse_type_string(param_type)?;
