# Module System
## Comprehensive Guide to Zeta's Module Organization

**Last Updated:** 2026-03-28  
**System Status:** Active Development (v0.3.9)

---

## 📋 Overview

Zeta's module system provides:
- **Namespace organization** - Logical grouping of related code
- **Visibility control** - Public/private access modifiers
- **Dependency management** - Explicit imports and exports
- **Code reuse** - Share functionality across projects
- **Compilation units** - Separate compilation and linking

### Design Philosophy
> "Explicit is better than implicit. Clear boundaries enable clear thinking."

---

## 🏗️ Architecture

### Module System Layers:

```
┌─────────────────────────────────────────┐
│          Source Files (.z)              │
│  (Physical organization on disk)        │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│          Module Declaration             │
│  (Logical namespace boundaries)         │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│          Import Resolution              │
│  (Resolve dependencies, handle cycles)  │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│          Visibility Checking            │
│  (Enforce public/private access rules)  │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│          Symbol Table Building          │
│  (Create global namespace for linking)  │
└─────────────────────────────────────────┘
```

---

## 📁 File System Organization

### Basic Structure:

```
project/
├── src/
│   ├── main.z          # Entry point
│   ├── lib.z           # Library root
│   ├── math/
│   │   ├── mod.z       # Module declaration
│   │   ├── vector.z    # Submodule
│   │   └── matrix.z    # Submodule
│   ├── utils/
│   │   ├── mod.z
│   │   └── logging.z
│   └── api/
│       ├── mod.z
│       └── client.z
├── tests/
│   └── integration.z
└── Cargo.toml          # Rust build metadata
```

### Module File Naming:
- **Module declaration**: `mod.z` (required for directories)
- **Regular module**: `*.z` (any name)
- **Main entry**: `main.z` or `lib.z`
- **Test files**: `*_test.z` or in `tests/` directory

---

## 📝 Module Declaration Syntax

### Basic Module Declaration:

```zeta
// File: src/math/mod.z

// Module declaration (optional for file-based modules)
module math;

// Public exports
pub fn add(x: i32, y: i32) -> i32 {
    x + y
}

// Private function (only accessible within module)
fn internal_helper() -> i32 {
    42
}

// Public struct
pub struct Vector {
    x: f64,
    y: f64,
    z: f64,
}

// Public implementation
impl Vector {
    pub fn new(x: f64, y: f64, z: f64) -> Vector {
        Vector { x, y, z }
    }
    
    pub fn magnitude(&self) -> f64 {
        (self.x * self.x + self.y * self.y + self.z * self.z).sqrt()
    }
}
```

### Submodule Declaration:

```zeta
// File: src/math/linear_algebra.z

// Declare as submodule of math
module math::linear_algebra;

pub struct Matrix {
    rows: usize,
    cols: usize,
    data: Vec<f64>,
}

impl Matrix {
    pub fn multiply(&self, other: &Matrix) -> Matrix {
        // Matrix multiplication implementation
        // ...
    }
}
```

### Inline Module Declaration:

```zeta
// File: src/main.z

// Inline module (contents in same file)
mod utils {
    pub fn helper() -> i32 {
        42
    }
    
    // Private to this inline module
    fn secret() -> i32 {
        1337
    }
}

fn main() {
    let result = utils::helper();
    println!("Result: {}", result);
}
```

---

## 🔗 Import System

### Basic Imports:

```zeta
// Import entire module
use math;

// Import specific items
use math::Vector;
use math::add;

// Import with alias
use math::Vector as Vec3;
use math::add as sum;

// Import multiple items
use math::{Vector, Matrix, add, multiply};

// Import all public items (use with caution!)
use math::*;
```

### Relative Imports:

```zeta
// File: src/math/vector.z

// Import from parent module
use super::constants;

// Import from sibling module
use super::matrix::Matrix;

// Import from root (crate root)
use crate::utils::logging;

// Import from current module (self)
use self::internal::helper;
```

### Re-exporting:

```zeta
// File: src/math/mod.z

// Re-export submodule contents
pub use linear_algebra::Matrix;
pub use linear_algebra::multiply;

// Re-export with different name
pub use vector::Vector as Vec3;

// Re-export entire module
pub mod linear_algebra;
```

### Import Groups:

```zeta
// Group imports for organization
use {
    math::{Vector, Matrix},
    utils::{logging, profiling},
    api::client,
};
```

---

## 👁️ Visibility Rules

### Visibility Modifiers:

```zeta
// Public - accessible from anywhere
pub fn public_function() -> i32 { 42 }

// Private - only accessible within module
fn private_function() -> i32 { 1337 }

// Module-private - only accessible within same file
mod module_private_function() -> i32 { 99 }

// Crate-public - accessible within same crate
pub(crate) fn crate_public_function() -> i32 { 77 }

// Path-specific - accessible to specific module
pub(in math::linear_algebra) fn restricted_function() -> i32 { 55 }
```

### Visibility Inheritance:

```zeta
// File: src/api/mod.z

pub mod client {
    // Public in parent module means accessible via api::client::connect
    pub fn connect() -> bool { true }
    
    // Private in submodule
    fn internal_handshake() -> bool { false }
}

pub mod server {
    // Can access sibling module's public items
    pub fn start() {
        if client::connect() {
            println!("Connected!");
        }
    }
}
```

### Struct Field Visibility:

```zeta
pub struct User {
    // Public field
    pub name: String,
    
    // Private field
    age: u32,
    
    // Module-private field
    mod password_hash: String,
}

impl User {
    // Public method can access private fields
    pub fn get_age(&self) -> u32 {
        self.age
    }
    
    // Private method
    fn validate_password(&self, input: &str) -> bool {
        // Can access private fields
        hash(input) == self.password_hash
    }
}
```

---

## 🔄 Module Resolution

### Resolution Algorithm:

1. **Current File Lookup**:
   ```zeta
   // Looks for item in current module first
   use item;  // Checks current file
   ```

2. **Module Path Resolution**:
   ```zeta
   // Resolves relative to current module
   use super::parent_item;      // Parent module
   use crate::root_item;        // Crate root
   use self::sibling_item;      // Current module
   ```

3. **External Crate Resolution**:
   ```zeta
   // Resolves through Cargo.toml dependencies
   use external_crate::item;
   ```

4. **Standard Library Resolution**:
   ```zeta
   // Built-in modules
   use std::collections::HashMap;
   use std::io::{Read, Write};
   ```

### Path Resolution Examples:

```zeta
// Absolute path (from crate root)
use ::math::vector::Vector;

// Relative path (from current module)
use .::submodule::item;
use super::parent_item;
use self::sibling_item;

// External crate
use serde::{Serialize, Deserialize};

// Standard library
use std::fmt::Display;
```

### Circular Dependency Handling:

```zeta
// File: src/a.z
use b::B;

pub struct A {
    b: B,
}

// File: src/b.z  
use a::A;

pub struct B {
    a: Option<A>,
}

// Resolution: Forward declaration or interface modules
```

---

## 🏭 Module Compilation

### Compilation Units:

```zeta
// Each .z file is a compilation unit
// Compilation order determined by dependencies

// main.z depends on math.z
use math;

// math.z depends on utils.z  
use utils;

// utils.z has no dependencies
```

### Separate Compilation:

```bash
# Compile modules separately
zeta compile src/math/mod.z -o math.o
zeta compile src/utils/mod.z -o utils.o
zeta compile src/main.z -o main.o

# Link together
zeta link main.o math.o utils.o -o program
```

### Incremental Compilation:

```bash
# Only recompile changed modules
zeta build --incremental

# Watch mode for development
zeta watch
```

---

## 📦 Crate System

### Crate Definition:

```toml
# Cargo.toml
[package]
name = "my_crate"
version = "0.1.0"
authors = ["Your Name <email@example.com>"]
edition = "2024"

[dependencies]
serde = "1.0"
tokio = { version = "1.0", features = ["full"] }

[lib]
name = "my_crate"
path = "src/lib.z"

[[bin]]
name = "my_program"
path = "src/main.z"
```

### Library Crate:

```zeta
// File: src/lib.z

// Library root module
pub mod math;
pub mod utils;
pub mod api;

// Re-export public API
pub use math::{Vector, Matrix};
pub use utils::logging;
pub use api::Client;
```

### Binary Crate:

```zeta
// File: src/main.z

use my_crate::{Vector, logging};

fn main() {
    let v = Vector::new(1.0, 2.0, 3.0);
    logging::info!("Vector created: {:?}", v);
}
```

### Workspace Crate:

```toml
# Cargo.toml (workspace root)
[workspace]
members = [
    "crates/core",
    "crates/utils", 
    "crates/cli",
]

# Each member has its own Cargo.toml
```

---

## 🛠️ Module Tools

### Module Discovery:

```bash
# List all modules in project
zeta modules list

# Show module dependencies
zeta modules deps src/main.z

# Visualize module graph
zeta modules graph | dot -Tpng > modules.png
```

### Refactoring Tools:

```bash
# Move item to different module
zeta refactor move --item math::Vector --to geometry::Vector

# Extract module from file
zeta refactor extract --file src/utils.z --module logging

# Inline module
zeta refactor inline --module small_module
```

### LSP Integration:

```zeta
// Editor provides:
// - Auto-import suggestions
// - Go to definition across modules
// - Find all references
// - Rename refactoring
// - Import organization
```

---

## 🧪 Testing Modules

### Unit Tests in Module:

```zeta
// File: src/math/vector.z

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_vector_addition() {
        let v1 = Vector::new(1.0, 2.0, 3.0);
        let v2 = Vector::new(4.0, 5.0, 6.0);
        let result = v1 + v2;
        assert_eq!(result.x, 5.0);
    }
}
```

### Integration Tests:

```zeta
// File: tests/integration.z

use my_crate::math::Vector;
use my_crate::utils::logging;

#[test]
fn test_integration() {
    logging::init();
    let v = Vector::new(1.0, 2.0, 3.0);
    assert!(v.magnitude() > 0.0);
}
```

### Test Utilities Module:

```zeta
// File: tests/test_utils.z

pub mod test_utils {
    pub fn setup() {
        // Test setup code
    }
    
    pub fn teardown() {
        // Test cleanup code
    }
}

// Use in tests
use test_utils::{setup, teardown};
```

---

## 🚀 Best Practices

### Module Organization:

1. **Cohesion**: Group related functionality together
2. **Size**: Keep modules focused (100-500 lines ideal)
3. **Depth**: Limit module nesting (3-4 levels max)
4. **Naming**: Use clear, descriptive names

### Import Guidelines:

```zeta
// GOOD: Clear, organized imports
use std::collections::{HashMap, HashSet};
use std::io::{self, Read, Write};
use crate::math::{Vector, Matrix};
use crate::utils::logging;

// BAD: Wildcard imports (except in tests)
use std::collections::*;  // Avoid in production code

// GOOD: Group by source
// Standard library
use std::fmt;
use std::io;

// External crates  
use serde;
use tokio;

// Internal modules
use crate::math;
use crate::utils;
```

### Visibility Guidelines:

1. **Default to private**: Make items public only when needed
2. **Use pub(crate)**: For internal crate APIs
3. **Document public API**: All public items need documentation
4. **Deprecate, don't remove**: Use `#[deprecated]` for API changes

### Error Patterns to Avoid:

```zeta
// BAD: Circular dependencies
// a.z uses b.z, b.z uses a.z

// BAD: Deep nesting
use crate::a::b::c::d::e::f::item;  // Too deep!

// BAD: Ambiguous imports
use crate::math::Vector;
use crate::geometry::Vector;  // Which Vector?

// SOLUTION: Use aliases
use crate::math::Vector as MathVector;
use crate::geometry::Vector as GeoVector;
```

---

## 🔧 Advanced Features

### Conditional Compilation:

```zeta
// Platform-specific modules
#[cfg(target_os = "linux")]
mod linux;

#[cfg(target_os = "windows")]
mod windows;

// Feature-flagged modules
#[cfg(feature = "network")]
mod network;

#[cfg(feature = "database")]
mod database;
```

### Procedural Macros in Modules:

```zeta
// Attribute macros
#[derive(Serialize, Deserialize)]
pub struct Data {
    field: String,
}

// Function-like macros
pub fn debug_log!(msg: &str) {
    #[cfg(debug_assertions)]
    println!("DEBUG: {}", msg);
}
```

### Module Attributes:

```zeta
// Module-level attributes
#[allow(unused_imports)]
mod utils;

#[deprecated(since = "0.2.0", note = "Use new_module instead")]
mod old_module;

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Config {
    value: String,
}
```

### Dynamic Module Loading:

```zeta
// Runtime module loading (advanced)
use std::dlopen;

unsafe {
    let lib = dlopen::Library::open("module.so").unwrap();
    let func: dlopen::Symbol<fn() -> i32> = lib.symbol("module_function").unwrap();
    let result = func();
}
```

---

## 🚨 Common Issues & Solutions

### "Module not found" Error:

```zeta
// ERROR: Cannot find module
use missing_module;  // Module doesn't exist

// SOLUTION: Check file exists and mod.z is present
// Create src/missing_module/mod.z
```

### "Item is private" Error:

```zeta
mod utils {
    fn helper() -> i32 { 42 }  // Private!
}

fn main() {
    let x = utils::helper();  // ERROR: helper is private
    
    // SOLUTION: Make it public or use public API
    // Change to: pub fn helper()
}
```

### Import Conflict:

```zeta
use crate::math::add;
use crate::utils::add;  // ERROR: Duplicate import

// SOLUTION 1: Use aliases
use crate::math::add as math_add;
use crate::utils::add as utils_add;

// SOLUTION 2: Use full paths
let result = crate::math::add(1, 2);
```

### Circular Dependency:

```zeta
// a.z
use b::B;  // Depends on b

// b.z  
use a::A;  // Depends on a - CIRCULAR!

// SOLUTION: Refactor common code to third module
// c.z (common types)
pub trait Common { /* ... */ }

// a.z and b.z both use c.z
```

---

## 📈 Performance Considerations

### Compilation Speed:

1. **Small modules**: Compile faster, parallelize better
2. **Minimal dependencies**: Reduce compilation graph
3. **Forward declarations**: Avoid full type knowledge early
4. **Incremental compilation**: Only rebuild changed modules

### Runtime Performance:

1. **Inline small modules**: Reduce indirection
2. **LTO (Link Time Optimization)**: Cross-module optimization
3. **Static linking**: Single binary, faster startup
4. **Dynamic linking**: Shared libraries, smaller binaries

### Memory Usage:

```zeta
// Module initialization
static INIT: Once = Once::new();

pub fn initialize() {
    INIT.call_once(|| {
        // Heavy initialization once per module
    });
}
```

---

## 🔮 Future Improvements

### Planned Features:

1. **Module-level documentation**:
   ```zeta
   //! # Math Module
   //! 
   //! Provides mathematical utilities.
   ```

2. **Module aliases**:
   ```zeta
   module geometry = "crate::math::geometry";
   ```

3. **Dynamic module reloading**