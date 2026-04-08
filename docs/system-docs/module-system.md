# Module System

**Last Updated:** 2026-04-08 12:14 GMT+1  
**System:** Module System (Namespace Management)  
**Status:** ✅ Complete  
**Examples:** 25+ runnable code examples

## 📋 Overview

Zeta's module system provides a powerful namespace management system that enables code organization, reuse, and encapsulation. The system is designed to be:
- **Hierarchical:** Nested modules for logical organization
- **Explicit:** Clear import/export semantics
- **Encapsulated:** Private vs public visibility control
- **Composable:** Easy to combine and extend modules

## 🎯 Core Concepts

### **1. Module Hierarchy**
Zeta modules form a tree structure:

```
project/
├── src/
│   ├── main.zeta          # Root module
│   ├── math/              # Submodule directory
│   │   ├── mod.zeta       # Module declaration
│   │   ├── arithmetic.zeta
│   │   └── algebra.zeta
│   └── utils/
│       ├── mod.zeta
│       └── helpers.zeta
```

### **2. Visibility Control**
- **`pub`:** Public items are accessible from outside the module
- **`pub(crate)`:** Accessible within the same crate
- **`pub(super)`:** Accessible to parent module
- **Default:** Private, only accessible within the same module

### **3. Import/Export System**
- **`use` declarations:** Bring items into scope
- **Re-exports:** Export items from submodules
- **Wildcard imports:** Import all public items (use with caution)

## 📚 Module Declaration Syntax

### **1. Inline Module Declaration**
```zeta
// Inline module in a single file
mod math {
    pub fn add(x: i32, y: i32) -> i32 {
        x + y
    }
    
    fn internal_helper() {
        // Private function
    }
}

fn main() {
    let result = math::add(5, 3);
    println!("Result: {}", result);
}
```

### **2. File-based Module Declaration**
```zeta
// File: src/math/mod.zeta
pub mod arithmetic;
pub mod algebra;

// Re-export for convenience
pub use arithmetic::add;
pub use arithmetic::subtract;
```

```zeta
// File: src/math/arithmetic.zeta
pub fn add(x: i32, y: i32) -> i32 {
    x + y
}

pub fn subtract(x: i32, y: i32) -> i32 {
    x - y
}

fn internal_multiply(x: i32, y: i32) -> i32 {
    x * y  // Private function
}
```

### **3. Module with Submodules**
```zeta
// File: src/utils/mod.zeta
pub mod string_utils;
pub mod math_utils;
pub mod date_utils;

// Convenience re-exports
pub use string_utils::{trim, capitalize};
pub use math_utils::clamp;
```

## 🛠️ Import Patterns

### **1. Basic Import**
```zeta
// Import specific items
use std::collections::HashMap;
use std::io::{Read, Write};

fn main() {
    let mut map = HashMap::new();
    map.insert("key", "value");
}
```

### **2. Relative Imports**
```zeta
// File: src/math/algebra.zeta
use super::arithmetic;  // Import from parent module
use crate::utils;       // Import from crate root
use self::polynomial;   // Import from current module

mod polynomial {
    pub fn evaluate() {
        // ...
    }
}
```

### **3. Renaming Imports**
```zeta
// Avoid name conflicts with renaming
use std::collections::HashMap as StdHashMap;
use my_crate::collections::HashMap as MyHashMap;

fn main() {
    let std_map = StdHashMap::new();
    let my_map = MyHashMap::new();
}
```

### **4. Wildcard Imports**
```zeta
// Import all public items (use sparingly)
use std::collections::*;

fn main() {
    let map = HashMap::new();
    let set = HashSet::new();
    let vec = VecDeque::new();
}
```

## 🔧 Advanced Module Features

### **1. Module Attributes**
```zeta
// Apply attributes to modules
#[cfg(test)]
mod tests {
    #[test]
    fn test_addition() {
        assert_eq!(2 + 2, 4);
    }
}

#[cfg(feature = "advanced")]
mod advanced_features {
    pub fn complex_operation() {
        // Only available with "advanced" feature
    }
}
```

### **2. Inline Module Expansion**
```zeta
// Modules can be declared inline with path
#[path = "custom/path/to/module.zeta"]
mod custom_module;

// Or with expression-based paths
#[path = concat!(env!("OUT_DIR"), "/generated.zeta")]
mod generated;
```

### **3. Module Aliases**
```zeta
// Create module aliases for convenience
mod m = very::long::module::path;

fn main() {
    m::some_function();
}
```

## 📦 Crate System

### **1. Crate Declaration**
```zeta
// Cargo.toml equivalent in Zeta
#![crate_name = "my_crate"]
#![crate_type = "lib"]  // or "bin", "cdylib", "staticlib"

// Module declarations follow
mod math;
mod utils;
mod api;
```

### **2. External Crate Dependencies**
```zeta
// Declare external dependencies
extern crate serde;
extern crate tokio;

// Use with full or shortened paths
use serde::{Serialize, Deserialize};
use tokio::runtime::Runtime;
```

### **3. Crate Visibility**
```zeta
// Control what's exported from the crate
pub mod public_api;     // Accessible to users
mod internal;           // Private to crate

// Re-export for clean public API
pub use public_api::{Client, Server, Config};
```

## 🏗️ Module Organization Patterns

### **1. Layered Architecture**
```zeta
// File structure for layered application
src/
├── domain/          # Business logic
│   ├── entities.zeta
│   ├── services.zeta
│   └── repositories.zeta
├── application/     # Use cases
│   ├── commands.zeta
│   └── queries.zeta
├── infrastructure/  # Technical details
│   ├── database.zeta
│   └── messaging.zeta
└── presentation/    # UI/API layer
    ├── controllers.zeta
    └── views.zeta
```

### **2. Feature-based Organization**
```zeta
// Organize by feature/domain
src/
├── auth/            # Authentication feature
│   ├── mod.zeta
│   ├── models.zeta
│   ├── service.zeta
│   └── middleware.zeta
├── billing/         # Billing feature
│   ├── mod.zeta
│   ├── plans.zeta
│   └── invoices.zeta
└── shared/          # Shared utilities
    ├── mod.zeta
    ├── errors.zeta
    └── validation.zeta
```

### **3. Plugin Architecture**
```zeta
// Base module defines plugin interface
pub mod plugin {
    pub trait Plugin {
        fn name(&self) -> &str;
        fn initialize(&self);
    }
    
    // Plugin registry
    pub struct Registry {
        plugins: Vec<Box<dyn Plugin>>,
    }
}

// Plugin implementations in separate modules
mod my_plugin {
    use crate::plugin::Plugin;
    
    pub struct MyPlugin;
    
    impl Plugin for MyPlugin {
        fn name(&self) -> &str {
            "my_plugin"
        }
        
        fn initialize(&self) {
            println!("MyPlugin initialized");
        }
    }
}
```

## 🔍 Name Resolution

### **1. Resolution Order**
Zeta resolves names in this order:
1. **Local scope:** Variables and parameters
2. **Module items:** Items in current module
3. **Imported items:** Items brought into scope with `use`
4. **Parent modules:** Items in parent modules
5. **Crate root:** Items at crate root
6. **External crates:** Items from dependencies

### **2. Absolute vs Relative Paths**
```zeta
// Absolute path (from crate root)
use crate::math::arithmetic::add;

// Relative path (from current module)
use super::parent_module::item;
use self::sibling_module::item;

// External crate path
use external_crate::module::item;
```

### **3. Shadowing and Conflicts**
```zeta
mod module_a {
    pub fn calculate() -> i32 { 42 }
}

mod module_b {
    pub fn calculate() -> i32 { 99 }
}

fn main() {
    // Fully qualified paths avoid conflicts
    let a = module_a::calculate();
    let b = module_b::calculate();
    
    // Import with renaming to avoid conflicts
    use module_a::calculate as calculate_a;
    use module_b::calculate as calculate_b;
    
    let x = calculate_a();
    let y = calculate_b();
}
```

## 🚀 Module System in Practice

### **1. Library Crate Structure**
```zeta
// File: src/lib.zeta
#![crate_name = "my_library"]
#![crate_type = "lib"]

// Public API
pub mod api {
    pub mod v1 {
        pub use crate::internal::public_function;
        pub use crate::internal::PublicStruct;
    }
}

// Internal implementation (not exported)
mod internal {
    pub fn public_function() {
        // Can call private helpers
        helper_function();
    }
    
    fn helper_function() {
        // Private implementation detail
    }
    
    pub struct PublicStruct {
        pub field: i32,
        private_field: String,
    }
}
```

### **2. Binary Crate Structure**
```zeta
// File: src/main.zeta
#![crate_name = "my_app"]
#![crate_type = "bin"]

mod config;
mod database;
mod web;
mod cli;

use crate::config::Config;
use crate::database::Database;
use crate::web::Server;
use crate::cli::Parser;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let config = Config::load()?;
    let db = Database::connect(&config)?;
    let server = Server::new(&config, db)?;
    
    server.run()
}
```

### **3. Workspace with Multiple Crates**
```zeta
// Workspace Cargo.toml equivalent
[workspace]
members = [
    "crates/core",
    "crates/cli",
    "crates/web",
    "crates/tests",
]

// In crate core/src/lib.zeta
pub mod types;
pub mod algorithms;
pub mod utilities;

// In crate cli/src/main.zeta
use core::types::Data;
use core::algorithms::process;

fn main() {
    let data = Data::new();
    let result = process(data);
    println!("Result: {:?}", result);
}
```

## 📝 Best Practices

### **1. Module Size and Organization**
```zeta
// GOOD: Small, focused modules
mod user_authentication;  // ~200 lines
mod payment_processing;   // ~300 lines
mod email_templates;      // ~150 lines

// BAD: Monolithic modules
mod everything;           // 5000+ lines
```

### **2. Visibility Control**
```zeta
// GOOD: Explicit visibility
pub struct PublicApi {
    pub field: String,
    internal_field: i32,  // Private by default
}

impl PublicApi {
    pub fn new() -> Self {
        Self {
            field: String::new(),
            internal_field: 0,
        }
    }
    
    // Private helper method
    fn calculate_internal(&self) -> i32 {
        self.internal_field * 2
    }
}

// BAD: Everything public
pub struct BadApi {
    pub everything: String,
    pub and_everything_else: i32,
}
```

### **3. Import Organization**
```zeta
// GOOD: Organized imports
// Standard library
use std::collections::{HashMap, HashSet};
use std::io::{Read, Write};

// External crates
use serde::{Serialize, Deserialize};
use tokio::runtime::Runtime;

// Local modules
use crate::config::Config;
use crate::database::Database;
use crate::utils::{helpers, validators};

// BAD: Disorganized imports
use std::collections::HashMap;
use crate::config::Config;
use serde::Serialize;
use std::io::Read;
use crate::database::Database;
```

### **4. Re-export Patterns**
```zeta
// GOOD: Clean public API
// In lib.zeta or mod.zeta
pub mod internal {
    pub mod v1 {
        pub struct ImplementationDetail;
    }
}

// Re-export only what users need
pub use internal::v1::ImplementationDetail as PublicType;

// BAD: Exposing implementation details
pub mod internal {
    pub mod v1 {
        pub struct ImplementationDetail;
    }
}
// Users have to navigate internal::v1::ImplementationDetail
```

## 🧪 Testing Modules

### **1. Test Modules**
```zeta
// Test module alongside implementation
mod math {
    pub fn add(x: i32, y: i32) -> i32 {
        x + y
    }
    
    #[cfg(test)]
    mod tests {
        use super::*;
        
        #[test]
        fn test_add_positive() {
            assert_eq!(add(2, 3), 5);
        }
        
        #[test]
        fn test_add_negative() {
            assert_eq!(add(-2, -3), -5);
        }
    }
}
```

### **2. Integration Tests**
```zeta
// File: tests/integration_test.zeta
use my_crate::PublicApi;

#[test]
fn test_public_api() {
    let api = PublicApi::new();
    let result = api.process();
    assert!(result.is_ok());
}

#[test]
fn test_module_interaction() {
    use my_crate::module_a::FunctionA;
    use my_crate::module_b::FunctionB;
    
    let a = FunctionA::new();
    let b = FunctionB::from(a);
    assert_eq!(b.value(), 42);
}
```

### **3. Mocking and Testing Utilities**
```zeta
// Test utilities module
#[cfg(test)]
pub mod test_utils {
    pub fn setup_database() -> MockDatabase {
        MockDatabase::new()
    }
    
    pub fn create_test_user() -> User {
        User {
            id: 1,
            name: "Test User".to_string(),
        }
    }
}

// Use in tests
#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::{setup_database, create_test_user};
    
    #[test]
    fn test_with_mocks() {
        let db = setup_database();
        let user = create_test_user();
        // Test with mocked dependencies
    }
}
```

## 🔍 Debugging Module Issues

### **Common Issues:**

1. **"Module not found" errors:**
   - Check file exists and is in correct location
   - Verify `mod` declaration matches filename
   - Ensure file has `.zeta` extension

2. **"Item is private" errors:**
   - Add `pub` modifier to make item public
   - Use `pub(crate)` for crate-internal access
   - Consider if item should be public at all

3. **Circular dependencies:**
   - Restructure modules to avoid cycles
   - Use trait objects or callbacks
   - Extract common functionality to shared module

4. **Import conflicts:**
   - Use fully qualified paths
   - Rename imports with `as` keyword
   - Reorganize module structure

### **Debugging Tools:**
```zeta
// Enable module debugging
#![feature(module_debug)]

// Print module tree
fn print_module_tree() {
    println!("Module tree:");
    // Debug output showing module hierarchy
}

// Check visibility
fn check_visibility(item: &dyn std::any::Any) {
    // Debug function to check item visibility
}
```

## 📈 Performance Considerations

### **Module System Overhead:**
- **Compilation:** Modules enable parallel compilation
- **Linking:** Only link what's actually used
- **Runtime:** No overhead for module system itself

### **Optimization Tips:**
```zeta
// Use inline modules for small, hot code
#[inline(always)]
mod hot_path {
    pub fn critical_function() {
        // Performance-critical code
    }
}

// Avoid deep module hierarchies in hot paths
// Prefer:
use crate::utils::math::fast::algorithm;
// Over:
use super::super::super::utils::math::fast::algorithm;
```

## 🚨 Module System Limitations

### **Current Limitations:**
1. **No conditional compilation at module level** (planned)
2. **Limited module attributes support** (expanding)
3. **No module-level documentation generation** (in progress)

### **Workarounds:**
```zeta
// For conditional modules, use feature flags
#[cfg(feature = "advanced")]
mod advanced {
    // Advanced features
}

// For documentation, use module-level comments
//! # Math Module
//! 
//! This module provides mathematical functions.
mod math {
    // Implementation
}
```

---

**A well-organized module system is the foundation of maintainable, scalable software. Zeta's module system provides the tools you need to build complex applications while keeping code organized and understandable.**

*Next: [Trait System Guide](./trait-system.md)*