//! Standard library module for Zeta.
//! 
//! This module handles `std::` imports and provides the standard library
//! functions for PrimeZeta compatibility.

// Re-export standard library modules
pub mod alloc;
pub mod collections;
pub mod io;
pub mod fmt;
pub mod time;
pub mod env;
pub mod math;
pub mod quantum;
pub mod bit;
pub mod simd;

/// Initializes the standard library module system.
pub fn init() {
    // Initialize all std modules
    alloc::init();
    collections::init();
    io::init();
    fmt::init();
    time::init();
    env::init();
    math::init();
    quantum::init();
    bit::init();
    simd::init();
}

/// Checks if a path is a standard library import.
pub fn is_std_import(path: &[String]) -> bool {
    !path.is_empty() && path[0] == "std"
}

/// Gets the standard library function implementations.
/// Returns a map of function name to Rust function pointer.
pub fn get_std_functions() -> ::std::collections::HashMap<&'static str, usize> {
    use crate::runtime::std;
    
    let mut map = ::std::collections::HashMap::new();
    
    // Core memory management (non-generic versions)
    map.insert("malloc", std::std_malloc as *const () as usize);
    map.insert("free", std::std_free as *const () as usize);
    
    // Basic I/O
    map.insert("print", std::std_print as *const () as usize);
    map.insert("println", std::std_println as *const () as usize);
    map.insert("args", std::std_args as *const () as usize);
    
    // Allocation functions
    alloc::register_functions(&mut map);
    
    // Collections functions
    collections::register_functions(&mut map);
    
    // I/O functions
    io::register_functions(&mut map);
    
    // Formatting functions
    fmt::register_functions(&mut map);
    
    // Time functions
    time::register_functions(&mut map);
    
    // Environment functions
    env::register_functions(&mut map);
    
    // Math functions
    math::register_functions(&mut map);
    
    // Quantum computing functions
    quantum::register_functions(&mut map);
    
    // Bit manipulation functions
    bit::register_functions(&mut map);
    
    // SIMD functions
    simd::register_functions(&mut map);
    
    map
}