//! Standard library module for Zeta.
//! 
//! This module handles `std::` imports and provides the standard library
//! functions for PrimeZeta compatibility.

/// Initializes the standard library module system.
pub fn init() {
    // Nothing to do for now, but this could register built-in std functions
    // with the resolver in the future.
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
    map.insert("malloc", std::std_malloc as *const () as usize);
    map.insert("free", std::std_free as *const () as usize);
    map.insert("print", std::std_print as *const () as usize);
    map.insert("println", std::std_println as *const () as usize);
    map.insert("args", std::std_args as *const () as usize);
    
    map
}