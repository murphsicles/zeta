//! Environment module for Zeta standard library.
//! 
//! Provides environment variable and system information:
//! - Environment variables
//! - Command line arguments
//! - System properties
//! - Working directory

use std::env;

/// Initializes the environment module.
pub fn init() {
    println!("Environment module initialized");
}

/// Registers environment functions with the runtime.
pub fn register_functions(map: &mut std::collections::HashMap<&'static str, usize>) {
    // Environment variable functions
    map.insert("env_var", env_var as *const () as usize);
    map.insert("env_set_var", env_set_var as *const () as usize);
    map.insert("env_var_exists", env_var_exists as *const () as usize);
    
    // Command line arguments
    map.insert("env_args", env_args as *const () as usize);
    map.insert("env_arg_count", env_arg_count as *const () as usize);
    map.insert("env_arg", env_arg as *const () as usize);
    
    // System information
    map.insert("env_current_dir", env_current_dir as *const () as usize);
    map.insert("env_set_current_dir", env_set_current_dir as *const () as usize);
    map.insert("env_home_dir", env_home_dir as *const () as usize);
    map.insert("env_temp_dir", env_temp_dir as *const () as usize);
    
    // System properties
    map.insert("env_os", env_os as *const () as usize);
    map.insert("env_arch", env_arch as *const () as usize);
    map.insert("env_cpu_count", env_cpu_count as *const () as usize);
}

// ============================================================================
// Environment Variable Operations
// ============================================================================

/// Gets an environment variable.
/// 
/// # Safety
/// name_ptr must point to valid UTF-8 string of name_len bytes.
/// Returns a pointer to the value string, or null if not found.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn env_var(name_ptr: *const u8, name_len: usize) -> *mut u8 { unsafe {
    let name_bytes = std::slice::from_raw_parts(name_ptr, name_len);
    let name = String::from_utf8_lossy(name_bytes);
    
    match env::var(name.as_ref()) {
        Ok(value) => {
            let boxed = value.into_bytes().into_boxed_slice();
            let ptr = boxed.as_ptr();
            std::mem::forget(boxed);
            ptr as *mut u8
        }
        Err(_) => std::ptr::null_mut(),
    }
}}

/// Sets an environment variable.
/// 
/// # Safety
/// name_ptr must point to valid UTF-8 string of name_len bytes.
/// value_ptr must point to valid UTF-8 string of value_len bytes.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn env_set_var(
    name_ptr: *const u8,
    name_len: usize,
    value_ptr: *const u8,
    value_len: usize,
) -> bool { unsafe {
    let name_bytes = std::slice::from_raw_parts(name_ptr, name_len);
    let value_bytes = std::slice::from_raw_parts(value_ptr, value_len);
    
    let name = String::from_utf8_lossy(name_bytes);
    let value = String::from_utf8_lossy(value_bytes);
    
    env::set_var(name.as_ref(), value.as_ref());
    true
}}

/// Checks if an environment variable exists.
/// 
/// # Safety
/// name_ptr must point to valid UTF-8 string of name_len bytes.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn env_var_exists(name_ptr: *const u8, name_len: usize) -> bool { unsafe {
    let name_bytes = std::slice::from_raw_parts(name_ptr, name_len);
    let name = String::from_utf8_lossy(name_bytes);
    
    env::var(name.as_ref()).is_ok()
}}

// ============================================================================
// Command Line Arguments
// ============================================================================

/// Gets all command line arguments.
/// 
/// # Safety
/// Returns a pointer to an array of string pointers.
/// The array is terminated by a null pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn env_args() -> *mut *mut u8 {
    let args: Vec<String> = env::args().collect();
    let mut arg_ptrs: Vec<*mut u8> = Vec::with_capacity(args.len() + 1);
    
    for arg in args {
        let boxed = arg.into_bytes().into_boxed_slice();
        let ptr = boxed.as_ptr() as *mut u8;
        std::mem::forget(boxed);
        arg_ptrs.push(ptr);
    }
    
    // Add null terminator
    arg_ptrs.push(std::ptr::null_mut());
    
    let boxed_ptrs = arg_ptrs.into_boxed_slice();
    let ptr = boxed_ptrs.as_ptr() as *mut *mut u8;
    std::mem::forget(boxed_ptrs);
    ptr
}

/// Gets the number of command line arguments.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn env_arg_count() -> usize {
    env::args().count()
}

/// Gets a specific command line argument.
/// 
/// # Safety
/// Returns a pointer to the argument string, or null if index out of bounds.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn env_arg(index: usize) -> *mut u8 {
    let args: Vec<String> = env::args().collect();
    
    if index < args.len() {
        let arg = args[index].clone();
        let boxed = arg.into_bytes().into_boxed_slice();
        let ptr = boxed.as_ptr();
        std::mem::forget(boxed);
        ptr as *mut u8
    } else {
        std::ptr::null_mut()
    }
}

// ============================================================================
// Directory Operations
// ============================================================================

/// Gets the current working directory.
/// 
/// # Safety
/// Returns a pointer to the directory path string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn env_current_dir() -> *mut u8 {
    match env::current_dir() {
        Ok(path) => {
            let path_str = path.to_string_lossy().to_string();
            let boxed = path_str.into_bytes().into_boxed_slice();
            let ptr = boxed.as_ptr();
            std::mem::forget(boxed);
            ptr as *mut u8
        }
        Err(_) => std::ptr::null_mut(),
    }
}

/// Sets the current working directory.
/// 
/// # Safety
/// path_ptr must point to valid UTF-8 string of path_len bytes.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn env_set_current_dir(path_ptr: *const u8, path_len: usize) -> bool { unsafe {
    let path_bytes = std::slice::from_raw_parts(path_ptr, path_len);
    let path_str = String::from_utf8_lossy(path_bytes);
    
    env::set_current_dir(path_str.as_ref()).is_ok()
}}

/// Gets the home directory.
/// 
/// # Safety
/// Returns a pointer to the home directory path string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn env_home_dir() -> *mut u8 {
    match env::var("HOME") {
        Ok(path) => {
            let boxed = path.into_bytes().into_boxed_slice();
            let ptr = boxed.as_ptr();
            std::mem::forget(boxed);
            ptr as *mut u8
        }
        Err(_) => {
            // Fallback for Windows
            match env::var("USERPROFILE") {
                Ok(path) => {
                    let boxed = path.into_bytes().into_boxed_slice();
                    let ptr = boxed.as_ptr();
                    std::mem::forget(boxed);
                    ptr as *mut u8
                }
                Err(_) => std::ptr::null_mut(),
            }
        }
    }
}

/// Gets the temporary directory.
/// 
/// # Safety
/// Returns a pointer to the temp directory path string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn env_temp_dir() -> *mut u8 {
    match env::temp_dir().to_str() {
        Some(path) => {
            let boxed = path.as_bytes().to_vec().into_boxed_slice();
            let ptr = boxed.as_ptr();
            std::mem::forget(boxed);
            ptr as *mut u8
        }
        None => std::ptr::null_mut(),
    }
}

// ============================================================================
// System Properties
// ============================================================================

/// Gets the operating system name.
/// 
/// # Safety
/// Returns a pointer to the OS name string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn env_os() -> *mut u8 {
    let os = if cfg!(target_os = "windows") {
        "windows"
    } else if cfg!(target_os = "macos") {
        "macos"
    } else if cfg!(target_os = "linux") {
        "linux"
    } else {
        "unknown"
    };
    
    let boxed = os.as_bytes().to_vec().into_boxed_slice();
    let ptr = boxed.as_ptr();
    std::mem::forget(boxed);
    ptr as *mut u8
}

/// Gets the CPU architecture.
/// 
/// # Safety
/// Returns a pointer to the architecture string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn env_arch() -> *mut u8 {
    let arch = if cfg!(target_arch = "x86") {
        "x86"
    } else if cfg!(target_arch = "x86_64") {
        "x86_64"
    } else if cfg!(target_arch = "arm") {
        "arm"
    } else if cfg!(target_arch = "aarch64") {
        "aarch64"
    } else {
        "unknown"
    };
    
    let boxed = arch.as_bytes().to_vec().into_boxed_slice();
    let ptr = boxed.as_ptr();
    std::mem::forget(boxed);
    ptr as *mut u8
}

/// Gets the number of CPU cores.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn env_cpu_count() -> usize {
    // Use std::thread::available_parallelism for CPU count
    match std::thread::available_parallelism() {
        Ok(parallelism) => parallelism.get(),
        Err(_) => 1, // Fallback to 1 core
    }
}
