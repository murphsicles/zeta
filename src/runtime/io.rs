//! Basic I/O runtime functions for benchmarks
#![allow(unsafe_code)]

use std::io::{self, Write};

/// Print an integer to stdout
/// 
/// # Safety
/// No safety concerns
#[unsafe(no_mangle)]
pub unsafe extern "C" fn print_i64(value: i64) {
    print!("{}", value);
}

/// Print a boolean to stdout
/// 
/// # Safety
/// No safety concerns
#[unsafe(no_mangle)]
pub unsafe extern "C" fn print_bool(value: i64) {
    print!("{}", value != 0);
}

/// Print a string (pointer to null-terminated bytes) to stdout
/// 
/// # Safety
/// ptr must point to valid null-terminated UTF-8 string or be null
#[unsafe(no_mangle)]
pub unsafe extern "C" fn print_str(ptr: i64) {
    if ptr == 0 {
        return;
    }
    
    let mut p = ptr as *const u8;
    let mut bytes = Vec::new();
    
    // Read until null terminator
    unsafe {
        while *p != 0 {
            bytes.push(*p);
            p = p.add(1);
        }
    }
    
    if let Ok(s) = String::from_utf8(bytes) {
        print!("{}", s);
    }
}

/// Print with newline (println equivalent for benchmarks)
/// 
/// # Safety
/// No safety concerns
#[unsafe(no_mangle)]
pub unsafe extern "C" fn println() {
    println!();
}

/// Print integer with newline
/// 
/// # Safety
/// No safety concerns
#[unsafe(no_mangle)]
pub unsafe extern "C" fn println_i64(value: i64) {
    println!("{}", value);
}

/// Print boolean with newline
/// 
/// # Safety
/// No safety concerns
#[unsafe(no_mangle)]
pub unsafe extern "C" fn println_bool(value: i64) {
    println!("{}", value != 0);
}

/// Print string with newline
/// 
/// # Safety
/// ptr must point to valid null-terminated UTF-8 string or be null
#[unsafe(no_mangle)]
pub unsafe extern "C" fn println_str(ptr: i64) {
    if ptr == 0 {
        println!();
        return;
    }
    
    let mut p = ptr as *const u8;
    let mut bytes = Vec::new();
    
    // Read until null terminator
    unsafe {
        while *p != 0 {
            bytes.push(*p);
            p = p.add(1);
        }
    }
    
    if let Ok(s) = String::from_utf8(bytes) {
        println!("{}", s);
    }
}

/// Flush stdout
/// 
/// # Safety
/// No safety concerns
#[unsafe(no_mangle)]
pub unsafe extern "C" fn flush() {
    let _ = io::stdout().flush();
}