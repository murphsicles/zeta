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

/// Test function: identical to println_i64 but returns i64
#[unsafe(no_mangle)]
pub unsafe extern "C" fn test_return_i64(value: i64) -> i64 {
    println!("[TEST_RETURN_I64] Called with value = {}", value);
    value + 1
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

// ── Portable fd/clock helpers (moved out of reactor.rs: no epoll dependency) ──

/// Set O_NONBLOCK on a file descriptor. Portable (fcntl on Linux and macOS).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn set_nonblocking(fd: i64) -> i64 {
    let flags = libc::fcntl(fd as i32, libc::F_GETFL, 0);
    if flags < 0 {
        return -1;
    }
    libc::fcntl(fd as i32, libc::F_SETFL, flags | libc::O_NONBLOCK) as i64
}

/// Monotonic clock in nanoseconds. Portable (CLOCK_MONOTONIC on all unix hosts).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn monotonic_ns() -> i64 {
    let mut ts: libc::timespec = std::mem::zeroed();
    if libc::clock_gettime(libc::CLOCK_MONOTONIC, &mut ts) == 0 {
        ts.tv_sec * 1_000_000_000 + ts.tv_nsec
    } else {
        -1
    }
}
