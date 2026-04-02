// src/runtime/std.rs
use std::env;

/// Allocates memory.
///
/// # Safety
/// Caller must ensure valid size, free returned pointer with std_free, and avoid use after free.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn std_malloc(size: usize) -> i64 {
    // For now, return a dummy pointer
    // In a real implementation, this would allocate actual memory
    0x1000 as i64
}

/// Frees memory allocated by std_malloc.
///
/// # Safety
/// Caller must ensure pointer from std_malloc or null, no use after free, and no double free.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn std_free(ptr: usize) {
    // For now, do nothing
    // In a real implementation, this would free the memory
}

/// Prints an integer to stdout.
///
/// # Safety
/// No safety concerns.
pub unsafe extern "C" fn std_print(value: i64) {
    print!("{}", value);
}

/// Prints an integer to stdout with newline.
///
/// # Safety
/// No safety concerns.
pub unsafe extern "C" fn std_println(value: i64) {
    println!("{}", value);
}

/// Gets command line arguments.
/// Returns a pointer to an array of string pointers.
/// The array is terminated by a null pointer.
///
/// # Safety
/// The returned pointer must be freed by the caller.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn std_args() -> *mut *mut u8 {
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

/// Allocates memory with calloc semantics (zero-initialized).
///
/// # Safety
/// Caller must ensure valid size, free returned pointer with std_free.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn std_calloc(count: i64, size: i64) -> i64 {
    // For now, return a dummy pointer
    // In a real implementation, this would allocate zero-initialized memory
    0x2000 as i64
}

/// Reallocates memory.
///
/// # Safety
/// ptr must be from std_malloc/std_calloc or null.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn std_realloc(ptr: i64, new_size: i64) -> i64 {
    // For now, return a dummy pointer
    // In a real implementation, this would reallocate memory
    0x3000 as i64
}
