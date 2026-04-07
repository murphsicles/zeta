//! I/O module for Zeta standard library.
//! 
//! Provides basic input/output operations:
//! - File I/O
//! - Console I/O
//! - Buffered I/O and streams

use std::fs;
use std::io::{self, Write};

/// Initializes the I/O module.
pub fn init() {
    println!("I/O module initialized");
}

/// Registers I/O functions with the runtime.
pub fn register_functions(map: &mut std::collections::HashMap<&'static str, usize>) {
    // File operations
    map.insert("file_open", file_open as *const () as usize);
    map.insert("file_close", file_close as *const () as usize);
    map.insert("file_read", file_read as *const () as usize);
    map.insert("file_write", file_write as *const () as usize);
    
    // Console operations
    map.insert("io_read_line", io_read_line as *const () as usize);
    map.insert("io_write", io_write as *const () as usize);
    map.insert("io_flush", io_flush as *const () as usize);
    
    // Stream operations
    map.insert("stream_new", stream_new as *const () as usize);
    map.insert("stream_read", stream_read as *const () as usize);
    map.insert("stream_write", stream_write as *const () as usize);
}

// ============================================================================
// File Operations
// ============================================================================

/// File handle structure
pub struct File {
    path: String,
    // In a real implementation, this would hold a file descriptor
    // For now, we'll just track the path
}

/// Opens a file for reading or writing.
/// 
/// # Safety
/// Returns a pointer to a File structure.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn file_open(path_ptr: *const u8, path_len: usize, mode: i32) -> *mut File { unsafe {
    let path_bytes = std::slice::from_raw_parts(path_ptr, path_len);
    let path = String::from_utf8_lossy(path_bytes).to_string();
    
    // mode: 0 = read, 1 = write, 2 = append
    let file = Box::new(File { path });
    Box::into_raw(file)
}}

/// Closes a file.
/// 
/// # Safety
/// file must be a valid pointer from file_open.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn file_close(file: *mut File) -> bool { unsafe {
    if !file.is_null() {
        let _ = Box::from_raw(file);
        true
    } else {
        false
    }
}}

/// Reads from a file.
/// 
/// # Safety
/// file must be a valid pointer from file_open.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn file_read(
    file: *const File,
    buffer: *mut u8,
    buffer_len: usize,
) -> isize { unsafe {
    if let Some(file) = file.as_ref() {
        match fs::read(&file.path) {
            Ok(data) => {
                let len = data.len().min(buffer_len);
                std::ptr::copy_nonoverlapping(data.as_ptr(), buffer, len);
                len as isize
            }
            Err(_) => -1,
        }
    } else {
        -1
    }
}}

/// Writes to a file.
/// 
/// # Safety
/// file must be a valid pointer from file_open.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn file_write(
    file: *const File,
    data_ptr: *const u8,
    data_len: usize,
) -> isize { unsafe {
    if let Some(file) = file.as_ref() {
        let data = std::slice::from_raw_parts(data_ptr, data_len);
        match fs::write(&file.path, data) {
            Ok(_) => data_len as isize,
            Err(_) => -1,
        }
    } else {
        -1
    }
}}

// ============================================================================
// Console Operations
// ============================================================================

/// Reads a line from standard input.
/// 
/// # Safety
/// buffer must point to valid memory of at least buffer_len bytes.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn io_read_line(buffer: *mut u8, buffer_len: usize) -> isize { unsafe {
    let mut input = String::new();
    match io::stdin().read_line(&mut input) {
        Ok(_) => {
            let bytes = input.as_bytes();
            let len = bytes.len().min(buffer_len);
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), buffer, len);
            len as isize
        }
        Err(_) => -1,
    }
}}

/// Writes data to standard output.
/// 
/// # Safety
/// data_ptr must point to valid memory of at least data_len bytes.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn io_write(data_ptr: *const u8, data_len: usize) -> isize { unsafe {
    let data = std::slice::from_raw_parts(data_ptr, data_len);
    match io::stdout().write_all(data) {
        Ok(_) => data_len as isize,
        Err(_) => -1,
    }
}}

/// Flushes standard output.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn io_flush() -> bool {
    match io::stdout().flush() {
        Ok(_) => true,
        Err(_) => false,
    }
}

// ============================================================================
// Stream Operations
// ============================================================================

/// Stream structure for buffered I/O
pub struct Stream {
    buffer: Vec<u8>,
    position: usize,
}

/// Creates a new stream.
/// 
/// # Safety
/// Returns a pointer to a Stream structure.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn stream_new() -> *mut Stream {
    let stream = Box::new(Stream {
        buffer: Vec::new(),
        position: 0,
    });
    Box::into_raw(stream)
}

/// Reads from a stream.
/// 
/// # Safety
/// stream must be a valid pointer from stream_new.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn stream_read(
    stream: *mut Stream,
    buffer: *mut u8,
    buffer_len: usize,
) -> isize { unsafe {
    if let Some(stream) = stream.as_mut() {
        let available = stream.buffer.len() - stream.position;
        let to_read = available.min(buffer_len);
        
        if to_read > 0 {
            let src = &stream.buffer[stream.position..stream.position + to_read];
            std::ptr::copy_nonoverlapping(src.as_ptr(), buffer, to_read);
            stream.position += to_read;
            to_read as isize
        } else {
            0
        }
    } else {
        -1
    }
}}

/// Writes to a stream.
/// 
/// # Safety
/// stream must be a valid pointer from stream_new.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn stream_write(
    stream: *mut Stream,
    data_ptr: *const u8,
    data_len: usize,
) -> isize { unsafe {
    if let Some(stream) = stream.as_mut() {
        let data = std::slice::from_raw_parts(data_ptr, data_len);
        stream.buffer.extend_from_slice(data);
        data_len as isize
    } else {
        -1
    }
}}
