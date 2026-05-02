//! Filesystem runtime functions for Zeta std::fs

use std::ffi::CString;
use std::fs;
use std::path::Path;

fn to_cstring_ptr(s: &str) -> i64 {
    let c = CString::new(s).unwrap();
    let len = c.as_bytes_with_nul().len();
    let ptr = unsafe { crate::runtime::std::std_malloc(len as usize) };
    if ptr == 0 { return 0; }
    unsafe { std::ptr::copy_nonoverlapping(c.as_ptr(), ptr as *mut i8, len); }
    ptr
}

fn from_cstr(ptr: i64) -> String {
    if ptr == 0 { return String::new(); }
    unsafe { std::ffi::CStr::from_ptr(ptr as *const std::ffi::c_char).to_string_lossy().into_owned() }
}

fn to_bytes(ptr: i64, len: i64) -> Vec<u8> {
    if ptr == 0 || len <= 0 { return vec![]; }
    unsafe { std::slice::from_raw_parts(ptr as *const u8, len as usize).to_vec() }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fs_read_to_string(path: i64) -> i64 {
    let path = from_cstr(path);
    match fs::read_to_string(&path) {
        Ok(content) => to_cstring_ptr(&content),
        Err(_) => -1,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fs_write(path: i64, data: i64, len: i64) -> i64 {
    let path = from_cstr(path);
    let bytes = to_bytes(data, len);
    match fs::write(&path, &bytes) {
        Ok(_) => 0,
        Err(_) => -1,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fs_create_dir(path: i64) -> i64 {
    match fs::create_dir(Path::new(&from_cstr(path))) {
        Ok(_) => 0, Err(_) => -1
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fs_create_dir_all(path: i64) -> i64 {
    match fs::create_dir_all(Path::new(&from_cstr(path))) {
        Ok(_) => 0, Err(_) => -1
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fs_remove_file(path: i64) -> i64 {
    match fs::remove_file(Path::new(&from_cstr(path))) {
        Ok(_) => 0, Err(_) => -1
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fs_remove_dir(path: i64) -> i64 {
    match fs::remove_dir(Path::new(&from_cstr(path))) {
        Ok(_) => 0, Err(_) => -1
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fs_rename(from: i64, to: i64) -> i64 {
    match fs::rename(Path::new(&from_cstr(from)), Path::new(&from_cstr(to))) {
        Ok(_) => 0, Err(_) => -1
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fs_copy(from: i64, to: i64) -> i64 {
    match fs::copy(Path::new(&from_cstr(from)), Path::new(&from_cstr(to))) {
        Ok(_) => 0, Err(_) => -1
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fs_exists(path: i64) -> i64 {
    Path::new(&from_cstr(path)).exists() as i64
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fs_is_file(path: i64) -> i64 {
    Path::new(&from_cstr(path)).is_file() as i64
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fs_is_dir(path: i64) -> i64 {
    Path::new(&from_cstr(path)).is_dir() as i64
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fs_metadata_len(path: i64) -> i64 {
    match fs::metadata(Path::new(&from_cstr(path))) {
        Ok(meta) => meta.len() as i64,
        Err(_) => -1,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fs_read_dir(path: i64) -> i64 {
    match fs::read_dir(Path::new(&from_cstr(path))) {
        Ok(entries) => {
            let names: Vec<String> = entries.filter_map(|e| e.ok())
                .filter_map(|e| e.file_name().into_string().ok())
                .collect();
            to_cstring_ptr(&names.join(","))
        }
        Err(_) => -1,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fs_canonicalize(path: i64) -> i64 {
    match fs::canonicalize(Path::new(&from_cstr(path))) {
        Ok(p) => to_cstring_ptr(&p.to_string_lossy()),
        Err(_) => -1,
    }
}
