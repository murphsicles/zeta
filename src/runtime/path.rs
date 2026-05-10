//! Path runtime functions for Zeta std::path

use std::ffi::CString;
use std::path::{Path, PathBuf};

fn to_cstring_ptr(s: &str) -> i64 {
    let c = CString::new(s).unwrap();
    let len = c.as_bytes_with_nul().len();
    let ptr = unsafe { crate::runtime::std::std_malloc(len) };
    if ptr == 0 {
        return 0;
    }
    unsafe {
        std::ptr::copy_nonoverlapping(c.as_ptr(), ptr as *mut i8, len);
    }
    ptr
}

fn from_cstr(ptr: i64) -> String {
    if ptr == 0 {
        return String::new();
    }
    unsafe {
        std::ffi::CStr::from_ptr(ptr as *const std::ffi::c_char)
            .to_string_lossy()
            .into_owned()
    }
}

fn path_from(ptr: i64) -> PathBuf {
    PathBuf::from(from_cstr(ptr))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn path_parent(path: i64) -> i64 {
    let p = path_from(path);
    p.parent()
        .map(|x| to_cstring_ptr(&x.to_string_lossy()))
        .unwrap_or(0)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn path_file_name(path: i64) -> i64 {
    let p = path_from(path);
    p.file_name()
        .map(|x| to_cstring_ptr(&x.to_string_lossy()))
        .unwrap_or(0)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn path_extension(path: i64) -> i64 {
    let p = path_from(path);
    p.extension()
        .map(|x| to_cstring_ptr(&x.to_string_lossy()))
        .unwrap_or(0)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn path_has_extension(path: i64, ext: i64) -> i64 {
    let p = path_from(path);
    let e = from_cstr(ext);
    p.extension().map(|x| x == e.as_str()).unwrap_or(false) as i64
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn path_is_absolute(path: i64) -> i64 {
    path_from(path).is_absolute() as i64
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn path_join(path: i64, other: i64) -> i64 {
    let p = path_from(path);
    let o = path_from(other);
    let joined = p.join(o);
    to_cstring_ptr(&joined.to_string_lossy())
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn path_absolute(path: i64) -> i64 {
    let p = path_from(path);
    match std::fs::canonicalize(&p) {
        Ok(abs) => to_cstring_ptr(&abs.to_string_lossy()),
        Err(_) => 0,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn path_as_str(path: i64) -> i64 {
    path
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn path_set_extension(path: i64, ext: i64) -> i64 {
    let mut pb = path_from(path);
    pb.set_extension(from_cstr(ext));
    to_cstring_ptr(&pb.to_string_lossy())
}
