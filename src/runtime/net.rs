//! TCP networking runtime functions for Zeta std::net

use std::ffi::CString;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::Mutex;

fn to_cstring_ptr(s: &str) -> i64 {
    let c = CString::new(s).unwrap();
    let len = c.as_bytes_with_nul().len();
    let p = unsafe { crate::runtime::std::std_malloc(len) };
    if p == 0 {
        return 0;
    }
    unsafe {
        std::ptr::copy_nonoverlapping(c.as_ptr(), p as *mut i8, len);
    }
    p
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

lazy_static::lazy_static! {
    static ref STREAMS: Mutex<std::collections::HashMap<i64, TcpStream>> = Mutex::new(std::collections::HashMap::new());
    static ref LISTENERS: Mutex<std::collections::HashMap<i64, TcpListener>> = Mutex::new(std::collections::HashMap::new());
}

static NEXT_ID: std::sync::atomic::AtomicI64 = std::sync::atomic::AtomicI64::new(1);

#[unsafe(no_mangle)]
pub unsafe extern "C" fn tcp_connect(host: i64, port: i64) -> i64 {
    let h = from_cstr(host);
    let addr = format!("{}:{}", h, port);
    match TcpStream::connect(&addr) {
        Ok(stream) => {
            let id = NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
            STREAMS.lock().unwrap().insert(id, stream);
            id
        }
        Err(_) => -1,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn tcp_write(stream: i64, data: i64, len: i64) -> i64 {
    unsafe {
        let mut s = STREAMS.lock().unwrap();
        if let Some(stream) = s.get_mut(&stream) {
            let buf = std::slice::from_raw_parts(data as *const u8, len as usize);
            match stream.write_all(buf) {
                Ok(_) => len,
                Err(_) => -1,
            }
        } else {
            -1
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn tcp_read(stream: i64, buf: i64, len: i64) -> i64 {
    unsafe {
        let mut s = STREAMS.lock().unwrap();
        if let Some(stream) = s.get_mut(&stream) {
            let buf = std::slice::from_raw_parts_mut(buf as *mut u8, len as usize);
            match stream.read(buf) {
                Ok(n) => n as i64,
                Err(_) => -1,
            }
        } else {
            -1
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn tcp_close(stream: i64) {
    STREAMS.lock().unwrap().remove(&stream);
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn tcp_bind(host: i64, port: i64) -> i64 {
    let h = from_cstr(host);
    let addr = format!("{}:{}", h, port);
    match TcpListener::bind(&addr) {
        Ok(listener) => {
            let id = NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
            LISTENERS.lock().unwrap().insert(id, listener);
            id
        }
        Err(_) => -1,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn tcp_accept(listener: i64) -> i64 {
    let l = LISTENERS.lock().unwrap();
    if let Some(l) = l.get(&listener) {
        match l.accept() {
            Ok((stream, _)) => {
                let id = NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                STREAMS.lock().unwrap().insert(id, stream);
                id
            }
            Err(_) => -1,
        }
    } else {
        -1
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn tcp_peer_addr(stream: i64) -> i64 {
    let s = STREAMS.lock().unwrap();
    if let Some(s) = s.get(&stream) {
        match s.peer_addr() {
            Ok(addr) => to_cstring_ptr(&addr.to_string()),
            Err(_) => 0,
        }
    } else {
        0
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn tcp_local_addr(stream: i64) -> i64 {
    let s = STREAMS.lock().unwrap();
    if let Some(s) = s.get(&stream) {
        match s.local_addr() {
            Ok(addr) => to_cstring_ptr(&addr.to_string()),
            Err(_) => 0,
        }
    } else {
        0
    }
}
