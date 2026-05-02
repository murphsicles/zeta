//! Lightweight thread runtime — wraps Rust std::thread + sleep

use std::sync::Mutex;

lazy_static::lazy_static! {
    static ref HANDLES: Mutex<std::collections::HashMap<i64, std::thread::JoinHandle<i64>>> = Mutex::new(std::collections::HashMap::new());
}

static NEXT_ID: std::sync::atomic::AtomicI64 = std::sync::atomic::AtomicI64::new(1);

/// Spawns a thread that calls func(arg) and returns the result.
/// Note: this spawns a real OS thread via std::thread::spawn.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn thread_spawn(func_ptr: i64, arg: i64) -> i64 {
    let id = NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    let handle = std::thread::spawn(move || {
        // Reconstruct function pointer from i64
        let func: extern "C" fn(i64) -> i64 = std::mem::transmute(func_ptr);
        func(arg)
    });
    HANDLES.lock().unwrap().insert(id, handle);
    id
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn thread_join(handle: i64) -> i64 {
    let mut handles = HANDLES.lock().unwrap();
    if let Some(h) = handles.remove(&handle) {
        h.join().unwrap_or(-1)
    } else {
        -1
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn thread_sleep_ms(ms: i64) {
    if ms > 0 {
        std::thread::sleep(std::time::Duration::from_millis(ms as u64));
    }
}
