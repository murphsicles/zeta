//! Process runtime — wraps Rust std::process::Command

use std::ffi::CString;
use std::process::Command;
use std::sync::Mutex;

lazy_static::lazy_static! {
    static ref COMMANDS: Mutex<std::collections::HashMap<i64, Command>> = Mutex::new(std::collections::HashMap::new());
    static ref OUTPUTS: Mutex<std::collections::HashMap<i64, std::process::Output>> = Mutex::new(std::collections::HashMap::new());
}

static NEXT_ID: std::sync::atomic::AtomicI64 = std::sync::atomic::AtomicI64::new(1);

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

#[unsafe(no_mangle)]
pub unsafe extern "C" fn process_command_new(cmd: i64) -> i64 {
    let prog = from_cstr(cmd);
    let id = NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    COMMANDS.lock().unwrap().insert(id, Command::new(&prog));
    id
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn process_command_arg(cmd: i64, arg: i64) {
    let a = from_cstr(arg);
    if let Some(c) = COMMANDS.lock().unwrap().get_mut(&cmd) {
        c.arg(&a);
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn process_command_output(cmd: i64) -> i64 {
    let mut cmds = COMMANDS.lock().unwrap();
    if let Some(mut c) = cmds.remove(&cmd) {
        match c.output() {
            Ok(output) => {
                let id = NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                OUTPUTS.lock().unwrap().insert(id, output);
                return id;
            }
            Err(_) => return -1,
        }
    }
    -1
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn process_command_status(cmd: i64) -> i64 {
    let mut cmds = COMMANDS.lock().unwrap();
    if let Some(mut c) = cmds.remove(&cmd) {
        match c.status() {
            Ok(status) => status.code().unwrap_or(-1) as i64,
            Err(_) => -1,
        }
    } else {
        -1
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn process_output_stdout(output: i64) -> i64 {
    let outputs = OUTPUTS.lock().unwrap();
    if let Some(o) = outputs.get(&output) {
        let c = CString::new(String::from_utf8_lossy(&o.stdout).as_ref()).unwrap();
        let len = c.as_bytes_with_nul().len();
        let ptr = unsafe { crate::runtime::std::std_malloc(len as usize) };
        if ptr != 0 {
            unsafe {
                std::ptr::copy_nonoverlapping(c.as_ptr(), ptr as *mut i8, len);
            }
        }
        ptr
    } else {
        0
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn process_output_stderr(output: i64) -> i64 {
    let outputs = OUTPUTS.lock().unwrap();
    if let Some(o) = outputs.get(&output) {
        let c = CString::new(String::from_utf8_lossy(&o.stderr).as_ref()).unwrap();
        let len = c.as_bytes_with_nul().len();
        let ptr = unsafe { crate::runtime::std::std_malloc(len as usize) };
        if ptr != 0 {
            unsafe {
                std::ptr::copy_nonoverlapping(c.as_ptr(), ptr as *mut i8, len);
            }
        }
        ptr
    } else {
        0
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn process_output_status(output: i64) -> i64 {
    let outputs = OUTPUTS.lock().unwrap();
    if let Some(o) = outputs.get(&output) {
        o.status.code().unwrap_or(-1) as i64
    } else {
        -1
    }
}
