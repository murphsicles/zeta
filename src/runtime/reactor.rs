// src/runtime/reactor.rs
// Zeta Tokio Reactor — epoll, waker pipe, timerfd, non-blocking I/O
// Direct unsafe extern "C" functions callable from Zeta via extern fn.

use std::collections::HashMap;
use std::sync::Mutex;

// ── Global state ──

lazy_static::lazy_static! {
    static ref REACTORS: Mutex<HashMap<i32, i32>> = Mutex::new(HashMap::new());
    static ref WAKER_WRITE_ENDS: Mutex<HashMap<i32, i32>> = Mutex::new(HashMap::new());
}

/// Create epoll reactor. Returns reactor fd, or -1 on error.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reactor_create() -> i64 {
    let epfd = libc::epoll_create1(libc::EPOLL_CLOEXEC);
    if epfd < 0 { return -1; }
    REACTORS.lock().unwrap().insert(epfd, epfd);
    epfd as i64
}

/// Register fd for events (1=read, 2=write, 3=both).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reactor_add(epfd: i64, fd: i64, events: i64) -> i64 {
    let mut ev: libc::epoll_event = std::mem::zeroed();
    ev.u64 = fd as u64;
    if events & 1 != 0 { ev.events |= libc::EPOLLIN as u32; }
    if events & 2 != 0 { ev.events |= libc::EPOLLOUT as u32; }
    ev.events |= (libc::EPOLLERR | libc::EPOLLHUP) as u32;
    libc::epoll_ctl(epfd as i32, libc::EPOLL_CTL_ADD, fd as i32, &mut ev) as i64
}

/// Modify registered events for a fd.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reactor_modify(epfd: i64, fd: i64, events: i64) -> i64 {
    let mut ev: libc::epoll_event = std::mem::zeroed();
    ev.u64 = fd as u64;
    if events & 1 != 0 { ev.events |= libc::EPOLLIN as u32; }
    if events & 2 != 0 { ev.events |= libc::EPOLLOUT as u32; }
    ev.events |= (libc::EPOLLERR | libc::EPOLLHUP) as u32;
    libc::epoll_ctl(epfd as i32, libc::EPOLL_CTL_MOD, fd as i32, &mut ev) as i64
}

/// Remove fd from reactor.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reactor_remove(epfd: i64, fd: i64) -> i64 {
    libc::epoll_ctl(epfd as i32, libc::EPOLL_CTL_DEL, fd as i32, std::ptr::null_mut()) as i64
}

const MAX_EVENTS: i32 = 1024;

thread_local! {
    static EVENT_BUF: std::cell::RefCell<[libc::epoll_event; 1024]> =
        const { std::cell::RefCell::new([libc::epoll_event { events: 0, u64: 0 }; 1024]) };
}

/// Poll for events. timeout_ms: -1=infinite, 0=nonblocking, >0=ms timeout.
/// Returns number of ready events (0 timeout, -1 error).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reactor_poll(epfd: i64, timeout_ms: i64) -> i64 {
    EVENT_BUF.with(|buf_cell| {
        let mut buf = buf_cell.borrow_mut();
        libc::epoll_wait(epfd as i32, buf.as_mut_ptr(), MAX_EVENTS, timeout_ms as i32) as i64
    })
}

/// Get the fd for ready event at index n (0-based).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reactor_event_fd(n: i64) -> i64 {
    EVENT_BUF.with(|buf_cell| {
        let buf = buf_cell.borrow();
        if n >= 0 && (n as usize) < MAX_EVENTS as usize {
            (buf[n as usize].u64) as i64
        } else { -1 }
    })
}

/// Get event flags: 1=readable, 2=writable, 4=error.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reactor_event_flags(n: i64) -> i64 {
    EVENT_BUF.with(|buf_cell| {
        let buf = buf_cell.borrow();
        if n < 0 || (n as usize) >= MAX_EVENTS as usize { return -1; }
        let ev = buf[n as usize].events;
        let mut flags: i64 = 0;
        if ev & libc::EPOLLIN as u32 != 0 { flags |= 1; }
        if ev & libc::EPOLLOUT as u32 != 0 { flags |= 2; }
        if ev & (libc::EPOLLERR | libc::EPOLLHUP) as u32 != 0 { flags |= 4; }
        flags
    })
}

/// Destroy reactor.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reactor_destroy(epfd: i64) {
    REACTORS.lock().unwrap().remove(&(epfd as i32));
    libc::close(epfd as i32);
}

/// Create a waker pipe pair. Returns read end fd.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_create() -> i64 {
    let mut fds: [i32; 2] = [0, 0];
    if libc::pipe2(fds.as_mut_ptr(), libc::O_CLOEXEC | libc::O_NONBLOCK) < 0 {
        return -1;
    }
    WAKER_WRITE_ENDS.lock().unwrap().insert(fds[0], fds[1]);
    fds[0] as i64
}

/// Wake a waker (write 1 byte to its pipe).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_wake(read_fd: i64) -> i64 {
    let write_fd = match WAKER_WRITE_ENDS.lock().unwrap().get(&(read_fd as i32)) {
        Some(&fd) => fd,
        None => return -1,
    };
    let byte: u8 = 1;
    let rc = libc::write(write_fd, &byte as *const u8 as *const std::ffi::c_void, 1);
    if rc > 0 { 0 } else { -1 }
}

/// Consume waker event (drain pipe bytes).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_consume(read_fd: i64) -> i64 {
    let mut buf: [u8; 8] = [0; 8];
    let rc = libc::read(read_fd as i32, buf.as_mut_ptr() as *mut std::ffi::c_void, 8);
    if rc > 0 { 0 } else { -1 }
}

/// Destroy waker pipe.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_destroy(read_fd: i64) {
    if let Some(write_fd) = WAKER_WRITE_ENDS.lock().unwrap().remove(&(read_fd as i32)) {
        libc::close(read_fd as i32);
        libc::close(write_fd);
    }
}

/// Create a monotonic timerfd.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn timerfd_create() -> i64 {
    libc::timerfd_create(libc::CLOCK_MONOTONIC, libc::TFD_CLOEXEC | libc::TFD_NONBLOCK) as i64
}

/// Arm timerfd after ns nanoseconds from now.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn timerfd_set(fd: i64, ns: i64) -> i64 {
    let spec = libc::itimerspec {
        it_interval: libc::timespec { tv_sec: 0, tv_nsec: 0 },
        it_value: libc::timespec {
            tv_sec: (ns / 1_000_000_000),
            tv_nsec: (ns % 1_000_000_000),
        },
    };
    libc::timerfd_settime(fd as i32, 0, &spec, std::ptr::null_mut()) as i64
}

/// Arm timerfd at an absolute monotonic time.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn timerfd_set_absolute(fd: i64, abs_ns: i64) -> i64 {
    let spec = libc::itimerspec {
        it_interval: libc::timespec { tv_sec: 0, tv_nsec: 0 },
        it_value: libc::timespec {
            tv_sec: (abs_ns / 1_000_000_000),
            tv_nsec: (abs_ns % 1_000_000_000),
        },
    };
    libc::timerfd_settime(fd as i32, libc::TFD_TIMER_ABSTIME, &spec, std::ptr::null_mut()) as i64
}

/// Read (clear) a timerfd. Returns number of expirations.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn timerfd_read(fd: i64) -> i64 {
    let mut val: u64 = 0;
    let rc = libc::read(fd as i32, &mut val as *mut u64 as *mut std::ffi::c_void, 8);
    if rc > 0 { val as i64 } else { -1 }
}

/// Set a fd to non-blocking via fcntl.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn set_nonblocking(fd: i64) -> i64 {
    let flags = libc::fcntl(fd as i32, libc::F_GETFL, 0);
    if flags < 0 { return -1; }
    libc::fcntl(fd as i32, libc::F_SETFL, flags | libc::O_NONBLOCK) as i64
}

/// Get monotonic time in nanoseconds.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn monotonic_ns() -> i64 {
    let mut ts: libc::timespec = std::mem::zeroed();
    if libc::clock_gettime(libc::CLOCK_MONOTONIC, &mut ts) == 0 {
        ts.tv_sec * 1_000_000_000 + ts.tv_nsec
    } else { -1 }
}
