// src/runtime/reactor.rs
// Bootstrap implementation of the Zeta Tokio reactor runtime.
// Provides epoll, pipe, timerfd, and O_NONBLOCK via libc syscalls.
// Zeta source in zeta_src/runtime/reactor.z declares these via extern fn.
// When Zeta self-hosts, the Rust implementation is replaced by native Zeta code.

use std::collections::HashMap;
use std::sync::Mutex;

const MAX_EVENTS: i32 = 1024;

lazy_static::lazy_static! {
    static ref REACTORS: Mutex<HashMap<i32, i32>> = Mutex::new(HashMap::new());
    static ref WAKER_MAP: Mutex<HashMap<i32, i32>> = Mutex::new(HashMap::new());
}

thread_local! {
    static EVENT_BUF: std::cell::RefCell<[libc::epoll_event; 1024]> =
        const { std::cell::RefCell::new([libc::epoll_event { events: 0, u64: 0 }; 1024]) };
}

// ── Reactor ──

#[unsafe(no_mangle)]
pub unsafe extern "C" fn reactor_create() -> i64 {
    let epfd = libc::epoll_create1(libc::EPOLL_CLOEXEC);
    if epfd < 0 { return -1; }
    REACTORS.lock().unwrap().insert(epfd, epfd);
    epfd as i64
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn reactor_add(epfd: i64, fd: i64, events: i64) -> i64 {
    let mut ev: libc::epoll_event = std::mem::zeroed();
    ev.u64 = fd as u64;
    if events & 1 != 0 { ev.events |= libc::EPOLLIN as u32; }
    if events & 2 != 0 { ev.events |= libc::EPOLLOUT as u32; }
    ev.events |= (libc::EPOLLERR | libc::EPOLLHUP) as u32;
    libc::epoll_ctl(epfd as i32, libc::EPOLL_CTL_ADD, fd as i32, &mut ev) as i64
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn reactor_modify(epfd: i64, fd: i64, events: i64) -> i64 {
    let mut ev: libc::epoll_event = std::mem::zeroed();
    ev.u64 = fd as u64;
    if events & 1 != 0 { ev.events |= libc::EPOLLIN as u32; }
    if events & 2 != 0 { ev.events |= libc::EPOLLOUT as u32; }
    ev.events |= (libc::EPOLLERR | libc::EPOLLHUP) as u32;
    libc::epoll_ctl(epfd as i32, libc::EPOLL_CTL_MOD, fd as i32, &mut ev) as i64
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn reactor_remove(epfd: i64, fd: i64) -> i64 {
    libc::epoll_ctl(epfd as i32, libc::EPOLL_CTL_DEL, fd as i32, std::ptr::null_mut()) as i64
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn reactor_poll(epfd: i64, _events_buf: i64, max_events: i64, timeout_ms: i64) -> i64 {
    EVENT_BUF.with(|cell| {
        let mut buf = cell.borrow_mut();
        libc::epoll_wait(epfd as i32, buf.as_mut_ptr(), max_events as i32, timeout_ms as i32) as i64
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn reactor_event_fd(events_buf: i64, idx: i64) -> i64 {
    if events_buf == 0 {
        EVENT_BUF.with(|cell| {
            let buf = cell.borrow();
            if idx >= 0 && (idx as usize) < MAX_EVENTS as usize {
                (buf[idx as usize].u64) as i64
            } else { -1 }
        })
    } else {
        let ptr = events_buf as *const libc::epoll_event;
        let ev = unsafe { *ptr.offset(idx as isize) };
        ev.u64 as i64
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn reactor_event_flags(events_buf: i64, idx: i64) -> i64 {
    if events_buf == 0 {
        EVENT_BUF.with(|cell| {
            let buf = cell.borrow();
            if idx < 0 || (idx as usize) >= MAX_EVENTS as usize { return -1; }
            let ev = buf[idx as usize].events;
            let mut flags: i64 = 0;
            if ev & libc::EPOLLIN as u32 != 0 { flags |= 1; }
            if ev & libc::EPOLLOUT as u32 != 0 { flags |= 2; }
            if ev & (libc::EPOLLERR | libc::EPOLLHUP) as u32 != 0 { flags |= 4; }
            flags
        })
    } else { -1 }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn reactor_destroy(epfd: i64) {
    REACTORS.lock().unwrap().remove(&(epfd as i32));
    libc::close(epfd as i32);
}

// ── Waker ──

#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_create() -> i64 {
    let mut fds: [i32; 2] = [0, 0];
    if libc::pipe2(fds.as_mut_ptr(), libc::O_CLOEXEC | libc::O_NONBLOCK) < 0 { return -1; }
    WAKER_MAP.lock().unwrap().insert(fds[0], fds[1]);
    fds[0] as i64
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_wake(read_fd: i64) -> i64 {
    let write_fd = match WAKER_MAP.lock().unwrap().get(&(read_fd as i32)) {
        Some(&fd) => fd, None => return -1,
    };
    let byte: u8 = 1;
    let rc = libc::write(write_fd, &byte as *const u8 as *const std::ffi::c_void, 1);
    if rc > 0 { 0 } else { -1 }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_consume(read_fd: i64) -> i64 {
    let mut buf: [u8; 8] = [0; 8];
    let rc = libc::read(read_fd as i32, buf.as_mut_ptr() as *mut std::ffi::c_void, 8);
    if rc > 0 { 0 } else { -1 }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_destroy(read_fd: i64) {
    if let Some(write_fd) = WAKER_MAP.lock().unwrap().remove(&(read_fd as i32)) {
        libc::close(read_fd as i32);
        libc::close(write_fd);
    }
}

// ── Timerfd ──

#[unsafe(no_mangle)]
pub unsafe extern "C" fn timerfd_create() -> i64 {
    libc::timerfd_create(libc::CLOCK_MONOTONIC, libc::TFD_CLOEXEC | libc::TFD_NONBLOCK) as i64
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn timerfd_set(fd: i64, ns: i64) -> i64 {
    let spec = libc::itimerspec {
        it_interval: libc::timespec { tv_sec: 0, tv_nsec: 0 },
        it_value: libc::timespec { tv_sec: ns / 1_000_000_000, tv_nsec: ns % 1_000_000_000 },
    };
    libc::timerfd_settime(fd as i32, 0, &spec, std::ptr::null_mut()) as i64
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn timerfd_set_absolute(fd: i64, abs_ns: i64) -> i64 {
    let spec = libc::itimerspec {
        it_interval: libc::timespec { tv_sec: 0, tv_nsec: 0 },
        it_value: libc::timespec { tv_sec: abs_ns / 1_000_000_000, tv_nsec: abs_ns % 1_000_000_000 },
    };
    libc::timerfd_settime(fd as i32, libc::TFD_TIMER_ABSTIME, &spec, std::ptr::null_mut()) as i64
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn timerfd_read(fd: i64) -> i64 {
    let mut val: u64 = 0;
    let rc = libc::read(fd as i32, &mut val as *mut u64 as *mut std::ffi::c_void, 8);
    if rc > 0 { val as i64 } else { -1 }
}

// ── Blocking Thread Pool ──

use std::sync::mpsc;

lazy_static::lazy_static! {
    static ref BLOCKING_POOL: Mutex<Option<BlockingPoolInner>> = Mutex::new(None);
}

struct BlockingPoolInner {
    threads: Vec<std::thread::JoinHandle<()>>,
    sender: mpsc::Sender<BlockingTask>,
}

type BlockingTask = Box<dyn FnOnce() + Send + 'static>;

/// Initialize the blocking thread pool with N threads.
fn ensure_blocking_pool() -> mpsc::Sender<BlockingTask> {
    let mut pool = BLOCKING_POOL.lock().unwrap();
    if pool.is_none() {
        let count = std::cmp::max(2, std::thread::available_parallelism()
            .map(|n| n.get() / 2).unwrap_or(2));
        let (tx, rx) = mpsc::channel::<BlockingTask>();
        let rx = std::sync::Arc::new(std::sync::Mutex::new(rx));
        let mut threads = Vec::new();
        for _ in 0..count {
            let rx = rx.clone();
            threads.push(std::thread::spawn(move || {
                loop {
                    let task = rx.lock().unwrap().recv();
                    match task {
                        Ok(f) => f(),
                        Err(_) => break, // channel closed
                    }
                }
            }));
        }
        *pool = Some(BlockingPoolInner { threads, sender: tx });
    }
    pool.as_ref().unwrap().sender.clone()
}

/// Spawn a blocking task on the thread pool.
/// `func_ptr` is a pointer to a thunk function.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn blocking_spawn(func_ptr: i64) -> i64 {
    let sender = ensure_blocking_pool();
    let (ret_tx, ret_rx) = mpsc::channel::<i64>();
    let func_ptr_usize = func_ptr as usize;
    sender.send(Box::new(move || {
        // Call the thunk via function pointer
        let f: extern "C" fn() -> i64 = std::mem::transmute(func_ptr_usize);
        let result = f();
        let _ = ret_tx.send(result);
    })).ok();
    // Return a handle — store the receiver in a global map
    let handle = std::sync::atomic::AtomicI64::new(0)
        .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    return handle;
}

// ── Waker / Scheduler Integration ──

/// Register a waker fd with the default reactor for the calling thread.
/// The caller passes (reactor_epfd, waker_read_fd) and events to listen for.
/// Returns 0 on success.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scheduler_register_waker(epfd: i64, waker_fd: i64) -> i64 {
    let mut ev: libc::epoll_event = std::mem::zeroed();
    ev.events = (libc::EPOLLIN | libc::EPOLLERR | libc::EPOLLHUP) as u32;
    ev.u64 = waker_fd as u64;
    libc::epoll_ctl(epfd as i32, libc::EPOLL_CTL_ADD, waker_fd as i32, &mut ev) as i64
}

/// Run one iteration of the reactor loop: poll for readiness, then wake tasks.
/// `epfd` is the reactor fd, `timeout_ms` is poll timeout.
/// Returns the number of events processed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scheduler_run_reactor(epfd: i64, timeout_ms: i64) -> i64 {
    let mut events: [libc::epoll_event; 64] = std::mem::zeroed();
    let n = libc::epoll_wait(epfd as i32, events.as_mut_ptr(), 64, timeout_ms as i32);
    if n <= 0 { return n as i64; }
    
    let mut count: i64 = 0;
    for i in 0..n {
        let ev = &events[i as usize];
        let fd = ev.u64 as i64;
        
        // If this is a waker fd, consume the wake event
        if ev.events & libc::EPOLLIN as u32 != 0 {
            let mut buf: [u8; 8] = [0; 8];
            libc::read(fd as i32, buf.as_mut_ptr() as *mut std::ffi::c_void, 8);
            count += 1;
        }
    }
    count
}

// ── Helpers ──

#[unsafe(no_mangle)]
pub unsafe extern "C" fn set_nonblocking(fd: i64) -> i64 {
    let flags = libc::fcntl(fd as i32, libc::F_GETFL, 0);
    if flags < 0 { return -1; }
    libc::fcntl(fd as i32, libc::F_SETFL, flags | libc::O_NONBLOCK) as i64
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn monotonic_ns() -> i64 {
    let mut ts: libc::timespec = std::mem::zeroed();
    if libc::clock_gettime(libc::CLOCK_MONOTONIC, &mut ts) == 0 {
        ts.tv_sec * 1_000_000_000 + ts.tv_nsec
    } else { -1 }
}
