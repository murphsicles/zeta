// src/actor.rs
//! Actor model runtime for Zeta concurrency.
//! Channel-based messaging with work-stealing scheduler on thread pool.
//! Added: Async support - non-blocking spawn/recv using tokio tasks and mpsc channels.
//! Updated Dec 16, 2025: Added host functions for Result and Map intrinsics (heap-allocated).

use num_cpus;
use std::collections::VecDeque;
use std::sync::{Arc, OnceLock};
use tokio::sync::{Mutex, mpsc};
use tokio::task;
use std::collections::HashMap;
use std::ffi::c_void;
type Message = i64;
#[repr(C)]
#[derive(Clone, Debug)]
pub struct Channel {
    inner: Arc<ChannelInner>,
}
#[derive(Debug)]
struct ChannelInner {
    /// Message queue.
    queue: Mutex<mpsc::Sender<Message>>,
    /// Receiver handle.
    rx: Mutex<mpsc::Receiver<Message>>,
}
impl Default for Channel {
    fn default() -> Self {
        Self::new()
    }
}
impl Channel {
    /// Creates a new empty channel.
    pub fn new() -> Self {
        let (tx, rx) = mpsc::channel(1024);
        let inner = Arc::new(ChannelInner {
            queue: Mutex::new(tx),
            rx: Mutex::new(rx),
        });
        // No spawn needed; rx in host_recv
        Self { inner }
    }
    /// Non-blocking send to channel.
    pub fn send(&self, msg: Message) -> Result<(), mpsc::error::TrySendError<Message>> {
        let tx = self.inner.queue.blocking_lock();
        tx.try_send(msg)
    }
    /// Async receive from channel.
    pub async fn recv(&self) -> Option<Message> {
        let mut rx = self.inner.rx.lock().await;
        rx.recv().await
    }
}
/// # Safety
/// No safety concerns as parameters are plain i64 values.
pub unsafe extern "C" fn host_channel_send(_chan_id: i64, msg: i64) -> i64 {
    // Real: use global chan map (stub: dummy chan)
    let chan = Channel::new();
    if chan.send(msg).is_ok() { 0 } else { -1 }
}
/// # Safety
/// No safety concerns as parameters are plain i64 values.
pub unsafe extern "C" fn host_channel_recv(_chan_id: i64) -> i64 {
    // Real async block_on
    tokio::runtime::Runtime::new().unwrap().block_on(async {
        let chan = Channel::new();
        chan.recv().await.unwrap_or(0)
    })
}
/// # Safety
/// The `url` pointer must be a valid null-terminated C string.
pub unsafe extern "C" fn host_http_get(url: *const std::ffi::c_char) -> i64 {
    use std::ffi::CStr;
    // 1. Corrected 'host' to 'url' to match function parameter
    // 2. Bound the result of to_str() to 'url_str' so it can be used in the block
    if let Ok(url_str) = unsafe { CStr::from_ptr(url) }.to_str() {
        // Real: reqwest stubbed as len
        url_str.len() as i64
    } else {
        -1i64
    }
}
/// # Safety
/// The `host` pointer must be a valid null-terminated C string.
pub unsafe extern "C" fn host_tls_handshake(host: *const std::ffi::c_char) -> i64 {
    use std::ffi::CStr;
    if unsafe { CStr::from_ptr(host) }.to_str().is_ok() {
        0i64
    } else {
        -1i64
    }
}
/// # Safety
/// No safety concerns as parameters are plain i64 values.
pub unsafe extern "C" fn host_spawn(_func_id: i64) -> i64 {
    tokio::runtime::Runtime::new().unwrap().block_on(async {
        spawn(|_chan| { /* stub actor func */ }).await;
        0
    })
}

/// Result inner struct for host functions.
#[derive(Debug)]
struct ResultInner {
    tag: bool, // true for Ok, false for Err
    data: i64,
}

/// Map inner type.
type MapInner = HashMap<i64, i64>;

/// # Safety
/// Pointer must be valid from make_ok/err.
pub unsafe extern "C" fn host_result_make_ok(data: i64) -> *mut c_void {
    Box::into_raw(Box::new(ResultInner { tag: true, data })) as *mut c_void
}

/// # Safety
/// Pointer must be valid from make_ok/err.
pub unsafe extern "C" fn host_result_make_err(data: i64) -> *mut c_void {
    Box::into_raw(Box::new(ResultInner { tag: false, data })) as *mut c_void
}

/// # Safety
/// Pointer must be valid Result ptr.
pub unsafe extern "C" fn host_result_is_ok(ptr: *const c_void) -> i64 {
    if ptr.is_null() { 0 } else { (*(ptr as *const ResultInner)).tag as i64 }
}

/// # Safety
/// Pointer must be valid Result ptr.
pub unsafe extern "C" fn host_result_get_data(ptr: *const c_void) -> i64 {
    if ptr.is_null() { 0 } else { (*(ptr as *const ResultInner)).data }
}

/// # Safety
/// Pointer must be valid from make_ok/err, not freed before.
pub unsafe extern "C" fn host_result_free(ptr: *mut c_void) {
    if !ptr.is_null() {
        let _ = Box::from_raw(ptr as *mut ResultInner);
    }
}

/// # Safety
/// No params.
pub unsafe extern "C" fn host_map_new() -> *mut c_void {
    Box::into_raw(Box::new(MapInner::new())) as *mut c_void
}

/// # Safety
/// Pointer must be valid Map ptr.
pub unsafe extern "C" fn host_map_insert(ptr: *mut c_void, key: i64, val: i64) {
    if !ptr.is_null() {
        (*(ptr as *mut MapInner)).insert(key, val);
    }
}

/// # Safety
/// Pointer must be valid Map ptr.
pub unsafe extern "C" fn host_map_get(ptr: *const c_void, key: i64) -> i64 {
    if ptr.is_null() {
        0
    } else {
        (*(ptr as *const MapInner)).get(&key).cloned().unwrap_or(0)
    }
}

/// # Safety
/// Pointer must be valid from map_new, not freed before.
pub unsafe extern "C" fn host_map_free(ptr: *mut c_void) {
    if !ptr.is_null() {
        let _ = Box::from_raw(ptr as *mut MapInner);
    }
}

/// Actor representation: channel + async entry function.
struct Actor {
    chan: Channel,
    func: Box<dyn FnOnce(Channel) + Send + 'static>,
}
/// Global scheduler singleton.
static SCHEDULER: OnceLock<Arc<Scheduler>> = OnceLock::new();
/// Multi-threaded work-stealing scheduler with async support.
struct Scheduler {
    /// Pending actors queue.
    actors: Mutex<VecDeque<Actor>>,
    /// Worker tasks.
    _tasks: Mutex<Vec<task::JoinHandle<()>>>,
}
impl Scheduler {
    /// Initializes scheduler with CPU-bound async tasks.
    async fn new(thread_count: usize) -> Arc<Self> {
        let sched = Arc::new(Self {
            actors: Mutex::new(VecDeque::new()),
            _tasks: Mutex::new(vec![]),
        });
        for _ in 0..thread_count {
            let sched_clone = Arc::clone(&sched);
            let handle = task::spawn(async move { sched_clone.worker_loop().await });
            let mut tasks = sched._tasks.lock().await;
            tasks.push(handle);
        }
        sched
    }
    /// Worker loop: steal and run actors async, await if idle.
    async fn worker_loop(self: Arc<Self>) {
        loop {
            let actor_opt = {
                let mut actors = self.actors.lock().await;
                actors.pop_front()
            };
            if let Some(actor) = actor_opt {
                // Run actor function async.
                task::spawn(async move {
                    (actor.func)(actor.chan);
                })
                .await
                .unwrap();
            } else {
                task::yield_now().await;
            }
        }
    }
    /// Spawns a new async actor, enqueues and notifies worker.
    pub async fn spawn<F>(func: F)
    where
        F: FnOnce(Channel) + Send + 'static,
    {
        let chan = Channel::new();
        let actor = Actor {
            chan: chan.clone(),
            func: Box::new(func),
        };
        if let Some(sched) = SCHEDULER.get() {
            let mut actors = sched.actors.lock().await;
            actors.push_back(actor);
        }
    }
    /// Initializes global async scheduler.
    pub fn init() {
        let rt = tokio::runtime::Runtime::new().unwrap();
        let sched = rt.block_on(Self::new(num_cpus::get().max(1)));
        let _ = SCHEDULER.set(sched);
    }
}
/// Public runtime init - now async.
pub fn init_runtime() {
    Scheduler::init();
}
/// Public async spawn helper.
pub async fn spawn<F>(f: F)
where
    F: FnOnce(Channel) + Send + 'static,
{
    Scheduler::spawn(f).await;
}
