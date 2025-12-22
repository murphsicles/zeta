// src/runtime/actor.rs
//! Provides the actor model runtime for concurrency in Zeta.
//! Implements channel-based messaging with a work-stealing scheduler on a thread pool.
//! Supports asynchronous non-blocking spawn and receive operations using Tokio tasks and MPSC channels.
//! Includes host functions for result handling, map operations, and external interactions like HTTP and TLS.
use num_cpus;
use reqwest::blocking::Client;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::ffi::c_void;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::{Arc, OnceLock};
use tokio::sync::{Mutex, mpsc};
use tokio::task;
type Message = i64;
/// Global counter for unique channel IDs.
#[allow(dead_code)]
static CHANNEL_ID_COUNTER: AtomicI64 = AtomicI64::new(0);
/// Global map of channel IDs to channels.
static CHANNEL_MAP: OnceLock<Arc<Mutex<HashMap<i64, Channel>>>> = OnceLock::new();
/// Communication channel for actor messages, compatible with C representations.
#[repr(C)]
#[derive(Clone, Debug)]
pub struct Channel {
    inner: Arc<ChannelInner>,
}
/// Internal structure managing the message queue and receiver.
#[derive(Debug)]
struct ChannelInner {
    queue: Mutex<mpsc::Sender<Message>>,
    rx: Mutex<mpsc::Receiver<Message>>,
}
impl Default for Channel {
    fn default() -> Self {
        Self::new()
    }
}
impl Channel {
    /// Creates a new unbounded channel with capacity for 1024 messages.
    pub fn new() -> Self {
        let (tx, rx) = mpsc::channel(1024);
        let inner = Arc::new(ChannelInner {
            queue: Mutex::new(tx),
            rx: Mutex::new(rx),
        });
        Self { inner }
    }
    /// Attempts a non-blocking send of a message to the channel.
    pub fn send(&self, msg: Message) -> Result<(), mpsc::error::TrySendError<Message>> {
        let tx = self.inner.queue.blocking_lock();
        tx.try_send(msg)
    }
    /// Asynchronously receives a message from the channel.
    pub async fn recv(&self) -> Option<Message> {
        let mut rx = self.inner.rx.lock().await;
        rx.recv().await
    }
}
/// Initializes the global channel map.
#[allow(dead_code)]
fn init_channel_map() {
    let _ = CHANNEL_MAP.set(Arc::new(Mutex::new(HashMap::new())));
}
/// Registers a channel and returns its ID.
#[allow(dead_code)]
fn register_channel(chan: Channel) -> i64 {
    init_channel_map();
    let id = CHANNEL_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
    if let Some(map) = CHANNEL_MAP.get() {
        let mut guard = tokio::runtime::Runtime::new().unwrap().block_on(map.lock());
        guard.insert(id, chan);
    }
    id
}
/// Looks up a channel by ID.
async fn get_channel(id: i64) -> Option<Channel> {
    if let Some(map) = CHANNEL_MAP.get() {
        let guard = map.lock().await;
        guard.get(&id).cloned()
    } else {
        None
    }
}
/// Host function to send a message to a channel.
/// # Safety
/// No safety concerns as parameters are plain i64 values.
pub unsafe extern "C" fn host_channel_send(chan_id: i64, msg: i64) -> i64 {
    tokio::runtime::Runtime::new().unwrap().block_on(async {
        if let Some(chan) = get_channel(chan_id).await {
            if chan.send(msg).is_ok() { 0 } else { -1 }
        } else {
            -1
        }
    })
}
/// Host function to receive a message from a channel.
/// # Safety
/// No safety concerns as parameters are plain i64 values.
pub unsafe extern "C" fn host_channel_recv(chan_id: i64) -> i64 {
    tokio::runtime::Runtime::new().unwrap().block_on(async {
        if let Some(chan) = get_channel(chan_id).await {
            chan.recv().await.unwrap_or(0)
        } else {
            0
        }
    })
}
/// Host function to perform an HTTP GET request and return response length.
/// # Safety
/// The `url` pointer must be a valid null-terminated C string.
pub unsafe extern "C" fn host_http_get(url: *const std::ffi::c_char) -> i64 {
    use std::ffi::CStr;
    if let Ok(url_str) = unsafe { CStr::from_ptr(url) }.to_str() {
        let client = Client::new();
        match client.get(url_str).send() {
            Ok(resp) if resp.status().is_success() => match resp.text() {
                Ok(body) => body.len() as i64,
                Err(_) => -1,
            },
            _ => -1,
        }
    } else {
        -1i64
    }
}
/// Host function to perform a TLS handshake.
/// # Safety
/// The `host` pointer must be a valid null-terminated C string.
pub unsafe extern "C" fn host_tls_handshake(host: *const std::ffi::c_char) -> i64 {
    use std::ffi::CStr;
    if let Ok(host_str) = unsafe { CStr::from_ptr(host) }.to_str() {
        // For simplicity, use reqwest with HTTPS to simulate handshake success
        let client = Client::new();
        let url = format!("https://{}", host_str);
        match clie...(truncated 1874 characters)...: i64) -> *mut c_void {
    Box::into_raw(Box::new(ResultInner { tag: true, data })) as *mut c_void
}
/// Host function to create an error result.
/// # Safety
/// Pointer must be valid from make_ok/err.
pub unsafe extern "C" fn host_result_make_err(data: i64) -> *mut c_void {
    Box::into_raw(Box::new(ResultInner { tag: false, data })) as *mut c_void
}
/// Host function to check if a result is ok.
/// # Safety
/// Pointer must be valid Result ptr.
pub unsafe extern "C" fn host_result_is_ok(ptr: *const c_void) -> i64 {
    if ptr.is_null() {
        0
    } else {
        unsafe { (*(ptr as *const ResultInner)).tag as i64 }
    }
}
/// Host function to retrieve data from a result.
/// # Safety
/// Pointer must be valid Result ptr.
pub unsafe extern "C" fn host_result_get_data(ptr: *const c_void) -> i64 {
    if ptr.is_null() {
        0
    } else {
        unsafe { (*(ptr as *const ResultInner)).data }
    }
}
/// Host function to free a result pointer.
/// # Safety
/// Pointer must be valid from make_ok/err, not freed before.
pub unsafe extern "C" fn host_result_free(ptr: *mut c_void) {
    if !ptr.is_null() {
        unsafe {
            let _ = Box::from_raw(ptr as *mut ResultInner);
        }
    }
}
/// Host function to create a new map.
/// # Safety
/// No params.
pub unsafe extern "C" fn host_map_new() -> *mut c_void {
    Box::into_raw(Box::new(MapInner::new())) as *mut c_void
}
/// Host function to insert a key-value pair into a map.
/// # Safety
/// Pointer must be valid Map ptr.
pub unsafe extern "C" fn host_map_insert(ptr: *mut c_void, key: i64, val: i64) {
    if !ptr.is_null() {
        unsafe {
            (*(ptr as *mut MapInner)).insert(key, val);
        }
    }
}
/// Host function to get a value from a map by key.
/// # Safety
/// Pointer must be valid Map ptr.
pub unsafe extern "C" fn host_map_get(ptr: *const c_void, key: i64) -> i64 {
    if ptr.is_null() {
        0
    } else {
        unsafe { (*(ptr as *const MapInner)).get(&key).cloned().unwrap_or(0) }
    }
}
/// Host function to free a map pointer.
/// # Safety
/// Pointer must be valid from map_new, not freed before.
pub unsafe extern "C" fn host_map_free(ptr: *mut c_void) {
    if !ptr.is_null() {
        unsafe {
            let _ = Box::from_raw(ptr as *mut MapInner);
        }
    }
}
/// Represents an actor with its channel and entry function.
struct Actor {
    chan: Channel,
    func: Box<dyn FnOnce(Channel) + Send + 'static>,
}
/// Global singleton for the scheduler.
static SCHEDULER: OnceLock<Arc<Scheduler>> = OnceLock::new();
/// Manages scheduling of actors across threads with work-stealing.
struct Scheduler {
    actors: Mutex<VecDeque<Actor>>,
    _tasks: Mutex<Vec<task::JoinHandle<()>>>,
}
impl Scheduler {
    /// Creates a new scheduler with worker tasks based on CPU count.
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
    /// Worker loop that processes actors or yields when idle.
    async fn worker_loop(self: Arc<Self>) {
        loop {
            let actor_opt = {
                let mut actors = self.actors.lock().await;
                actors.pop_front()
            };
            if let Some(actor) = actor_opt {
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
    /// Enqueues a new actor for scheduling.
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
    /// Initializes the global scheduler instance.
    pub fn init() {
        let rt = tokio::runtime::Runtime::new().unwrap();
        let sched = rt.block_on(Self::new(num_cpus::get().max(1)));
        let _ = SCHEDULER.set(sched);
    }
}
/// Initializes the actor runtime.
pub fn init_runtime() {
    Scheduler::init();
}
/// Spawns an actor asynchronously.
pub async fn spawn<F>(f: F)
where
    F: FnOnce(Channel) + Send + 'static,
{
    Scheduler::spawn(f).await;
}
}
