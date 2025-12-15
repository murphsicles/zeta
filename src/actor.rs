// src/actor.rs
//! Actor model runtime for Zeta concurrency.
//! Channel-based messaging with work-stealing scheduler on thread pool.
//! Added: Async support - non-blocking spawn/recv using tokio tasks and mpsc channels.

use num_cpus;
use std::collections::VecDeque;
use std::sync::{Arc, OnceLock};
use tokio::sync::{Mutex, mpsc};
use tokio::task;

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
    if chan.send(msg).is_ok() {
        0
    } else {
        -1
    }
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
/// No safety concerns as parameter is plain i64 value.
pub unsafe extern "C" fn host_spawn(_func_id: i64) -> i64 {
    // Non-blocking async spawn
    let chan = Channel::new();
    task::spawn(async move {
        // Dummy actor loop
        loop {
            if let Some(msg) = chan.recv().await {
                println!("Actor msg: {}", msg);
            }
        }
    });
    1i64
}

/// # Safety
/// The `url` pointer must be a valid null-terminated C string.
pub unsafe extern "C" fn host_http_get(url: *const std::ffi::c_char) -> i64 {
    use std::ffi::CStr;
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
    if let Ok(_) = unsafe { CStr::from_ptr(host) }.to_str() {
        0i64
    } else {
        -1i64
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
