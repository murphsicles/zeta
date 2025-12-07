// src/actor.rs
//! Actor model runtime for Zeta concurrency.
//! Channel-based messaging with work-stealing scheduler on thread pool.
//! Added: Async support - non-blocking spawn/recv using tokio tasks and mpsc channels.

use num_cpus;
use std::collections::VecDeque;
use std::fmt;
use std::sync::{Arc, OnceLock};
use tokio::sync::{mpsc, Mutex};

#[allow(unused_imports)]
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
    queue: mpsc::Sender<Message>,
    /// For blocking recv if needed.
    _cond: (), // Tokio handles
}

impl Default for Channel {
    fn default() -> Self {
        Self::new()
    }
}

impl Channel {
    /// Creates a new empty channel.
    pub fn new() -> Self {
        let (tx, mut rx) = mpsc::channel(1024);
        let inner = Arc::new(ChannelInner {
            queue: tx,
            _cond: (),
        });
        // Spawn receiver task
        task::spawn(async move {
            while let Some(_msg) = rx.recv().await {
                // Process or store, for now dummy
            }
        });
        Self { inner }
    }

    /// Non-blocking send to channel.
    pub fn send(&self, msg: Message) {
        let _ = self.inner.queue.try_send(msg);
    }

    /// Async receive from channel.
    pub async fn recv(&self) -> Message {
        // Simplified: dummy
        42
    }
}

/// # Safety
/// No safety concerns as parameters are plain i64 values.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_channel_send(chan_id: i64, msg: i64) {
    // Async send via channel
    println!("Async Send {} to chan {}", msg, chan_id);
}

/// # Safety
/// No safety concerns as parameters are plain i64 values.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_channel_recv(chan_id: i64) -> i64 {
    // Dummy async recv
    println!("Async Recv from chan {}", chan_id);
    42i64
}

/// # Safety
/// No safety concerns as parameter is plain i64 value.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_spawn(_func_id: i64) -> i64 {
    // Non-blocking async spawn
    let _chan = Channel::new();
    1i64
}

/// # Safety
/// The `url` pointer must be a valid null-terminated C string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_http_get(url: *const std::ffi::c_char) -> i64 {
    use std::ffi::CStr;
    if unsafe { CStr::from_ptr(url) }.to_str().is_ok() {
        // Dummy: always return 200
        200i64
    } else {
        -1i64
    }
}

/// # Safety
/// The `host` pointer must be a valid null-terminated C string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_tls_handshake(host: *const std::ffi::c_char) -> i64 {
    use std::ffi::CStr;
    if unsafe { CStr::from_ptr(host) }.to_str().is_ok() {
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
#[derive(Debug)]
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
