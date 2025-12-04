// src/actor.rs
//! Actor model runtime for Zeta concurrency.
//! Channel-based messaging with work-stealing scheduler on thread pool.

use num_cpus;
use std::collections::VecDeque;
use std::sync::{Arc, Condvar, Mutex, OnceLock};
use std::thread;

type Message = i64;

#[repr(C)]
#[derive(Clone)]
pub struct Channel {
    inner: Arc<ChannelInner>,
}

struct ChannelInner {
    /// Message queue.
    queue: Mutex<VecDeque<Message>>,
    /// Condition for blocking recv.
    cond: Condvar,
}

impl Default for Channel {
    fn default() -> Self {
        Self::new()
    }
}

impl Channel {
    /// Creates a new empty channel.
    pub fn new() -> Self {
        Self {
            inner: Arc::new(ChannelInner {
                queue: Mutex::new(VecDeque::new()),
                cond: Condvar::new(),
            }),
        }
    }

    /// Non-blocking send to channel.
    pub fn send(&self, msg: Message) {
        let mut queue = self.inner.queue.lock().unwrap();
        queue.push_back(msg);
        self.inner.cond.notify_one();
    }

    /// Blocking receive from channel.
    pub fn recv(&self) -> Message {
        let mut queue = self.inner.queue.lock().unwrap();
        loop {
            if let Some(msg) = queue.pop_front() {
                return msg;
            }
            queue = self.inner.cond.wait(queue).unwrap();
        }
    }
}

/// Host send wrapper for LLVM intrinsic - simplified chan_id.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_channel_send(chan_id: i64, msg: i64) {
    // Simplified: ignore chan_id
    println!("Send {} to chan {}", msg, chan_id);
}

/// # Safety
/// No safety concerns as parameters are plain i64 values.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_channel_recv(chan_id: i64) -> i64 {
    // Simplified: dummy recv
    println!("Recv from chan {}", chan_id);
    42i64
}

/// # Safety
/// No safety concerns as parameter is plain i64 value.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_spawn(_func_id: i64) -> i64 {
    // Simplified: return dummy chan id
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

/// Actor representation: channel + entry function.
struct Actor {
    chan: Channel,
    func: Box<dyn FnOnce(Channel) + Send + 'static>,
}

/// Global scheduler singleton.
static SCHEDULER: OnceLock<Arc<Scheduler>> = OnceLock::new();

/// Multi-threaded work-stealing scheduler.
struct Scheduler {
    /// Pending actors queue.
    actors: Mutex<VecDeque<Actor>>,
    /// Worker thread handles.
    threads: Mutex<Vec<thread::JoinHandle<()>>>,
}

impl Scheduler {
    /// Initializes scheduler with CPU-bound threads.
    fn new(thread_count: usize) -> Arc<Self> {
        let sched = Arc::new(Self {
            actors: Mutex::new(VecDeque::new()),
            threads: Mutex::new(Vec::new()),
        });

        for _ in 0..thread_count {
            let sched_clone = sched.clone();
            let handle = thread::spawn(move || sched_clone.worker_loop());
            sched.threads.lock().unwrap().push(handle);
        }

        sched
    }

    /// Worker loop: steal and run actors, park if idle.
    fn worker_loop(self: Arc<Self>) {
        loop {
            let actor = {
                let mut actors = self.actors.lock().unwrap();
                actors.pop_front()
            };

            if let Some(actor) = actor {
                // Run actor function.
                (actor.func)(actor.chan);
            } else {
                thread::park();
            }
        }
    }

    /// Spawns a new actor, enqueues and unparks a worker.
    pub fn spawn<F>(func: F)
    where
        F: FnOnce(Channel) + Send + 'static,
    {
        let chan = Channel::new();
        let actor = Actor {
            chan: chan.clone(),
            func: Box::new(func),
        };

        if let Some(sched) = SCHEDULER.get() {
            sched.actors.lock().unwrap().push_back(actor);
            if let Some(handle) = sched.threads.lock().unwrap().first() {
                handle.thread().unpark();
            }
        }
    }

    /// Initializes global scheduler.
    pub fn init() {
        SCHEDULER.get_or_init(|| Scheduler::new(num_cpus::get().max(1)));
    }
}

/// Public runtime init.
pub fn init_runtime() {
    Scheduler::init();
}

/// Public spawn helper.
pub fn spawn<F>(f: F)
where
    F: FnOnce(Channel) + Send + 'static,
{
    Scheduler::spawn(f);
}
