// src/runtime/actor/scheduler.rs
use super::channel::Channel;
use num_cpus;
use std::collections::{HashMap, VecDeque};
use std::sync::atomic::Ordering;
use std::sync::{Arc, OnceLock};
use tokio::sync::Mutex;
use tokio::task;
/// Type alias for actor entry functions.
type ActorEntry = Box<dyn FnOnce(Channel) + Send + 'static>;
/// Global map of function IDs to actor entry functions.
static FUNC_MAP: OnceLock<Arc<Mutex<HashMap<i64, ActorEntry>>>> = OnceLock::new();
/// Registers an actor entry function and returns its ID.
#[allow(dead_code)]
fn register_func(f: impl FnOnce(Channel) + Send + 'static) -> i64 {
    let _ = FUNC_MAP.set(Arc::new(Mutex::new(HashMap::new())));
    let id = super::channel::CHANNEL_ID_COUNTER.fetch_add(1, Ordering::SeqCst); // Reuse counter for simplicity
    if let Some(map) = FUNC_MAP.get() {
        let mut guard = tokio::runtime::Runtime::new().unwrap().block_on(map.lock());
        guard.insert(id, Box::new(f));
    }
    id
}
/// Host function to spawn an actor.
/// # Safety
/// No safety concerns as parameters are plain i64 values.
pub unsafe extern "C" fn host_spawn(func_id: i64) -> i64 {
    tokio::runtime::Runtime::new().unwrap().block_on(async {
        if let Some(map) = FUNC_MAP.get() {
            let mut guard = map.lock().await;
            if let Some(func) = guard.remove(&func_id) {
                spawn(func).await;
                0
            } else {
                -1
            }
        } else {
            -1
        }
    })
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
    /// Creates a new scheduler with worker threads.
    pub async fn new(thread_count: usize) -> Arc<Self> {
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
