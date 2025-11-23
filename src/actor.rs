// src/actor.rs
use num_cpus;
use std::collections::VecDeque;
use std::sync::{Arc, Condvar, Mutex, OnceLock};
use std::thread;

type Message = i64;

#[derive(Clone)]
pub struct Channel {
    inner: Arc<ChannelInner>,
}

struct ChannelInner {
    queue: Mutex<VecDeque<Message>>,
    cond: Condvar,
}

impl Channel {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(ChannelInner {
                queue: Mutex::new(VecDeque::new()),
                cond: Condvar::new(),
            }),
        }
    }

    pub fn send(&self, msg: Message) {
        let mut queue = self.inner.queue.lock().unwrap();
        queue.push_back(msg);
        self.inner.cond.notify_one();
    }

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

struct Actor {
    chan: Channel,
    func: Box<dyn FnOnce(Channel) + Send + 'static>,
}

static SCHEDULER: OnceLock<Arc<Scheduler>> = OnceLock::new();

struct Scheduler {
    actors: Mutex<VecDeque<Actor>>,
    threads: Mutex<Vec<thread::JoinHandle<()>>>,
}

impl Scheduler {
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

    fn worker_loop(self: Arc<Self>) {
        loop {
            let actor = {
                let mut actors = self.actors.lock().unwrap();
                actors.pop_front()
            };

            if let Some(actor) = actor {
                (actor.func)(actor.chan);
            } else {
                thread::park();
            }
        }
    }

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
            if let Some(handle) = sched.threads.lock().unwrap().get(0) {
                handle.thread().unpark();
            }
        }
    }

    pub fn init() {
        SCHEDULER.get_or_init(|| Scheduler::new(num_cpus::get().max(1)));
    }
}

pub fn init_runtime() {
    Scheduler::init();
}

pub fn spawn<F>(f: F)
where
    F: FnOnce(Channel) + Send + 'static,
{
    Scheduler::spawn(f);
}
