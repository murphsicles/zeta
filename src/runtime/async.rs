// src/runtime/async.rs
//! Async/await runtime support for Zeta v0.5.0

use std::collections::VecDeque;
use std::future::Future;
use std::pin::Pin;
use std::sync::RwLock;
use std::sync::atomic::{AtomicU64, Ordering};
use std::task::{Context, Poll, RawWaker, RawWakerVTable};

/// Next future ID counter
static NEXT_FUTURE_ID: AtomicU64 = AtomicU64::new(1);

/// A boxed future type alias to avoid nested `>>>` parse ambiguity
type BoxedFuture = Pin<Box<dyn Future<Output = i64> + Send + Sync>>;

/// Simple executor for async functions
pub struct Executor {
    tasks: RwLock<VecDeque<(u64, BoxedFuture)>>,
    results: RwLock<Vec<(u64, i64)>>,
}

impl Default for Executor {
    fn default() -> Self {
        Self::new()
    }
}

impl Executor {
    pub fn new() -> Self {
        Self {
            tasks: RwLock::new(VecDeque::new()),
            results: RwLock::new(Vec::new()),
        }
    }

    pub fn spawn<F>(&self, future: F) -> u64
    where
        F: Future<Output = i64> + Send + Sync + 'static,
    {
        let id = NEXT_FUTURE_ID.fetch_add(1, Ordering::Relaxed);
        self.tasks
            .write()
            .unwrap()
            .push_back((id, Box::pin(future) as BoxedFuture));
        id
    }

    /// Poll all pending tasks, returning completed results
    pub fn poll_all(&self) -> Vec<(u64, i64)> {
        static VTABLE: RawWakerVTable = RawWakerVTable::new(
            |_| RawWaker::new(std::ptr::null(), &VTABLE),
            |_| {},
            |_| {},
            |_| {},
        );

        let raw_waker = RawWaker::new(std::ptr::null(), &VTABLE);
        let waker = unsafe { std::task::Waker::from_raw(raw_waker) };
        let mut cx = Context::from_waker(&waker);

        let mut completed = Vec::new();
        let mut pending = VecDeque::new();

        // Poll all tasks once
        while let Some((id, mut task)) = self.tasks.write().unwrap().pop_front() {
            match task.as_mut().poll(&mut cx) {
                Poll::Ready(result) => {
                    completed.push((id, result));
                }
                Poll::Pending => {
                    pending.push_back((id, task));
                }
            }
        }

        // Put pending tasks back
        *self.tasks.write().unwrap() = pending;

        // Store completed results
        self.results.write().unwrap().extend(completed.clone());

        completed
    }

    /// Run a specific future to completion (for await)
    pub fn run_future(id: u64) -> Option<i64> {
        let executor = get_executor();

        // Poll until the task completes or we run out of tasks
        for _ in 0..10_000 {
            let completed = executor.poll_all();
            for (completed_id, result) in &completed {
                if *completed_id == id {
                    return Some(*result);
                }
            }
        }
        None
    }

    pub fn run(&self) -> Option<i64> {
        let result = self.poll_all();
        result.last().map(|(_, v)| *v)
    }
}

/// Get the global executor instance
pub fn get_executor() -> &'static Executor {
    static EXECUTOR: std::sync::OnceLock<Executor> = std::sync::OnceLock::new();
    EXECUTOR.get_or_init(Executor::new)
}

/// Initialize the async runtime
pub fn init_async_runtime() {
    get_executor();
}

/// A future that wraps a compiled Zeta async function.
/// When polled, it calls the function via its function table index.
struct ZetaFuture {
    func_id: i64,
    polled: bool,
}

impl Future for ZetaFuture {
    type Output = i64;

    fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.get_mut();
        if !this.polled {
            this.polled = true;
            // First poll: the function may complete immediately or return Pending
            // In practice, we'd call the actual compiled function here.
            // For now, we simulate by returning Ready with the func_id as result.
            Poll::Ready(this.func_id)
        } else {
            Poll::Ready(this.func_id)
        }
    }
}

// Host function to spawn an async task
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_async_spawn(func_id: i64) -> i64 {
    let executor = get_executor();
    let future = ZetaFuture {
        func_id,
        polled: false,
    };
    let future_id = executor.spawn(future);
    future_id as i64
}

// Host function to await a future
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_async_await(future_id: i64) -> i64 {
    // Poll the executor until the specified future completes
    let executor = get_executor();
    for _ in 0..10_000 {
        let completed = executor.poll_all();
        for (cid, result) in &completed {
            if *cid == future_id as u64 {
                return *result;
            }
        }
        // Brief yield to avoid busy-spin
        std::thread::yield_now();
    }
    // Timeout — return the future_id as a fallback
    future_id
}
