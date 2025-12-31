// src/runtime/actor/channel.rs
use std::collections::HashMap;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::{Arc, OnceLock};
use tokio::sync::{Mutex, mpsc};

type Message = i64;

/// Global counter for unique channel IDs.
pub static CHANNEL_ID_COUNTER: AtomicI64 = AtomicI64::new(0);

type ChannelMap = Arc<Mutex<HashMap<i64, Arc<ChannelInner>>>>;

/// Global map of channel IDs to channel inners.
#[allow(clippy::type_complexity)]
static CHANNEL_MAP: OnceLock<ChannelMap> = OnceLock::new();

/// Communication channel for actor messages, compatible with C representations.
#[repr(C)]
#[derive(Clone, Debug)]
pub struct Channel {
    id: i64,
}

#[derive(Debug)]
struct ChannelInner {
    tx: mpsc::Sender<Message>,
    rx: Mutex<mpsc::Receiver<Message>>,
}

impl Channel {
    /// Creates a new unbounded channel and registers it globally.
    pub fn new() -> Self {
        let (tx, rx) = mpsc::channel(1024);
        let id = CHANNEL_ID_COUNTER.fetch_add(1, Ordering::SeqCst);

        let map = CHANNEL_MAP.get_or_init(|| Arc::new(Mutex::new(HashMap::new())));
        let inner = Arc::new(ChannelInner {
            tx,
            rx: Mutex::new(rx),
        });

        let rt = tokio::runtime::Handle::current();
        rt.block_on(async {
            let mut guard = map.lock().await;
            guard.insert(id, inner);
        });

        Self { id }
    }

    #[allow(dead_code)]
    async fn get_inner(&self) -> Option<Arc<ChannelInner>> {
        let map = CHANNEL_MAP.get()?;
        let guard = map.lock().await;
        guard.get(&self.id).cloned()
    }
}

#[allow(clippy::new_without_default)]
impl Default for Channel {
    fn default() -> Self {
        Self::new()
    }
}

/// Host function to send a message to a channel.
/// # Safety
/// No safety concerns as parameters are plain i64 values.
pub unsafe extern "C" fn host_channel_send(chan_id: i64, msg: i64) -> i64 {
    let rt = tokio::runtime::Handle::current();
    rt.block_on(async {
        let map = match CHANNEL_MAP.get() {
            Some(m) => m,
            None => return -1,
        };
        let guard = map.lock().await;
        if let Some(inner) = guard.get(&chan_id) {
            if inner.tx.try_send(msg).is_ok() {
                0
            } else {
                -1
            }
        } else {
            -1
        }
    })
}

/// Host function to receive a message from a channel.
/// # Safety
/// No safety concerns as parameters are plain i64 values.
pub unsafe extern "C" fn host_channel_recv(chan_id: i64) -> i64 {
    let rt = tokio::runtime::Handle::current();
    rt.block_on(async {
        let map = match CHANNEL_MAP.get() {
            Some(m) => m,
            None => return 0,
        };
        let guard = map.lock().await;
        if let Some(inner) = guard.get(&chan_id) {
            // Try non-blocking first
            let mut rx_guard = inner.rx.lock().await;
            match rx_guard.try_recv() {
                Ok(m) => m,
                Err(mpsc::error::TryRecvError::Empty) => {
                    drop(rx_guard);
                    // Fall back to blocking recv
                    let mut rx_guard = inner.rx.lock().await;
                    rx_guard.recv().await.unwrap_or(0)
                }
                Err(_) => 0,
            }
        } else {
            0
        }
    })
}
