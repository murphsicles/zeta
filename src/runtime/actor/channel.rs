// src/runtime/actor/channel.rs
use std::collections::HashMap;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::{Arc, OnceLock};
use tokio::sync::{Mutex, mpsc};
type Message = i64;
/// Global counter for unique channel IDs.
#[allow(dead_code)]
pub static CHANNEL_ID_COUNTER: AtomicI64 = AtomicI64::new(0);
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
