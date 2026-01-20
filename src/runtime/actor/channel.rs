// src/runtime/actor/channel.rs
// Full channel runtime with global map and tokio mpsc

use std::collections::HashMap;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::{Arc, OnceLock};
use tokio::sync::{Mutex, mpsc};

pub type Message = i64;

pub static CHANNEL_ID_COUNTER: AtomicI64 = AtomicI64::new(0);

type ChannelMap = Arc<Mutex<HashMap<i64, Arc<ChannelInner>>>>;

static CHANNEL_MAP: OnceLock<ChannelMap> = OnceLock::new();

#[derive(Debug)]
struct ChannelInner {
    tx: mpsc::Sender<Message>,
    rx: Mutex<mpsc::Receiver<Message>>,
}

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

pub unsafe extern "C" fn host_channel_recv(chan_id: i64) -> i64 {
    let rt = tokio::runtime::Handle::current();
    rt.block_on(async {
        let map = match CHANNEL_MAP.get() {
            Some(m) => m,
            None => return 0,
        };
        let guard = map.lock().await;
        if let Some(inner) = guard.get(&chan_id) {
            let mut rx = inner.rx.lock().await;
            match rx.try_recv() {
                Ok(m) => m,
                Err(mpsc::error::TryRecvError::Empty) => {
                    drop(rx);
                    let mut rx = inner.rx.lock().await;
                    rx.recv().await.unwrap_or(0)
                }
                Err(_) => 0,
            }
        } else {
            0
        }
    })
}

// Internal helper called from zeta_src/runtime/actor/channel.z Channel::new()
pub fn register_channel(id: i64, tx: mpsc::Sender<Message>, rx: mpsc::Receiver<Message>) {
    let map = CHANNEL_MAP.get_or_init(|| Arc::new(Mutex::new(HashMap::new())));
    let rt = tokio::runtime::Handle::current();
    rt.block_on(async {
        let mut guard = map.lock().await;
        guard.insert(id, Arc::new(ChannelInner {
            tx,
            rx: Mutex::new(rx),
        }));
    });
}
