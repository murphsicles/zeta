// src/runtime/mpsc.rs
//! std::sync::mpsc-like channel implementation for Zeta v0.5.0

use std::collections::HashMap;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::mpsc::{Receiver, Sender, channel as std_channel};
use std::sync::{Arc, Mutex};

/// Global counter for channel IDs
static CHANNEL_ID_COUNTER: AtomicI64 = AtomicI64::new(0);

/// Global map of channel IDs to channel pairs
type ChannelMap = Arc<Mutex<HashMap<i64, (Sender<i64>, Receiver<i64>)>>>;
static CHANNEL_MAP: std::sync::OnceLock<ChannelMap> = std::sync::OnceLock::new();

// Create a new channel and return its ID
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_mpsc_channel() -> i64 {
    let (tx, rx) = std_channel();
    let id = CHANNEL_ID_COUNTER.fetch_add(1, Ordering::SeqCst);

    let map = CHANNEL_MAP.get_or_init(|| Arc::new(Mutex::new(HashMap::new())));
    let mut guard = map.lock().unwrap();
    guard.insert(id, (tx, rx));

    id
}

// Send a message through a channel
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_mpsc_send(chan_id: i64, msg: i64) -> i64 {
    let map = match CHANNEL_MAP.get() {
        Some(m) => m,
        None => return -1,
    };

    let guard = map.lock().unwrap();
    if let Some((tx, _)) = guard.get(&chan_id) {
        match tx.send(msg) {
            Ok(_) => 0,
            Err(_) => -1,
        }
    } else {
        -1
    }
}

// Receive a message from a channel (blocking)
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_mpsc_recv(chan_id: i64) -> i64 {
    let map = match CHANNEL_MAP.get() {
        Some(m) => m,
        None => return 0,
    };

    let guard = map.lock().unwrap();
    if let Some((_, rx)) = guard.get(&chan_id) {
        rx.recv().unwrap_or_default()
    } else {
        0
    }
}

// Try to receive a message from a channel (non-blocking)
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_mpsc_try_recv(chan_id: i64) -> i64 {
    let map = match CHANNEL_MAP.get() {
        Some(m) => m,
        None => return 0,
    };

    let guard = map.lock().unwrap();
    if let Some((_, rx)) = guard.get(&chan_id) {
        match rx.try_recv() {
            Ok(msg) => msg,
            Err(std::sync::mpsc::TryRecvError::Empty) => 0,
            Err(_) => -1,
        }
    } else {
        0
    }
}

// Clone a sender
#[unsafe(no_mangle)]
pub unsafe extern "C" fn host_mpsc_clone_sender(chan_id: i64) -> i64 {
    let map = match CHANNEL_MAP.get() {
        Some(m) => m,
        None => return -1,
    };

    let guard = map.lock().unwrap();
    if let Some((tx, _)) = guard.get(&chan_id) {
        let tx_clone = tx.clone();
        let new_id = CHANNEL_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        drop(guard);

        // For simplicity, just create a new channel
        // In a real implementation, we'd need to share the receiver
        let map2 = CHANNEL_MAP.get().unwrap();
        let mut guard2 = map2.lock().unwrap();
        let (new_tx, new_rx) = std::sync::mpsc::channel();
        guard2.insert(new_id, (tx_clone, new_rx));
        new_id
    } else {
        -1
    }
}
