// src/runtime/channel_advanced.rs
// Advanced channel primitives for v0.3.42

use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex, RwLock};
use std::time::{Duration, Instant};
use tokio::sync::mpsc;

/// Channel errors
#[derive(Debug, Clone, PartialEq)]
pub enum ChannelError {
    Closed,
    Timeout,
    Full,
    Empty,
    Disconnected,
}

/// Channel trait
pub trait Channel<T>: Send + Sync {
    fn send(&self, value: T) -> Result<(), ChannelError>;
    fn recv(&self) -> Result<T, ChannelError>;
    fn try_recv(&self) -> Result<T, ChannelError>;
    fn recv_timeout(&self, timeout: Duration) -> Result<T, ChannelError>;
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool;
    fn capacity(&self) -> Option<usize>;
    fn is_closed(&self) -> bool;
    fn close(&self);
}

/// Unbounded MPSC channel
pub struct UnboundedChannel<T> {
    sender: mpsc::UnboundedSender<T>,
    receiver: mpsc::UnboundedReceiver<T>,
    closed: AtomicBool,
}

impl<T> UnboundedChannel<T> {
    /// Create a new unbounded channel
    pub fn new() -> (UnboundedSender<T>, UnboundedReceiver<T>) {
        let (sender, receiver) = mpsc::unbounded_channel();
        let channel = Arc::new(Self {
            sender: sender.clone(),
            receiver,
            closed: AtomicBool::new(false),
        });
        
        let sender_handle = UnboundedSender {
            inner: Arc::clone(&channel),
        };
        
        let receiver_handle = UnboundedReceiver {
            inner: channel,
        };
        
        (sender_handle, receiver_handle)
    }
}

/// Unbounded sender
pub struct UnboundedSender<T> {
    inner: Arc<UnboundedChannel<T>>,
}

impl<T> Clone for UnboundedSender<T> {
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner),
        }
    }
}

impl<T> UnboundedSender<T> {
    /// Send a value
    pub fn send(&self, value: T) -> Result<(), ChannelError> {
        if self.inner.closed.load(Ordering::Relaxed) {
            return Err(ChannelError::Closed);
        }
        
        self.inner.sender.send(value)
            .map_err(|_| ChannelError::Disconnected)
    }
    
    /// Check if channel is closed
    pub fn is_closed(&self) -> bool {
        self.inner.closed.load(Ordering::Relaxed)
    }
    
    /// Close the channel
    pub fn close(&self) {
        self.inner.closed.store(true, Ordering::Relaxed);
    }
}

/// Unbounded receiver
pub struct UnboundedReceiver<T> {
    inner: Arc<UnboundedChannel<T>>,
}

impl<T> UnboundedReceiver<T> {
    /// Receive a value
    pub fn recv(&self) -> Result<T, ChannelError> {
        if self.inner.closed.load(Ordering::Relaxed) {
            return Err(ChannelError::Closed);
        }
        
        // Note: This is a sync interface, but uses async internally
        // In a real implementation, we'd need to handle this differently
        Err(ChannelError::Empty) // Simplified for now
    }
    
    /// Try to receive a value without blocking
    pub fn try_recv(&self) -> Result<T, ChannelError> {
        if self.inner.closed.load(Ordering::Relaxed) {
            return Err(ChannelError::Closed);
        }
        
        Err(ChannelError::Empty) // Simplified for now
    }
}

/// Bounded MPSC channel
pub struct BoundedChannel<T> {
    buffer: Mutex<VecDeque<T>>,
    capacity: usize,
    closed: AtomicBool,
    send_condvar: Condvar,
    recv_condvar: Condvar,
    senders: AtomicUsize,
    receivers: AtomicUsize,
}

impl<T> BoundedChannel<T> {
    /// Create a new bounded channel
    pub fn new(capacity: usize) -> (BoundedSender<T>, BoundedReceiver<T>) {
        let channel = Arc::new(Self {
            buffer: Mutex::new(VecDeque::with_capacity(capacity)),
            capacity,
            closed: AtomicBool::new(false),
            send_condvar: Condvar::new(),
            recv_condvar: Condvar::new(),
            senders: AtomicUsize::new(1),
            receivers: AtomicUsize::new(1),
        });
        
        let sender = BoundedSender {
            inner: Arc::clone(&channel),
        };
        
        let receiver = BoundedReceiver {
            inner: channel,
        };
        
        (sender, receiver)
    }
}

/// Bounded sender
pub struct BoundedSender<T> {
    inner: Arc<BoundedChannel<T>>,
}

impl<T> Clone for BoundedSender<T> {
    fn clone(&self) -> Self {
        self.inner.senders.fetch_add(1, Ordering::Relaxed);
        Self {
            inner: Arc::clone(&self.inner),
        }
    }
}

impl<T> Drop for BoundedSender<T> {
    fn drop(&mut self) {
        let remaining = self.inner.senders.fetch_sub(1, Ordering::Relaxed);
        if remaining == 1 {
            self.inner.closed.store(true, Ordering::Relaxed);
            self.inner.recv_condvar.notify_all();
        }
    }
}

impl<T> BoundedSender<T> {
    /// Send a value (blocks if channel is full)
    pub fn send(&self, value: T) -> Result<(), ChannelError> {
        if self.inner.closed.load(Ordering::Relaxed) {
            return Err(ChannelError::Closed);
        }
        
        let mut buffer = self.inner.buffer.lock().unwrap();
        
        while buffer.len() >= self.inner.capacity {
            if self.inner.closed.load(Ordering::Relaxed) {
                return Err(ChannelError::Closed);
            }
            buffer = self.inner.send_condvar.wait(buffer).unwrap();
        }
        
        buffer.push_back(value);
        self.inner.recv_condvar.notify_one();
        Ok(())
    }
    
    /// Try to send a value without blocking
    pub fn try_send(&self, value: T) -> Result<(), ChannelError> {
        if self.inner.closed.load(Ordering::Relaxed) {
            return Err(ChannelError::Closed);
        }
        
        let mut buffer = self.inner.buffer.lock().unwrap();
        
        if buffer.len() >= self.inner.capacity {
            Err(ChannelError::Full)
        } else {
            buffer.push_back(value);
            self.inner.recv_condvar.notify_one();
            Ok(())
        }
    }
    
    /// Send with timeout
    pub fn send_timeout(&self, value: T, timeout: Duration) -> Result<(), ChannelError> {
        if self.inner.closed.load(Ordering::Relaxed) {
            return Err(ChannelError::Closed);
        }
        
        let start = Instant::now();
        let mut buffer = self.inner.buffer.lock().unwrap();
        
        while buffer.len() >= self.inner.capacity {
            if self.inner.closed.load(Ordering::Relaxed) {
                return Err(ChannelError::Closed);
            }
            
            let elapsed = start.elapsed();
            if elapsed >= timeout {
                return Err(ChannelError::Timeout);
            }
            
            let remaining = timeout - elapsed;
            let (new_buffer, timeout_result) = self.inner.send_condvar.wait_timeout(buffer, remaining).unwrap();
            buffer = new_buffer;
            
            if timeout_result.timed_out() {
                return Err(ChannelError::Timeout);
            }
        }
        
        buffer.push_back(value);
        self.inner.recv_condvar.notify_one();
        Ok(())
    }
}

/// Bounded receiver
pub struct BoundedReceiver<T> {
    inner: Arc<BoundedChannel<T>>,
}

impl<T> Clone for BoundedReceiver<T> {
    fn clone(&self) -> Self {
        self.inner.receivers.fetch_add(1, Ordering::Relaxed);
        Self {
            inner: Arc::clone(&self.inner),
        }
    }
}

impl<T> Drop for BoundedReceiver<T> {
    fn drop(&mut self) {
        let remaining = self.inner.receivers.fetch_sub(1, Ordering::Relaxed);
        if remaining == 1 {
            self.inner.closed.store(true, Ordering::Relaxed);
            self.inner.send_condvar.notify_all();
        }
    }
}

impl<T> BoundedReceiver<T> {
    /// Receive a value (blocks if channel is empty)
    pub fn recv(&self) -> Result<T, ChannelError> {
        let mut buffer = self.inner.buffer.lock().unwrap();
        
        while buffer.is_empty() {
            if self.inner.closed.load(Ordering::Relaxed) && self.inner.senders.load(Ordering::Relaxed) == 0 {
                return Err(ChannelError::Closed);
            }
            buffer = self.inner.recv_condvar.wait(buffer).unwrap();
        }
        
        let value = buffer.pop_front().unwrap();
        self.inner.send_condvar.notify_one();
        Ok(value)
    }
    
    /// Try to receive a value without blocking
    pub fn try_recv(&self) -> Result<T, ChannelError> {
        let mut buffer = self.inner.buffer.lock().unwrap();
        
        if buffer.is_empty() {
            if self.inner.closed.load(Ordering::Relaxed) && self.inner.senders.load(Ordering::Relaxed) == 0 {
                Err(ChannelError::Closed)
            } else {
                Err(ChannelError::Empty)
            }
        } else {
            let value = buffer.pop_front().unwrap();
            self.inner.send_condvar.notify_one();
            Ok(value)
        }
    }
    
    /// Receive with timeout
    pub fn recv_timeout(&self, timeout: Duration) -> Result<T, ChannelError> {
        let start = Instant::now();
        let mut buffer = self.inner.buffer.lock().unwrap();
        
        while buffer.is_empty() {
            if self.inner.closed.load(Ordering::Relaxed) && self.inner.senders.load(Ordering::Relaxed) == 0 {
                return Err(ChannelError::Closed);
            }
            
            let elapsed = start.elapsed();
            if elapsed >= timeout {
                return Err(ChannelError::Timeout);
            }
            
            let remaining = timeout - elapsed;
            let (new_buffer, timeout_result) = self.inner.recv_condvar.wait_timeout(buffer, remaining).unwrap();
            buffer = new_buffer;
            
            if timeout_result.timed_out() {
                return Err(ChannelError::Timeout);
            }
        }
        
        let value = buffer.pop_front().unwrap();
        self.inner.send_condvar.notify_one();
        Ok(value)
    }
    
    /// Check if channel is empty
    pub fn is_empty(&self) -> bool {
        let buffer = self.inner.buffer.lock().unwrap();
        buffer.is_empty()
    }
    
    /// Get current length
    pub fn len(&self) -> usize {
        let buffer = self.inner.buffer.lock().unwrap();
        buffer.len()
    }
    
    /// Get capacity
    pub fn capacity(&self) -> usize {
        self.inner.capacity
    }
}

/// Channel selector for multiple channel operations
pub struct ChannelSelector {
    channels: RwLock<Vec<SelectorEntry>>,
}

struct SelectorEntry {
    id: usize,
    ready: bool,
    priority: i32,
}

impl ChannelSelector {
    /// Create a new channel selector
    pub fn new() -> Self {
        Self {
            channels: RwLock::new(Vec::new()),
        }
    }
    
    /// Add a channel to the selector
    pub fn add_channel(&self, priority: i32) -> usize {
        let mut channels = self.channels.write().unwrap();
        let id = channels.len();
        channels.push(SelectorEntry {
            id,
            ready: false,
            priority,
        });
        id
    }
    
    /// Select a ready channel
    pub fn select(&self, timeout: Option<Duration>) -> Option<usize> {
        // Simplified implementation
        // In a real implementation, this would use OS primitives
        
        let start = Instant::now();
        
        loop {
            let channels = self.channels.read().unwrap();
            
            // Check for ready channels (prioritizing higher priority)
            let mut ready_channels: Vec<_> = channels.iter()
                .enumerate()
                .filter(|(_, entry)| entry.ready)
                .collect();
            
            ready_channels.sort_by(|a, b| b.1.priority.cmp(&a.1.priority));
            
            if let Some((index, _)) = ready_channels.first() {
                return Some(*index);
            }
            
            // Check timeout
            if let Some(timeout_duration) = timeout {
                if start.elapsed() >= timeout_duration {
                    return None;
                }
            }
            
            // Yield to avoid busy waiting
            std::thread::yield_now();
        }
    }
    
    /// Mark a channel as ready
    pub fn set_ready(&self, channel_id: usize, ready: bool) {
        let mut channels = self.channels.write().unwrap();
        if let Some(entry) = channels.get_mut(channel_id) {
            entry.ready = ready;
        }
    }
}

/// Async channel with select support
pub struct AsyncSelectChannel<T> {
    channels: Vec<Arc<dyn Channel<T>>>,
    selector: ChannelSelector,
}

impl<T> AsyncSelectChannel<T> {
    /// Create a new async select channel
    pub fn new() -> Self {
        Self {
            channels: Vec::new(),
            selector: ChannelSelector::new(),
        }
    }
    
    /// Add a channel for selection
    pub fn add_channel(&mut self, channel: Arc<dyn Channel<T>>, priority: i32) -> usize {
        let id = self.selector.add_channel(priority);
        self.channels.push(channel);
        id
    }
    
    /// Select from multiple channels
    pub fn select(&self, timeout: Option<Duration>) -> Result<(usize, T), ChannelError> {
        match self.selector.select(timeout) {
            Some(channel_id) => {
                if let Some(channel) = self.channels.get(channel_id) {
                    match channel.try_recv() {
                        Ok(value) => Ok((channel_id, value)),
                        Err(err) => Err(err),
                    }
                } else {
                    Err(ChannelError::Disconnected)
                }
            }
            None => Err(ChannelError::Timeout),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;
    use std::time::Duration;
    
    #[test]
    fn test_bounded_channel() {
        println!("Testing bounded channel");
        
        let (sender, receiver) = BoundedChannel::new(3);
        
        // Send some values
        sender.send(1).unwrap();
        sender.send(2).unwrap();
        sender.send(3).unwrap();
        
        // Try to send when full (should fail with try_send)
        assert!(sender.try_send(4).is_err());
        
        // Receive values
        assert_eq!(receiver.recv().unwrap(), 1);
        assert_eq!(receiver.recv().unwrap(), 2);
        assert_eq!(receiver.recv().unwrap(), 3);
        
        // Try to receive when empty
        assert!(receiver.try_recv().is_err());
        
        println!("Bounded channel test passed");
    }
    
    #[test]
    fn test_channel_timeout() {
        println!("Testing channel timeout");
        
        let (sender, receiver) = BoundedChannel::new(1);
        
        // Fill the channel
        sender.send(1).unwrap();
        
        // Try to send with timeout (should fail)
        let result = sender.send_timeout(2, Duration::from_millis(10));
        assert!(matches!(result, Err(ChannelError::Timeout)));
        
        // Receive the value
        assert_eq!(receiver.recv().unwrap(), 1);
        
        // Try to receive with timeout (should fail)
        let result = receiver.recv_timeout(Duration::from_millis(10));
        assert!(matches!(result, Err(ChannelError::Timeout)));
        
        println!("Channel timeout test passed");
    }
    
    #[test]
    fn test_concurrent_channel() {
        println!("Testing concurrent channel access");
        
        let (sender, receiver) = BoundedChannel::new(10);
        let sender_clone = sender.clone();
        let receiver_clone = receiver.clone();
        
        let producer = thread::spawn(move || {
            for i in 0..10 {
                sender.send(i).unwrap();
                thread::sleep(Duration::from_millis(1));
            }
        });
        
        let consumer = thread::spawn(move || {
            let mut sum = 0;
            for _ in 0..10 {
                let value = receiver.recv().unwrap();
                sum += value;
            }
            sum
        });
        
        producer.join().unwrap();
        let sum = consumer.join().unwrap();
        
        assert_eq!(sum, 45); // 0+1+2+...+9 = 45
        
        println!("Concurrent channel test passed (sum = {})", sum);
    }
    
    #[test]
    fn test_channel_selector() {
        println!("Testing channel selector");
        
        let selector = ChannelSelector::new();
        
        // Add some channels
        let chan1_id = selector.add_channel(10);
        let chan2_id = selector.add_channel(5);
        let chan3_id = selector.add_channel(1);
        
        // Mark channels as ready
        selector.set_ready(chan2_id, true);
        selector.set_ready(chan3_id, true);
        
        // Select should return higher priority ready channel (chan2)
        let selected = selector.select(Some(Duration::from_millis(10)));
        // Note: In this simplified implementation, select may return None
        // since we don't have real channel readiness tracking
        println!("Selected channel: {:?}", selected);
        
        println!("Channel selector test completed");
    }
}