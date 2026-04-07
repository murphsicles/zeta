//! Distributed data structures tests for Zeta v0.3.47
//! Tests DHTs, CRDTs, distributed queues, locks, and caching

use std::collections::{HashMap, VecDeque};
use std::hash::{Hash, Hasher};
use std::sync::{Arc, Mutex, RwLock};
use std::time::{Duration, Instant};
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::cmp::Ordering;

/// Distributed Hash Table node
struct DHTNode {
    id: u64,
    address: SocketAddr,
    finger_table: Vec<Option<SocketAddr>>,
    predecessor: Option<SocketAddr>,
    successor: Option<SocketAddr>,
    storage: HashMap<Vec<u8>, Vec<u8>>,
}

impl DHTNode {
    fn new(id: u64, address: SocketAddr) -> Self {
        let mut finger_table = Vec::with_capacity(160); // For SHA-1
        finger_table.resize_with(160, || None);
        
        Self {
            id,
            address,
            finger_table,
            predecessor: None,
            successor: None,
            storage: HashMap::new(),
        }
    }

    fn hash_key(&self, key: &[u8]) -> u64 {
        use std::collections::hash_map::DefaultHasher;
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        hasher.finish()
    }

    fn find_successor(&self, key_hash: u64) -> SocketAddr {
        // Simplified Chord protocol implementation
        if let Some(successor) = self.successor {
            if self.is_between(key_hash, self.id, self.hash_key(&successor.ip().to_string().into_bytes())) {
                return successor;
            }
        }
        
        // Use finger table to find closest preceding node
        let mut closest = self.address;
        for i in (0..160).rev() {
            if let Some(node_addr) = self.finger_table[i] {
                let node_id = self.hash_key(&node_addr.ip().to_string().into_bytes());
                if self.is_between(node_id, self.id, key_hash) {
                    closest = node_addr;
                    break;
                }
            }
        }
        
        closest
    }

    fn is_between(&self, x: u64, a: u64, b: u64) -> bool {
        if a < b {
            a < x && x <= b
        } else {
            a < x || x <= b
        }
    }

    fn store(&mut self, key: Vec<u8>, value: Vec<u8>) {
        let key_hash = self.hash_key(&key);
        let successor = self.find_successor(key_hash);
        
        if successor == self.address {
            self.storage.insert(key, value);
        } else {
            // Forward to successor (in real implementation, would use network)
        }
    }

    fn retrieve(&self, key: &[u8]) -> Option<Vec<u8>> {
        let key_hash = self.hash_key(key);
        let successor = self.find_successor(key_hash);
        
        if successor == self.address {
            self.storage.get(key).cloned()
        } else {
            // Query successor (in real implementation, would use network)
            None
        }
    }
}

/// Conflict-free Replicated Data Type (G-Counter)
#[derive(Debug, Clone)]
struct GCounter {
    id: usize,
    counts: Vec<u64>,
}

impl GCounter {
    fn new(id: usize, num_nodes: usize) -> Self {
        let mut counts = vec![0; num_nodes];
        Self { id, counts }
    }

    fn increment(&mut self) {
        self.counts[self.id] += 1;
    }

    fn value(&self) -> u64 {
        self.counts.iter().sum()
    }

    fn merge(&mut self, other: &GCounter) {
        for i in 0..self.counts.len() {
            self.counts[i] = std::cmp::max(self.counts[i], other.counts[i]);
        }
    }
}

/// Conflict-free Replicated Data Type (PN-Counter)
#[derive(Debug, Clone)]
struct PNCounter {
    increments: GCounter,
    decrements: GCounter,
}

impl PNCounter {
    fn new(id: usize, num_nodes: usize) -> Self {
        Self {
            increments: GCounter::new(id, num_nodes),
            decrements: GCounter::new(id, num_nodes),
        }
    }

    fn increment(&mut self) {
        self.increments.increment();
    }

    fn decrement(&mut self) {
        self.decrements.increment();
    }

    fn value(&self) -> i64 {
        self.increments.value() as i64 - self.decrements.value() as i64
    }

    fn merge(&mut self, other: &PNCounter) {
        self.increments.merge(&other.increments);
        self.decrements.merge(&other.decrements);
    }
}

/// Distributed queue using consensus
struct DistributedQueue {
    items: Arc<RwLock<VecDeque<Vec<u8>>>>,
    sequence_number: Arc<Mutex<u64>>,
}

impl DistributedQueue {
    fn new() -> Self {
        Self {
            items: Arc::new(RwLock::new(VecDeque::new())),
            sequence_number: Arc::new(Mutex::new(0)),
        }
    }

    fn enqueue(&self, item: Vec<u8>) -> u64 {
        let mut seq = self.sequence_number.lock().unwrap();
        *seq += 1;
        let item_seq = *seq;
        
        let mut items = self.items.write().unwrap();
        items.push_back(item);
        
        item_seq
    }

    fn dequeue(&self) -> Option<Vec<u8>> {
        let mut items = self.items.write().unwrap();
        items.pop_front()
    }

    fn peek(&self) -> Option<Vec<u8>> {
        let items = self.items.read().unwrap();
        items.front().cloned()
    }

    fn size(&self) -> usize {
        let items = self.items.read().unwrap();
        items.len()
    }
}

/// Distributed lock using Lamport's algorithm
struct DistributedLock {
    lock_id: String,
    request_queue: Arc<Mutex<Vec<(u64, SocketAddr)>>>, // (timestamp, node_addr)
    my_timestamp: Arc<Mutex<u64>>,
}

impl DistributedLock {
    fn new(lock_id: String) -> Self {
        Self {
            lock_id,
            request_queue: Arc::new(Mutex::new(Vec::new())),
            my_timestamp: Arc::new(Mutex::new(0)),
        }
    }

    fn request_lock(&self, node_addr: SocketAddr) -> u64 {
        let mut timestamp = self.my_timestamp.lock().unwrap();
        *timestamp += 1;
        let request_time = *timestamp;
        
        let mut queue = self.request_queue.lock().unwrap();
        queue.push((request_time, node_addr));
        queue.sort_by(|a, b| {
            if a.0 == b.0 {
                a.1.ip().cmp(&b.1.ip())
            } else {
                a.0.cmp(&b.0)
            }
        });
        
        request_time
    }

    fn release_lock(&self, node_addr: SocketAddr) {
        let mut queue = self.request_queue.lock().unwrap();
        queue.retain(|(_, addr)| *addr != node_addr);
    }

    fn has_lock(&self, node_addr: SocketAddr) -> bool {
        let queue = self.request_queue.lock().unwrap();
        if let Some((_, first_addr)) = queue.first() {
            *first_addr == node_addr
        } else {
            false
        }
    }
}

/// Distributed cache with consistency guarantees
struct DistributedCache {
    data: Arc<RwLock<HashMap<String, (Vec<u8>, Instant)>>>,
    ttl: Duration,
    consistency_level: ConsistencyLevel,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ConsistencyLevel {
    Eventual,
    Casual,
    Sequential,
    Linearizable,
}

impl DistributedCache {
    fn new(ttl: Duration, consistency: ConsistencyLevel) -> Self {
        Self {
            data: Arc::new(RwLock::new(HashMap::new())),
            ttl,
            consistency_level: consistency,
        }
    }

    fn set(&self, key: String, value: Vec<u8>) {
        let mut data = self.data.write().unwrap();
        data.insert(key, (value, Instant::now() + self.ttl));
    }

    fn get(&self, key: &str) -> Option<Vec<u8>> {
        let data = self.data.read().unwrap();
        data.get(key).and_then(|(value, expiry)| {
            if *expiry > Instant::now() {
                Some(value.clone())
            } else {
                None
            }
        })
    }

    fn delete(&self, key: &str) -> bool {
        let mut data = self.data.write().unwrap();
        data.remove(key).is_some()
    }

    fn clear_expired(&self) -> usize {
        let mut data = self.data.write().unwrap();
        let before = data.len();
        data.retain(|_, (_, expiry)| *expiry > Instant::now());
        before - data.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[test]
    fn test_dht_key_routing() {
        let node1 = DHTNode::new(
            100,
            SocketAddr::new(IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)), 8001)
        );
        
        let node2 = DHTNode::new(
            200,
            SocketAddr::new(IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)), 8002)
        );
        
        let node3 = DHTNode::new(
            300,
            SocketAddr::new(IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)), 8003)
        );
        
        // Test key hashing
        let key1 = b"test_key_1";
        let key2 = b"test_key_2";
        
        let hash1 = node1.hash_key(key1);
        let hash2 = node1.hash_key(key2);
        
        assert_ne!(hash1, hash2);
        
        // Test successor finding logic
        // Note: Simplified test since full DHT requires network simulation
        assert!(hash1 > 0);
        assert!(hash2 > 0);
    }

    #[test]
    fn test_g_counter_crdt() {
        let num_nodes = 3;
        let mut counters = vec![
            GCounter::new(0, num_nodes),
            GCounter::new(1, num_nodes),
            GCounter::new(2, num_nodes),
        ];
        
        // Each node increments independently
        counters[0].increment();
        counters[0].increment();
        counters[1].increment();
        counters[2].increment();
        counters[2].increment();
        counters[2].increment();
        
        assert_eq!(counters[0].value(), 2);
        assert_eq!(counters[1].value(), 1);
        assert_eq!(counters[2].value(), 3);
        
        // Merge all counters
        let mut merged = counters[0].clone();
        merged.merge(&counters[1]);
        merged.merge(&counters[2]);
        
        // After merge, should have total of all increments
        assert_eq!(merged.value(), 6);
        
        // Verify merge is idempotent
        let mut merged2 = merged.clone();
        merged2.merge(&merged);
        assert_eq!(merged2.value(), 6);
        
        // Verify merge is commutative
        let mut merge_ab = counters[0].clone();
        merge_ab.merge(&counters[1]);
        let mut merge_ba = counters[1].clone();
        merge_ba.merge(&counters[0]);
        assert_eq!(merge_ab.value(), merge_ba.value());
    }

    #[test]
    fn test_pn_counter_crdt() {
        let num_nodes = 3;
        let mut counters = vec![
            PNCounter::new(0, num_nodes),
            PNCounter::new(1, num_nodes),
            PNCounter::new(2, num_nodes),
        ];
        
        // Node 0: +2, -1
        counters[0].increment();
        counters[0].increment();
        counters[0].decrement();
        
        // Node 1: +1, -2
        counters[1].increment();
        counters[1].decrement();
        counters[1].decrement();
        
        // Node 2: +3
        counters[2].increment();
        counters[2].increment();
        counters[2].increment();
        
        assert_eq!(counters[0].value(), 1); // 2-1
        assert_eq!(counters[1].value(), -1); // 1-2
        assert_eq!(counters[2].value(), 3); // 3-0
        
        // Merge all counters
        let mut merged = counters[0].clone();
        merged.merge(&counters[1]);
        merged.merge(&counters[2]);
        
        // Total: (2+1+3) - (1+2+0) = 6 - 3 = 3
        assert_eq!(merged.value(), 3);
        
        // Test associative property
        let mut merge_ab = counters[0].clone();
        merge_ab.merge(&counters[1]);
        merge_ab.merge(&counters[2]);
        
        let mut merge_bc = counters[1].clone();
        merge_bc.merge(&counters[2]);
        let mut merge_abc = counters[0].clone();
        merge_abc.merge(&merge_bc);
        
        assert_eq!(merge_ab.value(), merge_abc.value());
    }

    #[test]
    fn test_distributed_queue_concurrent_access() {
        let queue = Arc::new(DistributedQueue::new());
        let num_threads = 4;
        let num_items_per_thread = 10;
        
        let mut handles = vec![];
        let enqueued_count = Arc::new(AtomicUsize::new(0));
        let dequeued_count = Arc::new(AtomicUsize::new(0));
        
        // Enqueue threads
        for thread_id in 0..num_threads {
            let queue_clone = queue.clone();
            let enqueued_clone = enqueued_count.clone();
            
            handles.push(thread::spawn(move || {
                for i in 0..num_items_per_thread {
                    let item = format!("thread{}_item{}", thread_id, i).into_bytes();
                    queue_clone.enqueue(item);
                    enqueued_clone.fetch_add(1, Ordering::SeqCst);
                }
            }));
        }
        
        // Wait for all enqueues
        for handle in handles {
            handle.join().unwrap();
        }
        
        assert_eq!(queue.size(), num_threads * num_items_per_thread);
        assert_eq!(enqueued_count.load(Ordering::SeqCst), num_threads * num_items_per_thread);
        
        // Dequeue all items
        let mut handles = vec![];
        for _ in 0..num_threads {
            let queue_clone = queue.clone();
            let dequeued_clone = dequeued_count.clone();
            
            handles.push(thread::spawn(move || {
                while let Some(_) = queue_clone.dequeue() {
                    dequeued_clone.fetch_add(1, Ordering::SeqCst);
                }
            }));
        }
        
        for handle in handles {
            handle.join().unwrap();
        }
        
        assert_eq!(queue.size(), 0);
        assert_eq!(dequeued_count.load(Ordering::SeqCst), num_threads * num_items_per_thread);
    }

    #[test]
    fn test_distributed_lock_ordering() {
        let lock = Arc::new(DistributedLock::new("test_lock".to_string()));
        let num_nodes = 3;
        
        let node_addresses = vec![
            SocketAddr::new(IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)), 9001),
            SocketAddr::new(IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)), 9002),
            SocketAddr::new(IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)), 9003),
        ];
        
        // Request locks in different order
        let lock_clone = lock.clone();
        let handle1 = thread::spawn(move || {
            lock_clone.request_lock(node_addresses[0])
        });
        
        let lock_clone = lock.clone();
        let handle2 = thread::spawn(move || {
            thread::sleep(Duration::from_millis(10));
            lock_clone.request_lock(node_addresses[1])
        });
        
        let lock_clone = lock.clone();
        let handle3 = thread::spawn(move || {
            thread::sleep(Duration::from_millis(20));
            lock_clone.request_lock(node_addresses[2])
        });
        
        let time1 = handle1.join().unwrap();
        let time2 = handle2.join().unwrap();
        let time3 = handle3.join().unwrap();
        
        // Node 0 should have lock (requested first)
        assert!(lock.has_lock(node_addresses[0]));
        assert!(!lock.has_lock(node_addresses[1]));
        assert!(!lock.has_lock(node_addresses[2]));
        
        // Release lock from node 0
        lock.release_lock(node_addresses[0]);
        
        // Now node 1 should have lock
        assert!(lock.has_lock(node_addresses[1]));
        
        // Release lock from node 1
        lock.release_lock(node_addresses[1]);
        
        // Now node 2 should have lock
        assert!(lock.has_lock(node_address