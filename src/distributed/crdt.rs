//! Conflict-free Replicated Data Types (CRDTs)
//!
//! Built-in CRDT types for distributed state management with automatic
//! conflict resolution and merge operations.

use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};
use serde::{Serialize, Deserialize};

/// Node identifier for vector clocks
pub type NodeId = u64;

/// Vector clock for causal ordering
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct VectorClock {
    entries: HashMap<NodeId, u64>,
}

impl VectorClock {
    /// Create new vector clock
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }
    
    /// Increment counter for node
    pub fn increment(&mut self, node: NodeId) {
        let entry = self.entries.entry(node).or_insert(0);
        *entry += 1;
    }
    
    /// Get counter for node
    pub fn get(&self, node: NodeId) -> u64 {
        self.entries.get(&node).copied().unwrap_or(0)
    }
    
    /// Merge with another vector clock (take maximum)
    pub fn merge(&mut self, other: &VectorClock) {
        for (&node, &counter) in &other.entries {
            let entry = self.entries.entry(node).or_insert(0);
            *entry = (*entry).max(counter);
        }
    }
    
    /// Compare vector clocks
    pub fn compare(&self, other: &VectorClock) -> ClockOrdering {
        let mut less = false;
        let mut greater = false;
        
        let all_nodes: std::collections::HashSet<NodeId> = self.entries.keys()
            .chain(other.entries.keys())
            .copied()
            .collect();
        
        for node in all_nodes {
            let self_val = self.get(node);
            let other_val = other.get(node);
            
            if self_val < other_val {
                less = true;
            } else if self_val > other_val {
                greater = true;
            }
        }
        
        match (less, greater) {
            (false, false) => ClockOrdering::Equal,
            (true, false) => ClockOrdering::Less,
            (false, true) => ClockOrdering::Greater,
            (true, true) => ClockOrdering::Concurrent,
        }
    }
}

/// Vector clock ordering
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClockOrdering {
    Less,
    Greater,
    Equal,
    Concurrent,
}

/// Grow-only counter (G-Counter)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GCounter {
    entries: HashMap<NodeId, u64>,
}

impl GCounter {
    /// Create new G-Counter
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }
    
    /// Increment counter for this node
    pub fn increment(&mut self, node: NodeId) {
        let entry = self.entries.entry(node).or_insert(0);
        *entry += 1;
    }
    
    /// Get current value (sum of all nodes)
    pub fn value(&self) -> u64 {
        self.entries.values().sum()
    }
    
    /// Merge with another G-Counter (take maximum per node)
    pub fn merge(&mut self, other: &GCounter) {
        for (&node, &counter) in &other.entries {
            let entry = self.entries.entry(node).or_insert(0);
            *entry = (*entry).max(counter);
        }
    }
}

/// Positive-negative counter (PN-Counter)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PNCounter {
    increments: HashMap<NodeId, u64>,
    decrements: HashMap<NodeId, u64>,
}

impl PNCounter {
    /// Create new PN-Counter
    pub fn new() -> Self {
        Self {
            increments: HashMap::new(),
            decrements: HashMap::new(),
        }
    }
    
    /// Increment counter for this node
    pub fn increment(&mut self, node: NodeId) {
        let entry = self.increments.entry(node).or_insert(0);
        *entry += 1;
    }
    
    /// Decrement counter for this node
    pub fn decrement(&mut self, node: NodeId) {
        let entry = self.decrements.entry(node).or_insert(0);
        *entry += 1;
    }
    
    /// Get current value
    pub fn value(&self) -> i64 {
        let inc_sum: u64 = self.increments.values().sum();
        let dec_sum: u64 = self.decrements.values().sum();
        inc_sum as i64 - dec_sum as i64
    }
    
    /// Merge with another PN-Counter
    pub fn merge(&mut self, other: &PNCounter) {
        for (&node, &counter) in &other.increments {
            let entry = self.increments.entry(node).or_insert(0);
            *entry = (*entry).max(counter);
        }
        
        for (&node, &counter) in &other.decrements {
            let entry = self.decrements.entry(node).or_insert(0);
            *entry = (*entry).max(counter);
        }
    }
}

/// Grow-only set (G-Set)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GSet<T: Clone + Eq + std::hash::Hash> {
    elements: HashSet<T>,
}

impl<T: Clone + Eq + std::hash::Hash> GSet<T> {
    /// Create new G-Set
    pub fn new() -> Self {
        Self {
            elements: HashSet::new(),
        }
    }
    
    /// Add element to set
    pub fn add(&mut self, element: T) {
        self.elements.insert(element);
    }
    
    /// Check if element is in set
    pub fn contains(&self, element: &T) -> bool {
        self.elements.contains(element)
    }
    
    /// Get all elements
    pub fn elements(&self) -> Vec<T> {
        self.elements.iter().cloned().collect()
    }
    
    /// Merge with another G-Set (union)
    pub fn merge(&mut self, other: &GSet<T>) {
        self.elements.extend(other.elements.iter().cloned());
    }
}

/// Two-phase set (2P-Set)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TwoPSet<T: Clone + Eq + std::hash::Hash> {
    added: HashSet<T>,
    removed: HashSet<T>,
}

impl<T: Clone + Eq + std::hash::Hash> TwoPSet<T> {
    /// Create new 2P-Set
    pub fn new() -> Self {
        Self {
            added: HashSet::new(),
            removed: HashSet::new(),
        }
    }
    
    /// Add element to set
    pub fn add(&mut self, element: T) {
        if !self.removed.contains(&element) {
            self.added.insert(element);
        }
    }
    
    /// Remove element from set
    pub fn remove(&mut self, element: &T) {
        if self.added.contains(element) {
            self.removed.insert(element.clone());
        }
    }
    
    /// Check if element is in set
    pub fn contains(&self, element: &T) -> bool {
        self.added.contains(element) && !self.removed.contains(element)
    }
    
    /// Get all elements
    pub fn elements(&self) -> Vec<T> {
        self.added.iter()
            .filter(|e| !self.removed.contains(e))
            .cloned()
            .collect()
    }
    
    /// Merge with another 2P-Set
    pub fn merge(&mut self, other: &TwoPSet<T>) {
        self.added.extend(other.added.iter().cloned());
        self.removed.extend(other.removed.iter().cloned());
    }
}

/// Observed-remove set (OR-Set)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ORSet<T: Clone + Eq + std::hash::Hash> {
    elements: HashMap<T, Vec<VectorClock>>,
}

impl<T: Clone + Eq + std::hash::Hash> ORSet<T> {
    /// Create new OR-Set
    pub fn new() -> Self {
        Self {
            elements: HashMap::new(),
        }
    }
    
    /// Add element to set
    pub fn add(&mut self, element: T, node: NodeId, clock: &mut VectorClock) {
        clock.increment(node);
        
        let entry = self.elements.entry(element).or_insert_with(Vec::new);
        entry.push(clock.clone());
    }
    
    /// Remove element from set
    pub fn remove(&mut self, element: &T, node: NodeId, clock: &mut VectorClock) {
        if let Some(clocks) = self.elements.get_mut(element) {
            clock.increment(node);
            let mut new_clock = clock.clone();
            new_clock.increment(node); // Different event
            
            // Tag removal with new vector clock
            clocks.push(new_clock);
        }
    }
    
    /// Check if element is in set
    pub fn contains(&self, element: &T) -> bool {
        if let Some(clocks) = self.elements.get(element) {
            // Element is present if there's at least one add that hasn't been removed
            !clocks.is_empty()
        } else {
            false
        }
    }
    
    /// Get all elements
    pub fn elements(&self) -> Vec<T> {
        self.elements.keys()
            .filter(|k| self.contains(k))
            .cloned()
            .collect()
    }
    
    /// Merge with another OR-Set
    pub fn merge(&mut self, other: &ORSet<T>) {
        for (element, other_clocks) in &other.elements {
            let entry = self.elements.entry(element.clone()).or_insert_with(Vec::new);
            entry.extend(other_clocks.iter().cloned());
        }
    }
}

/// Last-write-wins register (LWW-Register)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LWWRegister<T: Clone> {
    value: Option<T>,
    timestamp: VectorClock,
}

impl<T: Clone> LWWRegister<T> {
    /// Create new LWW-Register
    pub fn new() -> Self {
        Self {
            value: None,
            timestamp: VectorClock::new(),
        }
    }
    
    /// Write value with timestamp
    pub fn write(&mut self, value: T, node: NodeId, clock: &mut VectorClock) {
        clock.increment(node);
        
        match self.timestamp.compare(clock) {
            ClockOrdering::Less | ClockOrdering::Concurrent => {
                self.value = Some(value);
                self.timestamp = clock.clone();
            }
            _ => {
                // Keep current value (later or equal timestamp)
            }
        }
    }
    
    /// Read value
    pub fn read(&self) -> Option<&T> {
        self.value.as_ref()
    }
    
    /// Merge with another LWW-Register
    pub fn merge(&mut self, other: &LWWRegister<T>) {
        match self.timestamp.compare(&other.timestamp) {
            ClockOrdering::Less => {
                self.value = other.value.clone();
                self.timestamp = other.timestamp.clone();
            }
            ClockOrdering::Greater => {
                // Keep current value
            }
            ClockOrdering::Equal => {
                // Values are concurrent, need resolution strategy
                // For simplicity, keep current value
                // In production, might use node ID as tiebreaker
            }
            ClockOrdering::Concurrent => {
                // Concurrent writes, need resolution
                // For LWW, we can use node ID as tiebreaker
                let self_max_node = self.timestamp.entries.keys().max().copied().unwrap_or(0);
                let other_max_node = other.timestamp.entries.keys().max().copied().unwrap_or(0);
                
                if other_max_node > self_max_node {
                    self.value = other.value.clone();
                    self.timestamp = other.timestamp.clone();
                }
            }
        }
    }
}

/// Thread-safe wrapper for CRDTs
pub struct SharedCRDT<T> {
    inner: Arc<RwLock<T>>,
}

impl<T> SharedCRDT<T> {
    /// Create new shared CRDT
    pub fn new(crdt: T) -> Self {
        Self {
            inner: Arc::new(RwLock::new(crdt)),
        }
    }
    
    /// Read CRDT value
    pub fn read<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&T) -> R,
    {
        let guard = self.inner.read().unwrap();
        f(&guard)
    }
    
    /// Write to CRDT
    pub fn write<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut T) -> R,
    {
        let mut guard = self.inner.write().unwrap();
        f(&mut guard)
    }
}

impl<T> Clone for SharedCRDT<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}