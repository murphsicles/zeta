//! Basic borrow checker for Zeta
//!
//! This module provides a simplified borrow checking system that tracks
//! ownership and borrowing to prevent data races and memory safety issues.

use std::collections::HashMap;
use std::sync::{RwLock, Arc};
use std::any::Any;

/// Borrow checker error
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BorrowError {
    AlreadyBorrowed,
    AlreadyMutablyBorrowed,
    NotOwned,
    InvalidBorrow,
    DeadlockDetected,
}

/// Borrow checker result
pub type BorrowResult<T> = Result<T, BorrowError>;

/// Borrow state for a value
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BorrowState {
    /// Value is owned and not borrowed
    Owned,
    /// Value is immutably borrowed (can have multiple)
    Borrowed(usize),
    /// Value is mutably borrowed (exclusive)
    MutablyBorrowed,
}

/// Borrow checker entry
struct BorrowEntry {
    state: BorrowState,
    owner: Option<String>,
}

impl BorrowEntry {
    fn new(owner: Option<String>) -> Self {
        Self {
            state: BorrowState::Owned,
            owner,
        }
    }
}

/// Basic borrow checker
pub struct BorrowChecker {
    entries: RwLock<HashMap<usize, BorrowEntry>>,
    next_id: std::sync::atomic::AtomicUsize,
}

impl BorrowChecker {
    /// Create a new borrow checker
    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            entries: RwLock::new(HashMap::new()),
            next_id: std::sync::atomic::AtomicUsize::new(1),
        })
    }
    
    /// Register a new value and return its ID
    pub fn register<T: Any>(&self, owner: Option<String>) -> usize {
        let id = self.next_id.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let mut entries = self.entries.write().unwrap();
        entries.insert(id, BorrowEntry::new(owner));
        id
    }
    
    /// Unregister a value
    pub fn unregister(&self, id: usize) -> BorrowResult<()> {
        let mut entries = self.entries.write().unwrap();
        
        if let Some(entry) = entries.get(&id) {
            match entry.state {
                BorrowState::Owned => {
                    entries.remove(&id);
                    Ok(())
                }
                BorrowState::Borrowed(_) => Err(BorrowError::AlreadyBorrowed),
                BorrowState::MutablyBorrowed => Err(BorrowError::AlreadyMutablyBorrowed),
            }
        } else {
            Err(BorrowError::NotOwned)
        }
    }
    
    /// Borrow a value immutably
    pub fn borrow(&self, id: usize) -> BorrowResult<BorrowGuard> {
        let mut entries = self.entries.write().unwrap();
        
        if let Some(entry) = entries.get_mut(&id) {
            match entry.state {
                BorrowState::Owned => {
                    entry.state = BorrowState::Borrowed(1);
                    Ok(BorrowGuard::new(id, self.clone()))
                }
                BorrowState::Borrowed(count) => {
                    entry.state = BorrowState::Borrowed(count + 1);
                    Ok(BorrowGuard::new(id, self.clone()))
                }
                BorrowState::MutablyBorrowed => Err(BorrowError::AlreadyMutablyBorrowed),
            }
        } else {
            Err(BorrowError::NotOwned)
        }
    }
    
    /// Borrow a value mutably
    pub fn borrow_mut(&self, id: usize) -> BorrowResult<MutBorrowGuard> {
        let mut entries = self.entries.write().unwrap();
        
        if let Some(entry) = entries.get_mut(&id) {
            match entry.state {
                BorrowState::Owned => {
                    entry.state = BorrowState::MutablyBorrowed;
                    Ok(MutBorrowGuard::new(id, self.clone()))
                }
                BorrowState::Borrowed(_) => Err(BorrowError::AlreadyBorrowed),
                BorrowState::MutablyBorrowed => Err(BorrowError::AlreadyMutablyBorrowed),
            }
        } else {
            Err(BorrowError::NotOwned)
        }
    }
    
    /// Release an immutable borrow
    fn release_borrow(&self, id: usize) -> BorrowResult<()> {
        let mut entries = self.entries.write().unwrap();
        
        if let Some(entry) = entries.get_mut(&id) {
            match entry.state {
                BorrowState::Borrowed(count) if count > 1 => {
                    entry.state = BorrowState::Borrowed(count - 1);
                    Ok(())
                }
                BorrowState::Borrowed(_) => {
                    entry.state = BorrowState::Owned;
                    Ok(())
                }
                _ => Err(BorrowError::InvalidBorrow),
            }
        } else {
            Err(BorrowError::NotOwned)
        }
    }
    
    /// Release a mutable borrow
    fn release_mut_borrow(&self, id: usize) -> BorrowResult<()> {
        let mut entries = self.entries.write().unwrap();
        
        if let Some(entry) = entries.get_mut(&id) {
            match entry.state {
                BorrowState::MutablyBorrowed => {
                    entry.state = BorrowState::Owned;
                    Ok(())
                }
                _ => Err(BorrowError::InvalidBorrow),
            }
        } else {
            Err(BorrowError::NotOwned)
        }
    }
    
    /// Check if a value can be borrowed immutably
    pub fn can_borrow(&self, id: usize) -> bool {
        let entries = self.entries.read().unwrap();
        
        if let Some(entry) = entries.get(&id) {
            match entry.state {
                BorrowState::Owned | BorrowState::Borrowed(_) => true,
                BorrowState::MutablyBorrowed => false,
            }
        } else {
            false
        }
    }
    
    /// Check if a value can be borrowed mutably
    pub fn can_borrow_mut(&self, id: usize) -> bool {
        let entries = self.entries.read().unwrap();
        
        if let Some(entry) = entries.get(&id) {
            match entry.state {
                BorrowState::Owned => true,
                BorrowState::Borrowed(_) | BorrowState::MutablyBorrowed => false,
            }
        } else {
            false
        }
    }
    
    /// Get borrow statistics
    pub fn stats(&self) -> BorrowStats {
        let entries = self.entries.read().unwrap();
        
        let mut stats = BorrowStats {
            total_values: entries.len(),
            owned: 0,
            borrowed: 0,
            mutably_borrowed: 0,
        };
        
        for entry in entries.values() {
            match entry.state {
                BorrowState::Owned => stats.owned += 1,
                BorrowState::Borrowed(count) => stats.borrowed += count,
                BorrowState::MutablyBorrowed => stats.mutably_borrowed += 1,
            }
        }
        
        stats
    }
}

/// Borrow statistics
#[derive(Debug, Clone, Copy)]
pub struct BorrowStats {
    pub total_values: usize,
    pub owned: usize,
    pub borrowed: usize,
    pub mutably_borrowed: usize,
}

/// Immutable borrow guard
pub struct BorrowGuard {
    id: usize,
    checker: Arc<BorrowChecker>,
    released: bool,
}

impl BorrowGuard {
    fn new(id: usize, checker: Arc<BorrowChecker>) -> Self {
        Self {
            id,
            checker,
            released: false,
        }
    }
    
    /// Get the borrowed value ID
    pub fn id(&self) -> usize {
        self.id
    }
}

impl Drop for BorrowGuard {
    fn drop(&mut self) {
        if !self.released {
            let _ = self.checker.release_borrow(self.id);
        }
    }
}

/// Mutable borrow guard
pub struct MutBorrowGuard {
    id: usize,
    checker: Arc<BorrowChecker>,
    released: bool,
}

impl MutBorrowGuard {
    fn new(id: usize, checker: Arc<BorrowChecker>) -> Self {
        Self {
            id,
            checker,
            released: false,
        }
    }
    
    /// Get the borrowed value ID
    pub fn id(&self) -> usize {
        self.id
    }
}

impl Drop for MutBorrowGuard {
    fn drop(&mut self) {
        if !self.released {
            let _ = self.checker.release_mut_borrow(self.id);
        }
    }
}

/// Smart pointer with borrow checking
pub struct CheckedPtr<T> {
    data: Arc<T>,
    id: usize,
    checker: Arc<BorrowChecker>,
}

impl<T> CheckedPtr<T> {
    /// Create a new checked pointer
    pub fn new(value: T, owner: Option<String>) -> Self {
        let checker = BorrowChecker::new();
        let id = checker.register::<T>(owner);
        
        Self {
            data: Arc::new(value),
            id,
            checker,
        }
    }
    
    /// Borrow immutably
    pub fn borrow(&self) -> BorrowResult<CheckedRef<T>> {
        let guard = self.checker.borrow(self.id)?;
        Ok(CheckedRef {
            data: self.data.clone(),
            _guard: guard,
        })
    }
    
    /// Borrow mutably (requires exclusive access)
    pub fn borrow_mut(&mut self) -> BorrowResult<CheckedRefMut<T>> {
        let guard = self.checker.borrow_mut(self.id)?;
        Ok(CheckedRefMut {
            data: Arc::get_mut(&mut self.data)
                .ok_or(BorrowError::AlreadyBorrowed)?,
            _guard: guard,
        })
    }
    
    /// Get the value ID
    pub fn id(&self) -> usize {
        self.id
    }
}

impl<T> Clone for CheckedPtr<T> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            id: self.checker.register::<T>(None),
            checker: self.checker.clone(),
        }
    }
}

/// Immutable reference with borrow checking
pub struct CheckedRef<T> {
    data: Arc<T>,
    _guard: BorrowGuard,
}

impl<T> std::ops::Deref for CheckedRef<T> {
    type Target = T;
    
    fn deref(&self) -> &T {
        &self.data
    }
}

/// Mutable reference with borrow checking
pub struct CheckedRefMut<T> {
    data: &mut T,
    _guard: MutBorrowGuard,
}

impl<T> std::ops::Deref for CheckedRefMut<T> {
    type Target = T;
    
    fn deref(&self) -> &T {
        self.data
    }
}

impl<T> std::ops::DerefMut for CheckedRefMut<T> {
    fn deref_mut(&mut self) -> &mut T {
        self.data
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_borrow_checker_basic() {
        let checker = BorrowChecker::new();
        
        // Register a value
        let id = checker.register::<i32>(Some("test".to_string()));
        
        // Should be able to borrow
        assert!(checker.can_borrow(id));
        assert!(checker.can_borrow_mut(id));
        
        // Borrow immutably
        let guard = checker.borrow(id).unwrap();
        
        // Can't borrow mutably while borrowed
        assert!(!checker.can_borrow_mut(id));
        
        // Can borrow immutably again
        let guard2 = checker.borrow(id).unwrap();
        
        // Drop guards
        drop(guard);
        drop(guard2);
        
        // Now can borrow mutably
        let mut_guard = checker.borrow_mut(id).unwrap();
        
        // Can't borrow at all while mutably borrowed
        assert!(!checker.can_borrow(id));
        assert!(!checker.can_borrow_mut(id));
        
        drop(mut_guard);
        
        // Unregister
        checker.unregister(id).unwrap();
    }
    
    #[test]
    fn test_checked_ptr() {
        let mut ptr = CheckedPtr::new(42, Some("owner".to_string()));
        
        // Borrow immutably multiple times
        let ref1 = ptr.borrow().unwrap();
        assert_eq!(**ref1, 42);
        
        let ref2 = ptr.borrow().unwrap();
        assert_eq!(**ref2, 42);
        
        // Can't borrow mutably while borrowed
        assert!(ptr.borrow_mut().is_err());
        
        drop(ref1);
        drop(ref2);
        
        // Now can borrow mutably
        let mut ref_mut = ptr.borrow_mut().unwrap();
        *ref_mut = 100;
        assert_eq!(**ref_mut, 100);
        
        drop(ref_mut);
        
        // Check the value was updated
        let ref3 = ptr.borrow().unwrap();
        assert_eq!(**ref3, 100);
    }
    
    #[test]
    fn test_borrow_stats() {
        let checker = BorrowChecker::new();
        
        let id1 = checker.register::<i32>(None);
        let id2 = checker.register::<String>(None);
        
        let stats = checker.stats();
        assert_eq!(stats.total_values, 2);
        assert_eq!(stats.owned, 2);
        assert_eq!(stats.borrowed, 0);
        assert_eq!(stats.mutably_borrowed, 0);
        
        let _guard1 = checker.borrow(id1).unwrap();
        let _guard2 = checker.borrow(id1).unwrap();
        let _guard3 = checker.borrow_mut(id2).unwrap();
        
        let stats = checker.stats();
        assert_eq!(stats.total_values, 2);
        assert_eq!(stats.owned, 0);
        assert_eq!(stats.borrowed, 2);
        assert_eq!(stats.mutably_borrowed, 1);
    }
    
    #[test]
    fn test_borrow_errors() {
        let checker = BorrowChecker::new();
        
        let id = checker.register::<i32>(None);
        
        // Try to borrow mutably twice
        let guard1 = checker.borrow_mut(id).unwrap();
        let result = checker.borrow_mut(id);
        assert_eq!(result, Err(BorrowError::AlreadyMutablyBorrowed));
        
        drop(guard1);
        
        // Try to borrow immutably then mutably
        let guard1 = checker.borrow(id).unwrap();
        let result = checker.borrow_mut(id);
        assert_eq!(result, Err(BorrowError::AlreadyBorrowed));
        
        drop(guard1);
        
        // Try to unregister while borrowed
        let guard1 = checker.borrow(id).unwrap();
        let result = checker.unregister(id);
        assert_eq!(result, Err(BorrowError::AlreadyBorrowed));
        
        drop(guard1);
        
        // Try to access non-existent ID
        let result = checker.borrow(999);
        assert_eq!(result, Err(BorrowError::NotOwned));
    }
}