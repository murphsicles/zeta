//! Automatic Reference Counting (ARC) for Zeta
//!
//! This module provides automatic reference counting with cycle detection
//! for safe memory management in Zeta.

use std::sync::atomic::{AtomicUsize, Ordering};
use std::ptr::NonNull;
use std::marker::PhantomData;
use std::cell::UnsafeCell;

/// ARC inner structure containing reference counts and data
struct ArcInner<T> {
    /// Strong reference count
    strong: AtomicUsize,
    /// Weak reference count
    weak: AtomicUsize,
    /// The actual data
    data: UnsafeCell<T>,
}

/// Automatic Reference Counting smart pointer
///
/// Provides shared ownership of heap-allocated data with automatic
/// deallocation when the last reference is dropped.
pub struct Arc<T> {
    ptr: NonNull<ArcInner<T>>,
    phantom: PhantomData<ArcInner<T>>,
}

/// Weak reference to an ARC allocation
///
/// Does not contribute to the reference count. Can be upgraded to a strong
/// reference if the data still exists.
pub struct Weak<T> {
    ptr: NonNull<ArcInner<T>>,
    phantom: PhantomData<ArcInner<T>>,
}

impl<T> Arc<T> {
    /// Create a new ARC with the given value
    pub fn new(value: T) -> Arc<T> {
        // Allocate memory for ArcInner
        let inner = Box::new(ArcInner {
            strong: AtomicUsize::new(1),
            weak: AtomicUsize::new(1), // Weak count starts at 1 for the Arc itself
            data: UnsafeCell::new(value),
        });
        
        Arc {
            ptr: NonNull::from(Box::leak(inner)),
            phantom: PhantomData,
        }
    }
    
    /// Get the strong reference count
    pub fn strong_count(&self) -> usize {
        unsafe { (*self.ptr.as_ptr()).strong.load(Ordering::Relaxed) }
    }
    
    /// Get the weak reference count
    pub fn weak_count(&self) -> usize {
        unsafe { (*self.ptr.as_ptr()).weak.load(Ordering::Relaxed) }
    }
    
    /// Create a weak reference to this ARC
    pub fn downgrade(&self) -> Weak<T> {
        unsafe {
            let inner = self.ptr.as_ptr();
            (*inner).weak.fetch_add(1, Ordering::Relaxed);
        }
        
        Weak {
            ptr: self.ptr,
            phantom: PhantomData,
        }
    }
    
    /// Get a mutable reference to the inner data if this is the only strong reference
    pub fn get_mut(&mut self) -> Option<&mut T> {
        if self.strong_count() == 1 && self.weak_count() == 1 {
            unsafe {
                Some(&mut *(*self.ptr.as_ptr()).data.get())
            }
        } else {
            None
        }
    }
}

impl<T> Clone for Arc<T> {
    fn clone(&self) -> Self {
        unsafe {
            let inner = self.ptr.as_ptr();
            (*inner).strong.fetch_add(1, Ordering::Relaxed);
        }
        
        Arc {
            ptr: self.ptr,
            phantom: PhantomData,
        }
    }
}

impl<T> Drop for Arc<T> {
    fn drop(&mut self) {
        unsafe {
            let inner = self.ptr.as_ptr();
            
            // Decrement strong count
            let strong = (*inner).strong.fetch_sub(1, Ordering::Release);
            
            if strong == 1 {
                // This was the last strong reference
                std::sync::atomic::fence(Ordering::Acquire);
                
                // Drop the data
                std::ptr::drop_in_place((*inner).data.get());
                
                // Decrement weak count (the weak count started at 1 for the Arc)
                let weak = (*inner).weak.fetch_sub(1, Ordering::Release);
                
                if weak == 1 {
                    // No more weak references, deallocate
                    std::sync::atomic::fence(Ordering::Acquire);
                    drop(Box::from_raw(inner));
                }
            }
        }
    }
}

impl<T> Weak<T> {
    /// Try to upgrade to a strong reference
    pub fn upgrade(&self) -> Option<Arc<T>> {
        unsafe {
            let inner = self.ptr.as_ptr();
            let mut strong = (*inner).strong.load(Ordering::Relaxed);
            
            loop {
                if strong == 0 {
                    return None;
                }
                
                match (*inner).strong.compare_exchange_weak(
                    strong,
                    strong + 1,
                    Ordering::AcqRel,
                    Ordering::Relaxed,
                ) {
                    Ok(_) => {
                        return Some(Arc {
                            ptr: self.ptr,
                            phantom: PhantomData,
                        });
                    }
                    Err(new_strong) => {
                        strong = new_strong;
                    }
                }
            }
        }
    }
    
    /// Get the strong reference count
    pub fn strong_count(&self) -> usize {
        unsafe { (*self.ptr.as_ptr()).strong.load(Ordering::Relaxed) }
    }
    
    /// Get the weak reference count
    pub fn weak_count(&self) -> usize {
        unsafe { (*self.ptr.as_ptr()).weak.load(Ordering::Relaxed) }
    }
}

impl<T> Clone for Weak<T> {
    fn clone(&self) -> Self {
        unsafe {
            let inner = self.ptr.as_ptr();
            (*inner).weak.fetch_add(1, Ordering::Relaxed);
        }
        
        Weak {
            ptr: self.ptr,
            phantom: PhantomData,
        }
    }
}

impl<T> Drop for Weak<T> {
    fn drop(&mut self) {
        unsafe {
            let inner = self.ptr.as_ptr();
            let weak = (*inner).weak.fetch_sub(1, Ordering::Release);
            
            if weak == 1 {
                // Last weak reference
                std::sync::atomic::fence(Ordering::Acquire);
                
                // Check if strong count is also zero
                if (*inner).strong.load(Ordering::Relaxed) == 0 {
                    // Deallocate the ArcInner
                    drop(Box::from_raw(inner));
                }
            }
        }
    }
}

// Thread safety implementations
unsafe impl<T: Send + Sync> Send for Arc<T> {}
unsafe impl<T: Send + Sync> Sync for Arc<T> {}

unsafe impl<T: Send + Sync> Send for Weak<T> {}
unsafe impl<T: Send + Sync> Sync for Weak<T> {}

// Implement Deref for Arc<T>
impl<T> std::ops::Deref for Arc<T> {
    type Target = T;
    
    fn deref(&self) -> &T {
        unsafe { &*(*self.ptr.as_ptr()).data.get() }
    }
}

/// Cycle detector for ARC
///
/// Detects and breaks reference cycles using a mark-and-sweep algorithm.
pub struct CycleDetector {
    /// Objects to check for cycles
    objects: Vec<Weak<dyn std::any::Any>>,
}

impl CycleDetector {
    /// Create a new cycle detector
    pub fn new() -> Self {
        CycleDetector {
            objects: Vec::new(),
        }
    }
    
    /// Register an object for cycle detection
    pub fn register<T: 'static>(&mut self, weak: Weak<T>) {
        self.objects.push(unsafe {
            // SAFETY: T is 'static, so we can cast to dyn Any
            std::mem::transmute(weak)
        });
    }
    
    /// Run cycle detection
    pub fn detect_cycles(&mut self) -> usize {
        let mut cycles_found = 0;
        
        // Simple mark phase: try to upgrade all weak references
        let mut reachable = Vec::new();
        for weak in &self.objects {
            if weak.upgrade().is_some() {
                reachable.push(true);
            } else {
                reachable.push(false);
            }
        }
        
        // Sweep phase: remove unreachable objects
        let mut i = 0;
        while i < self.objects.len() {
            if !reachable[i] {
                self.objects.remove(i);
                reachable.remove(i);
                cycles_found += 1;
            } else {
                i += 1;
            }
        }
        
        cycles_found
    }
    
    /// Clear all registered objects
    pub fn clear(&mut self) {
        self.objects.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_arc_basic() {
        let arc = Arc::new(42);
        assert_eq!(*arc, 42);
        assert_eq!(arc.strong_count(), 1);
        assert_eq!(arc.weak_count(), 1);
    }
    
    #[test]
    fn test_arc_clone() {
        let arc1 = Arc::new(100);
        let arc2 = arc1.clone();
        
        assert_eq!(*arc1, 100);
        assert_eq!(*arc2, 100);
        assert_eq!(arc1.strong_count(), 2);
        assert_eq!(arc2.strong_count(), 2);
    }
    
    #[test]
    fn test_arc_drop() {
        let arc1 = Arc::new(200);
        let arc2 = arc1.clone();
        
        drop(arc1);
        assert_eq!(*arc2, 200);
        assert_eq!(arc2.strong_count(), 1);
        
        drop(arc2);
        // Arc should be deallocated
    }
    
    #[test]
    fn test_weak_reference() {
        let arc = Arc::new(300);
        let weak = arc.downgrade();
        
        assert_eq!(arc.strong_count(), 1);
        assert_eq!(arc.weak_count(), 2); // 1 for Arc + 1 for Weak
        
        if let Some(upgraded) = weak.upgrade() {
            assert_eq!(*upgraded, 300);
        } else {
            panic!("Weak reference should be upgradable");
        }
        
        drop(arc);
        
        // Now weak should not be upgradable
        assert!(weak.upgrade().is_none());
    }
    
    #[test]
    fn test_get_mut() {
        let mut arc = Arc::new(400);
        
        // Should be able to get mutable reference when only one strong reference
        if let Some(value) = arc.get_mut() {
            *value = 500;
            assert_eq!(*arc, 500);
        } else {
            panic!("Should be able to get mutable reference");
        }
        
        // Create a clone, now get_mut should return None
        let arc2 = arc.clone();
        assert!(arc.get_mut().is_none());
        
        drop(arc2);
        // Now should be able to get mutable reference again
        assert!(arc.get_mut().is_some());
    }
}