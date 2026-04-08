//! Memory allocator primitives for Zeta
//!
//! This module provides basic memory allocation primitives that integrate
//! with the ARC system for safe memory management.

use std::alloc::{Layout, alloc, dealloc};
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

use super::arc::{Arc, Weak};

/// Memory allocation error
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AllocError {
    OutOfMemory,
    InvalidAlignment,
    InvalidSize,
    AlreadyFreed,
}

/// Memory allocation result
pub type AllocResult<T> = Result<T, AllocError>;

/// Memory block header for tracking allocations
struct BlockHeader {
    size: usize,
    magic: u32,
    _padding: [u8; 4], // Ensure alignment
}

impl BlockHeader {
    const MAGIC: u32 = 0xDEADBEEF;
    
    fn new(size: usize) -> Self {
        Self {
            size,
            magic: Self::MAGIC,
            _padding: [0; 4],
        }
    }
    
    fn validate(&self) -> bool {
        self.magic == Self::MAGIC
    }
}

/// Simple bump allocator for testing
pub struct BumpAllocator {
    start: *mut u8,
    current: AtomicUsize,
    end: usize,
    total_allocated: AtomicUsize,
    allocation_count: AtomicUsize,
}

impl BumpAllocator {
    /// Create a new bump allocator with the given memory range
    pub fn new(start: *mut u8, size: usize) -> Self {
        let start_addr = start as usize;
        Self {
            start,
            current: AtomicUsize::new(start_addr),
            end: start_addr + size,
            total_allocated: AtomicUsize::new(0),
            allocation_count: AtomicUsize::new(0),
        }
    }
    
    /// Allocate memory with the given size and alignment
    pub fn allocate(&self, size: usize, align: usize) -> AllocResult<NonNull<u8>> {
        if size == 0 {
            return Err(AllocError::InvalidSize);
        }
        
        if !align.is_power_of_two() {
            return Err(AllocError::InvalidAlignment);
        }
        
        let mut current = self.current.load(Ordering::Acquire);
        
        loop {
            // Align the pointer
            let aligned = (current + align - 1) & !(align - 1);
            let new_current = aligned + size;
            
            if new_current > self.end {
                return Err(AllocError::OutOfMemory);
            }
            
            match self.current.compare_exchange_weak(
                current,
                new_current,
                Ordering::AcqRel,
                Ordering::Acquire,
            ) {
                Ok(_) => {
                    // Update statistics
                    self.total_allocated.fetch_add(size, Ordering::Relaxed);
                    self.allocation_count.fetch_add(1, Ordering::Relaxed);
                    
                    // Return the aligned pointer
                    let ptr = aligned as *mut u8;
                    unsafe {
                        // Zero the memory
                        std::ptr::write_bytes(ptr, 0, size);
                    }
                    return Ok(unsafe { NonNull::new_unchecked(ptr) });
                }
                Err(actual_current) => {
                    current = actual_current;
                }
            }
        }
    }
    
    /// Get current usage
    pub fn usage(&self) -> usize {
        self.current.load(Ordering::Relaxed) - (self.start as usize)
    }
    
    /// Get total capacity
    pub fn capacity(&self) -> usize {
        self.end - (self.start as usize)
    }
    
    /// Get allocation statistics
    pub fn stats(&self) -> AllocStats {
        AllocStats {
            usage: self.usage(),
            capacity: self.capacity(),
            total_allocated: self.total_allocated.load(Ordering::Relaxed),
            allocation_count: self.allocation_count.load(Ordering::Relaxed),
        }
    }
    
    /// Reset the allocator (for testing only)
    pub fn reset(&self) {
        self.current.store(self.start as usize, Ordering::Release);
        self.total_allocated.store(0, Ordering::Release);
        self.allocation_count.store(0, Ordering::Release);
    }
}

/// Allocation statistics
#[derive(Debug, Clone, Copy)]
pub struct AllocStats {
    pub usage: usize,
    pub capacity: usize,
    pub total_allocated: usize,
    pub allocation_count: usize,
}

/// System allocator wrapper with ARC integration
pub struct SystemAllocator;

impl SystemAllocator {
    /// Allocate memory using the system allocator
    pub fn allocate(size: usize, align: usize) -> AllocResult<NonNull<u8>> {
        if size == 0 {
            return Err(AllocError::InvalidSize);
        }
        
        if !align.is_power_of_two() {
            return Err(AllocError::InvalidAlignment);
        }
        
        let layout = Layout::from_size_align(size, align)
            .map_err(|_| AllocError::InvalidAlignment)?;
        
        let ptr = unsafe { alloc(layout) };
        
        if ptr.is_null() {
            Err(AllocError::OutOfMemory)
        } else {
            Ok(unsafe { NonNull::new_unchecked(ptr) })
        }
    }
    
    /// Deallocate memory using the system allocator
    pub fn deallocate(ptr: NonNull<u8>, size: usize, align: usize) {
        if size == 0 {
            return;
        }
        
        if let Ok(layout) = Layout::from_size_align(size, align) {
            unsafe {
                dealloc(ptr.as_ptr(), layout);
            }
        }
    }
    
    /// Allocate and wrap in ARC
    pub fn allocate_arc<T>(value: T) -> AllocResult<Arc<T>> {
        Ok(Arc::new(value))
    }
    
    /// Allocate array and wrap in ARC
    pub fn allocate_array<T: Clone>(count: usize, value: T) -> AllocResult<Arc<[T]>> {
        let vec = vec![value; count];
        Ok(Arc::from(vec))
    }
}

/// Pool allocator for fixed-size objects
pub struct PoolAllocator<T> {
    block_size: usize,
    free_list: Vec<NonNull<u8>>,
    allocated: Vec<NonNull<u8>>,
}

impl<T> PoolAllocator<T> {
    /// Create a new pool allocator for type T
    pub fn new(capacity: usize) -> Self {
        let block_size = std::mem::size_of::<T>().max(std::mem::align_of::<T>());
        
        // Pre-allocate blocks
        let mut free_list = Vec::with_capacity(capacity);
        let mut allocated = Vec::with_capacity(capacity);
        
        for _ in 0..capacity {
            if let Ok(ptr) = SystemAllocator::allocate(block_size, std::mem::align_of::<T>()) {
                free_list.push(ptr);
                allocated.push(ptr);
            }
        }
        
        Self {
            block_size,
            free_list,
            allocated,
        }
    }
    
    /// Allocate an object from the pool
    pub fn allocate(&mut self) -> AllocResult<NonNull<T>> {
        if let Some(ptr) = self.free_list.pop() {
            Ok(ptr.cast())
        } else {
            Err(AllocError::OutOfMemory)
        }
    }
    
    /// Deallocate an object back to the pool
    pub fn deallocate(&mut self, ptr: NonNull<T>) {
        self.free_list.push(ptr.cast());
    }
    
    /// Get pool statistics
    pub fn stats(&self) -> PoolStats {
        PoolStats {
            block_size: self.block_size,
            total_blocks: self.allocated.len(),
            free_blocks: self.free_list.len(),
            used_blocks: self.allocated.len() - self.free_list.len(),
        }
    }
}

/// Pool statistics
#[derive(Debug, Clone, Copy)]
pub struct PoolStats {
    pub block_size: usize,
    pub total_blocks: usize,
    pub free_blocks: usize,
    pub used_blocks: usize,
}

/// Global allocator registry
pub struct GlobalAllocator {
    bump: Arc<BumpAllocator>,
    system: SystemAllocator,
}

impl GlobalAllocator {
    /// Create a new global allocator
    pub fn new(buffer_size: usize) -> Self {
        // Allocate a buffer for the bump allocator
        let layout = Layout::from_size_align(buffer_size, 8).unwrap();
        let buffer = unsafe { alloc(layout) };
        
        let bump = BumpAllocator::new(buffer, buffer_size);
        
        Self {
            bump: Arc::new(bump),
            system: SystemAllocator,
        }
    }
    
    /// Get the bump allocator
    pub fn bump(&self) -> &Arc<BumpAllocator> {
        &self.bump
    }
    
    /// Get the system allocator
    pub fn system(&self) -> &SystemAllocator {
        &self.system
    }
    
    /// Allocate using the appropriate allocator
    pub fn allocate(&self, size: usize, align: usize, use_bump: bool) -> AllocResult<NonNull<u8>> {
        if use_bump && size <= 1024 {
            self.bump.allocate(size, align)
        } else {
            SystemAllocator::allocate(size, align)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_bump_allocator_basic() {
        // Allocate a 1KB buffer
        let layout = Layout::from_size_align(1024, 8).unwrap();
        let buffer = unsafe { alloc(layout) };
        
        let allocator = BumpAllocator::new(buffer, 1024);
        
        // Allocate some memory
        let ptr1 = allocator.allocate(64, 8).unwrap();
        assert!(!ptr1.as_ptr().is_null());
        
        let ptr2 = allocator.allocate(128, 16).unwrap();
        assert!(!ptr2.as_ptr().is_null());
        
        // Check that pointers are different
        assert_ne!(ptr1.as_ptr(), ptr2.as_ptr());
        
        // Check statistics
        let stats = allocator.stats();
        assert!(stats.usage >= 64 + 128);
        assert_eq!(stats.capacity, 1024);
        
        // Clean up
        unsafe { dealloc(buffer, layout) };
    }
    
    #[test]
    fn test_bump_allocator_out_of_memory() {
        let layout = Layout::from_size_align(100, 8).unwrap();
        let buffer = unsafe { alloc(layout) };
        
        let allocator = BumpAllocator::new(buffer, 100);
        
        // Allocate too much memory
        let result = allocator.allocate(200, 8);
        assert_eq!(result, Err(AllocError::OutOfMemory));
        
        // Clean up
        unsafe { dealloc(buffer, layout) };
    }
    
    #[test]
    fn test_system_allocator() {
        // Allocate some memory
        let ptr = SystemAllocator::allocate(64, 8).unwrap();
        assert!(!ptr.as_ptr().is_null());
        
        // Deallocate
        SystemAllocator::deallocate(ptr, 64, 8);
    }
    
    #[test]
    fn test_pool_allocator() {
        struct TestStruct {
            value: i32,
            data: [u8; 32],
        }
        
        let mut pool = PoolAllocator::<TestStruct>::new(10);
        
        // Allocate from pool
        let ptr1 = pool.allocate().unwrap();
        unsafe {
            ptr1.as_ptr().write(TestStruct {
                value: 42,
                data: [0; 32],
            });
        }
        
        let ptr2 = pool.allocate().unwrap();
        
        // Check statistics
        let stats = pool.stats();
        assert_eq!(stats.total_blocks, 10);
        assert_eq!(stats.used_blocks, 2);
        assert_eq!(stats.free_blocks, 8);
        
        // Deallocate
        pool.deallocate(ptr1);
        pool.deallocate(ptr2);
        
        let stats = pool.stats();
        assert_eq!(stats.free_blocks, 10);
        assert_eq!(stats.used_blocks, 0);
    }
    
    #[test]
    fn test_arc_allocation() {
        // Test ARC allocation through system allocator
        let arc = SystemAllocator::allocate_arc(42).unwrap();
        assert_eq!(*arc, 42);
        
        let arc_array = SystemAllocator::allocate_array(5, 10).unwrap();
        assert_eq!(arc_array.len(), 5);
        assert_eq!(arc_array[0], 10);
        assert_eq!(arc_array[4], 10);
    }
}