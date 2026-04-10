//! Minimal allocator stub for compilation

use crate::memory::capability::MemoryCapability;
use crate::memory::error::{MemoryError, MemoryResult};

/// Allocate memory with specific rights
pub fn allocate_with_rights(
    _size: usize,
    _rights: u8,
    _region_id: Option<u64>,
) -> MemoryResult<MemoryCapability> {
    Err(MemoryError::AllocationFailed)
}

/// Reallocate memory
pub fn reallocate_with_rights(
    _cap: &MemoryCapability,
    _new_size: usize,
    _rights: u8,
) -> MemoryResult<MemoryCapability> {
    Err(MemoryError::AllocationFailed)
}

/// Free memory
pub fn free_with_rights(_cap: &MemoryCapability) -> MemoryResult<()> {
    Ok(())
}

/// Simple allocator
pub struct SimpleAllocator;

impl SimpleAllocator {
    pub fn new() -> Self {
        Self
    }
    
    pub fn allocate(&mut self, _size: usize) -> MemoryResult<MemoryCapability> {
        Err(MemoryError::AllocationFailed)
    }
    
    pub fn free(&mut self, _cap: MemoryCapability) -> MemoryResult<()> {
        Ok(())
    }
    
    pub fn clear(&mut self) {}
}

impl Default for SimpleAllocator {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert MemoryError to i32 for FFI
pub fn memory_error_to_code(err: MemoryError) -> i32 {
    match err {
        MemoryError::AllocationFailed => -1,
        MemoryError::InvalidRegion => -2,
        MemoryError::InsufficientRights => -3,
        MemoryError::ZeroSize => -4,
        MemoryError::AlignmentError => -5,
        MemoryError::InvalidCapability => -6,
    }
}