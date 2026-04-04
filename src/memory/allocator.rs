//! Capability-based allocator
//!
//! This allocator integrates the capability system with region-based
//! memory management to provide safe, efficient memory allocation.

use crate::memory::capability::{MemoryCapability, create_capability, free_capability, READ_RIGHT, WRITE_RIGHT, FREE_RIGHT, RESIZE_RIGHT};
use crate::memory::region::{register_allocation, unregister_allocation, root_region_id, validate_region_active};
use crate::memory::error::{MemoryError, MemoryResult};
use std::alloc::{alloc, dealloc, Layout};
use std::ptr;

/// Default alignment for allocations
const DEFAULT_ALIGNMENT: usize = 8;

/// Allocate memory with specific rights
pub fn allocate_with_rights(
    size: usize,
    rights: u8,
    region_id: Option<u64>,
) -> MemoryResult<MemoryCapability> {
    if size == 0 {
        return Err(MemoryError::ZeroSize);
    }
    
    // Use root region if none specified
    let region_id = region_id.unwrap_or_else(root_region_id);
    
    // Validate region is active
    validate_region_active(region_id)?;
    
    // Create layout with default alignment
    let layout = Layout::from_size_align(size, DEFAULT_ALIGNMENT)
        .map_err(|_| MemoryError::AlignmentError)?;
    
    // Allocate memory
    let ptr = unsafe { alloc(layout) };
    if ptr.is_null() {
        return Err(MemoryError::AllocationFailed);
    }
    
    // Create capability
    let capability = create_capability(
        region_id,
        ptr as usize,
        size,
        rights,
    );
    
    // Register allocation with region
    if let Err(err) = register_allocation(region_id, capability.allocation_id) {
        // Clean up allocation if registration fails
        unsafe { dealloc(ptr, layout) };
        return Err(err);
    }
    
    Ok(capability)
}

/// Reallocate memory with capability
pub fn reallocate_with_rights(
    cap: &MemoryCapability,
    new_size: usize,
    rights: u8,
) -> MemoryResult<MemoryCapability> {
    if new_size == 0 {
        return Err(MemoryError::ZeroSize);
    }
    
    // Check resize rights
    if (cap.rights & RESIZE_RIGHT) == 0 {
        return Err(MemoryError::InsufficientRights);
    }
    
    // Validate region is still active
    validate_region_active(cap.region_id)?;
    
    // Create new layout
    let new_layout = Layout::from_size_align(new_size, DEFAULT_ALIGNMENT)
        .map_err(|_| MemoryError::AlignmentError)?;
    
    // Allocate new memory
    let new_ptr = unsafe { alloc(new_layout) };
    if new_ptr.is_null() {
        return Err(MemoryError::AllocationFailed);
    }
    
    // Copy old data if needed
    if cap.base_ptr != 0 {
        let copy_size = std::cmp::min(cap.size, new_size);
        unsafe {
            ptr::copy_nonoverlapping(
                cap.base_ptr as *const u8,
                new_ptr,
                copy_size,
            );
        }
        
        // Zero out remaining bytes if growing
        if new_size > cap.size {
            let zero_start = unsafe { new_ptr.add(cap.size) };
            let zero_size = new_size - cap.size;
            unsafe {
                ptr::write_bytes(zero_start, 0, zero_size);
            }
        }
    } else {
        // Zero initialize new allocation
        unsafe {
            ptr::write_bytes(new_ptr, 0, new_size);
        }
    }
    
    // Create new capability
    let new_capability = create_capability(
        cap.region_id,
        new_ptr as usize,
        new_size,
        rights,
    );
    
    // Register new allocation
    register_allocation(cap.region_id, new_capability.allocation_id)?;
    
    // Free old allocation
    free_with_capability(cap)?;
    
    Ok(new_capability)
}

/// Free memory using capability
pub fn free_with_capability(cap: &MemoryCapability) -> MemoryResult<()> {
    // Check free rights
    if (cap.rights & FREE_RIGHT) == 0 {
        return Err(MemoryError::InsufficientRights);
    }
    
    // Create layout for deallocation
    let layout = Layout::from_size_align(cap.size, DEFAULT_ALIGNMENT)
        .map_err(|_| MemoryError::AlignmentError)?;
    
    // Deallocate memory
    if cap.base_ptr != 0 {
        unsafe {
            dealloc(cap.base_ptr as *mut u8, layout);
        }
    }
    
    // Unregister from region
    unregister_allocation(cap.allocation_id)?;
    
    // Free capability
    free_capability(cap.clone())?;
    
    Ok(())
}

/// Geometric resizing for dynamic arrays
pub fn geometric_resize(
    cap: &MemoryCapability,
    current_len: usize,
    element_size: usize,
) -> MemoryResult<MemoryCapability> {
    let current_capacity = cap.size / element_size;
    
    // Geometric growth: new_capacity = old_capacity * 1.5
    let new_capacity = if current_capacity == 0 {
        4  // Minimum capacity
    } else {
        current_capacity + current_capacity / 2 + 1
    };
    
    let new_size = new_capacity * element_size;
    
    // Resize with same rights
    reallocate_with_rights(cap, new_size, cap.rights)
}

/// Allocate memory for dynamic array
pub fn allocate_array<T>(
    initial_capacity: usize,
    region_id: Option<u64>,
) -> MemoryResult<MemoryCapability> {
    let element_size = std::mem::size_of::<T>();
    let size = initial_capacity * element_size;
    
    allocate_with_rights(
        size,
        READ_RIGHT | WRITE_RIGHT | FREE_RIGHT | RESIZE_RIGHT,
        region_id,
    )
}

/// Specialized allocation for bit arrays (Murphy's Sieve optimization)
pub fn allocate_bit_array(
    bit_count: usize,
    region_id: Option<u64>,
) -> MemoryResult<MemoryCapability> {
    // Calculate bytes needed (round up)
    let byte_count = (bit_count + 7) / 8;
    
    let capability = allocate_with_rights(
        byte_count,
        READ_RIGHT | WRITE_RIGHT | FREE_RIGHT,
        region_id,
    )?;
    
    // Zero initialize the bit array
    if capability.base_ptr != 0 {
        unsafe {
            ptr::write_bytes(capability.base_ptr as *mut u8, 0, byte_count);
        }
    }
    
    Ok(capability)
}

/// Memory pool for small allocations
pub struct MemoryPool {
    block_size: usize,
    free_list: Vec<usize>,
    region_id: u64,
}

impl MemoryPool {
    pub fn new(block_size: usize, region_id: u64) -> MemoryResult<Self> {
        validate_region_active(region_id)?;
        
        Ok(Self {
            block_size,
            free_list: Vec::new(),
            region_id,
        })
    }
    
    pub fn allocate(&mut self) -> MemoryResult<MemoryCapability> {
        if let Some(ptr) = self.free_list.pop() {
            // Reuse from free list
            create_capability(
                self.region_id,
                ptr,
                self.block_size,
                READ_RIGHT | WRITE_RIGHT | FREE_RIGHT,
            );
            
            // Note: We need to track these allocations properly
            // For simplicity, we'll allocate fresh memory
        }
        
        // Allocate new block
        allocate_with_rights(
            self.block_size,
            READ_RIGHT | WRITE_RIGHT | FREE_RIGHT,
            Some(self.region_id),
        )
    }
    
    pub fn free(&mut self, cap: MemoryCapability) -> MemoryResult<()> {
        if cap.size != self.block_size || cap.region_id != self.region_id {
            return Err(MemoryError::InvalidCapability);
        }
        
        // Add to free list instead of actually freeing
        self.free_list.push(cap.base_ptr);
        
        // Still need to unregister and free capability
        unregister_allocation(cap.allocation_id)?;
        free_capability(cap)?;
        
        Ok(())
    }
}

/// Initialize the allocator
pub fn init() {
    println!("Capability-based allocator initialized");
}

/// Register allocator functions with the runtime
pub fn register_functions(map: &mut std::collections::HashMap<&'static str, usize>) {
    map.insert("allocate_with_rights", allocate_with_rights as *const () as usize);
    map.insert("reallocate_with_rights", reallocate_with_rights as *const () as usize);
    map.insert("free_with_capability", free_with_capability as *const () as usize);
    map.insert("geometric_resize", geometric_resize as *const () as usize);
    map.insert("allocate_array", allocate_array::<i32> as *const () as usize);
    map.insert("allocate_bit_array", allocate_bit_array as *const () as usize);
}

// Export C-compatible functions
#[unsafe(no_mangle)]
pub unsafe extern "C" fn allocate_with_rights_c(
    size: usize,
    rights: u8,
    region_id: u64,
) -> *mut MemoryCapability {
    let region_id = if region_id == 0 { None } else { Some(region_id) };
    
    match allocate_with_rights(size, rights, region_id) {
        Ok(cap) => Box::into_raw(Box::new(cap)),
        Err(_) => std::ptr::null_mut(),
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn free_with_capability_c(cap_ptr: *mut MemoryCapability) -> i32 {
    if cap_ptr.is_null() {
        return -1;
    }
    
    let cap = unsafe { &*cap_ptr };
    match free_with_capability(cap) {
        Ok(_) => {
            unsafe { Box::from_raw(cap_ptr) };
            0
        }
        Err(err) => match err {
            MemoryError::InsufficientRights => -2,
            MemoryError::InvalidCapability => -1,
            _ => -99,
        },
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn geometric_resize_c(
    cap_ptr: *const MemoryCapability,
    current_len: usize,
    element_size: usize,
) -> *mut MemoryCapability {
    if cap_ptr.is_null() {
        return std::ptr::null_mut();
    }
    
    let cap = unsafe { &*cap_ptr };
    match geometric_resize(cap, current_len, element_size) {
        Ok(new_cap) => Box::into_raw(Box::new(new_cap)),
        Err(_) => std::ptr::null_mut(),
    }
}