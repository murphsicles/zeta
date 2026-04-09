# Capability-Based Memory Model for Zeta

## Overview
A novel memory management system that combines capability-based security with region-based allocation to provide memory safety without the complexity of Rust's borrow checker. Designed specifically for high-performance dynamic arrays like those needed for Murphy's Sieve algorithm.

## Core Principles

### 1. Capability-Based Access Control
- Every memory allocation returns a **capability** (token) that grants access rights
- Capabilities cannot be forged or duplicated without explicit permission
- Access rights: Read, Write, Free, Resize
- Capabilities are linear types - must be consumed to use

### 2. Region-Based Allocation
- Memory organized into **regions** with specific lifetimes
- Regions can be nested (parent-child relationships)
- When a region is freed, all allocations within it are automatically freed
- Enables efficient bulk deallocation

### 3. Dynamic Array Optimization
- Specialized allocation strategies for dynamic arrays
- Exponential growth with geometric resizing (1.5x factor)
- Memory pooling for small arrays
- Zero-copy resizing when possible

## Architecture

### Memory Capability Structure
```zeta
struct MemoryCapability {
    region_id: u64,      // Region identifier
    base_ptr: *mut u8,   // Base pointer
    size: usize,         // Allocation size
    rights: u8,          // Bitmask of access rights
    generation: u32,     // Generation counter for use-after-free detection
}

// Access rights bitmask
const READ_RIGHT: u8 = 0b0001;
const WRITE_RIGHT: u8 = 0b0010;
const FREE_RIGHT: u8 = 0b0100;
const RESIZE_RIGHT: u8 = 0b1000;
```

### Region Management
```zeta
struct MemoryRegion {
    id: u64,
    parent_id: Option<u64>,
    start_ptr: *mut u8,
    end_ptr: *mut u8,
    allocations: Vec<AllocationInfo>,
    is_active: bool,
}

struct AllocationInfo {
    base_ptr: *mut u8,
    size: usize,
    capability_generation: u32,
}
```

### Dynamic Array Implementation
```zeta
struct DynamicArray<T> {
    capability: MemoryCapability,  // Owns the memory
    len: usize,
    capacity: usize,
    element_size: usize,
}

impl<T> DynamicArray<T> {
    fn new(initial_capacity: usize) -> Result<DynamicArray<T>, MemoryError> {
        // Request capability with READ|WRITE|RESIZE rights
        let capability = allocate_with_rights(
            initial_capacity * size_of::<T>(),
            READ_RIGHT | WRITE_RIGHT | RESIZE_RIGHT
        )?;
        
        Ok(DynamicArray {
            capability,
            len: 0,
            capacity: initial_capacity,
            element_size: size_of::<T>(),
        })
    }
    
    fn push(&mut self, value: T) -> Result<(), MemoryError> {
        if self.len >= self.capacity {
            // Geometric resizing: new_capacity = old_capacity * 1.5
            let new_capacity = self.capacity + self.capacity / 2 + 1;
            self.resize(new_capacity)?;
        }
        
        // Calculate offset and write value
        let offset = self.len * self.element_size;
        unsafe {
            let ptr = (self.capability.base_ptr as *mut u8).add(offset) as *mut T;
            ptr.write(value);
        }
        self.len += 1;
        Ok(())
    }
    
    fn resize(&mut self, new_capacity: usize) -> Result<(), MemoryError> {
        // Use RESIZE right to reallocate
        let new_size = new_capacity * self.element_size;
        let new_capability = reallocate_with_rights(
            &self.capability,
            new_size,
            self.capability.rights
        )?;
        
        // Consume old capability, use new one
        self.capability = new_capability;
        self.capacity = new_capacity;
        Ok(())
    }
}
```

## Safety Guarantees

### 1. Spatial Safety
- Bounds checking on all array accesses
- Capability validation before each memory operation
- Generation counters prevent use-after-free

### 2. Temporal Safety
- Region lifetimes ensure timely deallocation
- Linear capabilities prevent double-free
- Automatic cleanup when regions are freed

### 3. Concurrency Safety
- Capabilities can be marked as thread-local or shared
- Atomic operations on shared capabilities
- No data races due to capability ownership

## Performance Optimizations for Murphy's Sieve

### 1. Bit Array Specialization
```zeta
struct BitArray {
    capability: MemoryCapability,
    bit_count: usize,
    
    fn set_bit(&mut self, index: usize, value: bool) -> Result<(), MemoryError> {
        let byte_index = index / 8;
        let bit_index = index % 8;
        
        // Bounds check using capability size
        if byte_index >= self.capability.size {
            return Err(MemoryError::OutOfBounds);
        }
        
        let byte_ptr = unsafe { self.capability.base_ptr.add(byte_index) };
        let byte = unsafe { byte_ptr.read() };
        
        let mask = 1 << bit_index;
        let new_byte = if value {
            byte | mask
        } else {
            byte & !mask
        };
        
        unsafe { byte_ptr.write(new_byte) };
        Ok(())
    }
}
```

### 2. Memory Pool for Small Arrays
- Pre-allocated pools for common sieve sizes (1K, 4K, 16K, 64K bits)
- Zero initialization overhead
- Cache-friendly allocation patterns

### 3. Zero-Copy Resizing
- When resizing within same region, reuse existing memory
- Only copy when crossing region boundaries
- Geometric growth minimizes copies

## API Design

### Core Functions
```zeta
// Memory management
fn allocate_with_rights(size: usize, rights: u8) -> Result<MemoryCapability, MemoryError>;
fn reallocate_with_rights(cap: &MemoryCapability, new_size: usize, rights: u8) -> Result<MemoryCapability, MemoryError>;
fn free_capability(cap: MemoryCapability) -> Result<(), MemoryError>;  // Consumes capability

// Region management
fn create_region(parent: Option<u64>) -> Result<u64, MemoryError>;
fn free_region(region_id: u64) -> Result<(), MemoryError>;

// Array operations
fn array_new<T>(capacity: usize) -> Result<DynamicArray<T>, MemoryError>;
fn array_push<T>(array: &mut DynamicArray<T>, value: T) -> Result<(), MemoryError>;
fn array_get<T>(array: &DynamicArray<T>, index: usize) -> Result<&T, MemoryError>;
```

### Murphy's Sieve Integration
```zeta
struct Sieve {
    bits: BitArray,
    limit: u64,
}

impl Sieve {
    fn new(limit: u64) -> Result<Sieve, MemoryError> {
        let bit_count = (limit as usize + 1) / 2;  // Store only odd numbers
        let bits = BitArray::new(bit_count)?;
        
        Ok(Sieve { bits, limit })
    }
    
    fn run(&mut self) -> Result<(), MemoryError> {
        let sqrt_limit = (self.limit as f64).sqrt() as u64;
        
        for i in (3..=sqrt_limit).step_by(2) {
            let i_index = (i as usize - 1) / 2;
            
            if !self.bits.get_bit(i_index)? {
                let start = i * i;
                for multiple in (start..=self.limit).step_by(i as usize * 2) {
                    let index = (multiple as usize - 1) / 2;
                    self.bits.set_bit(index, true)?;
                }
            }
        }
        Ok(())
    }
}
```

## Implementation Phases

### Phase 1: Core Capability System (2 hours)
- Implement capability structure and validation
- Basic allocate/free with rights checking
- Generation counter for use-after-free detection

### Phase 2: Region Management (1 hour)
- Region creation and destruction
- Parent-child region relationships
- Bulk deallocation

### Phase 3: Dynamic Array Optimization (1 hour)
- Geometric resizing implementation
- Bit array specialization
- Memory pooling

## Benefits Over Rust's Borrow Checker

1. **Simpler Mental Model**: Capabilities are linear resources, easier to reason about than lifetimes
2. **Runtime Flexibility**: Can allocate/deallocate based on runtime conditions
3. **Performance**: Region-based bulk operations reduce overhead
4. **Safety**: Formal capability system provides mathematical safety guarantees
5. **Composability**: Capabilities can be passed between functions without complex lifetime annotations

## Testing Strategy

1. Unit tests for capability validation
2. Integration tests for region management
3. Performance benchmarks for dynamic arrays
4. Murphy's Sieve correctness tests
5. Fuzz testing for memory safety

This memory model provides the safety guarantees needed for systems programming while maintaining the performance required for algorithms like Murphy's Sieve.