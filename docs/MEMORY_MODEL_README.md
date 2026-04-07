# Capability-Based Memory Model for Zeta

## Overview

Zeta's novel memory model combines **capability-based security** with **region-based allocation** to provide memory safety without the complexity of Rust's borrow checker. This model is specifically optimized for high-performance dynamic arrays like those needed for algorithms such as Murphy's Sieve.

## Key Features

### 1. Capability-Based Security
- **Unforgeable tokens**: Memory capabilities cannot be forged or duplicated
- **Linear types**: Capabilities must be consumed to use memory
- **Fine-grained rights**: Read, Write, Free, Resize permissions
- **Use-after-free detection**: Generation counters prevent stale access

### 2. Region-Based Management
- **Hierarchical regions**: Parent-child relationships for scoped memory
- **Bulk deallocation**: Free a region to free all allocations within it
- **Automatic cleanup**: No manual memory management required

### 3. Performance Optimizations
- **Geometric resizing**: 1.5x growth factor for dynamic arrays
- **Bit array specialization**: Optimized for sieve algorithms
- **Memory pooling**: Efficient allocation for small objects
- **Zero-copy resizing**: When possible within same region

## Architecture

### Core Components

```zeta
// Memory capability - the fundamental access token
struct MemoryCapability {
    region_id: u64,      // Region identifier
    base_ptr: usize,     // Base pointer
    size: usize,         // Allocation size
    rights: u8,          // Access rights bitmask
    generation: u32,     // Use-after-free detection
    allocation_id: u64,  // Unique allocation ID
}

// Memory region - hierarchical memory management
struct MemoryRegion {
    id: u64,
    parent_id: Option<u64>,
    allocations: Set<u64>,  // Allocation IDs in this region
    is_active: bool,
    name: String,
}

// Dynamic array - safe, growable array
struct DynamicArray<T> {
    capability: MemoryCapability,  // Owns the memory
    len: usize,
    capacity: usize,
    element_size: usize,
}
```

### Access Rights

```zeta
const READ_RIGHT: u8 = 0b0000_0001;    // Can read from memory
const WRITE_RIGHT: u8 = 0b0000_0010;   // Can write to memory
const FREE_RIGHT: u8 = 0b0000_0100;    // Can free memory
const RESIZE_RIGHT: u8 = 0b0000_1000;  // Can resize memory
const TRANSFER_RIGHT: u8 = 0b0001_0000; // Can transfer capability
```

## Usage Examples

### Basic Dynamic Array

```zeta
import std::memory;

fn main() -> i64 {
    // Create a dynamic array
    let mut array = memory::DynamicArray::new(10);
    
    // Push values
    for i in 0..20 {
        array.push(i * i);  // Automatically resizes when needed
    }
    
    // Access values
    let sum = array.get(5) + array.get(10);
    
    // Array is automatically freed when it goes out of scope
    return sum;
}
```

### Murphy's Sieve Implementation

```zeta
import std::memory;

fn count_primes(limit: u64) -> u64 {
    // Create sieve using bit array
    let mut sieve = memory::Sieve::new(limit);
    sieve.run();
    
    // Count primes
    return sieve.count_primes();
}

fn main() -> i64 {
    let limit = 1_000_000;
    let count = count_primes(limit);
    println("Primes <= {}: {}", limit, count);  // Should be 78,498
    return 0;
}
```

### Region-Based Memory Management

```zeta
import std::memory;

fn process_data() {
    // Create a region for temporary data
    let temp_region = memory::create_region(None, "temp_data");
    
    // Allocate arrays in this region
    let mut data1 = memory::DynamicArray::new_in_region(1000, temp_region);
    let mut data2 = memory::DynamicArray::new_in_region(1000, temp_region);
    
    // Use the arrays...
    
    // Free the entire region (automatically frees data1 and data2)
    memory::free_region(temp_region);
}
```

## Safety Guarantees

### 1. Spatial Safety
- **Bounds checking**: All array accesses are bounds-checked
- **Capability validation**: Every memory operation validates capabilities
- **Alignment enforcement**: All allocations are properly aligned

### 2. Temporal Safety
- **Use-after-free prevention**: Generation counters detect stale access
- **Double-free prevention**: Capability registry tracks allocation state
- **Automatic cleanup**: Regions ensure timely deallocation

### 3. Concurrency Safety
- **Thread-local capabilities**: Marked capabilities for thread-local use
- **Atomic operations**: Safe concurrent access to shared capabilities
- **No data races**: Capability ownership prevents concurrent modification

## Performance Characteristics

### Dynamic Array Operations
| Operation | Time Complexity | Notes |
|-----------|----------------|-------|
| `push` | O(1) amortized | Geometric resizing (1.5x factor) |
| `get` | O(1) | Bounds-checked access |
| `pop` | O(1) | No reallocation on shrink |
| `reserve` | O(n) | Copies elements when resizing |

### Murphy's Sieve Performance
- **Memory usage**: 1 bit per odd number (50% savings over byte array)
- **Cache efficiency**: Sequential access patterns
- **Algorithmic optimization**: Wheel factorization ready for integration

### Comparison with Rust's Borrow Checker

| Aspect | Zeta Capability Model | Rust Borrow Checker |
|--------|----------------------|---------------------|
| Learning curve | Moderate (linear types) | Steep (lifetimes) |
| Runtime overhead | Minimal (bounds checks) | None (compile-time) |
| Flexibility | High (runtime decisions) | Limited (compile-time) |
| Concurrency safety | Built-in (capability rights) | Requires explicit types |
| Memory safety | Guaranteed (capability system) | Guaranteed (type system) |

## Implementation Status

### Completed Components
- [x] Capability system with rights management
- [x] Region-based memory management
- [x] Dynamic array implementation
- [x] Bit array specialization
- [x] Murphy's Sieve algorithm
- [x] Memory error handling

### Integration Points
1. **Compiler integration**: Type system support for capabilities
2. **Runtime integration**: Memory allocation hooks
3. **Standard library**: `std::memory` module
4. **Language syntax**: Built-in array types using capability model

### Future Enhancements
1. **Compiler optimizations**: Eliminate bounds checks when provably safe
2. **Parallel collections**: Concurrent data structures using capabilities
3. **Garbage collection**: Optional GC for regions
4. **Formal verification**: Prove memory safety properties

## Testing

### Unit Tests
```bash
# Run memory model tests
cargo test --package zetac --lib memory

# Run specific test suite
cargo test --package zetac --lib memory::array
```

### Integration Tests
```zeta
// tests/memory_model_test.z
import std::memory;

fn test_dynamic_array() {
    let mut array = memory::DynamicArray::new(4);
    array.push(1);
    array.push(2);
    assert(array.len() == 2, "Array length incorrect");
}

fn main() -> i64 {
    test_dynamic_array();
    println("All tests passed!");
    return 0;
}
```

### Performance Benchmarks
```bash
# Benchmark Murphy's Sieve
cargo bench --bench murphy_sieve

# Compare with traditional malloc/free
cargo bench --bench memory_alloc
```

## API Reference

### Core Functions

#### Memory Allocation
```zeta
// Allocate memory with specific rights
fn allocate_with_rights(size: usize, rights: u8, region_id: Option<u64>) 
    -> Result<MemoryCapability, MemoryError>

// Free memory using capability
fn free_with_capability(cap: &MemoryCapability) -> Result<(), MemoryError>

// Resize memory allocation
fn reallocate_with_rights(cap: &MemoryCapability, new_size: usize, rights: u8)
    -> Result<MemoryCapability, MemoryError>
```

#### Region Management
```zeta
// Create a new memory region
fn create_region(parent_id: Option<u64>, name: &str) -> Result<u64, MemoryError>

// Free a region and all allocations within it
fn free_region(region_id: u64) -> Result<Vec<u64>, MemoryError>

// Get the root region ID
fn root_region_id() -> u64
```

#### Dynamic Arrays
```zeta
// Create a new dynamic array
fn DynamicArray::new(initial_capacity: usize) -> Result<DynamicArray<T>, MemoryError>

// Push a value onto the array
fn push(&mut self, value: T) -> Result<(), MemoryError>

// Get a reference to an element
fn get(&self, index: usize) -> Result<&T, MemoryError>

// Get the array length
fn len(&self) -> usize
```

#### Bit Arrays (Murphy's Sieve)
```zeta
// Create a new bit array
fn BitArray::new(bit_count: usize) -> Result<BitArray, MemoryError>

// Set a bit to a value
fn set_bit(&mut self, index: usize, value: bool) -> Result<(), MemoryError>

// Get a bit value
fn get_bit(&self, index: usize) -> Result<bool, MemoryError>
```

## Contributing

### Development Guidelines
1. **Safety first**: All memory operations must validate capabilities
2. **Performance matters**: Optimize for cache locality and minimal copying
3. **Test thoroughly**: Include unit tests for all memory operations
4. **Document completely**: Explain safety guarantees and performance characteristics

### Code Organization
```
src/memory/
├── mod.rs          # Module exports
├── capability.rs   # Capability system
├── region.rs       # Region management
├── allocator.rs    # Memory allocator
├── array.rs        # Dynamic arrays
├── error.rs        # Error types
└── tests/          # Test files
```

### Adding New Features
1. Define capability rights for new operations
2. Implement region-aware allocation
3. Add comprehensive tests
4. Update documentation
5. Benchmark performance impact

## License

This memory model is part of the Zeta programming language and is licensed under the MIT License. See the LICENSE file for details.

## Acknowledgments

- Inspired by capability-based security research
- Based on region-based memory management literature
- Optimized for real-world algorithms like Murphy's Sieve
- Designed for the Zeta programming language ecosystem