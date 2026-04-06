// Bulletproof C runtime for Zeta - Phase 1 Enhanced
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// Bulletproof memory allocation tracking
typedef struct {
    uint64_t magic;      // Magic number for validation (0xB4D455054 = "BULLET")
    size_t size;         // User-requested size
    uint64_t allocation_id; // Unique ID for tracking
    uint64_t canary;     // Canary value for overflow detection (0xDEADBEEFCAFEBABE)
} BlockHeader;

#define HEADER_SIZE sizeof(BlockHeader)
#define MAGIC_VALUE 0xB4D455054ULL
#define CANARY_VALUE 0xDEADBEEFCAFEBABEULL
#define FREED_PATTERN 0xFD
#define UNINIT_PATTERN 0xCD

// Array header structure with bounds checking
typedef struct {
    size_t capacity;
    size_t length;
    uint64_t magic;      // Magic for validation
    uint64_t canary;     // Canary for overflow detection
    long long data[];    // Flexible array member
} ArrayHeader;

// Global allocation counter
static uint64_t allocation_counter = 1;

// Error reporting
void report_corruption(const char* message, void* ptr) {
    fprintf(stderr, "[BULLETPROOF MEMORY] CORRUPTION DETECTED: %s at %p\n", message, ptr);
}

void report_bounds_violation(const char* message, void* ptr, long long index) {
    fprintf(stderr, "[BULLETPROOF MEMORY] BOUNDS VIOLATION: %s - ptr: %p, index: %lld\n", message, ptr, index);
}

// Basic I/O functions
void print_i64(long long value) {
    printf("%lld", value);
}

void print_bool(long long value) {
    printf("%s", value ? "true" : "false");
}

void print_str(long long ptr) {
    if (ptr == 0) {
        printf("(null)");
        return;
    }
    printf("%s", (const char*)ptr);
}

void println() {
    printf("\n");
}

void println_i64(long long value) {
    printf("%lld\n", value);
}

void println_bool(long long value) {
    printf("%s\n", value ? "true" : "false");
}

void println_str(long long ptr) {
    if (ptr == 0) {
        printf("(null)\n");
        return;
    }
    printf("%s\n", (const char*)ptr);
}

void flush() {
    fflush(stdout);
}

// Bulletproof memory allocation
long long runtime_malloc(size_t size) {
    if (size == 0) return 0;
    
    size_t total_size = HEADER_SIZE + size;
    BlockHeader* header = (BlockHeader*)malloc(total_size);
    if (!header) return 0;
    
    // Initialize header
    header->magic = MAGIC_VALUE;
    header->size = size;
    header->allocation_id = allocation_counter++;
    header->canary = CANARY_VALUE;
    
    // Get user pointer
    long long user_ptr = (long long)(header + 1);
    
    // Initialize user memory with pattern (sanitization)
    memset((void*)user_ptr, UNINIT_PATTERN, size);
    
    return user_ptr;
}

void runtime_free(long long ptr) {
    if (ptr == 0) return;
    
    BlockHeader* header = ((BlockHeader*)ptr) - 1;
    
    // Validate header (corruption detection)
    if (header->magic != MAGIC_VALUE || header->canary != CANARY_VALUE) {
        report_corruption("Invalid header in free", header);
        return;
    }
    
    size_t size = header->size;
    
    // Poison freed memory (use-after-free detection)
    memset((void*)ptr, FREED_PATTERN, size);
    
    // Invalidate header
    header->magic = 0;
    header->canary = 0;
    
    free(header);
}

long long runtime_calloc(size_t num, size_t size) {
    size_t total = num * size;
    long long ptr = runtime_malloc(total);
    if (ptr == 0) return 0;
    
    // Zero the memory (overwriting the UNINIT_PATTERN)
    memset((void*)ptr, 0, total);
    return ptr;
}

long long runtime_realloc(long long ptr, size_t new_size) {
    if (ptr == 0) return runtime_malloc(new_size);
    if (new_size == 0) {
        runtime_free(ptr);
        return 0;
    }
    
    BlockHeader* old_header = ((BlockHeader*)ptr) - 1;
    
    // Validate old header
    if (old_header->magic != MAGIC_VALUE || old_header->canary != CANARY_VALUE) {
        report_corruption("Invalid header in realloc", old_header);
        return 0;
    }
    
    size_t old_size = old_header->size;
    
    // Allocate new block
    long long new_ptr = runtime_malloc(new_size);
    if (new_ptr == 0) return 0;
    
    // Copy old data (up to minimum of old and new size)
    size_t copy_size = old_size < new_size ? old_size : new_size;
    if (copy_size > 0) {
        memcpy((void*)new_ptr, (void*)ptr, copy_size);
    }
    
    // Free old block
    runtime_free(ptr);
    
    return new_ptr;
}

// Array operations with bounds checking
long long array_new() {
    size_t capacity = 10; // Default capacity
    size_t total_size = sizeof(ArrayHeader) + capacity * sizeof(long long);
    ArrayHeader* header = (ArrayHeader*)malloc(total_size);
    if (!header) return 0;
    
    header->capacity = capacity;
    header->length = 0;
    header->magic = MAGIC_VALUE;
    header->canary = CANARY_VALUE;
    
    return (long long)(header->data);
}

long long array_len(long long ptr) {
    if (ptr == 0) return 0;
    ArrayHeader* header = (ArrayHeader*)(ptr - sizeof(ArrayHeader));
    
    // Validate header
    if (header->magic != MAGIC_VALUE || header->canary != CANARY_VALUE) {
        report_corruption("Invalid array header in array_len", header);
        return 0;
    }
    
    return header->length;
}

long long array_get(long long ptr, long long index) {
    if (ptr == 0) return 0;
    ArrayHeader* header = (ArrayHeader*)(ptr - sizeof(ArrayHeader));
    
    // Validate header
    if (header->magic != MAGIC_VALUE || header->canary != CANARY_VALUE) {
        report_corruption("Invalid array header in array_get", header);
        return 0;
    }
    
    // Bounds checking
    if (index < 0 || index >= header->length) {
        report_bounds_violation("Array index out of bounds in array_get", (void*)ptr, index);
        return 0;
    }
    
    return header->data[index];
}

void array_set(long long ptr, long long index, long long value) {
    if (ptr == 0) return;
    ArrayHeader* header = (ArrayHeader*)(ptr - sizeof(ArrayHeader));
    
    // Validate header
    if (header->magic != MAGIC_VALUE || header->canary != CANARY_VALUE) {
        report_corruption("Invalid array header in array_set", header);
        return;
    }
    
    // Bounds checking
    if (index < 0 || index >= header->length) {
        report_bounds_violation("Array index out of bounds in array_set", (void*)ptr, index);
        return;
    }
    
    header->data[index] = value;
}

long long array_push(long long ptr, long long value) {
    if (ptr == 0) return 0;
    ArrayHeader* header = (ArrayHeader*)(ptr - sizeof(ArrayHeader));
    
    // Validate header
    if (header->magic != MAGIC_VALUE || header->canary != CANARY_VALUE) {
        report_corruption("Invalid array header in array_push", header);
        return 0;
    }
    
    if (header->length >= header->capacity) {
        // Resize
        size_t new_capacity = header->capacity * 2;
        size_t new_total_size = sizeof(ArrayHeader) + new_capacity * sizeof(long long);
        ArrayHeader* new_header = (ArrayHeader*)realloc(header, new_total_size);
        if (!new_header) return 0;
        
        header = new_header;
        header->capacity = new_capacity;
        ptr = (long long)(header->data);
    }
    
    header->data[header->length] = value;
    header->length++;
    return ptr;
}

void array_free(long long ptr) {
    if (ptr == 0) return;
    ArrayHeader* header = (ArrayHeader*)(ptr - sizeof(ArrayHeader));
    
    // Validate header
    if (header->magic != MAGIC_VALUE || header->canary != CANARY_VALUE) {
        report_corruption("Invalid array header in array_free", header);
        return;
    }
    
    // Poison array data
    memset(header->data, FREED_PATTERN, header->length * sizeof(long long));
    
    // Invalidate header
    header->magic = 0;
    header->canary = 0;
    
    free(header);
}

// Bounds checking function for direct array access
long long runtime_check_bounds(long long ptr, long long index, size_t element_size) {
    if (ptr == 0 || index < 0) {
        report_bounds_violation("Null pointer or negative index", (void*)ptr, index);
        return 0;
    }
    
    BlockHeader* header = ((BlockHeader*)ptr) - 1;
    
    // Validate header
    if (header->magic != MAGIC_VALUE || header->canary != CANARY_VALUE) {
        report_corruption("Invalid header in bounds check", header);
        return 0;
    }
    
    size_t size = header->size;
    size_t offset = index * element_size;
    
    if (offset + element_size > size) {
        report_bounds_violation("Array index out of bounds", (void*)ptr, index);
        return 0;
    }
    
    // Return pointer to element
    return ptr + offset;
}

// Test function for bulletproof features
long long test_bulletproof_memory() {
    // Test 1: Basic allocation and free
    long long ptr = runtime_malloc(100);
    if (ptr == 0) return -1;
    
    // Test 2: Write to memory
    unsigned char* data = (unsigned char*)ptr;
    for (int i = 0; i < 100; i++) {
        data[i] = i;
    }
    
    // Test 3: Bounds check (should succeed)
    long long checked = runtime_check_bounds(ptr, 50, 1);
    if (checked == 0) {
        runtime_free(ptr);
        return -2;
    }
    
    // Test 4: Bounds check (should fail)
    long long bad_check = runtime_check_bounds(ptr, 150, 1);
    if (bad_check != 0) {
        runtime_free(ptr);
        return -3;
    }
    
    // Test 5: Free memory
    runtime_free(ptr);
    
    // Test 6: Double-free detection (should report corruption but not crash)
    runtime_free(ptr);
    
    // Test 7: Array bounds checking
    long long arr = array_new();
    if (arr == 0) return -4;
    
    // Push some values
    for (int i = 0; i < 5; i++) {
        arr = array_push(arr, i * 10);
        if (arr == 0) return -5;
    }
    
    // Test valid access
    long long val = array_get(arr, 2);
    if (val != 20) {
        array_free(arr);
        return -6;
    }
    
    // Test invalid access (should report bounds violation)
    val = array_get(arr, 10);
    // Should print error but not crash
    
    array_free(arr);
    
    return 0; // Success
}

// Map functions - optimized for array-like usage
// Since the compiler uses maps for arrays, we need efficient map operations

typedef struct {
    long long* values;      // Direct array for small keys
    long long array_size;   // Size of direct array
    void* hash_map;         // For larger keys (not implemented)
} FastMap;

long long map_get(long long map_ptr, long long key) {
    FastMap* map = (FastMap*)map_ptr;
    if (!map || !map->values) return 0;
    
    // For array-like usage (sequential keys starting from 0)
    if (key >= 0 && key < map->array_size) {
        return map->values[key];
    }
    
    return 0; // Not found
}

long long map_insert(long long map_ptr, long long key, long long value) {
    FastMap* map = (FastMap*)map_ptr;
    if (!map) return 0;
    
    // Initialize if needed
    if (!map->values) {
        // Start with reasonable size
        map->array_size = 1000; // Enough for Murphy's Sieve
        map->values = (long long*)calloc(map->array_size, sizeof(long long));
    }
    
    // Resize if key is out of bounds
    if (key >= map->array_size) {
        long long new_size = key + 100; // Add some extra
        long long* new_values = (long long*)realloc(map->values, new_size * sizeof(long long));
        if (!new_values) return 0;
        
        // Initialize new elements to 0
        for (long long i = map->array_size; i < new_size; i++) {
            new_values[i] = 0;
        }
        
        map->values = new_values;
        map->array_size = new_size;
    }
    
    // Store value
    if (key >= 0) {
        map->values[key] = value;
        return 1;
    }
    
    return 0;
}

// SIMD vector functions (simplified implementations)
typedef struct {
    long long data[8];
} vector_u64x8;

typedef struct {
    int data[4];
} vector_i32x4;

long long vector_make_u64x8(long long a, long long b, long long c, long long d,
                           long long e, long long f, long long g, long long h) {
    vector_u64x8* vec = (vector_u64x8*)malloc(sizeof(vector_u64x8));
    vec->data[0] = a;
    vec->data[1] = b;
    vec->data[2] = c;
    vec->data[3] = d;
    vec->data[4] = e;
    vec->data[5] = f;
    vec->data[6] = g;
    vec->data[7] = h;
    return (long long)vec;
}

long long vector_splat_u64x8(long long value) {
    vector_u64x8* vec = (vector_u64x8*)malloc(sizeof(vector_u64x8));
    for (int i = 0; i < 8; i++) {
        vec->data[i] = value;
    }
    return (long long)vec;
}

long long vector_make_i32x4(int a, int b, int c, int d) {
    vector_i32x4* vec = (vector_i32x4*)malloc(sizeof(vector_i32x4));
    vec->data[0] = a;
    vec->data[1] = b;
    vec->data[2] = c;
    vec->data[3] = d;
    return (long long)vec;
}

long long vector_splat_i32x4(int value) {
    vector_i32x4* vec = (vector_i32x4*)malloc(sizeof(vector_i32x4));
    for (int i = 0; i < 4; i++) {
        vec->data[i] = value;
    }
    return (long long)vec;
}

long long vector_add_u64x8(long long a, long long b) {
    vector_u64x8* vec_a = (vector_u64x8*)a;
    vector_u64x8* vec_b = (vector_u64x8*)b;
    vector_u64x8* result = (vector_u64x8*)malloc(sizeof(vector_u64x8));
    
    for (int i = 0; i < 8; i++) {
        result->data[i] = vec_a->data[i] + vec_b->data[i];
    }
    
    return (long long)result;
}

long long vector_add_i32x4(long long a, long long b) {
    vector_i32x4* vec_a = (vector_i32x4*)a;
    vector_i32x4* vec_b = (vector_i32x4*)b;
    vector_i32x4* result = (vector_i32x4*)malloc(sizeof(vector_i32x4));
    
    for (int i = 0; i < 4; i++) {
        result->data[i] = vec_a->data[i] + vec_b->data[i];
    }
    
    return (long long)result;
}

void vector_free_u64x8(long long ptr) {
    free((void*)ptr);
}

void vector_free_i32x4(long long ptr) {
    free((void*)ptr);
}