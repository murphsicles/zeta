#include <stdio.h>
#include <stdint.h>

// Simulating the DynamicArray structure from host.rs
typedef struct {
    int64_t capacity;
    int64_t length;
    int64_t* data;
} DynamicArray;

// Simulating the ArrayHeader structure from array.rs
typedef struct {
    size_t len;
    size_t capacity;
    // data follows immediately after
} ArrayHeader;

void test_layouts() {
    printf("Testing memory layouts:\n");
    printf("Sizeof DynamicArray: %zu\n", sizeof(DynamicArray));
    printf("Sizeof ArrayHeader: %zu\n", sizeof(ArrayHeader));
    printf("Offset of data in DynamicArray: %zu\n", offsetof(DynamicArray, data));
    printf("\n");
    
    // Simulate what array_new returns (pointer to data after ArrayHeader)
    ArrayHeader* header = (ArrayHeader*)malloc(sizeof(ArrayHeader) + 5 * sizeof(int64_t));
    header->len = 5;
    header->capacity = 5;
    int64_t* data_ptr = (int64_t*)(header + 1); // Pointer after header
    
    printf("ArrayHeader at: %p\n", header);
    printf("Data pointer (after header): %p\n", data_ptr);
    printf("Difference: %zu bytes\n", (size_t)data_ptr - (size_t)header);
    
    // Now simulate what compiler expects (DynamicArray pointer)
    DynamicArray* dyn_array = (DynamicArray*)malloc(sizeof(DynamicArray));
    dyn_array->capacity = 5;
    dyn_array->length = 5;
    dyn_array->data = (int64_t*)malloc(5 * sizeof(int64_t));
    
    printf("\nDynamicArray at: %p\n", dyn_array);
    printf("DynamicArray.data at: %p\n", dyn_array->data);
    printf("Difference: %zu bytes\n", (size_t)dyn_array->data - (size_t)dyn_array);
    
    free(header);
    free(dyn_array->data);
    free(dyn_array);
}

int main() {
    test_layouts();
    return 0;
}