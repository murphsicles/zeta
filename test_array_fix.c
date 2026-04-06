#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

// Define the DynamicArray structure matching our Rust code
typedef struct {
    int64_t capacity;
    int64_t length;
    int64_t* data;
} DynamicArray;

// Declare the Rust functions
extern int64_t array_new(int64_t capacity);
extern int64_t array_len(int64_t ptr);
extern int64_t array_get(int64_t ptr, int64_t index);
extern void array_set(int64_t ptr, int64_t index, int64_t value);
extern int64_t array_push(int64_t ptr, int64_t value);
extern void array_free(int64_t ptr);

void test_dynamic_array() {
    printf("Testing DynamicArray API compatibility...\n");
    
    // Create a new array with capacity 5
    int64_t arr_ptr = array_new(5);
    printf("array_new(5) returned: %lld\n", arr_ptr);
    
    if (arr_ptr == 0) {
        printf("ERROR: array_new failed!\n");
        return;
    }
    
    // Cast to DynamicArray to inspect structure
    DynamicArray* arr = (DynamicArray*)arr_ptr;
    printf("DynamicArray at: %p\n", arr);
    printf("  capacity: %lld\n", arr->capacity);
    printf("  length: %lld\n", arr->length);
    printf("  data: %p\n", arr->data);
    
    // Test array_len
    int64_t len = array_len(arr_ptr);
    printf("array_len returned: %lld\n", len);
    
    // Test array_push
    for (int i = 1; i <= 3; i++) {
        int64_t result = array_push(arr_ptr, i * 10);
        printf("array_push(%d) returned: %lld\n", i*10, result);
    }
    
    // Check length after pushes
    len = array_len(arr_ptr);
    printf("array_len after pushes: %lld\n", len);
    
    // Test array_get
    for (int i = 0; i < 3; i++) {
        int64_t value = array_get(arr_ptr, i);
        printf("array_get(%d) returned: %lld\n", i, value);
    }
    
    // Test array_set
    array_set(arr_ptr, 1, 999);
    printf("array_set(1, 999) called\n");
    
    // Verify the change
    int64_t value = array_get(arr_ptr, 1);
    printf("array_get(1) after set: %lld\n", value);
    
    // Test bounds checking
    printf("\nTesting bounds checking:\n");
    int64_t out_of_bounds = array_get(arr_ptr, 100);
    printf("array_get(100) (out of bounds) returned: %lld\n", out_of_bounds);
    
    // Free the array
    array_free(arr_ptr);
    printf("array_free called\n");
}

int main() {
    test_dynamic_array();
    return 0;
}