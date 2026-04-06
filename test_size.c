#include <stdio.h>
#include <stdint.h>
#include <stddef.h>

typedef struct {
    uint64_t magic;
    size_t capacity;
    size_t length;
    uint64_t canary;
    long long data[];
} ArrayHeader;

typedef struct {
    uint64_t magic;
    size_t capacity;
    size_t length;
    uint64_t canary;
} BlockHeader;

int main() {
    printf("sizeof(ArrayHeader) = %zu\n", sizeof(ArrayHeader));
    printf("sizeof(BlockHeader) = %zu\n", sizeof(BlockHeader));
    printf("offsetof(ArrayHeader, magic) = %zu\n", offsetof(ArrayHeader, magic));
    printf("offsetof(ArrayHeader, capacity) = %zu\n", offsetof(ArrayHeader, capacity));
    printf("offsetof(ArrayHeader, length) = %zu\n", offsetof(ArrayHeader, length));
    printf("offsetof(ArrayHeader, canary) = %zu\n", offsetof(ArrayHeader, canary));
    printf("offsetof(BlockHeader, magic) = %zu\n", offsetof(BlockHeader, magic));
    printf("offsetof(BlockHeader, capacity) = %zu\n", offsetof(BlockHeader, capacity));
    printf("offsetof(BlockHeader, length) = %zu\n", offsetof(BlockHeader, length));
    printf("offsetof(BlockHeader, canary) = %zu\n", offsetof(BlockHeader, canary));
    return 0;
}