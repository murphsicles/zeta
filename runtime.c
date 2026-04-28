#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Memory allocation
void *runtime_malloc(long long size) {
    return malloc(size);
}
void runtime_free(void *ptr) {
    free(ptr);
}

// Array operations (Zeta dynamic arrays are {ptr, length, capacity} = 24 bytes)
long long array_get(long long ary, long long index) {
    long long *ptr = (long long *)ary;
    return ptr[index];  // ptr[0]=data_ptr, ptr[1]=len, ptr[2]=cap, then data starts at ptr[3]
}
long long stack_array_get(long long base, long long index) {
    return ((long long *)base)[index];
}
void array_set(long long ary, long long index, long long value) {
    ((long long *)ary)[index] = value;
}
void stack_array_set(long long base, long long index, long long value) {
    ((long long *)base)[index] = value;
}
long long array_push(long long ary, long long value) {
    // Simple realloc implementation
    long long *a = (long long *)ary;
    long long len = a[1];
    long long cap = a[2];
    if (len >= cap) {
        cap = cap == 0 ? 8 : cap * 2;
        a = realloc(a, (cap + 3) * 8);
        a[0] = (long long)&a[3];
        a[2] = cap;
    }
    a[3 + len] = value;
    a[1] = len + 1;
    return (long long)a;
}
void array_free(long long ary) {
    free((void *)ary);
}
void array_set_len(long long ary, long long len) {
    ((long long *)ary)[1] = len;
}

// Printer
void println_i64(long long value) {
    printf("%lld\n", value);
    fflush(stdout);
}
