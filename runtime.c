#include <stdio.h>
#include <time.h>
#include <stdlib.h>

// Read clock — called at start/end, not inside hot loop
long long get_time_us(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (long long)ts.tv_sec * 1000000LL + (long long)ts.tv_nsec / 1000LL;
}

// Read clock inside C function — LLVM cannot optimize this away
// Used inside the hot loop for timing checks
long long current_us(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (long long)ts.tv_sec * 1000000LL + (long long)ts.tv_nsec / 1000LL;
}

// Returns 1 if (current_us() - start_us) >= target_us, else 0
// Clock read and comparison done inside C to prevent LLVM from hoisting
long long time_is_up(long long start_us, long long target_us) {
    long long now = current_us();
    return (now - start_us) >= target_us ? 1 : 0;
}

// Print competition result line
void print_result(long long passes, long long elapsed_us) {
    double secs = (double)elapsed_us / 1000000.0;
    printf("murphsicles;%lld;%.6f;1;algorithm=base,faithful=yes,bits=1\n", passes, secs);
    fflush(stdout);
}

void println_i64(long long value) {
    printf("%lld\n", value);
    fflush(stdout);
}

// Zeta array bridge (needed for stack-allocated array access in generated code)
long long array_get(long long base, long long idx) { return ((long long*)base)[idx]; }
void array_set(long long base, long long idx, long long val) { ((long long*)base)[idx] = val; }
long long stack_array_get(long long base, long long idx) { return ((long long*)base)[idx]; }
void stack_array_set(long long base, long long idx, long long val) { ((long long*)base)[idx] = val; }
long long array_push(long long ary, long long val) { return ary; }
void array_free(long long ary) {}
void array_set_len(long long ary, long long len) {}

// Memory
void *runtime_malloc(long long size) { return malloc(size); }
void runtime_free(void *ptr) { free(ptr); }
