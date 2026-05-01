#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>

// === PrimeZeta competition runtime ===
long long get_time_us(void) { struct timespec ts; clock_gettime(CLOCK_MONOTONIC, &ts); return (long long)ts.tv_sec * 1000000LL + (long long)ts.tv_nsec / 1000LL; }
long long current_us(void) { struct timespec ts; clock_gettime(CLOCK_MONOTONIC, &ts); return (long long)ts.tv_sec * 1000000LL + (long long)ts.tv_nsec / 1000LL; }
long long time_is_up(long long start_us, long long target_us) { long long now = current_us(); return (now - start_us) >= target_us ? 1 : 0; }
void print_result(long long passes, long long elapsed_us) { printf("murphsicles;%lld;%.6f;1;algorithm=base,faithful=yes,bits=1\n", passes, elapsed_us/1000000.0); fflush(stdout); }
void println_i64(long long value) { printf("%lld\n", value); fflush(stdout); }

// Memory
void *runtime_malloc(long long size) { return malloc(size); }
void runtime_free(void *ptr) { free(ptr); }
void *runtime_calloc(long long count, long long size) { return calloc(count, size); }
void *runtime_realloc(void *ptr, long long size) { return realloc(ptr, size); }
void std_free(void *ptr) { free(ptr); }

// Array bridge
long long array_get(long long base, long long idx) { return ((long long*)base)[idx]; }
void array_set(long long base, long long idx, long long val) { ((long long*)base)[idx] = val; }
long long stack_array_get(long long base, long long idx) { return ((long long*)base)[idx]; }
void stack_array_set(long long base, long long idx, long long val) { ((long long*)base)[idx] = val; }
long long array_push(long long ary, long long val) { return ary; }
void array_free(long long ary) {}
void array_set_len(long long ary, long long len) {}
void *array_new(long long cap) { return calloc(cap, 8); }
long long array_len(long long ary) { if (!ary) return 0; return *((long long*)ary - 1); }

// Print/IO
void print_i64(long long val) { printf("%lld", val); fflush(stdout); }
void print_bool(long long val) { printf(val ? "true" : "false"); fflush(stdout); }
void print_str(long long val) { printf("%s", (char*)val); fflush(stdout); }
void println() { printf("\n"); fflush(stdout); }
void flush() { fflush(stdout); }
long long test_return_i64(long long val) { return val; }

// Zeta reference operator (identity)
long long amp(long long val) { return val; }

// Result/Option helpers
long long host_result_is_ok(long long val) { return 1; }
long long host_result_get_data(long long val) { return val; }
long long host_result_make_ok(long long val) { return val; }
long long host_result_make_err(long long val) { return val; }
void host_result_free(long long val) {}
long long Ok(long long val) { return val; }
long long Err(long long val) { return val; }
long long option_make_some(long long val) { return val; }
long long option_make_none() { return 0; }
long long option_is_some(long long val) { return val != 0; }
long long option_get_data(long long val) { return val; }
void option_free(long long val) {}
long long is_null_i64(long long val) { return val == 0; }
long long is_null_bool(long long val) { return val == 0; }
long long clone_i64(long long val) { return val; }
long long clone_bool(long long val) { return val; }
long long unwrap_i64(long long val) { return val; }
long long unwrap_or_i64(long long val) { return val; }

// Map/Dict
void *map_new() { return calloc(1, 64); }
void *MapInner__new() { return calloc(1, 64); }
long long map_get(void *map, long long key) { return 0; }
void map_insert(void *map, long long key, long long val) {}
void map_free(void *map) { free(map); }
long long insert_i64(void *map, long long key, long long val) { return 0; }

// String
long long host_str_len(long long s) { return s ? strlen((char*)s) : 0; }
long long host_str_concat(long long a, long long b) { return a; }
long long host_str_to_uppercase(long long s) { return s; }
long long host_str_to_lowercase(long long s) { return s; }
long long host_str_trim(long long s) { return s; }
long long host_str_starts_with(long long s) { return 0; }
long long host_str_ends_with(long long s) { return 0; }
long long host_str_contains(long long s) { return 0; }
long long host_str_replace(long long s) { return s; }
long long to_string_str(long long ptr) { return ptr; }
long long str_join_i64(long long sep, long long strs) { return 0; }
void *String__new() { return calloc(1, 8); }

// AstNode constructors (:: → __ for C ABI)
long long AstNode__Lit(long long val) { return val; }
long long AstNode__FuncDef(long long val) { return val; }
long long AstNode__Call(long long val) { return val; }
long long AstNode__Var(long long val) { return val; }
long long AstNode__BinaryOp(long long val) { return val; }
long long AstNode__If(long long val) { return val; }
long long AstNode__Return(long long val) { return val; }
long long AstNode__Let(long long val) { return val; }
long long AstNode__Match(long long val) { return val; }
long long AstNode__StructDef(long long val) { return val; }
long long AstNode__EnumDef(long long val) { return val; }
long long AstNode__ImplBlock(long long val) { return val; }
long long AstNode__Use(long long val) { return val; }
long long AstNode__ModDef(long long val) { return val; }
long long AstNode__Program(long long val) { return val; }
long long AstNode__Unit(long long val) { return val; }

// Box
void *Box__new(long long val) { void *p = malloc(8); *(long long*)p = val; return p; }
long long Box__into_raw(void *p) { return (long long)p; }
void *Box__from_raw(long long p) { return (void*)p; }

// Time
long long host_datetime_now() { return time(0); }
long long SystemTime__now() { return time(0); }
long long duration_since_i64(long long start) { return 0; }
long long as_millis_i64(long long val) { return val; }

// Misc
long long query_i64(long long val) { return 0; }
long long new_inst_i64(long long val) { return val; }
long long get_i64(long long val) { return val; }
long long cloned_i64(long long val) { return val; }
long long parse_type_i64(long long val) { return 0; }
long long peek_i64(long long val) { return val; }
void *ptr__null_mut() { return 0; }
void *linked_list_new() { return 0; }
long long push_back_i64(void *list, long long val) { return 0; }
long long pop_front_i64(void *list) { return 0; }
long long iter_i64(void *list) { return 0; }
long long find_i64(void *list, long long val) { return 0; }
long long split_i64(void *list) { return 0; }
long long or_i64(long long a, long long b) { return a | b; }
long long and_i64(long long a, long long b) { return a & b; }
long long scheduler__init_runtime() { return 0; }
long long Error__new(long long val) { return val; }

// SIMD
void vector_free_u64x8(long long v) {}
void vector_free_i32x4(long long v) {}
void vector_free_u64x4(long long v) {}

// AVX
long long __builtin_v4i64_store(long long ptr, long long v) { return 0; }
long long __builtin_v4i64_andnot(long long a, long long b) { return 0; }


void llvm_memset_i64(void *ptr, long long val, long long size) { memset(ptr, (int)val, size); }
