/* Zenith runtime for Zeta — low-level C functions
 * Compile: gcc -O2 -fPIC -c zenith_runtime.c -o zenith_runtime_c.o
 * Link: ... zenith_runtime_c.o -lzstd
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/syscall.h>

/* ───── CRC32C table (must be before the function that uses it) ───── */

static const uint32_t CRC32C_TABLE[256] = {
    0x00000000,0xF26B8303,0xE13B70F7,0x1350F3F4,0xC79A971F,0x35F1141C,
    0x26A1E7E8,0xD4CA64EB,0x8AD958CF,0x78B2DBCC,0x6BE22838,0x9989AB3B,
    0x4D43CFD0,0xBF284CD3,0xAC78BF27,0x5E133C24,0x105EC76F,0xE235446C,
    0xF165B798,0x030E349B,0xD7C45070,0x25AFD373,0x36FF2087,0xC494A384,
    0x9A879FA0,0x68EC1CA3,0x7BBCEF57,0x89D76C54,0x5D1D08BF,0xAF768BBC,
    0xBC267848,0x4E4DFB4B,0x20BD8EDE,0xD2D60DDD,0xC186FE29,0x33ED7D2A,
    0xE72719C1,0x154C9AC2,0x061C6936,0xF477EA35,0xAA64D611,0x580F5512,
    0x4B5FA6E6,0xB93425E5,0x6DFE410E,0x9F95C20D,0x8CC531F9,0x7EAEB2FA,
    0x30E349B1,0xC288CAB2,0xD1D83946,0x23B3BA45,0xF779DEAE,0x05125DAD,
    0x1642AE59,0xE4292D5A,0xBA3A117E,0x4851927D,0x5B016189,0xA96AE28A,
    0x7DA08661,0x8FCB0562,0x9C9BF696,0x6EF07595,0x417B1DBC,0xB3109EBF,
    0xA0406D4B,0x522BEE48,0x86E18AA3,0x748A09A0,0x67DAFA54,0x95B17957,
    0xCBA24573,0x39C9C670,0x2A993584,0xD8F2B687,0x0C38D26C,0xFE53516F,
    0xED03A29B,0x1F682198,0x5125DAD3,0xA34E59D0,0xB01EAA24,0x42752927,
    0x96BF4DCC,0x64D4CECF,0x77843D3B,0x85EFBE38,0xDBFC821C,0x2997011F,
    0x3AC7F2EB,0xC8AC71E8,0x1C661503,0xEE0D9600,0xFD5D65F4,0x0F36E6F7,
    0x61C69362,0x93AD1061,0x80FDE395,0x72966096,0xA65C047D,0x5437877E,
    0x4767748A,0x470CF789,0x191FCBAD,0xEB7448AE,0xF824BB5A,0x0A4F3859,
    0xDE855CB2,0x2CEEDFB1,0x3FBE2C45,0xCDD5AF46,0x8398540D,0x71F3D70E,
    0x62A324FA,0x90C8A7F9,0x4402C312,0xB6694011,0xA539B3E5,0x575230E6,
    0x09410CC2,0xFB2A8FC1,0xE87A7C35,0x1A11FF36,0xCEDB9BDD,0x3CB018DE,
    0x2FE0EB2A,0xDD8B6829,0x82F63B78,0x709DB87B,0x63CD4B8F,0x91A6C88C,
    0x456CAC67,0xB7072F64,0xA457DC90,0x563C5F93,0x082F63B7,0xFA44E0B4,
    0xE9141340,0x1B7F9043,0xCFB5F4A8,0x3DDE77AB,0x2E8E845F,0xDCE5075C,
    0x92A8FC17,0x60C37F14,0x73938CE0,0x81F80FE3,0x55326B08,0xA759E80B,
    0xB4091BFF,0x466298FC,0x1871A4D8,0xEA1A27DB,0xF94AD42F,0x0B21572C,
    0xDFEB33C7,0x2D80B0C4,0x3ED04330,0xCCBBC033,0xA24BB5A6,0x502036A5,
    0x4370C551,0xB11B4652,0x65D122B9,0x97BAA1BA,0x84EA524E,0x7681D14D,
    0x2892ED69,0xDAF96E6A,0xC9A99D9E,0x3BC21E9D,0xEF087A76,0x1D63F975,
    0x0E330A81,0xFC588982,0xB21572C9,0x407EF1CA,0x532E023E,0xA145813D,
    0x758FE5D6,0x87E466D5,0x94B49521,0x66DF1622,0x38CC2A06,0xCAA7A905,
    0xD9F75AF1,0x2B9CD9F2,0xFF56BD19,0x0D3D3E1A,0x1E6DCDEE,0xEC064EED,
    0xC38D26C4,0x31E6A5C7,0x22B65633,0xD0DDD530,0x0417B1DB,0xF67C32D8,
    0xE52CC12C,0x1747422F,0x49547E0B,0xBB3FFD08,0xA86F0EFC,0x5A048DFF,
    0x8ECEE914,0x7CA56A17,0x6FF599E3,0x9D9E1AE0,0xD3D3E1AB,0x21B862A8,
    0x32E8915C,0xC083125F,0x144976B4,0xE622F5B7,0xF5720643,0x07198540,
    0x590AB964,0xAB613A67,0xB831C993,0x4A5A4A90,0x9E902E7B,0x6CFBAD78,
    0x7FAB5E8C,0x8DC0DD8F,0xE330A81A,0x115B2B19,0x020BD8ED,0xF0605BEE,
    0x24AA3F05,0xD6C1BC06,0xC5914FF2,0x37FACCF1,0x69E9F0D5,0x9B8273D6,
    0x88D28022,0x7AB90321,0xAE7367CA,0x5C18E4C9,0x4F48173D,0xBD23943E,
    0xF36E6F75,0x0105EC76,0x12551F82,0xE03E9C81,0x34F4F86A,0xC69F7B69,
    0xD5CF889D,0x27A40B9E,0x79B737BA,0x8BDCB4B9,0x988C474D,0x6AE7C44E,
    0xBE2DA0A5,0x4C4623A6,0x5F16D052,0xAD7D5351,
};

/* ───── Existing Zeta runtime functions ───── */

void* runtime_malloc(long size) { return malloc((size_t)size); }
void runtime_free(void* ptr) { free(ptr); }

void println_i64(long val) { printf("%ld\n", val); }
void print_i64(long val) { printf("%ld", val); }
void print_str(const char* s) { fputs(s, stdout); }
long host_str_len(const char* s) { return (long)strlen(s); }

long get_time_us(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000000L + ts.tv_nsec / 1000L;
}

/* ───── POSIX File I/O ───── */

long posix_open_str(const char* path, long flags, long mode) {
    return (long)open(path, (int)flags, (mode_t)mode);
}
long posix_close(long fd) { return (long)close((int)fd); }
long posix_write(long fd, long buf, long count) {
    return (long)write((int)fd, (void*)buf, (size_t)count);
}
long posix_read(long fd, long buf, long count) {
    return (long)read((int)fd, (void*)buf, (size_t)count);
}
long posix_pwrite(long fd, long buf, long count, long offset) {
    return (long)pwrite((int)fd, (void*)buf, (size_t)count, (off_t)offset);
}
long posix_pread(long fd, long buf, long count, long offset) {
    return (long)pread((int)fd, (void*)buf, (size_t)count, (off_t)offset);
}
long posix_lseek(long fd, long offset, long whence) {
    return (long)lseek((int)fd, (off_t)offset, (int)whence);
}
long posix_fsync(long fd) { return (long)fsync((int)fd); }
long posix_mkdir(const char* path, long mode) {
    return (long)mkdir(path, (mode_t)mode);
}

/* ───── Memory-mapped I/O ───── */

long mmap_alloc(long addr, long length, long prot, long flags, long fd, long offset) {
    void* r = mmap((void*)addr, (size_t)length, (int)prot, (int)flags, (int)fd, (off_t)offset);
    return (r == MAP_FAILED) ? -1 : (long)r;
}
long munmap_free(long addr, long length) {
    return (long)munmap((void*)addr, (size_t)length);
}

/* ───── CRC32C — hardware SSE 4.2 + software fallback ───── */

long crc32c_hardware(long crc, long val) {
#if defined(__x86_64__) && defined(__SSE4_2__) && defined(__GNUC__)
    return (long)__builtin_ia32_crc32di((unsigned long long)crc, (unsigned long long)val);
#else
    /* Software CRC32C */
    uint32_t c = (uint32_t)(crc ^ 0xFFFFFFFF);
    uint64_t v = (uint64_t)val;
    int i;
    for (i = 0; i < 8; i++) {
        c = (c >> 8) ^ CRC32C_TABLE[(c ^ (v & 0xFF)) & 0xFF];
        v >>= 8;
    }
    return (long)(c ^ 0xFFFFFFFF);
#endif
}

/* ───── Zstd compression (links against -lzstd) ───── */

extern size_t ZSTD_compress(void* dst, size_t dstCapacity,
                             const void* src, size_t srcSize,
                             int compressionLevel);
extern size_t ZSTD_compressBound(size_t srcSize);
extern unsigned ZSTD_isError(size_t code);

long zstd_compress(long dst, long dst_cap, long src, long src_size) {
    size_t r = ZSTD_compress((void*)dst, (size_t)dst_cap,
                              (const void*)src, (size_t)src_size, 3);
    return ZSTD_isError(r) ? 0 : (long)r;
}

/* ───── Generic syscall wrapper (used by Zeta's syscall() built-in) ───── */

long zenith_syscall(long number, long a1, long a2, long a3, long a4, long a5, long a6) {
    register long rax asm("rax") = number;
    register long rdi asm("rdi") = a1;
    register long rsi asm("rsi") = a2;
    register long rdx asm("rdx") = a3;
    register long r10 asm("r10") = a4;
    register long r8  asm("r8") = a5;
    register long r9  asm("r9") = a6;
    asm volatile("syscall"
        : "+r"(rax)
        : "r"(rdi), "r"(rsi), "r"(rdx), "r"(r10), "r"(r8), "r"(r9)
        : "rcx", "r11", "memory");
    return rax;
}

/* ───── io_uring (raw syscalls) ───── */

#ifndef SYS_io_uring_setup
#define SYS_io_uring_setup 425
#endif
#ifndef SYS_io_uring_enter
#define SYS_io_uring_enter 426
#endif
#ifndef SYS_io_uring_register
#define SYS_io_uring_register 427
#endif

long io_uring_setup(long entries, long params_ptr) {
    return syscall(SYS_io_uring_setup, (unsigned)entries, (void*)params_ptr);
}
long io_uring_enter(long ring_fd, long to_submit, long min_complete, long flags) {
    return syscall(SYS_io_uring_enter, (unsigned)ring_fd, (unsigned long)to_submit,
                   (unsigned long)min_complete, (unsigned)flags);
}
long io_uring_register(long ring_fd, long opcode, long arg, long nr_args) {
    return syscall(SYS_io_uring_register, (unsigned)ring_fd, (unsigned)opcode,
                   (void*)arg, (unsigned)nr_args);
}
