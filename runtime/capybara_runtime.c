// Capybara Runtime Extensions for Zeta. Replaceable by native Zeta.

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/mman.h>
#include <linux/io_uring.h>
#include <fcntl.h>
#include <stdio.h>

// ─── File I/O ────────────────────────────────────────────────────────────
long posix_open_mode(const char* p, int f, int m) { return (long)open(p, f, m); }
long posix_fchmod(int f, int m) { return (long)fchmod(f, m); }

// ─── CLI ─────────────────────────────────────────────────────────────────
long capy_argc(long a, long b) { return a; }
long capy_argv_i(long a, long b, long i) {
    if (i < 0 || i >= a) return 0;
    return (long)((const char**)(long)b)[i];
}
long capy_streq(long a, long b) { return strcmp((const char*)a, (const char*)b) == 0 ? 1 : 0; }
long capy_atoi(long s) { return (long)atol((const char*)s); }

// ─── Memory ──────────────────────────────────────────────────────────────
long capy_store_i64(long a, long v) { *(long*)a = v; return 0; }
long capy_load_i64(long a) { return *(long*)a; }

// ─── TCP Sockets ─────────────────────────────────────────────────────────
long net_listen(long port) {
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) return -1;
    int opt = 1;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
    struct sockaddr_in addr = {0};
    addr.sin_family = AF_INET; addr.sin_port = htons((uint16_t)port);
    addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if (bind(fd, (struct sockaddr*)&addr, sizeof(addr)) < 0) { close(fd); return -2; }
    if (listen(fd, 128) < 0) { close(fd); return -3; }
    return fd;
}
long net_accept(long sfd) { struct sockaddr_in a; socklen_t al = sizeof(a); return accept(sfd, (struct sockaddr*)&a, &al); }
long net_send(long f, long b, long l) { return (long)write((int)f, (const void*)b, (size_t)l); }
long net_recv(long f, long b, long l) { return (long)read((int)f, (void*)b, (size_t)l); }
long net_connect(const char* host, long port) {
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons((uint16_t)port);
    inet_pton(AF_INET, host, &addr.sin_addr);
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) return -1;
    if (connect(fd, (struct sockaddr*)&addr, sizeof(addr)) < 0) { close(fd); return -2; }
    return fd;
}
// Set file descriptor to non-blocking mode.
long net_nonblock(long fd) {
    int flags = fcntl((int)fd, F_GETFL, 0);
    if (flags < 0) return -1;
    return (long)fcntl((int)fd, F_SETFL, flags | O_NONBLOCK);
}
long capy_flush() { fflush(stdout); return 0; }
long net_close(long f) { close((int)f); return 0; }

// ─── io_uring Async I/O ─────────────────────────────────────────────────
#ifndef __NR_io_uring_setup
#define __NR_io_uring_setup 425
#define __NR_io_uring_enter  426
#endif

// State block offsets (128 bytes). Zeta reads/writes these via capy_load/store.
//   0: ring_fd, 8: sq_ptr, 16: sqe_ptr, 24: cq_ptr
//  32: sq_entries, 40: cq_entries
//  48: sq_off.tail, 56: cq_off.head, 64: sq_off.array, 72: cq_off.cqes
//  80: sq_off.ring_mask, 88: cq_off.ring_mask, 96: cq_off.tail, 104: sq_off.head

long iouring_init(long st, long entries) {
    struct io_uring_params p;
    memset(&p, 0, sizeof(p));
    int fd = (int)syscall(__NR_io_uring_setup, entries, &p);
    if (fd < 0) return -1;
    int sq_size = p.sq_off.array + p.sq_entries * sizeof(unsigned);
    void* sq_ptr = mmap(0, sq_size, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_POPULATE, fd, IORING_OFF_SQ_RING);
    if (sq_ptr == MAP_FAILED) { close(fd); return -2; }
    int sqe_size = p.sq_entries * sizeof(struct io_uring_sqe);
    void* sqe_ptr = mmap(0, sqe_size, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_POPULATE, fd, IORING_OFF_SQES);
    if (sqe_ptr == MAP_FAILED) { munmap(sq_ptr, sq_size); close(fd); return -3; }
    int cq_size = p.cq_off.cqes + p.cq_entries * sizeof(struct io_uring_cqe);
    void* cq_ptr = mmap(0, cq_size, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_POPULATE, fd, IORING_OFF_CQ_RING);
    if (cq_ptr == MAP_FAILED) { munmap(sqe_ptr, sqe_size); munmap(sq_ptr, sq_size); close(fd); return -4; }
    capy_store_i64(st+0, fd); capy_store_i64(st+8, (long)sq_ptr); capy_store_i64(st+16, (long)sqe_ptr);
    capy_store_i64(st+24, (long)cq_ptr); capy_store_i64(st+32, p.sq_entries);
    capy_store_i64(st+40, p.cq_entries); capy_store_i64(st+48, p.sq_off.tail);
    capy_store_i64(st+56, p.cq_off.head); capy_store_i64(st+64, p.sq_off.array);
    capy_store_i64(st+72, p.cq_off.cqes); capy_store_i64(st+80, p.sq_off.ring_mask);
    capy_store_i64(st+88, p.cq_off.ring_mask); capy_store_i64(st+96, p.cq_off.tail);
    capy_store_i64(st+104, p.sq_off.head); return 0;
}

// Get next SQE pointer (does NOT advance tail — Zeta fills SQE first).
long iouring_sqe(long st) {
    long sq = capy_load_i64(st+8), sqe = capy_load_i64(st+16);
    unsigned t = *(unsigned*)(sq + capy_load_i64(st+48));
    unsigned h = *(unsigned*)(sq + capy_load_i64(st+104));
    if (t - h >= capy_load_i64(st+32)) return 0;
    long idx = capy_load_i64(st+120);  // internal counter since last submit
    capy_store_i64(st+120, idx + 1);    // increment for next call
    return (long)((struct io_uring_sqe*)sqe + ((t + idx) & capy_load_i64(st+80)));
}

// Advance SQ tail by count, submit count SQEs, optionally wait.
// Returns number of completions, or negative on error.
long iouring_submit(long st, long count, long wait) {
    long sq = capy_load_i64(st+8);
    long sq_off_tail = capy_load_i64(st+48);
    long sq_off_array = capy_load_i64(st+64);
    long sq_mask = capy_load_i64(st+80);
    unsigned* tp = (unsigned*)(sq + sq_off_tail);
    unsigned* ap = (unsigned*)(sq + sq_off_array);
    unsigned t = *tp;
    long i;
    for (i = 0; i < count; i++) {
        unsigned idx = (t + i) & sq_mask;
        *(ap + i) = idx;
    }
    *tp = t + count;
    capy_store_i64(st+120, 0);  // reset internal counter
    long flags = wait > 0 ? IORING_ENTER_GETEVENTS : 0;
    return (long)syscall(__NR_io_uring_enter, capy_load_i64(st+0), count, wait, flags, NULL, 0);
}

// Read completion. Returns user_data, or 0 if none.
long iouring_cqe(long st) {
    long cq = capy_load_i64(st+24);
    unsigned* hp = (unsigned*)(cq + capy_load_i64(st+56));
    unsigned* tp = (unsigned*)(cq + capy_load_i64(st+96));
    if (*hp == *tp) return 0;
    struct io_uring_cqe* cqes = (struct io_uring_cqe*)(cq + capy_load_i64(st+72));
    unsigned idx = *hp & capy_load_i64(st+88);
    long res = cqes[idx].res, ud = cqes[idx].user_data;
    *hp = *hp + 1;
    capy_store_i64(st+112, res); // store result for iouring_cqe_res
    return ud;
}

// Read stored result from last CQE.
long iouring_cqe_res(long st) { return capy_load_i64(st+112); }

// SQE fill helpers: zero the SQE and set common fields.
long uring_accept_sqe(long sqe, long fd, long ud) {
    struct io_uring_sqe* s = (struct io_uring_sqe*)sqe;
    memset(s, 0, sizeof(*s)); s->opcode = IORING_OP_ACCEPT; s->fd = fd; s->user_data = ud;
    return 0;
}
long uring_read_sqe(long sqe, long fd, long buf, long len, long ud) {
    struct io_uring_sqe* s = (struct io_uring_sqe*)sqe;
    memset(s, 0, sizeof(*s)); s->opcode = IORING_OP_READ; s->fd = fd;
    s->addr = buf; s->len = len; s->user_data = ud;
    return 0;
}
long uring_write_sqe(long sqe, long fd, long buf, long len, long ud) {
    struct io_uring_sqe* s = (struct io_uring_sqe*)sqe;
    memset(s, 0, sizeof(*s)); s->opcode = IORING_OP_WRITE; s->fd = fd;
    s->addr = buf; s->len = len; s->user_data = ud;
    return 0;
}

// ─── Raw syscall wrapper (used by Zeta syscall() builtin) ──────────
long zenith_syscall(long n, long a1, long a2, long a3, long a4, long a5, long a6) {
    register long rax asm("rax") = n;
    register long rdi asm("rdi") = a1;
    register long rsi asm("rsi") = a2;
    register long rdx asm("rdx") = a3;
    register long r10 asm("r10") = a4;
    register long r8  asm("r8")  = a5;
    register long r9  asm("r9")  = a6;
    asm volatile("syscall" : "+r"(rax) : "r"(rdi), "r"(rsi), "r"(rdx), "r"(r10), "r"(r8), "r"(r9) : "rcx", "r11", "memory");
    return rax;
}
