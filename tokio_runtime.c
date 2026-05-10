// Zeta Tokio AOT runtime — complete extern fn implementations.
// Zeta calls these via extern fn. Each wraps one or more Linux syscalls.
#define _GNU_SOURCE
#include <sys/epoll.h>
#include <sys/timerfd.h>
#include <sys/socket.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

// ── Reactor ──
int64_t reactor_create(void) { return epoll_create1(EPOLL_CLOEXEC); }

int64_t reactor_add(int64_t e, int64_t f, int64_t ev) {
    struct epoll_event ee; memset(&ee,0,sizeof(ee)); ee.data.fd=(int)f;
    if (ev&1) ee.events|=EPOLLIN; if (ev&2) ee.events|=EPOLLOUT;
    ee.events|=EPOLLERR|EPOLLHUP;
    return epoll_ctl((int)e, EPOLL_CTL_ADD, (int)f, &ee);
}
int64_t reactor_modify(int64_t e, int64_t f, int64_t ev) {
    struct epoll_event ee; memset(&ee,0,sizeof(ee)); ee.data.fd=(int)f;
    if (ev&1) ee.events|=EPOLLIN; if (ev&2) ee.events|=EPOLLOUT;
    ee.events|=EPOLLERR|EPOLLHUP;
    return epoll_ctl((int)e, EPOLL_CTL_MOD, (int)f, &ee);
}
int64_t reactor_remove(int64_t e, int64_t f) { return epoll_ctl((int)e, EPOLL_CTL_DEL, (int)f, NULL); }

static __thread struct epoll_event zt_buf[1024];
int64_t reactor_poll(int64_t e, int64_t buf, int64_t max, int64_t t) {
    int n = epoll_wait((int)e, zt_buf, (int)max, (int)t);
    if (buf) for (int i = 0; i < n && i < max; i++) {
        ((int64_t*)buf)[i*2]=zt_buf[i].data.fd; ((int64_t*)buf)[i*2+1]=zt_buf[i].events;
    } return n;
}
int64_t reactor_event_fd(int64_t buf, int64_t i) { return ((int64_t*)buf)[i*2]; }
int64_t reactor_event_flags(int64_t buf, int64_t i) { return ((int64_t*)buf)[i*2+1]; }
void reactor_destroy(int64_t e) { close((int)e); }

// ── Waker ──
int64_t waker_create(void) { int fds[2]; return pipe2(fds,O_CLOEXEC|O_NONBLOCK)<0?-1:fds[0]; }
int64_t waker_wake(int64_t r) { char b=1; return write((int)(r+1),&b,1)>0?0:-1; }
int64_t waker_consume(int64_t r) { char b[8]; return read((int)r,b,8)>0?0:-1; }
void waker_destroy(int64_t r) { close((int)r); close((int)(r+1)); }

// ── Timerfd ──
int64_t zt_timerfd_create(void) { return timerfd_create(CLOCK_MONOTONIC, TFD_CLOEXEC|TFD_NONBLOCK); }
int64_t zt_timerfd_set(int64_t f, int64_t ns) {
    struct itimerspec s; memset(&s,0,sizeof(s));
    s.it_value.tv_sec=ns/1000000000; s.it_value.tv_nsec=ns%1000000000;
    return timerfd_settime((int)f,0,&s,NULL);
}
int64_t zt_timerfd_set_abs(int64_t f, int64_t an) {
    struct itimerspec s; memset(&s,0,sizeof(s));
    s.it_value.tv_sec=an/1000000000; s.it_value.tv_nsec=an%1000000000;
    return timerfd_settime((int)f,TFD_TIMER_ABSTIME,&s,NULL);
}
int64_t zt_timerfd_read(int64_t f) { uint64_t v; return read((int)f,&v,8)>0?(int64_t)v:-1; }

// ── Helpers ──
int64_t set_nonblocking(int64_t f) {
    int fl=fcntl((int)f,F_GETFL,0); return fl<0?-1:fcntl((int)f,F_SETFL,fl|O_NONBLOCK);
}
int64_t monotonic_ns(void) {
    struct timespec ts; return clock_gettime(CLOCK_MONOTONIC,&ts)?-1:(int64_t)ts.tv_sec*1000000000+ts.tv_nsec;
}

// ── Scheduler ──
int64_t scheduler_register_waker(int64_t e, int64_t w) {
    struct epoll_event ev; ev.events=EPOLLIN|EPOLLERR|EPOLLHUP; ev.data.fd=(int)w;
    return epoll_ctl((int)e,EPOLL_CTL_ADD,(int)w,&ev);
}
int64_t scheduler_run_reactor(int64_t e, int64_t t) {
    int n=epoll_wait((int)e,zt_buf,1024,(int)t); if(n<=0)return n;
    int c=0; for(int i=0;i<n;i++) if(zt_buf[i].events&EPOLLIN)
        {char b[8];read((int)zt_buf[i].data.fd,b,8);c++;} return c;
}

// ── C String Helpers ──
static const char zt_all_ifaces[] = "0.0.0.0";

int64_t cstr_all_interfaces(void) {
    return (int64_t)&zt_all_ifaces[0];
}

// ── C String Helpers ──
// Provide a C string "0.0.0.0" for TCP bind/connect when host is null.
// The Zeta echo server calls tcp_bind(0, 8080) — without this, the
// host resolves to "" which produces ":8080" (invalid address).

int64_t host_str_all_interfaces(void) {
    static const char addr[] = "0.0.0.0";
    return (int64_t)addr;
}
