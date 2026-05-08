#define _GNU_SOURCE
#include <sys/epoll.h>
#include <sys/timerfd.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include <poll.h>
#include <pthread.h>

static struct epoll_event zt_buf[1024];
static pthread_mutex_t zt_lock = PTHREAD_MUTEX_INITIALIZER;

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
void reactor_destroy(int64_t e) { close((int)e); }

int64_t reactor_poll(int64_t e, int64_t buf, int64_t max, int64_t t) {
    (void)e; // epfd - unused, we use poll() for timeout
    // Use nanosleep for the timeout, then epoll_wait non-blocking
    struct timespec ts;
    ts.tv_sec = t / 1000;
    ts.tv_nsec = (t % 1000) * 1000000;
    // Sleep for the full duration
    pthread_mutex_lock(&zt_lock);
    nanosleep(&ts, NULL);
    // Drain any events (non-blocking)
    int n = epoll_wait((int)e, zt_buf, (int)max, 0);
    pthread_mutex_unlock(&zt_lock);
    if (buf && n > 0) for (int i = 0; i < n && i < max; i++) {
        ((int64_t*)buf)[i*2]=zt_buf[i].data.fd;
        ((int64_t*)buf)[i*2+1]=zt_buf[i].events;
    }
    return n;
}

int64_t waker_create(void) { int fds[2]; return pipe2(fds,O_CLOEXEC|O_NONBLOCK)<0?-1:fds[0]; }
int64_t waker_wake(int64_t r) { char b=1; return write((int)(r+1),&b,1)>0?0:-1; }
int64_t waker_consume(int64_t r) { char b[8]; return read((int)r,b,8)>0?0:-1; }
void waker_destroy(int64_t r) { close((int)r); close((int)(r+1)); }

int64_t set_nonblocking(int64_t f) {
    int fl=fcntl((int)f,F_GETFL,0); return fl<0?-1:fcntl((int)f,F_SETFL,fl|O_NONBLOCK);
}
__attribute__((force_align_arg_pointer)) int64_t monotonic_ns(void) {
    struct timespec ts; return clock_gettime(CLOCK_MONOTONIC,&ts)?-1:(int64_t)ts.tv_sec*1000000000+ts.tv_nsec;
}

int64_t scheduler_register_waker(int64_t e, int64_t w) {
    struct epoll_event ev; ev.events=EPOLLIN|EPOLLERR|EPOLLHUP; ev.data.fd=(int)w;
    return epoll_ctl((int)e,EPOLL_CTL_ADD,(int)w,&ev);
}
int64_t scheduler_run_reactor(int64_t e, int64_t t) {
    struct timespec ts;
    ts.tv_sec = t / 1000;
    ts.tv_nsec = (t % 1000) * 1000000;
    pthread_mutex_lock(&zt_lock);
    nanosleep(&ts, NULL);
    int n=epoll_wait((int)e,zt_buf,1024,0);
    pthread_mutex_unlock(&zt_lock);
    if(n<=0)return n; int c=0;
    for(int i=0;i<n;i++) if(zt_buf[i].events&EPOLLIN)
        {char b[8];read((int)zt_buf[i].data.fd,b,8);c++;} return c;
}
