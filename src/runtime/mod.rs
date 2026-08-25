// src/runtime/mod.rs
pub mod actor;
pub mod array;
pub mod r#async;
pub mod async_advanced;
pub mod atomic;
pub mod channel_advanced;
pub mod char_;
pub mod duration;
pub mod fs;
pub mod host;
pub mod identity;
pub mod io;
pub mod jit_stubs;
pub mod map;
pub mod memory;
pub mod memory_bulletproof;
pub mod mpsc;
pub mod net;
pub mod option;
pub mod path;
pub mod process;
// The reactor uses Linux-only syscalls (epoll, timerfd). On other platforms
// the async reactor is unavailable; the Zeta reactor.z source replaces this
// when self-hosting.
#[cfg(target_os = "linux")]
pub mod reactor;
pub mod std;
pub mod sync;
pub mod thread_;
pub mod vec;
pub mod vector;
pub mod xai;
pub mod zeta_runtime;
