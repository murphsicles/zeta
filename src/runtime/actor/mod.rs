// src/runtime/actor/mod.rs
pub mod channel;
pub mod scheduler;
pub use channel::{host_channel_recv, host_channel_send, Channel};
pub use scheduler::{host_spawn, init_runtime, spawn};
