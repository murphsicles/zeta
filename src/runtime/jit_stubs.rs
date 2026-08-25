//! Host stubs for standard runtime symbols that have no host implementation.
//!
//! The codegen declares a fixed set of standard runtime functions in every
//! module (operators, sieve, SIMD builtins, async runtime, ...). The JIT
//! must be able to resolve each one that the compiled code may call; a
//! declaration without a mapping resolves to NULL and segfaults when called.
//!
//! These stubs are mapped in `jit.rs` (see the explicit mapping table). On
//! Linux the async family maps to the real implementations in `reactor.rs`
//! instead (see the cfg-gated table there).

#![allow(dead_code, unused_variables)]

// ── Clock helpers (real implementations — portable: libc clock_gettime) ──

/// Current UNIX time in seconds.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn datetime_now() -> i64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(-1)
}

/// Monotonic clock in microseconds (zeroed at first call).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn get_time_us() -> i64 {
    use std::time::Instant;
    static START: std::sync::OnceLock<Instant> = std::sync::OnceLock::new();
    let start = START.get_or_init(Instant::now);
    start.elapsed().as_micros() as i64
}

// ── Test-harness helpers (no-op / inert) ──

/// Inert: no timing source available to compare against.
pub unsafe extern "C" fn time_is_up(_start_us: i64, _limit_us: i64) -> i64 {
    0
}

/// Inert: test result printer has no portable behavior.
pub unsafe extern "C" fn print_result(_a: i64, _b: i64) {}

// ── Murphy sieve runtime (no Rust implementation exists) ──

pub unsafe extern "C" fn run_sieve(_n: i64) -> i64 {
    -1
}
pub unsafe extern "C" fn run_sieve_timed(_n: i64, _t: i64) -> i64 {
    -1
}
pub unsafe extern "C" fn parallel_sieve(_n: i64, _t: i64) -> i64 {
    -1
}
pub unsafe extern "C" fn parallel_sieve_timed(_n: i64, _t: i64, _x: i64) -> i64 {
    -1
}
pub unsafe extern "C" fn sieve_step(_a: i64, _b: i64, _c: i64, _d: i64) {}

// ── Generic call thunk ──

/// Returns the callee pointer; never dereferenced.
pub unsafe extern "C" fn call_i64(fn_ptr: i64, _arg: i64) -> i64 {
    fn_ptr
}

// ── SIMD / vector builtins (inert on hosts without the feature) ──

pub unsafe extern "C" fn avx512_byte_fill(_v: i64, _x: i64, _y: i64) {}
pub unsafe extern "C" fn avx512_count_bits(_a: i64, _b: i64) -> i64 {
    0
}
pub unsafe extern "C" fn __builtin_v4i64_andnot(
    _a: i64,
    _b: i64,
    _c: i64,
    _d: i64,
    _e: i64,
    _f: i64,
) {
}
pub unsafe extern "C" fn __builtin_v4i64_store(
    _a: i64,
    _b: i64,
    _c: i64,
    _d: i64,
    _e: i64,
    _f: i64,
) {
}

// ── Qword memory builtins (inert) ──

pub unsafe extern "C" fn read_qword_builtin(_a: i64, _b: i64) -> i64 {
    0
}
pub unsafe extern "C" fn write_qword_builtin(_a: i64, _b: i64, _c: i64) {}

// ── Bit-vector helpers (inert) ──

pub unsafe extern "C" fn clear_bit(_base: i64, _index: i64) {}
pub unsafe extern "C" fn test_bit(_base: i64, _index: i64) -> i64 {
    0
}

// ── Operator functions (wrapping arithmetic, total: no traps) ──

pub unsafe extern "C" fn add_i64(a: i64, b: i64) -> i64 {
    a.wrapping_add(b)
}
pub unsafe extern "C" fn sub_i64(a: i64, b: i64) -> i64 {
    a.wrapping_sub(b)
}
pub unsafe extern "C" fn mul_i64(a: i64, b: i64) -> i64 {
    a.wrapping_mul(b)
}
pub unsafe extern "C" fn div_i64(a: i64, b: i64) -> i64 {
    if b == 0 { 0 } else { a.wrapping_div(b) }
}
pub unsafe extern "C" fn mod_i64(a: i64, b: i64) -> i64 {
    if b == 0 { 0 } else { a.wrapping_rem(b) }
}
pub unsafe extern "C" fn and_i64(a: i64, b: i64) -> i64 {
    a & b
}
pub unsafe extern "C" fn or_i64(a: i64, b: i64) -> i64 {
    a | b
}
pub unsafe extern "C" fn xor_i64(a: i64, b: i64) -> i64 {
    a ^ b
}
pub unsafe extern "C" fn not_i64(a: i64) -> i64 {
    !a
}
pub unsafe extern "C" fn shl_i64(a: i64, b: i64) -> i64 {
    a.wrapping_shl(b as u32 & 63)
}
pub unsafe extern "C" fn shr_i64(a: i64, b: i64) -> i64 {
    a.wrapping_shr(b as u32 & 63)
}
pub unsafe extern "C" fn eq_i64(a: i64, b: i64) -> i64 {
    (a == b) as i64
}
pub unsafe extern "C" fn ne_i64(a: i64, b: i64) -> i64 {
    (a != b) as i64
}
pub unsafe extern "C" fn lt_i64(a: i64, b: i64) -> i64 {
    (a < b) as i64
}
pub unsafe extern "C" fn gt_i64(a: i64, b: i64) -> i64 {
    (a > b) as i64
}
pub unsafe extern "C" fn le_i64(a: i64, b: i64) -> i64 {
    (a <= b) as i64
}
pub unsafe extern "C" fn ge_i64(a: i64, b: i64) -> i64 {
    (a >= b) as i64
}

// ── Async runtime stubs (non-Linux only) ──
//
// On Linux these symbols map to the real implementations in `reactor.rs`
// (see the cfg-gated table in jit.rs). On other hosts the async reactor is
// unavailable: the stubs let the JIT resolve the standard declarations
// instead of leaving them as NULL (which segfaults when called). They
// report "unavailable" via -1 / no-op.
#[cfg(not(target_os = "linux"))]
pub mod async_stubs {
    pub unsafe extern "C" fn reactor_create() -> i64 {
        -1
    }
    pub unsafe extern "C" fn reactor_add(_e: i64, _f: i64, _ev: i64) -> i64 {
        -1
    }
    pub unsafe extern "C" fn reactor_remove(_e: i64, _f: i64) -> i64 {
        -1
    }
    pub unsafe extern "C" fn reactor_poll(_e: i64, _buf: i64, _max: i64, _t: i64) -> i64 {
        -1
    }
    pub unsafe extern "C" fn reactor_event_fd(_buf: i64, _i: i64) -> i64 {
        -1
    }
    pub unsafe extern "C" fn reactor_event_flags(_buf: i64, _i: i64) -> i64 {
        0
    }
    pub unsafe extern "C" fn reactor_destroy(_e: i64) {}
    pub unsafe extern "C" fn waker_create() -> i64 {
        -1
    }
    pub unsafe extern "C" fn waker_wake(_f: i64) -> i64 {
        -1
    }
    pub unsafe extern "C" fn waker_consume(_f: i64) -> i64 {
        -1
    }
    pub unsafe extern "C" fn waker_destroy(_f: i64) {}
    pub unsafe extern "C" fn scheduler_register_waker(_e: i64, _f: i64) -> i64 {
        -1
    }
    pub unsafe extern "C" fn scheduler_run_reactor(_e: i64, _t: i64) -> i64 {
        -1
    }
}
