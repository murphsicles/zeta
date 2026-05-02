//! Duration arithmetic runtime — wraps Rust std::time::Duration
//! Duration stored as heap-allocated Box<Duration>, pointer passed as i64.

use std::time::Duration;

fn to_dur(d: i64) -> Option<Box<Duration>> {
    if d == 0 {
        None
    } else {
        unsafe { Some(Box::from_raw(d as *mut Duration)) }
    }
}

fn into_ptr(d: Duration) -> i64 {
    Box::into_raw(Box::new(d)) as i64
}

fn borrow(d: i64) -> Option<&'static Duration> {
    if d == 0 {
        None
    } else {
        unsafe { Some(&*(d as *const Duration)) }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn duration_add(a: i64, b: i64) -> i64 {
    let a = borrow(a);
    let b = borrow(b);
    match (a, b) {
        (Some(ad), Some(bd)) => into_ptr(*ad + *bd),
        _ => 0,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn duration_sub(a: i64, b: i64) -> i64 {
    let a = borrow(a);
    let b = borrow(b);
    match (a, b) {
        (Some(ad), Some(bd)) => into_ptr(ad.checked_sub(*bd).unwrap_or(Duration::ZERO)),
        _ => 0,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn duration_mul(d: i64, scale: i64) -> i64 {
    borrow(d)
        .map(|d| into_ptr(d.mul_f64(scale as f64)))
        .unwrap_or(0)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn duration_div(d: i64, divisor: i64) -> i64 {
    match borrow(d) {
        Some(dur) if divisor > 0 => {
            let nanos = dur.as_nanos() / divisor as u128;
            into_ptr(Duration::from_nanos(nanos as u64))
        }
        _ => 0,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn duration_lt(a: i64, b: i64) -> i64 {
    match (borrow(a), borrow(b)) {
        (Some(ad), Some(bd)) => (*ad < *bd) as i64,
        _ => 0,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn duration_eq(a: i64, b: i64) -> i64 {
    match (borrow(a), borrow(b)) {
        (Some(ad), Some(bd)) => (*ad == *bd) as i64,
        _ => 0,
    }
}
