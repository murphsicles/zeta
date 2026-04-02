//! Time module for Zeta standard library.
//! 
//! Provides timing and date/time operations:
//! - System time
//! - Duration
//! - Timestamps
//! - Sleep operations

use std::time::{SystemTime, Instant, Duration, UNIX_EPOCH};
use std::thread;

/// Initializes the time module.
pub fn init() {
    println!("Time module initialized");
}

/// Registers time functions with the runtime.
pub fn register_functions(map: &mut std::collections::HashMap<&'static str, usize>) {
    // System time functions
    map.insert("system_time_now", system_time_now as *const () as usize);
    map.insert("system_time_since_epoch", system_time_since_epoch as *const () as usize);
    
    // Instant functions
    map.insert("instant_now", instant_now as *const () as usize);
    map.insert("instant_elapsed", instant_elapsed as *const () as usize);
    
    // Duration functions
    map.insert("duration_from_secs", duration_from_secs as *const () as usize);
    map.insert("duration_from_millis", duration_from_millis as *const () as usize);
    map.insert("duration_from_micros", duration_from_micros as *const () as usize);
    map.insert("duration_from_nanos", duration_from_nanos as *const () as usize);
    map.insert("duration_as_secs", duration_as_secs as *const () as usize);
    map.insert("duration_as_millis", duration_as_millis as *const () as usize);
    map.insert("duration_as_micros", duration_as_micros as *const () as usize);
    map.insert("duration_as_nanos", duration_as_nanos as *const () as usize);
    
    // Sleep function
    map.insert("thread_sleep", thread_sleep as *const () as usize);
    
    // UNIX epoch constant
    map.insert("unix_epoch", unix_epoch as *const () as usize);
}

// ============================================================================
// System Time Operations
// ============================================================================

/// SystemTime structure
pub struct ZetaSystemTime {
    inner: SystemTime,
}

/// Gets the current system time.
/// 
/// # Safety
/// Returns a pointer to a SystemTime structure.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn system_time_now() -> *mut ZetaSystemTime {
    let time = Box::new(ZetaSystemTime {
        inner: SystemTime::now(),
    });
    Box::into_raw(time)
}

/// Gets the duration since UNIX epoch for a SystemTime.
/// 
/// # Safety
/// system_time must be a valid pointer from system_time_now.
/// Returns nanoseconds since UNIX epoch, or 0 on error.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn system_time_since_epoch(system_time: *const ZetaSystemTime) -> u128 { unsafe {
    if let Some(system_time) = system_time.as_ref() {
        match system_time.inner.duration_since(UNIX_EPOCH) {
            Ok(duration) => duration.as_nanos(),
            Err(_) => 0,
        }
    } else {
        0
    }
}}

// ============================================================================
// Instant Operations
// ============================================================================

/// Instant structure
pub struct ZetaInstant {
    inner: Instant,
}

/// Gets the current instant.
/// 
/// # Safety
/// Returns a pointer to an Instant structure.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn instant_now() -> *mut ZetaInstant {
    let instant = Box::new(ZetaInstant {
        inner: Instant::now(),
    });
    Box::into_raw(instant)
}

/// Gets the elapsed time since an instant.
/// 
/// # Safety
/// instant must be a valid pointer from instant_now.
/// Returns nanoseconds elapsed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn instant_elapsed(instant: *const ZetaInstant) -> u128 { unsafe {
    if let Some(instant) = instant.as_ref() {
        instant.inner.elapsed().as_nanos()
    } else {
        0
    }
}}

// ============================================================================
// Duration Operations
// ============================================================================

/// Duration structure
pub struct ZetaDuration {
    inner: Duration,
}

/// Creates a duration from seconds.
/// 
/// # Safety
/// Returns a pointer to a Duration structure.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn duration_from_secs(secs: u64) -> *mut ZetaDuration {
    let duration = Box::new(ZetaDuration {
        inner: Duration::from_secs(secs),
    });
    Box::into_raw(duration)
}

/// Creates a duration from milliseconds.
/// 
/// # Safety
/// Returns a pointer to a Duration structure.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn duration_from_millis(millis: u64) -> *mut ZetaDuration {
    let duration = Box::new(ZetaDuration {
        inner: Duration::from_millis(millis),
    });
    Box::into_raw(duration)
}

/// Creates a duration from microseconds.
/// 
/// # Safety
/// Returns a pointer to a Duration structure.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn duration_from_micros(micros: u64) -> *mut ZetaDuration {
    let duration = Box::new(ZetaDuration {
        inner: Duration::from_micros(micros),
    });
    Box::into_raw(duration)
}

/// Creates a duration from nanoseconds.
/// 
/// # Safety
/// Returns a pointer to a Duration structure.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn duration_from_nanos(nanos: u64) -> *mut ZetaDuration {
    let duration = Box::new(ZetaDuration {
        inner: Duration::from_nanos(nanos),
    });
    Box::into_raw(duration)
}

/// Gets the duration in seconds.
/// 
/// # Safety
/// duration must be a valid pointer from duration_from_*.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn duration_as_secs(duration: *const ZetaDuration) -> u64 { unsafe {
    if let Some(duration) = duration.as_ref() {
        duration.inner.as_secs()
    } else {
        0
    }
}}

/// Gets the duration in milliseconds.
/// 
/// # Safety
/// duration must be a valid pointer from duration_from_*.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn duration_as_millis(duration: *const ZetaDuration) -> u128 { unsafe {
    if let Some(duration) = duration.as_ref() {
        duration.inner.as_millis()
    } else {
        0
    }
}}

/// Gets the duration in microseconds.
/// 
/// # Safety
/// duration must be a valid pointer from duration_from_*.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn duration_as_micros(duration: *const ZetaDuration) -> u128 { unsafe {
    if let Some(duration) = duration.as_ref() {
        duration.inner.as_micros()
    } else {
        0
    }
}}

/// Gets the duration in nanoseconds.
/// 
/// # Safety
/// duration must be a valid pointer from duration_from_*.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn duration_as_nanos(duration: *const ZetaDuration) -> u128 { unsafe {
    if let Some(duration) = duration.as_ref() {
        duration.inner.as_nanos()
    } else {
        0
    }
}}

// ============================================================================
// Thread Operations
// ============================================================================

/// Sleeps for the specified number of milliseconds.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn thread_sleep(millis: u64) {
    thread::sleep(Duration::from_millis(millis));
}

// ============================================================================
// Constants
// ============================================================================

/// Gets the UNIX epoch as a SystemTime.
/// 
/// # Safety
/// Returns a pointer to a SystemTime structure representing UNIX epoch.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn unix_epoch() -> *mut ZetaSystemTime {
    let epoch = Box::new(ZetaSystemTime {
        inner: UNIX_EPOCH,
    });
    Box::into_raw(epoch)
}
