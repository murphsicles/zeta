// src/runtime/sync.rs
//! Atomic operations and synchronization primitives for Zeta v0.5.0

use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Mutex, RwLock};

/// Atomic boolean type for Zeta
#[repr(C)]
pub struct ZetaAtomicBool {
    inner: AtomicBool,
}

impl ZetaAtomicBool {
    pub fn new(value: bool) -> Self {
        Self {
            inner: AtomicBool::new(value),
        }
    }

    pub fn load(&self, order: Ordering) -> bool {
        self.inner.load(order)
    }

    pub fn store(&self, value: bool, order: Ordering) {
        self.inner.store(value, order);
    }

    pub fn swap(&self, value: bool, order: Ordering) -> bool {
        self.inner.swap(value, order)
    }

    pub fn compare_exchange(
        &self,
        current: bool,
        new: bool,
        success: Ordering,
        failure: Ordering,
    ) -> Result<bool, bool> {
        self.inner.compare_exchange(current, new, success, failure)
    }
}

/// Atomic usize type for Zeta
#[repr(C)]
pub struct ZetaAtomicUsize {
    inner: AtomicUsize,
}

impl ZetaAtomicUsize {
    pub fn new(value: usize) -> Self {
        Self {
            inner: AtomicUsize::new(value),
        }
    }

    pub fn load(&self, order: Ordering) -> usize {
        self.inner.load(order)
    }

    pub fn store(&self, value: usize, order: Ordering) {
        self.inner.store(value, order);
    }

    pub fn swap(&self, value: usize, order: Ordering) -> usize {
        self.inner.swap(value, order)
    }

    pub fn fetch_add(&self, value: usize, order: Ordering) -> usize {
        self.inner.fetch_add(value, order)
    }

    pub fn fetch_sub(&self, value: usize, order: Ordering) -> usize {
        self.inner.fetch_sub(value, order)
    }

    pub fn compare_exchange(
        &self,
        current: usize,
        new: usize,
        success: Ordering,
        failure: Ordering,
    ) -> Result<usize, usize> {
        self.inner.compare_exchange(current, new, success, failure)
    }
}

/// Mutex type for Zeta
#[repr(C)]
pub struct ZetaMutex<T> {
    inner: Mutex<T>,
}

impl<T> ZetaMutex<T> {
    pub fn new(value: T) -> Self {
        Self {
            inner: Mutex::new(value),
        }
    }

    pub fn lock(&self) -> std::sync::MutexGuard<'_, T> {
        self.inner.lock().unwrap()
    }

    pub fn try_lock(&self) -> Option<std::sync::MutexGuard<'_, T>> {
        self.inner.try_lock().ok()
    }
}

/// RwLock type for Zeta
#[repr(C)]
pub struct ZetaRwLock<T> {
    inner: RwLock<T>,
}

impl<T> ZetaRwLock<T> {
    pub fn new(value: T) -> Self {
        Self {
            inner: RwLock::new(value),
        }
    }

    pub fn read(&self) -> std::sync::RwLockReadGuard<'_, T> {
        self.inner.read().unwrap()
    }

    pub fn write(&self) -> std::sync::RwLockWriteGuard<'_, T> {
        self.inner.write().unwrap()
    }

    pub fn try_read(&self) -> Option<std::sync::RwLockReadGuard<'_, T>> {
        self.inner.try_read().ok()
    }

    pub fn try_write(&self) -> Option<std::sync::RwLockWriteGuard<'_, T>> {
        self.inner.try_write().ok()
    }
}

// Host functions for atomic operations - commented out for now

// #[no_mangle]
// pub unsafe extern "C" fn host_atomic_bool_new(value: i64) -> *mut ZetaAtomicBool {
//     let atomic = Box::new(ZetaAtomicBool::new(value != 0));
//     Box::into_raw(atomic)
// }

// #[no_mangle]
// pub unsafe extern "C" fn host_atomic_bool_load(ptr: *const ZetaAtomicBool) -> i64 {
//     if ptr.is_null() {
//         return 0;
//     }
//     unsafe {
//         let atomic = &*ptr;
//         atomic.load(Ordering::SeqCst) as i64
//     }
// }

// #[no_mangle]
// pub unsafe extern "C" fn host_atomic_bool_store(ptr: *mut ZetaAtomicBool, value: i64) {
//     if ptr.is_null() {
//         return;
//     }
//     unsafe {
//         let atomic = &*ptr;
//         atomic.store(value != 0, Ordering::SeqCst);
//     }
// }

// #[no_mangle]
// pub unsafe extern "C" fn host_atomic_usize_new(value: i64) -> *mut ZetaAtomicUsize {
//     let atomic = Box::new(ZetaAtomicUsize::new(value as usize));
//     Box::into_raw(atomic)
// }

// #[no_mangle]
// pub unsafe extern "C" fn host_atomic_usize_load(ptr: *const ZetaAtomicUsize) -> i64 {
//     if ptr.is_null() {
//         return 0;
//     }
//     unsafe {
//         let atomic = &*ptr;
//         atomic.load(Ordering::SeqCst) as i64
//     }
// }

// #[no_mangle]
// pub unsafe extern "C" fn host_atomic_usize_store(ptr: *mut ZetaAtomicUsize, value: i64) {
//     if ptr.is_null() {
//         return;
//     }
//     unsafe {
//         let atomic = &*ptr;
//         atomic.store(value as usize, Ordering::SeqCst);
//     }
// }

// #[no_mangle]
// pub unsafe extern "C" fn host_atomic_usize_fetch_add(ptr: *mut ZetaAtomicUsize, value: i64) -> i64 {
//     if ptr.is_null() {
//         return 0;
//     }
//     unsafe {
//         let atomic = &*ptr;
//         atomic.fetch_add(value as usize, Ordering::SeqCst) as i64
//     }
// }

// #[no_mangle]
// pub unsafe extern "C" fn host_atomic_usize_fetch_sub(ptr: *mut ZetaAtomicUsize, value: i64) -> i64 {
//     if ptr.is_null() {
//         return 0;
//     }
//     unsafe {
//         let atomic = &*ptr;
//         atomic.fetch_sub(value as usize, Ordering::SeqCst) as i64
//     }
// }
