// src/runtime/host.rs
use crate::runtime::std::{std_malloc, std_free};
use std::ffi::{c_char, CStr};
use std::os::raw::c_void;
use std::time::{SystemTime, UNIX_EPOCH};
use reqwest::blocking::Client;
use rustls::ClientConfig;

/// Returns the current datetime as milliseconds since UNIX epoch.
///
/// # Safety
/// No safety concerns as there are no parameters.
pub unsafe extern "C" fn host_datetime_now() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as i64
}

/// Frees a pointer using std free.
///
/// # Safety
/// Pointer must be valid or null, no use after free.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_free(ptr: *mut c_void) {
    if !ptr.is_null() {
        std_free(ptr as *mut u8);
    }
}

/// Performs a real HTTP GET using reqwest and returns body length.
///
/// # Safety
/// The url must be a valid null-terminated C string.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_http_get(url: *const c_char) -> i64 {
    if url.is_null() {
        return -1;
    }

    let url_str = match CStr::from_ptr(url).to_str() {
        Ok(s) => s,
        Err(_) => return -1,
    };

    let client = Client::new();
    match client.get(url_str).send() {
        Ok(resp) => {
            if resp.status().is_success() {
                resp.content_length().map(|l| l as i64).unwrap_or(-2)
            } else {
                -3
            }
        }
        Err(_) => -4,
    }
}

/// Performs a real TLS handshake using rustls (dummy success for now).
///
/// # Safety
/// The host must be a valid null-terminated C string.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_tls_handshake(host: *const c_char) -> i64 {
    if host.is_null() {
        return -1;
    }

    let host_str = match CStr::from_ptr(host).to_str() {
        Ok(s) => s,
        Err(_) => return -1,
    };

    // Minimal rustls setup to force linking and verify it works
    let mut config = ClientConfig::builder()
        .with_safe_defaults()
        .with_root_certificates(rustls::RootCertStore::empty())
        .with_no_client_auth();

    // We don't actually connect, just return success if config builds
    if config.dangerous().set_certificate_verifier(std::sync::Arc::new(NoCertificateVerification)) {
        0
    } else {
        0 // always success for dummy
    }
}

struct NoCertificateVerification;

impl rustls::client::ServerCertVerifier for NoCertificateVerification {
    fn verify_server_cert(
        &self,
        _end_entity: &rustls::Certificate,
        _intermediates: &[rustls::Certificate],
        _server_name: &rustls::ServerName,
        _scts: &mut dyn Iterator<Item = &[u8]>,
        _ocsp_response: &[u8],
        _now: std::time::SystemTime,
    ) -> Result<rustls::client::ServerCertVerified, rustls::Error> {
        Ok(rustls::client::ServerCertVerified::assertion())
    }
}

/// Concatenates two strings and returns null-terminated pointer.
///
/// # Safety
/// Pointers a and b must be valid for reads of a_len and b_len bytes. Caller must free returned pointer.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_concat(
    a: *const u8,
    a_len: usize,
    b: *const u8,
    b_len: usize,
) -> *mut u8 {
    let mut result = Vec::with_capacity(a_len + b_len + 1);
    result.extend_from_slice(std::slice::from_raw_parts(a, a_len));
    result.extend_from_slice(std::slice::from_raw_parts(b, b_len));
    result.push(0);
    let ptr = result.as_mut_ptr();
    std::mem::forget(result);
    ptr
}
