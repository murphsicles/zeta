//! Stub for std::ffi::c_void
//! c_void is used for raw pointers to opaque C data

/// Opaque type representing a void pointer in C
#[repr(u8)]
pub enum c_void {
    /// Variant 1
    #[doc(hidden)]
    __variant1,
    /// Variant 2  
    #[doc(hidden)]
    __variant2,
}