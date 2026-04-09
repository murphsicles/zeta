//! Memory error stub

pub type MemoryResult<T> = Result<T, MemoryError>;

#[derive(Debug)]
pub enum MemoryError {
    AllocationFailed,
    InvalidRegion,
    InsufficientRights,
    ZeroSize,
    AlignmentError,
    InvalidCapability,
}