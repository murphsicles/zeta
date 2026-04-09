//! Memory capability stub

pub struct MemoryCapability;

pub const READ_RIGHT: u8 = 1;
pub const WRITE_RIGHT: u8 = 2;
pub const FREE_RIGHT: u8 = 4;
pub const RESIZE_RIGHT: u8 = 8;

pub fn create_capability() -> MemoryCapability {
    MemoryCapability
}

pub fn free_capability(_cap: MemoryCapability) {}