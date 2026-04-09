//! Memory region stub

pub fn register_allocation(_size: usize, _region_id: u64) -> Result<(), ()> {
    Ok(())
}

pub fn unregister_allocation(_region_id: u64) -> Result<(), ()> {
    Ok(())
}

pub fn root_region_id() -> u64 {
    0
}

pub fn validate_region_active(_region_id: u64) -> bool {
    true
}