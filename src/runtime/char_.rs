//! Character predicates runtime — wraps Rust std::char

#[unsafe(no_mangle)]
pub unsafe extern "C" fn char_is_digit(ch: i64) -> i64 {
    char::from_u32(ch as u32).map(|c| c.is_digit(10) as i64).unwrap_or(0)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn char_is_alphabetic(ch: i64) -> i64 {
    char::from_u32(ch as u32).map(|c| c.is_alphabetic() as i64).unwrap_or(0)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn char_is_alphanumeric(ch: i64) -> i64 {
    char::from_u32(ch as u32).map(|c| c.is_alphanumeric() as i64).unwrap_or(0)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn char_is_lowercase(ch: i64) -> i64 {
    char::from_u32(ch as u32).map(|c| c.is_lowercase() as i64).unwrap_or(0)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn char_is_uppercase(ch: i64) -> i64 {
    char::from_u32(ch as u32).map(|c| c.is_uppercase() as i64).unwrap_or(0)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn char_is_whitespace(ch: i64) -> i64 {
    char::from_u32(ch as u32).map(|c| c.is_whitespace() as i64).unwrap_or(0)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn char_to_lowercase(ch: i64) -> i64 {
    char::from_u32(ch as u32).map(|c| c.to_lowercase().next().unwrap_or(c) as i64).unwrap_or(0)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn char_to_uppercase(ch: i64) -> i64 {
    char::from_u32(ch as u32).map(|c| c.to_uppercase().next().unwrap_or(c) as i64).unwrap_or(0)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn char_from_u32(code: i64) -> i64 {
    char::from_u32(code as u32).map(|c| c as i64).unwrap_or(0)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn char_to_digit(ch: i64, radix: i64) -> i64 {
    char::from_u32(ch as u32).and_then(|c| c.to_digit(radix as u32)).map(|d| d as i64).unwrap_or(-1)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn char_is_control(ch: i64) -> i64 {
    char::from_u32(ch as u32).map(|c| c.is_control() as i64).unwrap_or(0)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn char_is_numeric(ch: i64) -> i64 {
    char::from_u32(ch as u32).map(|c| c.is_numeric() as i64).unwrap_or(0)
}
