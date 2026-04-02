//! Math module for Zeta standard library.
//! 
//! Provides mathematical operations and constants:
//! - Mathematical constants (PI, E, etc.)
//! - Trigonometric functions
//! - Exponential and logarithmic functions
//! - Power and root functions
//! - Hyperbolic functions
//! - Special functions

use std::f64::consts;

/// Initializes the math module.
pub fn init() {
    println!("Math module initialized");
}

/// Registers math functions with the runtime.
pub fn register_functions(map: &mut std::collections::HashMap<&'static str, usize>) {
    // Constants
    map.insert("math_pi", math_pi as *const () as usize);
    map.insert("math_e", math_e as *const () as usize);
    map.insert("math_tau", math_tau as *const () as usize);
    map.insert("math_ln2", math_ln2 as *const () as usize);
    map.insert("math_ln10", math_ln10 as *const () as usize);
    map.insert("math_log2_e", math_log2_e as *const () as usize);
    map.insert("math_log10_e", math_log10_e as *const () as usize);
    map.insert("math_sqrt2", math_sqrt2 as *const () as usize);
    map.insert("math_sqrt3", math_sqrt3 as *const () as usize);
    
    // Trigonometric functions
    map.insert("math_sin", math_sin as *const () as usize);
    map.insert("math_cos", math_cos as *const () as usize);
    map.insert("math_tan", math_tan as *const () as usize);
    map.insert("math_asin", math_asin as *const () as usize);
    map.insert("math_acos", math_acos as *const () as usize);
    map.insert("math_atan", math_atan as *const () as usize);
    map.insert("math_atan2", math_atan2 as *const () as usize);
    
    // Exponential and logarithmic functions
    map.insert("math_exp", math_exp as *const () as usize);
    map.insert("math_exp2", math_exp2 as *const () as usize);
    map.insert("math_ln", math_ln as *const () as usize);
    map.insert("math_log10", math_log10 as *const () as usize);
    map.insert("math_log2", math_log2 as *const () as usize);
    map.insert("math_log", math_log as *const () as usize);
    
    // Power and root functions
    map.insert("math_pow", math_pow as *const () as usize);
    map.insert("math_sqrt", math_sqrt as *const () as usize);
    map.insert("math_cbrt", math_cbrt as *const () as usize);
    map.insert("math_hypot", math_hypot as *const () as usize);
    
    // Hyperbolic functions
    map.insert("math_sinh", math_sinh as *const () as usize);
    map.insert("math_cosh", math_cosh as *const () as usize);
    map.insert("math_tanh", math_tanh as *const () as usize);
    map.insert("math_asinh", math_asinh as *const () as usize);
    map.insert("math_acosh", math_acosh as *const () as usize);
    map.insert("math_atanh", math_atanh as *const () as usize);
    
    // Special functions
    map.insert("math_abs", math_abs as *const () as usize);
    map.insert("math_floor", math_floor as *const () as usize);
    map.insert("math_ceil", math_ceil as *const () as usize);
    map.insert("math_round", math_round as *const () as usize);
    map.insert("math_trunc", math_trunc as *const () as usize);
    map.insert("math_fract", math_fract as *const () as usize);
    map.insert("math_signum", math_signum as *const () as usize);
    
    // PrimeZeta specific math functions
    map.insert("math_gcd", math_gcd as *const () as usize);
    map.insert("math_lcm", math_lcm as *const () as usize);
    map.insert("math_is_prime", math_is_prime as *const () as usize);
    map.insert("math_next_prime", math_next_prime as *const () as usize);
    map.insert("math_prime_factors", math_prime_factors as *const () as usize);
}

// ============================================================================
// Mathematical Constants
// ============================================================================

/// Gets the value of π (pi).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_pi() -> f64 {
    consts::PI
}

/// Gets the value of e (Euler's number).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_e() -> f64 {
    consts::E
}

/// Gets the value of τ (tau, 2π).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_tau() -> f64 {
    consts::TAU
}

/// Gets the natural logarithm of 2.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_ln2() -> f64 {
    consts::LN_2
}

/// Gets the natural logarithm of 10.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_ln10() -> f64 {
    consts::LN_10
}

/// Gets the base-2 logarithm of e.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_log2_e() -> f64 {
    consts::LOG2_E
}

/// Gets the base-10 logarithm of e.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_log10_e() -> f64 {
    consts::LOG10_E
}

/// Gets the square root of 2.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_sqrt2() -> f64 {
    consts::SQRT_2
}

/// Gets the square root of 3.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_sqrt3() -> f64 {
    // Not in std::f64::consts, calculate it
    3.0_f64.sqrt()
}

// ============================================================================
// Trigonometric Functions
// ============================================================================

/// Computes the sine of an angle (in radians).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_sin(x: f64) -> f64 {
    x.sin()
}

/// Computes the cosine of an angle (in radians).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_cos(x: f64) -> f64 {
    x.cos()
}

/// Computes the tangent of an angle (in radians).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_tan(x: f64) -> f64 {
    x.tan()
}

/// Computes the arcsine of a number.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_asin(x: f64) -> f64 {
    x.asin()
}

/// Computes the arccosine of a number.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_acos(x: f64) -> f64 {
    x.acos()
}

/// Computes the arctangent of a number.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_atan(x: f64) -> f64 {
    x.atan()
}

/// Computes the arctangent of y/x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_atan2(y: f64, x: f64) -> f64 {
    y.atan2(x)
}

// ============================================================================
// Exponential and Logarithmic Functions
// ============================================================================

/// Computes e raised to the power x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_exp(x: f64) -> f64 {
    x.exp()
}

/// Computes 2 raised to the power x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_exp2(x: f64) -> f64 {
    x.exp2()
}

/// Computes the natural logarithm of x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_ln(x: f64) -> f64 {
    x.ln()
}

/// Computes the base-10 logarithm of x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_log10(x: f64) -> f64 {
    x.log10()
}

/// Computes the base-2 logarithm of x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_log2(x: f64) -> f64 {
    x.log2()
}

/// Computes the logarithm of x with base b.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_log(x: f64, b: f64) -> f64 {
    x.log(b)
}

// ============================================================================
// Power and Root Functions
// ============================================================================

/// Computes x raised to the power y.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_pow(x: f64, y: f64) -> f64 {
    x.powf(y)
}

/// Computes the square root of x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_sqrt(x: f64) -> f64 {
    x.sqrt()
}

/// Computes the cube root of x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_cbrt(x: f64) -> f64 {
    x.cbrt()
}

/// Computes sqrt(x² + y²).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_hypot(x: f64, y: f64) -> f64 {
    x.hypot(y)
}

// ============================================================================
// Hyperbolic Functions
// ============================================================================

/// Computes the hyperbolic sine of x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_sinh(x: f64) -> f64 {
    x.sinh()
}

/// Computes the hyperbolic cosine of x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_cosh(x: f64) -> f64 {
    x.cosh()
}

/// Computes the hyperbolic tangent of x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_tanh(x: f64) -> f64 {
    x.tanh()
}

/// Computes the inverse hyperbolic sine of x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_asinh(x: f64) -> f64 {
    x.asinh()
}

/// Computes the inverse hyperbolic cosine of x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_acosh(x: f64) -> f64 {
    x.acosh()
}

/// Computes the inverse hyperbolic tangent of x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_atanh(x: f64) -> f64 {
    x.atanh()
}

// ============================================================================
// Special Functions
// ============================================================================

/// Computes the absolute value of x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_abs(x: f64) -> f64 {
    x.abs()
}

/// Computes the largest integer less than or equal to x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_floor(x: f64) -> f64 {
    x.floor()
}

/// Computes the smallest integer greater than or equal to x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_ceil(x: f64) -> f64 {
    x.ceil()
}

/// Computes the nearest integer to x, rounding half-way cases away from zero.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_round(x: f64) -> f64 {
    x.round()
}

/// Computes the integer part of x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_trunc(x: f64) -> f64 {
    x.trunc()
}

/// Computes the fractional part of x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_fract(x: f64) -> f64 {
    x.fract()
}

/// Computes the sign of x.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_signum(x: f64) -> f64 {
    x.signum()
}

// ============================================================================
// PrimeZeta Specific Math Functions
// ============================================================================

/// Computes the greatest common divisor of a and b.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_gcd(a: i64, b: i64) -> i64 {
    let mut x = a.abs();
    let mut y = b.abs();
    
    while y != 0 {
        let temp = y;
        y = x % y;
        x = temp;
    }
    
    x
}

/// Computes the least common multiple of a and b.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_lcm(a: i64, b: i64) -> i64 {
    if a == 0 || b == 0 {
        return 0;
    }
    
    let gcd = unsafe { math_gcd(a, b) };
    (a / gcd).abs() * b.abs()
}

/// Checks if a number is prime.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_is_prime(n: i64) -> bool {
    if n <= 1 {
        return false;
    }
    if n <= 3 {
        return true;
    }
    if n % 2 == 0 || n % 3 == 0 {
        return false;
    }
    
    let mut i = 5;
    while i * i <= n {
        if n % i == 0 || n % (i + 2) == 0 {
            return false;
        }
        i += 6;
    }
    
    true
}

/// Finds the next prime number greater than n.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_next_prime(n: i64) -> i64 {
    let mut candidate = if n < 2 { 2 } else { n + 1 };
    
    loop {
        if unsafe { math_is_prime(candidate) } {
            return candidate;
        }
        candidate += 1;
    }
}

/// Gets the prime factors of a number.
/// 
/// # Safety
/// Returns a pointer to an array of i64 pairs (prime, exponent).
/// The array is terminated by a pair of zeros.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn math_prime_factors(n: i64) -> *mut i64 {
    let mut num = n.abs();
    if num <= 1 {
        // Return array with just the terminator
        let factors: Vec<i64> = vec![0, 0];
        let boxed = factors.into_boxed_slice();
        let ptr = boxed.as_ptr() as *mut i64;
        std::mem::forget(boxed);
        return ptr;
    }
    
    let mut factors = Vec::new();
    let mut divisor = 2;
    
    while divisor * divisor <= num {
        if num % divisor == 0 {
            let mut count = 0;
            while num % divisor == 0 {
                num /= divisor;
                count += 1;
            }
            factors.push(divisor);
            factors.push(count);
        }
        divisor += if divisor == 2 { 1 } else { 2 };
    }
    
    if num > 1 {
        factors.push(num);
        factors.push(1);
    }
    
    // Add terminator
    factors.push(0);
    factors.push(0);
    
    let boxed = factors.into_boxed_slice();
    let ptr = boxed.as_ptr() as *mut i64;
    std::mem::forget(boxed);
    ptr
}