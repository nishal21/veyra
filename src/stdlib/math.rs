//! Math Operations
//! 
//! Mathematical functions and constants.

/// Mathematical constant PI
pub const PI: f64 = std::f64::consts::PI;

/// Mathematical constant E
pub const E: f64 = std::f64::consts::E;

/// Square root
pub fn sqrt(x: f64) -> f64 {
    x.sqrt()
}

/// Power function
pub fn pow(base: f64, exp: f64) -> f64 {
    base.powf(exp)
}

/// Absolute value
pub fn abs(x: f64) -> f64 {
    x.abs()
}

/// Floor
pub fn floor(x: f64) -> i64 {
    x.floor() as i64
}

/// Ceiling
pub fn ceil(x: f64) -> i64 {
    x.ceil() as i64
}

/// Round
pub fn round(x: f64) -> i64 {
    x.round() as i64
}

/// Sine
pub fn sin(x: f64) -> f64 {
    x.sin()
}

/// Cosine
pub fn cos(x: f64) -> f64 {
    x.cos()
}

/// Tangent
pub fn tan(x: f64) -> f64 {
    x.tan()
}

/// Arc sine
pub fn asin(x: f64) -> f64 {
    x.asin()
}

/// Arc cosine
pub fn acos(x: f64) -> f64 {
    x.acos()
}

/// Arc tangent
pub fn atan(x: f64) -> f64 {
    x.atan()
}

/// Arc tangent with two arguments
pub fn atan2(y: f64, x: f64) -> f64 {
    y.atan2(x)
}

/// Natural logarithm
pub fn ln(x: f64) -> f64 {
    x.ln()
}

/// Base-10 logarithm
pub fn log10(x: f64) -> f64 {
    x.log10()
}

/// Base-2 logarithm
pub fn log2(x: f64) -> f64 {
    x.log2()
}

/// Exponential function
pub fn exp(x: f64) -> f64 {
    x.exp()
}

/// Minimum of two values
pub fn min(a: f64, b: f64) -> f64 {
    a.min(b)
}

/// Maximum of two values
pub fn max(a: f64, b: f64) -> f64 {
    a.max(b)
}

/// Clamp value between min and max
pub fn clamp(x: f64, min_val: f64, max_val: f64) -> f64 {
    x.max(min_val).min(max_val)
}

/// Random number between 0 and 1
pub fn random() -> f64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    let seed = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    
    // Simple LCG random
    let a: u128 = 6364136223846793005;
    let c: u128 = 1442695040888963407;
    let m: u128 = 1 << 64;
    let result = (a.wrapping_mul(seed).wrapping_add(c)) % m;
    (result as f64) / (m as f64)
}

/// Random integer between min and max (inclusive)
pub fn random_int(min: i64, max: i64) -> i64 {
    let r = random();
    min + ((r * ((max - min + 1) as f64)) as i64)
}

/// Hyperbolic sine
pub fn sinh(x: f64) -> f64 {
    x.sinh()
}

/// Hyperbolic cosine
pub fn cosh(x: f64) -> f64 {
    x.cosh()
}

/// Hyperbolic tangent
pub fn tanh(x: f64) -> f64 {
    x.tanh()
}

/// Check if value is NaN
pub fn is_nan(x: f64) -> bool {
    x.is_nan()
}

/// Check if value is infinite
pub fn is_infinite(x: f64) -> bool {
    x.is_infinite()
}

/// Sign of value (-1, 0, or 1)
pub fn sign(x: f64) -> i64 {
    if x > 0.0 {
        1
    } else if x < 0.0 {
        -1
    } else {
        0
    }
}
