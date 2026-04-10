//! SIMD type definitions and operations for Zeta
//!
//! This module provides SIMD vector type definitions, type aliases, and
//! type-level operations for SIMD programming.

use super::{ArraySize, Type};

/// SIMD vector type information
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SimdTypeInfo {
    /// Element type (e.g., i32, f64)
    pub element_type: Type,
    /// Number of lanes (elements) in the vector
    pub lane_count: usize,
    /// Total bit width of the vector
    pub bit_width: usize,
}

impl SimdTypeInfo {
    /// Create a new SIMD type info
    pub fn new(element_type: Type, lane_count: usize) -> Option<Self> {
        let element_bits = match element_type {
            Type::I8 | Type::U8 => 8,
            Type::I16 | Type::U16 => 16,
            Type::I32 | Type::U32 | Type::F32 => 32,
            Type::I64 | Type::U64 | Type::F64 => 64,
            _ => return None,
        };
        
        let bit_width = lane_count * element_bits;
        
        // Validate SIMD constraints
        if !lane_count.is_power_of_two() || lane_count > 64 {
            return None;
        }
        
        Some(SimdTypeInfo {
            element_type,
            lane_count,
            bit_width,
        })
    }
    
    /// Check if this SIMD type requires AVX-512
    pub fn requires_avx512(&self) -> bool {
        self.bit_width > 256
    }
    
    /// Check if this SIMD type is supported on the current hardware
    pub fn is_supported(&self) -> bool {
        // TODO: Implement CPU feature detection
        // For now, assume all types are supported
        true
    }
    
    /// Get the corresponding Zeta type
    pub fn to_type(&self) -> Type {
        Type::Vector(
            Box::new(self.element_type.clone()),
            ArraySize::Literal(self.lane_count),
        )
    }
    
    /// Create a SIMD type from a Zeta type
    pub fn from_type(ty: &Type) -> Option<Self> {
        match ty {
            Type::Vector(element_type, size) => {
                let lane_count = match size {
                    ArraySize::Literal(n) => *n,
                    _ => return None,
                };
                
                SimdTypeInfo::new(**element_type.clone(), lane_count)
            }
            _ => None,
        }
    }
}

/// Common SIMD type aliases
pub mod aliases {
    use super::{ArraySize, Type};
    
    /// 8-element vector of u64 (512 bits)
    pub fn u64x8() -> Type {
        Type::Vector(Box::new(Type::U64), ArraySize::Literal(8))
    }
    
    /// 4-element vector of f32 (128 bits)
    pub fn f32x4() -> Type {
        Type::Vector(Box::new(Type::F32), ArraySize::Literal(4))
    }
    
    /// 8-element vector of f32 (256 bits)
    pub fn f32x8() -> Type {
        Type::Vector(Box::new(Type::F32), ArraySize::Literal(8))
    }
    
    /// 16-element vector of f32 (512 bits)
    pub fn f32x16() -> Type {
        Type::Vector(Box::new(Type::F32), ArraySize::Literal(16))
    }
    
    /// 4-element vector of f64 (256 bits)
    pub fn f64x4() -> Type {
        Type::Vector(Box::new(Type::F64), ArraySize::Literal(4))
    }
    
    /// 8-element vector of f64 (512 bits)
    pub fn f64x8() -> Type {
        Type::Vector(Box::new(Type::F64), ArraySize::Literal(8))
    }
    
    /// 8-element vector of i32 (256 bits)
    pub fn i32x8() -> Type {
        Type::Vector(Box::new(Type::I32), ArraySize::Literal(8))
    }
    
    /// 16-element vector of i32 (512 bits)
    pub fn i32x16() -> Type {
        Type::Vector(Box::new(Type::I32), ArraySize::Literal(16))
    }
    
    /// 4-element vector of i64 (256 bits)
    pub fn i64x4() -> Type {
        Type::Vector(Box::new(Type::I64), ArraySize::Literal(4))
    }
    
    /// 8-element vector of i64 (512 bits)
    pub fn i64x8() -> Type {
        Type::Vector(Box::new(Type::I64), ArraySize::Literal(8))
    }
}

/// SIMD operation traits
pub mod operations {
    use super::SimdTypeInfo;
    use super::Type;
    
    /// Trait for SIMD arithmetic operations
    pub trait SimdArithmetic {
        /// Check if addition is supported
        fn supports_add(&self) -> bool;
        
        /// Check if subtraction is supported
        fn supports_sub(&self) -> bool;
        
        /// Check if multiplication is supported
        fn supports_mul(&self) -> bool;
        
        /// Check if division is supported
        fn supports_div(&self) -> bool;
    }
    
    /// Trait for SIMD bitwise operations
    pub trait SimdBitwise {
        /// Check if bitwise AND is supported
        fn supports_and(&self) -> bool;
        
        /// Check if bitwise OR is supported
        fn supports_or(&self) -> bool;
        
        /// Check if bitwise XOR is supported
        fn supports_xor(&self) -> bool;
        
        /// Check if shift operations are supported
        fn supports_shift(&self) -> bool;
    }
    
    /// Trait for SIMD comparison operations
    pub trait SimdComparison {
        /// Check if equality comparison is supported
        fn supports_eq(&self) -> bool;
        
        /// Check if inequality comparison is supported
        fn supports_ne(&self) -> bool;
        
        /// Check if greater-than comparison is supported
        fn supports_gt(&self) -> bool;
        
        /// Check if less-than comparison is supported
        fn supports_lt(&self) -> bool;
    }
    
    /// Trait for SIMD reduction operations
    pub trait SimdReduction {
        /// Check if horizontal addition is supported
        fn supports_hadd(&self) -> bool;
        
        /// Check if horizontal minimum is supported
        fn supports_hmin(&self) -> bool;
        
        /// Check if horizontal maximum is supported
        fn supports_hmax(&self) -> bool;
    }
    
    impl SimdArithmetic for SimdTypeInfo {
        fn supports_add(&self) -> bool {
            // All SIMD types support addition
            true
        }
        
        fn supports_sub(&self) -> bool {
            // All SIMD types support subtraction
            true
        }
        
        fn supports_mul(&self) -> bool {
            // Most SIMD types support multiplication
            // Some older architectures might not support certain types
            true
        }
        
        fn supports_div(&self) -> bool {
            // Floating-point SIMD types support division
            // Integer division is less common in SIMD
            matches!(
                self.element_type,
                Type::F32 | Type::F64
            )
        }
    }
    
    impl SimdBitwise for SimdTypeInfo {
        fn supports_and(&self) -> bool {
            // Integer SIMD types support bitwise operations
            !matches!(
                self.element_type,
                Type::F32 | Type::F64
            )
        }
        
        fn supports_or(&self) -> bool {
            // Integer SIMD types support bitwise operations
            !matches!(
                self.element_type,
                Type::F32 | Type::F64
            )
        }
        
        fn supports_xor(&self) -> bool {
            // Integer SIMD types support bitwise operations
            !matches!(
                self.element_type,
                Type::F32 | Type::F64
            )
        }
        
        fn supports_shift(&self) -> bool {
            // Integer SIMD types support shift operations
            !matches!(
                self.element_type,
                Type::F32 | Type::F64
            )
        }
    }
    
    impl SimdComparison for SimdTypeInfo {
        fn supports_eq(&self) -> bool {
            // All SIMD types support equality comparison
            true
        }
        
        fn supports_ne(&self) -> bool {
            // All SIMD types support inequality comparison
            true
        }
        
        fn supports_gt(&self) -> bool {
            // All SIMD types support greater-than comparison
            true
        }
        
        fn supports_lt(&self) -> bool {
            // All SIMD types support less-than comparison
            true
        }
    }
    
    impl SimdReduction for SimdTypeInfo {
        fn supports_hadd(&self) -> bool {
            // Most SIMD architectures support horizontal addition
            true
        }
        
        fn supports_hmin(&self) -> bool {
            // Most SIMD architectures support horizontal minimum
            true
        }
        
        fn supports_hmax(&self) -> bool {
            // Most SIMD architectures support horizontal maximum
            true
        }
    }
}

/// SIMD mask type for conditional operations
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SimdMask {
    /// Number of lanes in the mask
    pub lane_count: usize,
    /// Bit width of the mask (usually 8, 16, 32, or 64 bits)
    pub bit_width: usize,
}

impl SimdMask {
    /// Create a new SIMD mask
    pub fn new(lane_count: usize) -> Self {
        // AVX-512 uses 8-bit masks for up to 8 lanes, 16-bit for 16 lanes, etc.
        let bit_width = if lane_count <= 8 {
            8
        } else if lane_count <= 16 {
            16
        } else if lane_count <= 32 {
            32
        } else {
            64
        };
        
        SimdMask {
            lane_count,
            bit_width,
        }
    }
    
    /// Create a mask for a SIMD vector type
    pub fn for_simd_type(info: &SimdTypeInfo) -> Self {
        SimdMask::new(info.lane_count)
    }
}

/// SIMD memory operations
pub mod memory {
    use super::SimdTypeInfo;
    
    /// Memory alignment requirements
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum Alignment {
        /// Unaligned access (slower but more flexible)
        Unaligned,
        /// Aligned to element size
        Element,
        /// Aligned to vector size (best performance)
        Vector,
    }
    
    /// Memory access pattern
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum AccessPattern {
        /// Contiguous memory access
        Contiguous,
        /// Strided memory access
        Strided(usize),
        /// Gather/scatter (random access)
        ScatterGather,
    }
    
    /// Check if a SIMD type supports aligned loads
    pub fn supports_aligned_load(info: &SimdTypeInfo) -> bool {
        // All SIMD types support aligned loads
        true
    }
    
    /// Check if a SIMD type supports unaligned loads
    pub fn supports_unaligned_load(info: &SimdTypeInfo) -> bool {
        // Most modern SIMD architectures support unaligned loads
        // (though they may be slower)
        true
    }
    
    /// Check if a SIMD type supports gather operations
    pub fn supports_gather(info: &SimdTypeInfo) -> bool {
        // Gather requires AVX-512 or similar
        info.requires_avx512()
    }
    
    /// Check if a SIMD type supports scatter operations
    pub fn supports_scatter(info: &SimdTypeInfo) -> bool {
        // Scatter requires AVX-512 or similar
        info.requires_avx512()
    }
    
    /// Get recommended alignment for a SIMD type
    pub fn recommended_alignment(info: &SimdTypeInfo) -> Alignment {
        if info.bit_width >= 256 {
            // Wide vectors benefit more from alignment
            Alignment::Vector
        } else {
            Alignment::Element
        }
    }
}

/// SIMD type compatibility checking
pub mod compatibility {
    use super::SimdTypeInfo;
    use super::Type;
    
    /// Check if two SIMD types are compatible for binary operations
    pub fn are_compatible(a: &SimdTypeInfo, b: &SimdTypeInfo) -> bool {
        a.element_type == b.element_type && a.lane_count == b.lane_count
    }
    
    /// Check if a SIMD type can be cast to another
    pub fn can_cast(from: &SimdTypeInfo, to: &SimdTypeInfo) -> bool {
        // Same lane count required
        if from.lane_count != to.lane_count {
            return false;
        }
        
        // Check if cast is valid
        match (&from.element_type, &to.element_type) {
            // Integer to integer (with same or different signedness)
            (Type::I8, Type::I8) | (Type::I16, Type::I16) |
            (Type::I32, Type::I32) | (Type::I64, Type::I64) |
            (Type::U8, Type::U8) | (Type::U16, Type::U16) |
            (Type::U32, Type::U32) | (Type::U64, Type::U64) |
            // Integer to floating-point
            (Type::I32, Type::F32) | (Type::I64, Type::F64) |
            (Type::U32, Type::F32) | (Type::U64, Type::F64) |
            // Floating-point to integer (with truncation)
            (Type::F32, Type::I32) | (Type::F64, Type::I64) |
            (Type::F32, Type::U32) | (Type::F64, Type::U64) |
            // Floating-point precision change
            (Type::F32, Type::F64) | (Type::F64, Type::F32) => true,
            
            _ => false,
        }
    }
    
    /// Get the common type for two SIMD types
    pub fn common_type(a: &SimdTypeInfo, b: &SimdTypeInfo) -> Option<SimdTypeInfo> {
        if a.lane_count != b.lane_count {
            return None;
        }
        
        // For now, require exact match
        // TODO: Implement type promotion rules
        if a.element_type == b.element_type {
            Some(a.clone())
        } else {
            None
        }
    }
}