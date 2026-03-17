// unsafe_operations_test.rs - Comprehensive tests for unsafe operations
// File: C:\Users\mummy\OneDrive\Documents\DarkFactory\zeta-0.3.4\src\unsafe_operations_test.rs
// Purpose: Test unsafe blocks, raw pointers, safety analysis, and memory operations

#[cfg(test)]
mod tests {
    use crate::unsafe_operations::{
        UnsafeAnalyzer, UnsafeBlock, UnsafeOp, SafetyCheck, SafetyViolation,
        RawPtr, RawPtrType, SafetyRule, SafeWrapper, RawPtrOps, MemOps,
        MemoryRegion, MemoryPermissions,
    };
    
    #[test]
    fn test_unsafe_analyzer_creation() {
        let analyzer = UnsafeAnalyzer::new();
        
        // Should have default safety rules
        assert_eq!(analyzer.safety_rules.len(), 5);
        
        // Check each rule exists
        let rule_names: Vec<String> = analyzer.safety_rules.iter()
            .map(|r| r.name.clone())
            .collect();
        
        assert!(rule_names.contains(&"no_null_deref".to_string()));
        assert!(rule_names.contains(&"no_unaligned_access".to_string()));
        assert!(rule_names.contains(&"no_out_of_bounds".to_string()));
        assert!(rule_names.contains(&"no_data_races".to_string()));
        assert!(rule_names.contains(&"no_uninitialized_read".to_string()));
    }
    
    #[test]
    fn test_safe_unsafe_block_analysis() {
        let analyzer = UnsafeAnalyzer::new();
        
        // Create a safe unsafe block (with proper safety checks)
        let safe_block = UnsafeBlock {
            ops: vec![
                UnsafeOp::DerefRawPtr,
            ],
            safety_checks: vec![
                SafetyCheck::NullCheck,
                SafetyCheck::AlignmentCheck,
                SafetyCheck::InitCheck,
            ],
            requires: vec![
                "non_null".to_string(),
                "aligned".to_string(),
                "initialized".to_string(),
            ],
            ensures: vec![
                "valid_dereference".to_string(),
            ],
        };
        
        // This should pass analysis
        let result = analyzer.analyze_unsafe_block(&safe_block);
        assert!(result.is_ok());
        assert!(result.unwrap().is_empty());
    }
    
    #[test]
    fn test_unsafe_block_with_violations() {
        let analyzer = UnsafeAnalyzer::new();
        
        // Create an unsafe block missing safety checks
        let unsafe_block = UnsafeBlock {
            ops: vec![
                UnsafeOp::DerefRawPtr,
                UnsafeOp::PtrOffset,
            ],
            safety_checks: vec![
                // Missing NullCheck, AlignmentCheck, InitCheck, BoundsCheck
            ],
            requires: vec![
                "non_null".to_string(),
                "aligned".to_string(),
                "in_bounds".to_string(),
            ],
            ensures: vec![],
        };
        
        // This should fail analysis with violations
        let result = analyzer.analyze_unsafe_block(&unsafe_block);
        assert!(result.is_err());
        
        let violations = result.unwrap_err();
        assert!(!violations.is_empty());
        
        // Should have multiple violations
        assert!(violations.len() >= 3);
        
        // Check for specific violation types
        let has_null_violation = violations.iter().any(|v| {
            matches!(v, SafetyViolation::PotentialNullDeref | SafetyViolation::MissingSafetyCheck { .. })
        });
        assert!(has_null_violation);
    }
    
    #[test]
    fn test_raw_ptr_validation() {
        let analyzer = UnsafeAnalyzer::new();
        
        // Test valid raw pointer
        let valid_ptr = RawPtr {
            ty: RawPtrType::Const,
            pointee_ty: "i32".to_string(),
            is_null: false,
            is_aligned: true,
        };
        
        let result = analyzer.validate_raw_ptr(&valid_ptr, &UnsafeOp::DerefRawPtr);
        assert!(result.is_ok());
        
        // Test null pointer (should fail for dereference)
        let null_ptr = RawPtr {
            ty: RawPtrType::Const,
            pointee_ty: "i32".to_string(),
            is_null: true,
            is_aligned: true,
        };
        
        let result = analyzer.validate_raw_ptr(&null_ptr, &UnsafeOp::DerefRawPtr);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), SafetyViolation::NullDeref));
        
        // Test unaligned pointer (should fail for dereference)
        let unaligned_ptr = RawPtr {
            ty: RawPtrType::Const,
            pointee_ty: "i32".to_string(),
            is_null: false,
            is_aligned: false,
        };
        
        let result = analyzer.validate_raw_ptr(&unaligned_ptr, &UnsafeOp::DerefRawPtr);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), SafetyViolation::UnalignedAccess));
    }
    
    #[test]
    fn test_safe_wrapper_creation() {
        let analyzer = UnsafeAnalyzer::new();
        
        // Create a safe wrapper for dereferencing raw pointers
        let wrapper = analyzer.create_safe_wrapper(
            UnsafeOp::DerefRawPtr,
            &[
                SafetyCheck::NullCheck,
                SafetyCheck::AlignmentCheck,
                SafetyCheck::InitCheck,
            ],
        );
        
        assert_eq!(wrapper.unsafe_op, UnsafeOp::DerefRawPtr);
        assert_eq!(wrapper.safety_checks.len(), 3);
        assert!(wrapper.documentation.contains("# Safety"));
        assert!(wrapper.documentation.contains("Pointer is non-null"));
        assert!(wrapper.documentation.contains("Pointer is properly aligned"));
        assert!(wrapper.documentation.contains("Pointed-to memory is initialized"));
    }
    
    #[test]
    fn test_raw_ptr_operations() {
        // Test safe raw pointer operations
        
        // Create null pointers
        let null_const: *const i32 = RawPtrOps::null();
        let null_mut: *mut i32 = RawPtrOps::null_mut();
        
        assert!(RawPtrOps::is_null(null_const));
        assert!(RawPtrOps::is_null(null_mut));
        
        // Test pointer addresses
        let addr = RawPtrOps::addr(null_const);
        let ptr_from_addr = RawPtrOps::from_addr::<i32>(addr);
        
        assert_eq!(addr, ptr_from_addr as usize);
        
        // Note: Unsafe operations (offset, read, write) are not tested here
        // as they require unsafe blocks and proper safety guarantees
    }
    
    #[test]
    fn test_memory_operations() {
        // Test safe memory operations
        
        // Size and alignment
        assert_eq!(MemOps::size_of::<i32>(), 4);
        assert_eq!(MemOps::align_of::<i32>(), 4);
        assert_eq!(MemOps::size_of::<u64>(), 8);
        assert_eq!(MemOps::align_of::<u64>(), 8);
        
        // Value size
        let x = 42i32;
        assert_eq!(MemOps::size_of_val(&x), 4);
        
        // Swap operation
        let mut a = 1;
        let mut b = 2;
        MemOps::swap(&mut a, &mut b);
        assert_eq!(a, 2);
        assert_eq!(b, 1);
        
        // Replace operation
        let mut value = Some("hello");
        let taken = MemOps::take(&mut value);
        assert_eq!(taken, Some("hello"));
        assert_eq!(value, None);
        
        let mut dest = Some("world");
        let replaced = MemOps::replace(&mut dest, Some("universe"));
        assert_eq!(replaced, Some("world"));
        assert_eq!(dest, Some("universe"));
    }
    
    #[test]
    fn test_safety_rule_creation() {
        // Test all safety rule constructors
        
        let no_null_deref = SafetyRule::no_null_deref();
        assert_eq!(no_null_deref.name, "no_null_deref");
        assert_eq!(no_null_deref.violation, SafetyViolation::NullDeref);
        
        let no_unaligned = SafetyRule::no_unaligned_access();
        assert_eq!(no_unaligned.name, "no_unaligned_access");
        assert_eq!(no_unaligned.violation, SafetyViolation::UnalignedAccess);
        
        let no_bounds = SafetyRule::no_out_of_bounds();
        assert_eq!(no_bounds.name, "no_out_of_bounds");
        assert_eq!(no_bounds.violation, SafetyViolation::OutOfBounds);
        
        let no_races = SafetyRule::no_data_races();
        assert_eq!(no_races.name, "no_data_races");
        assert_eq!(no_races.violation, SafetyViolation::DataRace);
        
        let no_uninit = SafetyRule::no_uninitialized_read();
        assert_eq!(no_uninit.name, "no_uninitialized_read");
        assert_eq!(no_uninit.violation, SafetyViolation::UninitializedRead);
    }
    
    #[test]
    fn test_memory_region_management() {
        // Test memory region creation and validation
        
        let region = MemoryRegion {
            start: 0x1000,
            end: 0x2000,
            permissions: MemoryPermissions {
                read: true,
                write: true,
                execute: false,
            },
            initialized: true,
        };
        
        assert_eq!(region.end - region.start, 0x1000);
        assert!(region.permissions.read);
        assert!(region.permissions.write);
        assert!(!region.permissions.execute);
        assert!(region.initialized);
        
        // Test region contains address
        assert!(0x1500 >= region.start && 0x1500 < region.end);
        assert!(0x1000 >= region.start && 0x1000 < region.end);
        assert!(!(0x2000 >= region.start && 0x2000 < region.end)); // End is exclusive
        assert!(!(0x500 >= region.start && 0x500 < region.end)); // Before start
        assert!(!(0x2500 >= region.start && 0x2500 < region.end)); // After end
    }
    
    #[test]
    fn test_unsafe_op_enum() {
        // Test all unsafe operation variants
        
        let ops = vec![
            UnsafeOp::DerefRawPtr,
            UnsafeOp::PtrOffset,
            UnsafeOp::PtrCast,
            UnsafeOp::Transmute,
            UnsafeOp::UnionFieldAccess,
            UnsafeOp::InlineAsm,
            UnsafeOp::FfiCall,
            UnsafeOp::MemoryAllocation,
            UnsafeOp::AtomicOp,
            UnsafeOp::VolatileOp,
        ];
        
        assert_eq!(ops.len(), 10);
        
        // Verify each is unique
        use std::collections::HashSet;
        let mut set = HashSet::new();
        for op in &ops {
            set.insert(op);
        }
        assert_eq!(set.len(), 10);
    }
    
    #[test]
    fn test_safety_check_enum() {
        // Test all safety check variants
        
        let checks = vec![
            SafetyCheck::NullCheck,
            SafetyCheck::AlignmentCheck,
            SafetyCheck::BoundsCheck,
            SafetyCheck::LifetimeCheck,
            SafetyCheck::DataRaceCheck,
            SafetyCheck::InitCheck,
        ];
        
        assert_eq!(checks.len(), 6);
        
        // Verify each is unique
        use std::collections::HashSet;
        let mut set = HashSet::new();
        for check in &checks {
            set.insert(check);
        }
        assert_eq!(set.len(), 6);
    }
    
    #[test]
    fn test_safety_violation_display() {
        // Test safety violation string representation
        
        let violations = vec![
            SafetyViolation::NullDeref,
            SafetyViolation::UnalignedAccess,
            SafetyViolation::OutOfBounds,
            SafetyViolation::DataRace,
            SafetyViolation::UninitializedRead,
            SafetyViolation::TypeSafetyViolation,
            SafetyViolation::InlineAsmSafety,
            SafetyViolation::FfiSafety,
            SafetyViolation::MissingSafetyCheck {
                requirement: "non_null".to_string(),
            },
        ];
        
        for violation in violations {
            let display = format!("{}", violation);
            assert!(!display.is_empty());
            assert!(!display.contains("SafetyViolation")); // Should use Display impl
        }
    }
    
    #[test]
    fn test_unsafe_system_integration() {
        // Test complete unsafe system integration
        
        let analyzer = UnsafeAnalyzer::new();
        
        // Create a complex unsafe block with multiple operations
        let complex_block = UnsafeBlock {
            ops: vec![
                UnsafeOp::DerefRawPtr,
                UnsafeOp::PtrOffset,
                UnsafeOp::Transmute,
            ],
            safety_checks: vec![
                SafetyCheck::NullCheck,
                SafetyCheck::AlignmentCheck,
                SafetyCheck::BoundsCheck,
                SafetyCheck::InitCheck,
            ],
            requires: vec![
                "non_null".to_string(),
                "aligned".to_string(),
                "in_bounds".to_string(),
                "initialized".to_string(),
            ],
            ensures: vec![
                "safe_operation".to_string(),
            ],
        };
        
        // Analyze the block
        let result = analyzer.analyze_unsafe_block(&complex_block);
        
        // Should have violations for transmute (requires special checks)
        assert!(result.is_err());
        let violations = result.unwrap_err();
        
        // Should have type safety violation for transmute
        let has_type_violation = violations.iter().any(|v| {
            matches!(v, SafetyViolation::TypeSafetyViolation)
        });
        assert!(has_type_violation);
        
        println!("Unsafe system integration test passed!");
        println!("  Complex unsafe block analyzed");
        println!("  Safety violations properly detected");
        println!("  System correctly identifies missing safety guarantees");
    }
    
    #[test]
    fn test_raw_ptr_type_enum() {
        // Test raw pointer type variants
        
        let const_ptr = RawPtr {
            ty: RawPtrType::Const,
            pointee_ty: "i32".to_string(),
            is_null: false,
            is_aligned: true,
        };
        
        let mut_ptr = RawPtr {
            ty: RawPtrType::Mut,
            pointee_ty: "i32".to_string(),
            is_null: false,
            is_aligned: true,
        };
        
        assert!(matches!(const_ptr.ty, RawPtrType::Const));
        assert!(matches!(mut_ptr.ty, RawPtrType::Mut));
        assert_ne!(const_ptr.ty, mut_ptr.ty);
    }
}