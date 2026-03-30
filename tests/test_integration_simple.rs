// Simple test to verify integration module compiles
// This test should be run with: cargo test --features integration

#[cfg(feature = "integration")]
mod test_integration {
    use zetac::integration::{CoordinationManager, GenericIntegration};

    #[test]
    fn test_integration_creation() {
        // Test that we can create integration components
        let integration = GenericIntegration::new();
        assert!(!integration.has_errors());

        let _manager = CoordinationManager::new();
        // Just creating it is enough for this test
    }

    #[test]
    fn test_coordination_protocols() {
        use zetac::integration::ComponentStatus;

        let mut manager = CoordinationManager::new();

        // Test component registration
        manager.register_component("parser");
        manager.register_component("type_checker");
        manager.register_component("codegen");
        manager.register_component("integration");

        // Test status update
        manager.update_status("parser", ComponentStatus::Processing, "Parsing source");
    }
}

#[cfg(not(feature = "integration"))]
mod test_integration {
    #[test]
    fn test_integration_disabled() {
        // When integration feature is disabled, we should still compile
        assert!(true, "Integration feature is disabled as expected");
    }
}
