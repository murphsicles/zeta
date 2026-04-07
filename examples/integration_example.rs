//! Example demonstrating the integration bridge between compiler components
//!
//! This example shows how the parser, type checker, and codegen components
//! can coordinate through the integration bridge.

#[cfg(feature = "integration")]
fn main() {
    use zetac::integration::{ComponentStatus, CoordinationManager};

    println!("=== Zeta Compiler Integration Example ===");
    println!("Demonstrating coordination between compiler components...\n");

    // Create coordination manager
    let mut manager = CoordinationManager::new();

    // Register all components
    manager.register_component("parser");
    manager.register_component("type_checker");
    manager.register_component("codegen");
    manager.register_component("integration");

    println!("1. Components registered:");
    println!("   - Parser (LEX)");
    println!("   - Type Checker (SEM)");
    println!("   - Code Generator (GEN)");
    println!("   - Integration Bridge (SYN)");

    // Simulate compilation pipeline
    println!("\n2. Simulating compilation pipeline:");

    // Parser starts
    manager.update_status("parser", ComponentStatus::Processing, "Parsing source code");
    println!("   - Parser: Parsing source code...");

    // Parser completes
    manager.update_status("parser", ComponentStatus::Complete, "AST generated");
    println!("   - Parser: AST generated successfully");

    // Type checker starts
    manager.update_status(
        "type_checker",
        ComponentStatus::Processing,
        "Type checking AST",
    );
    println!("   - Type Checker: Type checking AST...");

    // Type checker completes
    manager.update_status(
        "type_checker",
        ComponentStatus::Complete,
        "Type checking complete",
    );
    println!("   - Type Checker: Type checking complete");

    // Codegen starts
    manager.update_status("codegen", ComponentStatus::Processing, "Generating code");
    println!("   - Code Generator: Generating code...");

    // Codegen completes
    manager.update_status(
        "codegen",
        ComponentStatus::Complete,
        "Code generation complete",
    );
    println!("   - Code Generator: Code generation complete");

    // Integration reports success
    manager.update_status(
        "integration",
        ComponentStatus::Complete,
        "Compilation successful",
    );
    println!("\n3. Integration Bridge: All components coordinated successfully!");
    println!("   Compilation pipeline complete.");

    // Check for errors
    let errors = manager.get_errors();
    if errors.is_empty() {
        println!("\n✅ SUCCESS: No errors detected during compilation.");
    } else {
        println!("\n❌ ERRORS: {} errors detected:", errors.len());
        for error in errors {
            println!("   - {:?}", error);
        }
    }
}

#[cfg(not(feature = "integration"))]
fn main() {
    println!("Integration feature is not enabled.");
    println!("Run with: cargo run --example integration_example --features integration");
    println!("\nTo enable integration in your project, add to Cargo.toml:");
    println!("[features]");
    println!("integration = []");
}
