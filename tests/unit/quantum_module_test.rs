//! Test for quantum computing module integration

#[test]
fn test_quantum_module_compiles() {
    // Test that we can use the quantum module
    use zetac::std::quantum::*;
    
    // Create a complex number
    let c = Complex::new(1.0, 2.0);
    assert_eq!(c.re, 1.0);
    assert_eq!(c.im, 2.0);
    
    // Test complex number operations
    let c2 = Complex::new(3.0, 4.0);
    let sum = c + c2;
    assert_eq!(sum.re, 4.0);
    assert_eq!(sum.im, 6.0);
    
    // Test qubit states
    let zero_state = QubitState::zero();
    assert!(zero_state.is_normalized());
    
    // Test quantum state
    let state = QuantumState::new(2);
    assert_eq!(state.qubits, 2);
    assert_eq!(state.amplitudes.len(), 4); // 2^2 = 4
    
    // Test quantum circuit
    let circuit = QuantumCircuit::new(2);
    assert_eq!(circuit.qubits, 2);
    
    println!("Quantum module compiles and basic types work!");
}

#[test]
fn test_quantum_algorithms_module() {
    // Test that algorithms module exists
    use zetac::std::quantum::algorithms;
    
    // Test Shor's algorithm type exists
    let _shor = algorithms::ShorsAlgorithm::new(15);
    
    // Test Grover's algorithm type exists  
    let grover = algorithms::GroversAlgorithm::new(8, |x| x == 3);
    let iterations = grover.optimal_iterations();
    assert!(iterations > 0);
    
    println!("Quantum algorithms module works!");
}

#[test]
fn test_quantum_hybrid_module() {
    // Test that hybrid module exists
    use zetac::std::quantum::hybrid;
    
    // Test variational ansatz
    let ansatz = hybrid::VariationalAnsatz::new(2, 3);
    assert_eq!(ansatz.qubits, 2);
    assert_eq!(ansatz.layers, 3);
    assert_eq!(ansatz.parameter_count(), 6); // 2 * 3
    
    println!("Quantum hybrid module works!");
}