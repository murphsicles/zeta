//! Smoke test for ML module
//! 
//! Quick test to verify basic functionality

#[test]
fn test_ml_module_smoke() {
    // Just verify we can compile and use the ML module
    use openclaw::ml::tensor::Tensor;
    
    // Create a tensor
    let t = Tensor::new(vec![1.0, 2.0, 3.0, 4.0], vec![2, 2], false);
    assert_eq!(t.shape(), &[2, 2]);
    assert_eq!(t.numel(), 4);
    
    // Test addition
    let t2 = Tensor::new(vec![5.0, 6.0, 7.0, 8.0], vec![2, 2], false);
    let sum = Tensor::add(&t, &t2);
    assert_eq!(sum.data(), &[6.0, 8.0, 10.0, 12.0]);
    
    println!("ML module smoke test passed!");
}

#[test]
fn test_neural_network_smoke() {
    use openclaw::ml::nn::Dense;
    use openclaw::ml::tensor::Tensor;
    
    // Create a dense layer
    let dense = Dense::new(10, 5, true);
    
    // Create input
    let input = Tensor::rand(vec![2, 10], false); // batch of 2
    
    // Forward pass
    let output = dense.forward(&input);
    assert_eq!(output.shape(), &[2, 5]);
    
    println!("Neural network smoke test passed!");
}

#[test]
fn test_training_smoke() {
    use openclaw::ml::training::MSELoss;
    use openclaw::ml::tensor::Tensor;
    
    // Create loss function
    let loss_fn = MSELoss;
    
    // Create predictions and targets
    let predictions = Tensor::new(vec![1.0, 2.0, 3.0, 4.0], vec![2, 2], false);
    let targets = Tensor::new(vec![0.0, 1.0, 2.0, 3.0], vec![2, 2], false);
    
    // Compute loss
    let loss = loss_fn.compute(&predictions, &targets);
    assert!(loss.data()[0] > 0.0);
    
    println!("Training smoke test passed!");
}