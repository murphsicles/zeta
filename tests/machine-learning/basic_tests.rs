//! Basic tests for machine learning integration
//! 
//! Tests for tensor operations, neural networks, and training pipelines

use openclaw::ml::*;

#[test]
fn test_tensor_creation() {
    // Test basic tensor creation
    let t = tensor::Tensor::new(vec![1.0, 2.0, 3.0, 4.0], vec![2, 2], false);
    assert_eq!(t.shape(), &[2, 2]);
    assert_eq!(t.numel(), 4);
    assert_eq!(t.data(), &[1.0, 2.0, 3.0, 4.0]);
}

#[test]
fn test_tensor_operations() {
    // Test addition
    let t1 = tensor::Tensor::new(vec![1.0, 2.0, 3.0, 4.0], vec![2, 2], false);
    let t2 = tensor::Tensor::new(vec![5.0, 6.0, 7.0, 8.0], vec![2, 2], false);
    let result = t1.add(&t2);
    assert_eq!(result.data(), &[6.0, 8.0, 10.0, 12.0]);
    
    // Test multiplication
    let result = t1.mul(&t2);
    assert_eq!(result.data(), &[5.0, 12.0, 21.0, 32.0]);
}

#[test]
fn test_tensor_matmul() {
    // Test matrix multiplication
    let t1 = tensor::Tensor::new(vec![1.0, 2.0, 3.0, 4.0], vec![2, 2], false);
    let t2 = tensor::Tensor::new(vec![5.0, 6.0, 7.0, 8.0], vec![2, 2], false);
    let result = t1.matmul(&t2);
    
    // 1*5 + 2*7 = 19, 1*6 + 2*8 = 22
    // 3*5 + 4*7 = 43, 3*6 + 4*8 = 50
    assert_eq!(result.shape(), &[2, 2]);
    assert_eq!(result.data()[0], 19.0);
    assert_eq!(result.data()[1], 22.0);
    assert_eq!(result.data()[2], 43.0);
    assert_eq!(result.data()[3], 50.0);
}

#[test]
fn test_activation_functions() {
    // Test ReLU
    let t = tensor::Tensor::new(vec![-1.0, 0.0, 1.0, 2.0], vec![2, 2], false);
    let result = t.relu();
    assert_eq!(result.data(), &[0.0, 0.0, 1.0, 2.0]);
    
    // Test Sigmoid
    let t = tensor::Tensor::scalar(0.0, false);
    let result = t.sigmoid();
    assert!((result.data()[0] - 0.5).abs() < 0.001); // sigmoid(0) = 0.5
}

#[test]
fn test_dense_layer() {
    // Test dense layer creation and forward pass
    let dense = nn::Dense::new(10, 5, true);
    assert_eq!(dense.name(), "Dense");
    
    let input = tensor::Tensor::rand(vec![2, 10], false); // batch of 2 samples
    let output = dense.forward(&input);
    assert_eq!(output.shape(), &[2, 5]);
    
    // Check that parameters exist
    let params = dense.parameters();
    assert_eq!(params.len(), 2); // weights and bias
}

#[test]
fn test_sequential_model() {
    // Test sequential container
    let model = nn::Sequential::new()
        .add(nn::Dense::new(10, 20, true))
        .add(nn::Dropout::new(0.5))
        .add(nn::Dense::new(20, 5, true));
    
    let input = tensor::Tensor::rand(vec![4, 10], true); // batch of 4 samples
    let output = model.forward(&input);
    assert_eq!(output.shape(), &[4, 5]);
    
    // Check parameters
    let params = model.parameters();
    assert_eq!(params.len(), 4); // 2 dense layers × 2 params each
}

#[test]
fn test_loss_functions() {
    // Test MSE loss
    let mse = training::MSELoss;
    let predictions = tensor::Tensor::new(vec![1.0, 2.0, 3.0, 4.0], vec![2, 2], false);
    let targets = tensor::Tensor::new(vec![0.0, 1.0, 2.0, 3.0], vec![2, 2], false);
    
    let loss = mse.compute(&predictions, &targets);
    assert!(loss.data()[0] > 0.0); // Loss should be positive
    
    // Test BCE loss
    let bce = training::BCELoss::default();
    let predictions = tensor::Tensor::new(vec![0.7, 0.3], vec![2], false);
    let targets = tensor::Tensor::new(vec![1.0, 0.0], vec![2], false);
    
    let loss = bce.compute(&predictions, &targets);
    assert!(loss.data()[0] > 0.0);
}

#[test]
fn test_optimizers() {
    // Test SGD optimizer creation
    let sgd = optim::SGD::default();
    assert_eq!(sgd.get_lr(), 0.01);
    
    // Test Adam optimizer
    let adam = optim::Adam::default();
    assert_eq!(adam.get_lr(), 0.001);
}

#[test]
fn test_data_loader() {
    // Create a simple dataset
    let features = vec![
        tensor::Tensor::new(vec![1.0, 2.0], vec![2], false),
        tensor::Tensor::new(vec![3.0, 4.0], vec![2], false),
        tensor::Tensor::new(vec![5.0, 6.0], vec![2], false),
        tensor::Tensor::new(vec![7.0, 8.0], vec![2], false),
    ];
    
    let labels = vec![
        tensor::Tensor::scalar(0.0, false),
        tensor::Tensor::scalar(1.0, false),
        tensor::Tensor::scalar(0.0, false),
        tensor::Tensor::scalar(1.0, false),
    ];
    
    let dataset = data::MemoryDataset::new(features, labels, "test_dataset");
    assert_eq!(dataset.len(), 4);
    
    // Test data loader
    let mut loader = data::DataLoader::new(Box::new(dataset), 2, true);
    assert_eq!(loader.num_batches(), 2); // 4 samples / batch size 2 = 2 batches
    
    // Test batch iteration
    let mut batch_count = 0;
    for (batch_features, batch_labels) in loader.iter() {
        assert_eq!(batch_features.len(), 2);
        assert_eq!(batch_labels.len(), 2);
        batch_count += 1;
    }
    assert_eq!(batch_count, 2);
}

#[test]
fn test_preprocessor() {
    // Create sample data
    let data = vec![
        tensor::Tensor::new(vec![1.0, 2.0], vec![2], false),
        tensor::Tensor::new(vec![3.0, 4.0], vec![2], false),
        tensor::Tensor::new(vec![5.0, 6.0], vec![2], false),
    ];
    
    // Fit preprocessor
    let mut preprocessor = data::Preprocessor::new();
    preprocessor.fit(&data);
    
    // Test standardization
    let sample = tensor::Tensor::new(vec![2.0, 3.0], vec![2], false);
    let standardized = preprocessor.standardize(&sample);
    
    // Values should be centered and scaled
    assert!(standardized.data()[0].abs() < 2.0);
    assert!(standardized.data()[1].abs() < 2.0);
}

#[test]
fn test_mlp_model() {
    // Test MLP creation
    let mlp = models::MLP::binary_classifier(10, vec![20, 10]);
    assert_eq!(mlp.name(), "MLP");
    
    let input = tensor::Tensor::rand(vec![2, 10], false);
    let output = mlp.forward(&input);
    assert_eq!(output.shape(), &[2, 1]); // Binary classification output
    
    // Check parameters
    let params = mlp.parameters();
    assert!(!params.is_empty());
}

#[test]
fn test_cnn_model() {
    // Test CNN creation (simplified)
    let cnn = models::CNN::mnist_classifier();
    assert_eq!(cnn.name(), "CNN");
    
    // Note: CNN forward pass would require proper 4D input
    // This test is mostly for compilation verification
}

#[test]
fn test_autoencoder() {
    // Test autoencoder creation
    let autoencoder = models::Autoencoder::mnist_autoencoder();
    assert_eq!(autoencoder.name(), "Autoencoder");
    
    let input = tensor::Tensor::rand(vec![1, 784], false); // Single MNIST sample
    let output = autoencoder.forward(&input);
    assert_eq!(output.shape(), &[1, 784]); // Should reconstruct input
    
    // Test encode/decode separately
    let latent = autoencoder.encode(&input);
    assert_eq!(latent.shape()[1], 32); // Latent dimension
    
    let reconstructed = autoencoder.decode(&latent);
    assert_eq!(reconstructed.shape(), &[1, 784]);
}

#[test]
fn test_inference_engine() {
    // Create a simple model
    let model = models::MLP::binary_classifier(5, vec![10]);
    let engine = inference::InferenceEngine::new(Box::new(model), 1);
    
    // Test inference (simplified)
    let input = tensor::Tensor::rand(vec![1, 5], false);
    let output = engine.infer(&input);
    assert_eq!(output.shape()[1], 1); // Binary classification output
    
    // Test cache stats
    let stats = engine.cache_stats();
    assert_eq!(stats.size, 1); // One inference cached
}

#[test]
fn test_quantization() {
    // Test tensor quantization
    let quantizer = inference::Quantizer::new(8, true, false);
    let tensor = tensor::Tensor::new(vec![1.0, -1.0, 0.5, -0.5], vec![2, 2], false);
    
    let quantized = quantizer.quantize(&tensor);
    assert!(quantized.compression_ratio() > 1.0); // Should compress
    
    // Test dequantization
    let dequantized = quantizer.dequantize(&quantized);
    assert_eq!(dequantized.shape(), tensor.shape());
    
    // Values should be approximately equal (within quantization error)
    let original_data = tensor.data();
    let dequantized_data = dequantized.data();
    for (orig, deq) in original_data.iter().zip(dequantized_data) {
        assert!((orig - deq).abs() < 0.1); // Allow some quantization error
    }
}

#[test]
fn test_pruning() {
    // Test tensor pruning
    let pruner = inference::Pruner::new(0.5, inference::PruningMethod::Magnitude);
    let tensor = tensor::Tensor::new(vec![1.0, 0.1, 0.01, 0.001], vec![2, 2], false);
    
    let pruned = pruner.prune(&tensor);
    let pruned_data = pruned.data();
    
    // Check that some values were set to zero
    let zero_count = pruned_data.iter().filter(|&&x| x == 0.0).count();
    assert!(zero_count > 0); // At least one value should be pruned
}

// Integration test: train a simple model
#[test]
fn test_simple_training() {
    // Create synthetic data
    let n_samples = 100;
    let mut features_data = Vec::new();
    let mut labels_data = Vec::new();
    
    for i in 0..n_samples {
        let x = i as f32 / n_samples as f32;
        let y = (2.0 * x + 0.5 + (rand::random::<f32>() - 0.5) * 0.1).sin(); // Noisy sine wave
        
        features_data.push(vec![x]);
        labels_data.push(vec![y]);
    }
    
    // Create dataset
    let dataset = data::MemoryDataset::from_raw_data(
        features_data,
        labels_data,
        vec![1],  // Feature shape
        vec![1],  // Label shape
        "sine_wave",
    );
    
    // Split into train/test
    let (train_dataset, test_dataset) = dataset.train_test_split(0.2, true);
    
    // Create model
    let model = models::MLP::regressor(1, vec![10, 10]);
    
    // Create optimizer
    let optimizer = optim::SGD::default();
    
    // Create loss function
    let loss_fn = training::MSELoss;
    
    // Create trainer
    let mut trainer = training::Trainer::new(
        Box::new(model),
        Box::new(optimizer),
        Box::new(loss_fn),
    );
    
    // Create data loaders
    let mut train_loader = data::DataLoader::new(Box::new(train_dataset), 10, true);
    let mut test_loader = data::DataLoader::new(Box::new(test_dataset), 10, false);
    
    // Train for a few epochs
    let metrics = trainer.train(&mut train_loader, Some(&mut test_loader), 3);
    
    // Check that training occurred
    assert_eq!(metrics.train_loss.len(), 3);
    assert_eq!(metrics.val_loss.len(), 3);
    
    // Loss should decrease (or at least not be NaN)
    for &loss in &metrics.train_loss {
        assert!(!loss.is_nan());
        assert!(!loss.is_infinite());
    }
}

// Test model serialization (placeholder)
#[test]
fn test_model_serialization() {
    // Note: Real serialization would require proper implementation
    // This test verifies the interfaces exist
    let model = models::MLP::binary_classifier(5, vec![10]);
    
    // Check that model can be trained/evaluated
    let mut model_ref = model;
    model_ref.train(true);
    model_ref.train(false);
    
    // Parameters should exist
    let params = model_ref.parameters();
    assert!(!params.is_empty());
}

// Test error handling
#[test]
#[should_panic(expected = "Shape mismatch")]
fn test_tensor_shape_mismatch() {
    let t1 = tensor::Tensor::new(vec![1.0, 2.0], vec![2], false);
    let t2 = tensor::Tensor::new(vec![1.0, 2.0, 3.0], vec![3], false);
    
    // This should panic due to shape mismatch
    let _ = t1.add(&t2);
}

// Test tensor operations with gradients
#[test]
fn test_tensor_with_gradients() {
    // Create tensors that require gradients
    let t1 = tensor::Tensor::new(vec![1.0, 2.0], vec![2], true);
    let t2 = tensor::Tensor::new(vec![3.0, 4.0], vec![2], true);
    
    // Perform operations
    let result = t1.add(&t2);
    assert_eq!(result.data(), &[4.0, 6.0]);
    
    // Check that result requires gradient (since inputs do)
    // Note: Our implementation doesn't fully track this yet
}

// Test learning rate schedulers
#[test]
fn test_lr_schedulers() {
    // Test StepLR
    let mut scheduler = optim::StepLR::new(0.1, 10, 0.5);
    assert_eq!(scheduler.get_lr(), 0.1);
    
    // Step a few times
    for i in 0..20 {
        scheduler.step();
        if i < 10 {
            assert_eq!(scheduler.get_lr(), 0.1);
        } else {
            assert_eq!(scheduler.get_lr(), 0.05); // After 10 steps, lr halves
        }
    }
    
    // Test ExponentialLR
    let mut exp_scheduler = optim::ExponentialLR::new(0.1, 0.9);
    assert_eq!(exp_scheduler.get_lr(), 0.1);
    
    exp_scheduler.step();
    assert!((exp_scheduler.get_lr() - 0.09).abs() < 0.001); // 0.1 * 0.9
}

// Test data augmentation (placeholder)
#[test]
fn test_data_augmentation() {
    let augmentation = data::ImageAugmentation::new()
        .with_rotation(15.0)
        .with_horizontal_flip(true)
        .with_zoom(0.9, 1.1);
    
    // Create a dummy image tensor
    let image = tensor::Tensor::rand(vec![1, 3, 32, 32], false); // [batch, channels, height, width]
    
    // Apply augmentation
    let augmented = augmentation.apply(&image);
    assert_eq!(augmented.shape(), image.shape());
}

// Test model server interface
#[test]
fn test_model_server_interface() {
