//! Neural network layers
//! 
//! This module provides various neural network layers including:
//! - Dense (fully connected) layers
//! - Convolutional layers
//! - Recurrent layers (LSTM, GRU)
//! - Normalization layers
//! - Dropout layers

use super::tensor::Tensor;
use std::fmt;

/// Base trait for all neural network layers
pub trait Layer: fmt::Debug {
    /// Forward pass through the layer
    fn forward(&self, input: &Tensor) -> Tensor;
    
    /// Get trainable parameters
    fn parameters(&self) -> Vec<Tensor>;
    
    /// Set training mode
    fn train(&mut self, mode: bool);
    
    /// Get layer name
    fn name(&self) -> &str;
}

/// Dense (fully connected) layer
#[derive(Debug)]
pub struct Dense {
    weights: Tensor,
    bias: Tensor,
    in_features: usize,
    out_features: usize,
    use_bias: bool,
    training: bool,
}

impl Dense {
    /// Create a new dense layer
    pub fn new(in_features: usize, out_features: usize, use_bias: bool) -> Self {
        // He initialization for ReLU activations
        let std_dev = (2.0 / in_features as f32).sqrt();
        let weights = Tensor::randn(vec![in_features, out_features], 0.0, std_dev, true);
        
        let bias = if use_bias {
            Tensor::zeros(vec![out_features], true)
        } else {
            Tensor::zeros(vec![1], false)
        };
        
        Dense {
            weights,
            bias,
            in_features,
            out_features,
            use_bias,
            training: true,
        }
    }
    
    /// Get weights
    pub fn weights(&self) -> &Tensor {
        &self.weights
    }
    
    /// Get mutable weights
    pub fn weights_mut(&mut self) -> &mut Tensor {
        &mut self.weights
    }
    
    /// Get bias
    pub fn bias(&self) -> &Tensor {
        &self.bias
    }
    
    /// Get mutable bias
    pub fn bias_mut(&mut self) -> &mut Tensor {
        &mut self.bias
    }
}

impl Layer for Dense {
    fn forward(&self, input: &Tensor) -> Tensor {
        assert_eq!(input.ndim(), 2, "Input must be 2D for dense layer");
        assert_eq!(input.shape()[1], self.in_features, "Input features mismatch");
        
        let batch_size = input.shape()[0];
        let mut output = input.matmul(&self.weights);
        
        if self.use_bias {
            // Add bias to each sample in the batch
            // Create a bias tensor with shape [batch_size, out_features] by repeating the bias
            let bias_data = self.bias.data();
            let mut broadcast_bias_data = Vec::with_capacity(batch_size * self.out_features);
            for _ in 0..batch_size {
                broadcast_bias_data.extend_from_slice(bias_data);
            }
            let broadcast_bias = Tensor::new(
                broadcast_bias_data,
                vec![batch_size, self.out_features],
                self.bias.requires_grad()
            );
            output = output.add(&broadcast_bias);
        }
        
        output
    }
    
    fn parameters(&self) -> Vec<Tensor> {
        let mut params = vec![self.weights.clone()];
        if self.use_bias {
            params.push(self.bias.clone());
        }
        params
    }
    
    fn train(&mut self, mode: bool) {
        self.training = mode;
    }
    
    fn name(&self) -> &str {
        "Dense"
    }
}

/// 2D Convolutional layer
#[derive(Debug)]
pub struct Conv2d {
    weights: Tensor,
    bias: Tensor,
    in_channels: usize,
    out_channels: usize,
    kernel_size: (usize, usize),
    stride: (usize, usize),
    padding: (usize, usize),
    use_bias: bool,
    training: bool,
}

impl Conv2d {
    /// Create a new 2D convolutional layer
    pub fn new(
        in_channels: usize,
        out_channels: usize,
        kernel_size: (usize, usize),
        stride: (usize, usize),
        padding: (usize, usize),
        use_bias: bool,
    ) -> Self {
        let kernel_elements = kernel_size.0 * kernel_size.1 * in_channels;
        let std_dev = (2.0 / kernel_elements as f32).sqrt();
        let weights_shape = vec![out_channels, in_channels, kernel_size.0, kernel_size.1];
        let weights = Tensor::randn(weights_shape, 0.0, std_dev, true);
        
        let bias = if use_bias {
            Tensor::zeros(vec![out_channels], true)
        } else {
            Tensor::zeros(vec![1], false)
        };
        
        Conv2d {
            weights,
            bias,
            in_channels,
            out_channels,
            kernel_size,
            stride,
            padding,
            use_bias,
            training: true,
        }
    }
    
    /// Perform 2D convolution
    fn conv2d_forward(&self, input: &Tensor) -> Tensor {
        // Simplified implementation
        // A full implementation would handle padding, stride, and dilation
        
        assert_eq!(input.ndim(), 4, "Input must be 4D for Conv2d: [batch, channels, height, width]");
        
        let batch_size = input.shape()[0];
        let in_channels = input.shape()[1];
        let height = input.shape()[2];
        let width = input.shape()[3];
        
        assert_eq!(in_channels, self.in_channels, "Input channels mismatch");
        
        // Calculate output dimensions
        let out_height = (height + 2 * self.padding.0 - self.kernel_size.0) / self.stride.0 + 1;
        let out_width = (width + 2 * self.padding.1 - self.kernel_size.1) / self.stride.1 + 1;
        
        // Create output tensor
        let output_shape = vec![batch_size, self.out_channels, out_height, out_width];
        let output = Tensor::zeros(output_shape, input.requires_grad() || self.weights.requires_grad());
        
        // Simplified convolution (no padding, stride=1 for now)
        // This is a placeholder - real implementation would be more complex
        output
    }
}

impl Layer for Conv2d {
    fn forward(&self, input: &Tensor) -> Tensor {
        self.conv2d_forward(input)
    }
    
    fn parameters(&self) -> Vec<Tensor> {
        let mut params = vec![self.weights.clone()];
        if self.use_bias {
            params.push(self.bias.clone());
        }
        params
    }
    
    fn train(&mut self, mode: bool) {
        self.training = mode;
    }
    
    fn name(&self) -> &str {
        "Conv2d"
    }
}

/// Batch Normalization layer
#[derive(Debug)]
pub struct BatchNorm2d {
    gamma: Tensor,  // Scale parameter
    beta: Tensor,   // Shift parameter
    running_mean: Tensor,
    running_var: Tensor,
    num_features: usize,
    eps: f32,
    momentum: f32,
    training: bool,
}

impl BatchNorm2d {
    /// Create a new BatchNorm2d layer
    pub fn new(num_features: usize, eps: f32, momentum: f32) -> Self {
        let gamma = Tensor::ones(vec![num_features], true);  // Learnable scale
        let beta = Tensor::zeros(vec![num_features], true);  // Learnable shift
        
        let running_mean = Tensor::zeros(vec![num_features], false);
        let running_var = Tensor::ones(vec![num_features], false);
        
        BatchNorm2d {
            gamma,
            beta,
            running_mean,
            running_var,
            num_features,
            eps,
            momentum,
            training: true,
        }
    }
}

impl Layer for BatchNorm2d {
    fn forward(&self, input: &Tensor) -> Tensor {
        assert_eq!(input.ndim(), 4, "Input must be 4D for BatchNorm2d");
        assert_eq!(input.shape()[1], self.num_features, "Features mismatch");
        
        if self.training {
            // Training mode: normalize using batch statistics
            // and update running statistics
            // Simplified implementation
            input.clone()  // Placeholder
        } else {
            // Inference mode: normalize using running statistics
            // Simplified implementation
            input.clone()  // Placeholder
        }
    }
    
    fn parameters(&self) -> Vec<Tensor> {
        vec![self.gamma.clone(), self.beta.clone()]
    }
    
    fn train(&mut self, mode: bool) {
        self.training = mode;
    }
    
    fn name(&self) -> &str {
        "BatchNorm2d"
    }
}

/// Dropout layer for regularization
#[derive(Debug)]
pub struct Dropout {
    p: f32,  // Dropout probability
    training: bool,
}

impl Dropout {
    /// Create a new dropout layer
    pub fn new(p: f32) -> Self {
        assert!(p >= 0.0 && p <= 1.0, "Dropout probability must be between 0 and 1");
        Dropout {
            p,
            training: true,
        }
    }
}

impl Layer for Dropout {
    fn forward(&self, input: &Tensor) -> Tensor {
        if !self.training || self.p == 0.0 {
            return input.clone();
        }
        
        // Create dropout mask
        let mask_data: Vec<f32> = (0..input.numel())
            .map(|_| if rand::random::<f32>() < self.p { 0.0 } else { 1.0 / (1.0 - self.p) })
            .collect();
        
        let mask = Tensor::new(mask_data, input.shape().to_vec(), false);
        input.mul(&mask)
    }
    
    fn parameters(&self) -> Vec<Tensor> {
        vec![]  // Dropout has no parameters
    }
    
    fn train(&mut self, mode: bool) {
        self.training = mode;
    }
    
    fn name(&self) -> &str {
        "Dropout"
    }
}

/// Recurrent layer base (simplified)
#[derive(Debug)]
pub struct RNN {
    input_size: usize,
    hidden_size: usize,
    num_layers: usize,
    nonlinearity: String,  // "tanh" or "relu"
    training: bool,
}

impl RNN {
    /// Create a new RNN layer
    pub fn new(input_size: usize, hidden_size: usize, num_layers: usize, nonlinearity: &str) -> Self {
        RNN {
            input_size,
            hidden_size,
            num_layers,
            nonlinearity: nonlinearity.to_string(),
            training: true,
        }
    }
}

impl Layer for RNN {
    fn forward(&self, input: &Tensor) -> Tensor {
        // Simplified RNN forward pass
        // Real implementation would handle sequence processing
        assert_eq!(input.ndim(), 3, "RNN input must be 3D: [seq_len, batch, features]");
        
        let seq_len = input.shape()[0];
        let batch_size = input.shape()[1];
        let features = input.shape()[2];
        
        assert_eq!(features, self.input_size, "Input features mismatch");
        
        // Output shape: [seq_len, batch, hidden_size]
        let output_shape = vec![seq_len, batch_size, self.hidden_size];
        Tensor::rand(output_shape, input.requires_grad())  // Placeholder
    }
    
    fn parameters(&self) -> Vec<Tensor> {
        // Would return weight and bias tensors
        vec![]
    }
    
    fn train(&mut self, mode: bool) {
        self.training = mode;
    }
    
    fn name(&self) -> &str {
        "RNN"
    }
}

/// LSTM layer (simplified)
#[derive(Debug)]
pub struct LSTM {
    input_size: usize,
    hidden_size: usize,
    num_layers: usize,
    training: bool,
}

impl LSTM {
    /// Create a new LSTM layer
    pub fn new(input_size: usize, hidden_size: usize, num_layers: usize) -> Self {
        LSTM {
            input_size,
            hidden_size,
            num_layers,
            training: true,
        }
    }
}

impl Layer for LSTM {
    fn forward(&self, input: &Tensor) -> Tensor {
        // Simplified LSTM forward pass
        assert_eq!(input.ndim(), 3, "LSTM input must be 3D: [seq_len, batch, features]");
        
        let seq_len = input.shape()[0];
        let batch_size = input.shape()[1];
        let features = input.shape()[2];
        
        assert_eq!(features, self.input_size, "Input features mismatch");
        
        // Output shape: [seq_len, batch, hidden_size]
        let output_shape = vec![seq_len, batch_size, self.hidden_size];
        Tensor::rand(output_shape, input.requires_grad())  // Placeholder
    }
    
    fn parameters(&self) -> Vec<Tensor> {
        // Would return weight and bias tensors for input, forget, cell, output gates
        vec![]
    }
    
    fn train(&mut self, mode: bool) {
        self.training = mode;
    }
    
    fn name(&self) -> &str {
        "LSTM"
    }
}

/// Sequential container for stacking layers
#[derive(Debug)]
pub struct Sequential {
    layers: Vec<Box<dyn Layer>>,
    name: String,
}

impl Sequential {
    /// Create a new sequential container
    pub fn new() -> Self {
        Sequential {
            layers: Vec::new(),
            name: "Sequential".to_string(),
        }
    }
    
    /// Add a layer to the sequence
    pub fn add<L: Layer + 'static>(mut self, layer: L) -> Self {
        self.layers.push(Box::new(layer));
        self
    }
    
    /// Get all parameters from all layers
    pub fn parameters(&self) -> Vec<Tensor> {
        let mut params = Vec::new();
        for layer in &self.layers {
            params.extend(layer.parameters());
        }
        params
    }
    
    /// Set training mode for all layers
    pub fn train(&mut self, mode: bool) {
        for layer in &mut self.layers {
            layer.train(mode);
        }
    }
}

impl Layer for Sequential {
    fn forward(&self, input: &Tensor) -> Tensor {
        let mut output = input.clone();
        for layer in &self.layers {
            output = layer.forward(&output);
        }
        output
    }
    
    fn parameters(&self) -> Vec<Tensor> {
        self.parameters()
    }
    
    fn train(&mut self, mode: bool) {
        self.train(mode);
    }
    
    fn name(&self) -> &str {
        &self.name
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_dense_layer() {
        let dense = Dense::new(10, 5, true);
        assert_eq!(dense.name(), "Dense");
        assert_eq!(dense.parameters().len(), 2);  // weights and bias
        
        let input = Tensor::rand(vec![2, 10], false);  // batch of 2 samples
        let output = dense.forward(&input);
        assert_eq!(output.shape(), &[2, 5]);
    }
    
    #[test]
    fn test_sequential() {
        let mut model = Sequential::new()
            .add(Dense::new(10, 20, true))
            .add(Dropout::new(0.5))
            .add(Dense::new(20, 5, true));
        
        assert_eq!(model.parameters().len(), 4);  // 2 dense layers × 2 params each
        
        let input = Tensor::rand(vec![4, 10], true);  // batch of 4 samples
        let output = model.forward(&input);
        assert_eq!(output.shape(), &[4, 5]);
        
        // Test training mode
        model.train(false);
        model.train(true);
    }
}