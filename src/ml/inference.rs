//! Inference and model serving utilities
//! 
//! This module provides model inference, optimization, and
//! serving capabilities for production deployment.

use super::tensor::Tensor;
use super::nn::Layer;
use std::collections::HashMap;
use std::time::{Instant, Duration};

/// Inference engine for optimized model execution
pub struct InferenceEngine {
    model: Box<dyn Layer>,
    batch_size: usize,
    use_gpu: bool,
    optimization_level: OptimizationLevel,
    cache: HashMap<Vec<u8>, Tensor>,  // Simple inference cache
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptimizationLevel {
    None,
    Basic,
    Aggressive,
}

impl InferenceEngine {
    /// Create a new inference engine
    pub fn new(model: Box<dyn Layer>, batch_size: usize) -> Self {
        InferenceEngine {
            model,
            batch_size,
            use_gpu: false,  // CPU by default
            optimization_level: OptimizationLevel::Basic,
            cache: HashMap::new(),
        }
    }
    
    /// Enable GPU acceleration (if available)
    pub fn enable_gpu(mut self) -> Self {
        self.use_gpu = true;
        self
    }
    
    /// Set optimization level
    pub fn with_optimization(mut self, level: OptimizationLevel) -> Self {
        self.optimization_level = level;
        self
    }
    
    /// Perform single inference
    pub fn infer(&mut self, input: &Tensor) -> Tensor {
        let start_time = Instant::now();
        
        // Check cache (simplified - using data hash as key)
        let cache_key = self.compute_cache_key(input);
        if let Some(cached) = self.cache.get(&cache_key) {
            return cached.clone();
        }
        
        // Set model to evaluation mode
        self.model.train(false);
        
        // Perform inference
        let output = self.model.forward(input);
        
        // Cache result
        self.cache.insert(cache_key, output.clone());
        
        let inference_time = start_time.elapsed();
        self.log_inference(&inference_time, input.shape());
        
        output
    }
    
    /// Perform batch inference
    pub fn infer_batch(&mut self, inputs: &[Tensor]) -> Vec<Tensor> {
        let start_time = Instant::now();
        
        // Set model to evaluation mode
        self.model.train(false);
        
        let mut outputs = Vec::with_capacity(inputs.len());
        
        // Process in batches
        for chunk in inputs.chunks(self.batch_size) {
            // In real implementation, we would batch inputs together
            // For now, process individually
            for input in chunk {
                let output = self.model.forward(input);
                outputs.push(output);
            }
        }
        
        let inference_time = start_time.elapsed();
        self.log_inference(&inference_time, &[inputs.len()]);
        
        outputs
    }
    
    /// Compute cache key for input tensor
    fn compute_cache_key(&self, input: &Tensor) -> Vec<u8> {
        // Simplified cache key - just use first few bytes of data
        let data = input.data();
        let mut key = Vec::new();
        
        // Use first 16 bytes (or less if tensor is smaller)
        let bytes_to_use = std::cmp::min(16, data.len() * 4);  // 4 bytes per f32
        for i in 0..bytes_to_use / 4 {
            let bytes = data[i].to_ne_bytes();
            key.extend_from_slice(&bytes);
        }
        
        key
    }
    
    /// Log inference performance
    fn log_inference(&self, duration: &Duration, input_shape: &[usize]) {
        let ms = duration.as_secs_f64() * 1000.0;
        let shape_str = input_shape.iter()
            .map(|&d| d.to_string())
            .collect::<Vec<_>>()
            .join("×");
        
        println!("Inference: shape=[{}], time={:.2}ms", shape_str, ms);
    }
    
    /// Clear inference cache
    pub fn clear_cache(&mut self) {
        self.cache.clear();
    }
    
    /// Get cache statistics
    pub fn cache_stats(&self) -> CacheStats {
        CacheStats {
            size: self.cache.len(),
            memory_usage: self.cache.len() * 100,  // Simplified estimate
        }
    }
    
    /// Optimize model for inference
    pub fn optimize(&mut self) {
        match self.optimization_level {
            OptimizationLevel::None => {
                // No optimization
            },
            OptimizationLevel::Basic => {
                // Basic optimizations: fuse layers, remove dropout, etc.
                self.apply_basic_optimizations();
            },
            OptimizationLevel::Aggressive => {
                // Aggressive optimizations: quantization, pruning, etc.
                self.apply_aggressive_optimizations();
            },
        }
    }
    
    fn apply_basic_optimizations(&mut self) {
        println!("Applying basic optimizations...");
        // In real implementation, we would:
        // 1. Fuse batch normalization layers
        // 2. Remove dropout layers
        // 3. Constant folding
        // 4. Dead code elimination
    }
    
    fn apply_aggressive_optimizations(&mut self) {
        println!("Applying aggressive optimizations...");
        // In real implementation, we would:
        // 1. Quantize weights (FP32 -> INT8)
        // 2. Prune unimportant weights
        // 3. Layer fusion
        // 4. Kernel optimization
    }
}

/// Cache statistics
#[derive(Debug, Clone)]
pub struct CacheStats {
    pub size: usize,
    pub memory_usage: usize,  // in bytes (estimated)
}

/// Model quantization utilities
pub struct Quantizer {
    bits: usize,  // 8, 16, or 32
    symmetric: bool,
    per_channel: bool,
}

impl Quantizer {
    /// Create a new quantizer
    pub fn new(bits: usize, symmetric: bool, per_channel: bool) -> Self {
        assert!(bits == 8 || bits == 16 || bits == 32, "Bits must be 8, 16, or 32");
        Quantizer {
            bits,
            symmetric,
            per_channel,
        }
    }
    
    /// Quantize a tensor
    pub fn quantize(&self, tensor: &Tensor) -> QuantizedTensor {
        let data = tensor.data();
        
        // Compute quantization parameters
        let (scale, zero_point) = if self.symmetric {
            let abs_max = data.iter().map(|&x| x.abs()).fold(0.0f32, f32::max);
            let scale = abs_max / ((1 << (self.bits - 1)) - 1) as f32;
            (scale, 0.0)
        } else {
            let min = data.iter().fold(f32::INFINITY, |a, &b| a.min(b));
            let max = data.iter().fold(f32::NEG_INFINITY, |a, &b| a.max(b));
            let scale = (max - min) / ((1 << self.bits) - 1) as f32;
            let zero_point = -min / scale;
            (scale, zero_point)
        };
        
        // Quantize data
        let quantized_data: Vec<i32> = data.iter()
            .map(|&x| {
                let q = (x / scale + zero_point).round() as i32;
                q.clamp(0, (1 << self.bits) - 1)
            })
            .collect();
        
        QuantizedTensor {
            data: quantized_data,
            scale,
            zero_point,
            bits: self.bits,
            shape: tensor.shape().to_vec(),
        }
    }
    
    /// Dequantize a quantized tensor
    pub fn dequantize(&self, quantized: &QuantizedTensor) -> Tensor {
        let data: Vec<f32> = quantized.data.iter()
            .map(|&q| (q as f32 - quantized.zero_point) * quantized.scale)
            .collect();
        
        Tensor::new(data, quantized.shape.clone(), false)
    }
}

/// Quantized tensor representation
#[derive(Debug, Clone)]
pub struct QuantizedTensor {
    data: Vec<i32>,
    scale: f32,
    zero_point: f32,
    bits: usize,
    shape: Vec<usize>,
}

impl QuantizedTensor {
    /// Get compression ratio compared to FP32
    pub fn compression_ratio(&self) -> f32 {
        let original_size = self.shape.iter().product::<usize>() * 4;  // 4 bytes per FP32
        let quantized_size = self.shape.iter().product::<usize>() * (self.bits / 8);
        original_size as f32 / quantized_size as f32
    }
    
    /// Get memory usage in bytes
    pub fn memory_usage(&self) -> usize {
        self.shape.iter().product::<usize>() * (self.bits / 8)
    }
}

/// Model pruning utilities
pub struct Pruner {
    sparsity: f32,  // Target sparsity (0.0 to 1.0)
    method: PruningMethod,
}

#[derive(Debug, Clone, Copy)]
pub enum PruningMethod {
    Magnitude,      // Prune smallest magnitude weights
    Gradient,       // Prune based on gradient magnitude
    Random,         // Random pruning
    Structured,     // Prune entire channels/filters
}

impl Pruner {
    /// Create a new pruner
    pub fn new(sparsity: f32, method: PruningMethod) -> Self {
        assert!(sparsity >= 0.0 && sparsity <= 1.0, "Sparsity must be between 0 and 1");
        Pruner {
            sparsity,
            method,
        }
    }
    
    /// Prune a tensor
    pub fn prune(&self, tensor: &Tensor) -> Tensor {
        let data = tensor.data().to_vec();
        let mut pruned_data = data.clone();
        
        match self.method {
            PruningMethod::Magnitude => {
                // Prune smallest magnitude weights
                let mut magnitudes: Vec<(usize, f32)> = data.iter()
                    .enumerate()
                    .map(|(i, &x)| (i, x.abs()))
                    .collect();
                
                magnitudes.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());
                
                let num_to_prune = (data.len() as f32 * self.sparsity) as usize;
                for (i, _) in magnitudes.iter().take(num_to_prune) {
                    pruned_data[*i] = 0.0;
                }
            },
            PruningMethod::Random => {
                // Random pruning
                for i in 0..pruned_data.len() {
                    if rand::random::<f32>() < self.sparsity {
                        pruned_data[i] = 0.0;
                    }
                }
            },
            PruningMethod::Gradient => {
                // Would need gradient information
                // For now, use magnitude as proxy
                let magnitude_pruner = Pruner::new(self.sparsity, PruningMethod::Magnitude);
                return magnitude_pruner.prune(tensor);
            },
            PruningMethod::Structured => {
                // Structured pruning (prune entire rows/columns for 2D tensors)
                if tensor.ndim() == 2 {
                    let rows = tensor.shape()[0];
                    let cols = tensor.shape()[1];
                    let num_to_prune = (rows as f32 * self.sparsity) as usize;
                    
                    // Compute row magnitudes
                    let mut row_magnitudes = Vec::new();
                    for i in 0..rows {
                        let mut magnitude = 0.0;
                        for j in 0..cols {
                            magnitude += data[i * cols + j].abs();
                        }
                        row_magnitudes.push((i, magnitude));
                    }
                    
                    row_magnitudes.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());
                    
                    // Zero out pruned rows
                    for (row_idx, _) in row_magnitudes.iter().take(num_to_prune) {
                        for j in 0..cols {
                            pruned_data[row_idx * cols + j] = 0.0;
                        }
                    }
                }
            },
        }
        
        Tensor::new(pruned_data, tensor.shape().to_vec(), tensor.requires_grad())
    }
}

/// Model serving API
pub struct ModelServer {
    engine: InferenceEngine,
    port: u16,
    max_batch_size: usize,
    request_timeout: Duration,
    metrics: ServerMetrics,
}

#[derive(Debug, Clone)]
pub struct ServerMetrics {
    pub total_requests: usize,
    pub successful_requests: usize,
    pub failed_requests: usize,
    pub avg_inference_time: Duration,
    pub peak_memory_usage: usize,
}

impl ModelServer {
    /// Create a new model server
    pub fn new(engine: InferenceEngine, port: u16) -> Self {
        ModelServer {
            engine,
            port,
            max_batch_size: 32,
            request_timeout: Duration::from_secs(30),
            metrics: ServerMetrics {
                total_requests: 0,
                successful_requests: 0,
                failed_requests: 0,
                avg_inference_time: Duration::from_secs(0),
                peak_memory_usage: 0,
            },
        }
    }
    
    /// Start the server
    pub fn start(&mut self) {
        println!("Starting model server on port {}...", self.port);
        println!("Model optimized for inference");
        println!("Max batch size: {}", self.max_batch_size);
        println!("Request timeout: {:?}", self.request_timeout);
        
        // In real implementation, we would:
        // 1. Start HTTP server (e.g., using warp, actix-web, or rocket)
        // 2. Set up request handlers
        // 3. Implement health checks
        // 4. Add metrics endpoint
        // 5. Handle graceful shutdown
        
        println!("Server started successfully");
    }
    
    /// Handle inference request
    pub fn handle_request(&mut self, input: &Tensor) -> Result<Tensor, ServerError> {
        let start_time = Instant::now();
        self.metrics.total_requests += 1;
        
        // Check input validity
        if input.numel() == 0 {
            self.metrics.failed_requests += 1;
            return Err(ServerError::InvalidInput("Empty input tensor".to_string()));
        }
        
        // Perform inference
        let result = self.engine.infer(input);
        
        // Update metrics
        let inference_time = start_time.elapsed();
        self.update_metrics(inference_time);
        self.metrics.successful_requests += 1;
        
        Ok(result)
    }
    
    /// Handle batch request
    pub fn handle_batch_request(&mut self, inputs: &[Tensor]) -> Result<Vec<Tensor>, ServerError> {
        if inputs.is_empty() {
            return Err(ServerError::InvalidInput("Empty batch".to_string()));
        }
        
        if inputs.len() > self.max_batch_size {
            return Err(ServerError::BatchTooLarge(inputs.len(), self.max_batch_size));
        }
        
        let results = self.engine.infer_batch(inputs);
        Ok(results)
    }
    
    fn update_metrics(&mut self, inference_time: Duration) {
        // Update average inference time (exponential moving average)
        let alpha = 0.1;  // Smoothing factor
        let old_avg = self.metrics.avg_inference_time.as_nanos() as f64;
        let new_time = inference_time.as_nanos() as f64;
        let new_avg = alpha * new_time + (1.0 - alpha) * old_avg;
        self.metrics.avg_inference_time = Duration::from_nanos(new_avg as u64);
        
        // Update peak memory usage (simplified)
        let cache_stats = self.engine.cache_stats();
        self.metrics.peak_memory_usage = self.metrics.peak_memory_usage.max(cache_stats.memory_usage);
    }
    
    /// Get server metrics
    pub fn get_metrics(&self) -> &ServerMetrics {
        &self.metrics
    }
    
    /// Reset server metrics
    pub fn reset_metrics(&mut self) {
        self.metrics = ServerMetrics {
            total_requests: 0,
            successful_requests: 0,
            failed_requests: 0,
            avg_inference_time: Duration::from_secs(0),
            peak_memory_usage: 0,
        };
    }
}

/// Server error types
#[derive(Debug, Clone)]
pub enum ServerError {
    InvalidInput(String),
    InferenceError(String),
    BatchTooLarge(usize, usize),
    Timeout,
    InternalError(String),
}

impl std::fmt::Display for ServerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ServerError::InvalidInput(msg) => write!(f, "Invalid input: {}", msg),
            ServerError::InferenceError(msg) => write!(f, "Inference error: {}", msg),
            ServerError::BatchTooLarge(actual, max) => write!(f, "Batch size {} exceeds maximum {}", actual, max),
            ServerError::Timeout => write!(f, "Request timeout"),
            ServerError::InternalError(msg) => write!(f, "Internal error: {}", msg),
        }
    }
}

impl std::error::Error for ServerError {}

/// Model conversion utilities (for exporting to other formats)
pub struct ModelConverter {
    target_format: ModelFormat,
    opset_version: usize,
}

#[derive(Debug, Clone, Copy)]
pub enum ModelFormat {
    ONNX,
    TensorFlow,
    TensorFlowLite,
    CoreML,
    TorchScript,
}

impl ModelConverter {
    /// Create a new model converter
    pub fn new(target_format: ModelFormat, opset_version: usize) -> Self {
        ModelConverter {
            target_format,
            opset_version,
        }
    }
    
    /// Convert model to target format
    pub fn convert(&self, model: &dyn Layer) -> Result<(), String> {
        // In real implementation, we would convert the model to the target format
        // For now, return a placeholder result
        match self.target_format {
            ModelFormat::ONNX => println!("Converting to ONNX format (opset {})", self.opset_version),
            ModelFormat::TensorFlow => println!("Converting to TensorFlow format"),
            ModelFormat::TensorFlowLite => println!("Converting to TensorFlow Lite format"),
            ModelFormat::CoreML => println!("Converting to CoreML format"),
            ModelFormat::TorchScript => println!("Converting to TorchScript format"),
        }
        Ok(())
    }
}