//! Training pipelines and utilities
//! 
//! This module provides training loops, checkpointing, and
//! model evaluation utilities.

use super::tensor::Tensor;
use super::nn::Layer;
use super::optim::Optimizer;
use super::data::DataLoader;
use std::path::Path;
use std::fs::{File, create_dir_all};
use std::io::{Write, BufReader};
use serde::{Serialize, Deserialize};

/// Loss functions for training
pub trait LossFunction {
    /// Compute loss between predictions and targets
    fn compute(&self, predictions: &Tensor, targets: &Tensor) -> Tensor;
    
    /// Compute gradient of loss with respect to predictions
    fn gradient(&self, predictions: &Tensor, targets: &Tensor) -> Tensor;
    
    /// Get loss name
    fn name(&self) -> &str;
}

/// Mean Squared Error loss
#[derive(Debug, Clone)]
pub struct MSELoss;

impl LossFunction for MSELoss {
    fn compute(&self, predictions: &Tensor, targets: &Tensor) -> Tensor {
        assert_eq!(predictions.shape(), targets.shape(), "Shape mismatch for MSE loss");
        
        let diff = Tensor::sub(predictions, targets);
        let squared = Tensor::mul(&diff, &diff);
        
        // Mean over all elements
        let sum = squared.data().iter().sum::<f32>();
        let mean = sum / predictions.numel() as f32;
        
        Tensor::scalar(mean, predictions.requires_grad() || targets.requires_grad())
    }
    
    fn gradient(&self, predictions: &Tensor, targets: &Tensor) -> Tensor {
        assert_eq!(predictions.shape(), targets.shape(), "Shape mismatch for MSE gradient");
        
        // dL/dy = 2/n * (y - t)
        let diff = Tensor::sub(predictions, targets);
        let scale = 2.0 / predictions.numel() as f32;
        
        let grad_data: Vec<f32> = diff.data().iter().map(|&x| x * scale).collect();
        Tensor::new(grad_data, predictions.shape().to_vec(), false)
    }
    
    fn name(&self) -> &str {
        "MSE"
    }
}

/// Cross-Entropy loss (for classification)
#[derive(Debug, Clone)]
pub struct CrossEntropyLoss {
    ignore_index: Option<usize>,
    reduction: Reduction,
}

#[derive(Debug, Clone, Copy)]
pub enum Reduction {
    Mean,
    Sum,
    None,
}

impl CrossEntropyLoss {
    /// Create a new CrossEntropyLoss
    pub fn new(ignore_index: Option<usize>, reduction: Reduction) -> Self {
        CrossEntropyLoss {
            ignore_index,
            reduction,
        }
    }
    
    /// Create CrossEntropyLoss with default parameters
    pub fn default() -> Self {
        CrossEntropyLoss {
            ignore_index: None,
            reduction: Reduction::Mean,
        }
    }
}

impl LossFunction for CrossEntropyLoss {
    fn compute(&self, predictions: &Tensor, targets: &Tensor) -> Tensor {
        // For simplicity, assume predictions are logits and targets are class indices
        // Real implementation would handle softmax and one-hot encoding
        
        // Simplified: compute negative log likelihood
        let mut loss_sum = 0.0;
        let mut count = 0;
        
        let pred_data = predictions.data();
        let target_data = targets.data();
        
        for (i, &target) in target_data.iter().enumerate() {
            if let Some(ignore_idx) = self.ignore_index {
                if target as usize == ignore_idx {
                    continue;
                }
            }
            
            // Simplified: loss = -log(softmax(prediction)[target])
            // For now, use a placeholder
            loss_sum += 1.0;  // Placeholder
            count += 1;
        }
        
        let loss = match self.reduction {
            Reduction::Mean if count > 0 => loss_sum / count as f32,
            Reduction::Sum => loss_sum,
            Reduction::None => loss_sum,  // Would need to return per-element losses
            _ => 0.0,
        };
        
        Tensor::scalar(loss, predictions.requires_grad() || targets.requires_grad())
    }
    
    fn gradient(&self, predictions: &Tensor, targets: &Tensor) -> Tensor {
        // Gradient of cross-entropy loss with softmax
        // dL/dz = softmax(z) - one_hot(target)
        
        // Simplified placeholder
        Tensor::zeros(predictions.shape().to_vec(), false)
    }
    
    fn name(&self) -> &str {
        "CrossEntropy"
    }
}

/// Binary Cross-Entropy loss
#[derive(Debug, Clone)]
pub struct BCELoss {
    reduction: Reduction,
}

impl BCELoss {
    /// Create a new BCELoss
    pub fn new(reduction: Reduction) -> Self {
        BCELoss { reduction }
    }
    
    /// Create BCELoss with default parameters
    pub fn default() -> Self {
        BCELoss { reduction: Reduction::Mean }
    }
}

impl LossFunction for BCELoss {
    fn compute(&self, predictions: &Tensor, targets: &Tensor) -> Tensor {
        // BCE loss: -[t * log(p) + (1-t) * log(1-p)]
        
        let mut loss_sum = 0.0;
        let pred_data = predictions.data();
        let target_data = targets.data();
        
        for (i, (&pred, &target)) in pred_data.iter().zip(target_data).enumerate() {
            // Clamp predictions to avoid log(0)
            let p = pred.max(1e-12).min(1.0 - 1e-12);
            loss_sum += -(target * p.ln() + (1.0 - target) * (1.0 - p).ln());
        }
        
        let loss = match self.reduction {
            Reduction::Mean => loss_sum / pred_data.len() as f32,
            Reduction::Sum => loss_sum,
            Reduction::None => loss_sum,  // Would need to return per-element losses
        };
        
        Tensor::scalar(loss, predictions.requires_grad() || targets.requires_grad())
    }
    
    fn gradient(&self, predictions: &Tensor, targets: &Tensor) -> Tensor {
        // Gradient of BCE: (p - t) / [p * (1-p)]
        
        let pred_data = predictions.data();
        let target_data = targets.data();
        let mut grad_data = Vec::with_capacity(pred_data.len());
        
        for (&pred, &target) in pred_data.iter().zip(target_data) {
            let p = pred.max(1e-12).min(1.0 - 1e-12);
            let grad = (p - target) / (p * (1.0 - p));
            grad_data.push(grad);
        }
        
        Tensor::new(grad_data, predictions.shape().to_vec(), false)
    }
    
    fn name(&self) -> &str {
        "BCE"
    }
}

/// Training metrics tracker
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Metrics {
    pub train_loss: Vec<f32>,
    pub val_loss: Vec<f32>,
    pub train_accuracy: Vec<f32>,
    pub val_accuracy: Vec<f32>,
    pub learning_rates: Vec<f32>,
    pub epoch_times: Vec<f64>,
}

impl Metrics {
    /// Create new metrics tracker
    pub fn new() -> Self {
        Metrics {
            train_loss: Vec::new(),
            val_loss: Vec::new(),
            train_accuracy: Vec::new(),
            val_accuracy: Vec::new(),
            learning_rates: Vec::new(),
            epoch_times: Vec::new(),
        }
    }
    
    /// Add epoch metrics
    pub fn add_epoch(
        &mut self,
        train_loss: f32,
        val_loss: f32,
        train_accuracy: f32,
        val_accuracy: f32,
        learning_rate: f32,
        epoch_time: f64,
    ) {
        self.train_loss.push(train_loss);
        self.val_loss.push(val_loss);
        self.train_accuracy.push(train_accuracy);
        self.val_accuracy.push(val_accuracy);
        self.learning_rates.push(learning_rate);
        self.epoch_times.push(epoch_time);
    }
    
    /// Get best validation loss
    pub fn best_val_loss(&self) -> Option<f32> {
        self.val_loss.iter().cloned().fold(None, |min, x| {
            match min {
                None => Some(x),
                Some(y) => Some(y.min(x)),
            }
        })
    }
    
    /// Get best validation accuracy
    pub fn best_val_accuracy(&self) -> Option<f32> {
        self.val_accuracy.iter().cloned().fold(None, |max, x| {
            match max {
                None => Some(x),
                Some(y) => Some(y.max(x)),
            }
        })
    }
    
    /// Save metrics to file
    pub fn save<P: AsRef<Path>>(&self, path: P) -> std::io::Result<()> {
        let json = serde_json::to_string_pretty(self)?;
        let mut file = File::create(path)?;
        file.write_all(json.as_bytes())?;
        Ok(())
    }
    
    /// Load metrics from file
    pub fn load<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
        let file = File::open(path)?;
        let reader = BufReader::new(file);
        let metrics = serde_json::from_reader(reader)?;
        Ok(metrics)
    }
}

/// Trainer for neural networks
pub struct Trainer {
    model: Box<dyn Layer>,
    optimizer: Box<dyn Optimizer>,
    loss_fn: Box<dyn LossFunction>,
    metrics: Metrics,
    checkpoint_dir: Option<String>,
    early_stopping_patience: usize,
    best_val_loss: f32,
    patience_counter: usize,
}

impl Trainer {
    /// Create a new trainer
    pub fn new(
        model: Box<dyn Layer>,
        optimizer: Box<dyn Optimizer>,
        loss_fn: Box<dyn LossFunction>,
    ) -> Self {
        Trainer {
            model,
            optimizer,
            loss_fn,
            metrics: Metrics::new(),
            checkpoint_dir: None,
            early_stopping_patience: 0,
            best_val_loss: f32::INFINITY,
            patience_counter: 0,
        }
    }
    
    /// Set checkpoint directory
    pub fn with_checkpoint_dir(mut self, dir: &str) -> Self {
        self.checkpoint_dir = Some(dir.to_string());
        self
    }
    
    /// Set early stopping patience
    pub fn with_early_stopping(mut self, patience: usize) -> Self {
        self.early_stopping_patience = patience;
        self
    }
    
    /// Train the model for one epoch
    pub fn train_epoch(
        &mut self,
        train_loader: &mut DataLoader,
        val_loader: &mut Option<&mut DataLoader>,
    ) -> (f32, f32, f32, f32) {
        use std::time::Instant;
        let start_time = Instant::now();
        
        // Set model to training mode
        self.model.train(true);
        
        let mut total_loss = 0.0;
        let mut total_correct = 0;
        let mut total_samples = 0;
        let mut batch_count = 0;
        
        // Training loop
        for (batch_features, batch_labels) in train_loader.iter() {
            self.optimizer.zero_grad();
            
            let mut batch_loss = 0.0;
            let mut batch_correct = 0;
            
            for (features, labels) in batch_features.iter().zip(&batch_labels) {
                // Forward pass
                let predictions = self.model.forward(features);
                
                // Compute loss
                let loss = self.loss_fn.compute(&predictions, labels);
                batch_loss += loss.data()[0];
                
                // Compute accuracy (for classification)
                // Simplified: assume single label prediction
                if predictions.numel() == 1 && labels.numel() == 1 {
                    let pred_val = predictions.data()[0];
                    let label_val = labels.data()[0];
                    if (pred_val > 0.5 && label_val > 0.5) || (pred_val <= 0.5 && label_val <= 0.5) {
                        batch_correct += 1;
                    }
                }
                
                // Backward pass (simplified)
                // In real implementation, we would compute gradients
            }
            
            // Update parameters
            self.optimizer.step();
            
            total_loss += batch_loss;
            total_correct += batch_correct;
            total_samples += batch_features.len();
            batch_count += 1;
        }
        
        let train_loss = if batch_count > 0 { total_loss / batch_count as f32 } else { 0.0 };
        let train_accuracy = if total_samples > 0 { total_correct as f32 / total_samples as f32 } else { 0.0 };
        
        // Validation
        let (val_loss, val_accuracy) = if let Some(val_loader) = val_loader {
            self.validate(val_loader)
        } else {
            (0.0, 0.0)
        };
        
        let epoch_time = start_time.elapsed().as_secs_f64();
        
        // Update metrics
        self.metrics.add_epoch(
            train_loss,
            val_loss,
            train_accuracy,
            val_accuracy,
            self.optimizer.get_lr(),
            epoch_time,
        );
        
        // Check early stopping
        if self.early_stopping_patience > 0 {
            if val_loss < self.best_val_loss {
                self.best_val_loss = val_loss;
                self.patience_counter = 0;
                
                // Save best model
                if let Some(ref dir) = self.checkpoint_dir {
                    self.save_checkpoint(&format!("{}/best_model", dir)).ok();
                }
            } else {
                self.patience_counter += 1;
            }
        }
        
        // Save checkpoint
        if let Some(ref dir) = self.checkpoint_dir {
            let epoch = self.metrics.train_loss.len();
            if epoch % 5 == 0 {  // Save every 5 epochs
                self.save_checkpoint(&format!("{}/epoch_{}", dir, epoch)).ok();
            }
        }
        
        (train_loss, val_loss, train_accuracy, val_accuracy)
    }
    
    /// Validate the model
    pub fn validate(&self, val_loader: &mut DataLoader) -> (f32, f32) {
        // Set model to evaluation mode
        let model = self.model.as_ref();
        // Note: Need mutable reference to call train(false), but we're in &self
        // In real implementation, we would handle this differently
        
        let mut total_loss = 0.0;
        let mut total_correct = 0;
        let mut total_samples = 0;
        let mut batch_count = 0;
        
        for (batch_features, batch_labels) in val_loader.iter() {
            let mut batch_loss = 0.0;
            let mut batch_correct = 0;
            
            for (features, labels) in batch_features.iter().zip(&batch_labels) {
                let predictions = self.model.forward(features);
                let loss = self.loss_fn.compute(&predictions, labels);
                batch_loss += loss.data()[0];
                
                // Compute accuracy
                if predictions.numel() == 1 && labels.numel() == 1 {
                    let pred_val = predictions.data()[0];
                    let label_val = labels.data()[0];
                    if (pred_val > 0.5 && label_val > 0.5) || (pred_val <= 0.5 && label_val <= 0.5) {
                        batch_correct += 1;
                    }
                }
            }
            
            total_loss += batch_loss;
            total_correct += batch_correct;
            total_samples += batch_features.len();
            batch_count += 1;
        }
        
        let val_loss = if batch_count > 0 { total_loss / batch_count as f32 } else { 0.0 };
        let val_accuracy = if total_samples > 0 { total_correct as f32 / total_samples as f32 } else { 0.0 };
        
        (val_loss, val_accuracy)
    }
    
    /// Train for multiple epochs
    pub fn train(
        &mut self,
        train_loader: &mut DataLoader,
        mut val_loader: Option<&mut DataLoader>,
        epochs: usize,
    ) -> &Metrics {
        println!("Starting training for {} epochs", epochs);
        
        for epoch in 1..=epochs {
            let (train_loss, val_loss, train_acc, val_acc) = self.train_epoch(train_loader, &mut val_loader);
            
            println!("Epoch {}/{}: train_loss={:.4}, val_loss={:.4}, train_acc={:.4}, val_acc={:.4}, lr={:.6}",
                epoch, epochs, train_loss, val_loss, train_acc, val_acc, self.optimizer.get_lr());
            
            // Check early stopping
            if self.early_stopping_patience > 0 && self.patience_counter >= self.early_stopping_patience {
                println!("Early stopping triggered after {} epochs", epoch);
                break;
            }
        }
        
        &self.metrics
    }
    
    /// Save model checkpoint
    pub fn save_checkpoint<P: AsRef<Path>>(&self, path: P) -> std::io::Result<()> {
        // Create directory if it doesn't exist
        if let Some(parent) = path.as_ref().parent() {
            create_dir_all(parent)?;
        }
        
        // In real implementation, we would serialize the model state
        // For now, create a placeholder file
        let mut file = File::create(path)?;
        file.write_all(b"Model checkpoint placeholder")?;
        Ok(())
    }
    
    /// Load model checkpoint
    pub fn load_checkpoint<P: AsRef<Path>>(&mut self, path: P) -> std::io::Result<()> {
        // In real implementation, we would deserialize the model state
        // For now, just verify the file exists
        let _file = File::open(path)?;
        Ok(())
    }
}