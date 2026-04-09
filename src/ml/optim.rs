//! Optimization algorithms for neural network training
//! 
//! This module provides various optimization algorithms including:
//! - Stochastic Gradient Descent (SGD)
//! - SGD with Momentum
//! - Adam
//! - RMSprop
//! - Adagrad
//! - Adadelta

use super::tensor::Tensor;
use std::collections::HashMap;

/// Base trait for all optimizers
pub trait Optimizer {
    /// Update parameters based on gradients
    fn step(&mut self);
    
    /// Zero out gradients
    fn zero_grad(&mut self);
    
    /// Add parameters to optimize
    fn add_parameters(&mut self, parameters: Vec<Tensor>);
    
    /// Get learning rate
    fn get_lr(&self) -> f32;
    
    /// Set learning rate
    fn set_lr(&mut self, lr: f32);
}

/// Stochastic Gradient Descent optimizer
#[derive(Debug)]
pub struct SGD {
    parameters: Vec<Tensor>,
    lr: f32,
    momentum: f32,
    dampening: f32,
    nesterov: bool,
    velocity: HashMap<usize, Tensor>,  // Parameter index -> velocity
}

impl SGD {
    /// Create a new SGD optimizer
    pub fn new(lr: f32, momentum: f32, dampening: f32, nesterov: bool) -> Self {
        SGD {
            parameters: Vec::new(),
            lr,
            momentum,
            dampening,
            nesterov,
            velocity: HashMap::new(),
        }
    }
    
    /// Create SGD with default parameters
    pub fn default() -> Self {
        SGD::new(0.01, 0.0, 0.0, false)
    }
    
    /// Create SGD with momentum
    pub fn with_momentum(lr: f32, momentum: f32) -> Self {
        SGD::new(lr, momentum, 0.0, false)
    }
    
    /// Create SGD with Nesterov momentum
    pub fn nesterov(lr: f32, momentum: f32) -> Self {
        SGD::new(lr, momentum, 0.0, true)
    }
}

impl Optimizer for SGD {
    fn step(&mut self) {
        for (i, param) in self.parameters.iter_mut().enumerate() {
            // Simplified: would need actual gradient computation
            // For now, just demonstrate the algorithm structure
            
            if self.momentum > 0.0 {
                // Momentum update
                let grad = Tensor::rand(param.shape().to_vec(), false);  // Placeholder for gradient
                
                if !self.velocity.contains_key(&i) {
                    // Initialize velocity
                    self.velocity.insert(i, Tensor::zeros(param.shape().to_vec(), false));
                }
                
                let velocity = self.velocity.get_mut(&i).unwrap();
                // velocity = momentum * velocity - lr * grad
                // param = param + velocity
                
                if self.nesterov {
                    // Nesterov momentum
                    // velocity_prev = velocity
                    // velocity = momentum * velocity - lr * grad
                    // param = param - momentum * velocity_prev + (1 + momentum) * velocity
                }
            } else {
                // Vanilla SGD
                // param = param - lr * grad
            }
        }
    }
    
    fn zero_grad(&mut self) {
        // Reset gradients for all parameters
        for param in &mut self.parameters {
            // In a real implementation, we would zero out the gradient tensor
            // param.grad = Some(Tensor::zeros(...))
        }
    }
    
    fn add_parameters(&mut self, parameters: Vec<Tensor>) {
        self.parameters.extend(parameters);
    }
    
    fn get_lr(&self) -> f32 {
        self.lr
    }
    
    fn set_lr(&mut self, lr: f32) {
        self.lr = lr;
    }
}

/// Adam optimizer (Adaptive Moment Estimation)
#[derive(Debug)]
pub struct Adam {
    parameters: Vec<Tensor>,
    lr: f32,
    betas: (f32, f32),
    eps: f32,
    weight_decay: f32,
    amsgrad: bool,
    
    // State
    step: usize,
    exp_avg: HashMap<usize, Tensor>,  // First moment estimate
    exp_avg_sq: HashMap<usize, Tensor>, // Second moment estimate
    max_exp_avg_sq: HashMap<usize, Tensor>, // For AMSGrad
}

impl Adam {
    /// Create a new Adam optimizer
    pub fn new(
        lr: f32,
        betas: (f32, f32),
        eps: f32,
        weight_decay: f32,
        amsgrad: bool,
    ) -> Self {
        Adam {
            parameters: Vec::new(),
            lr,
            betas,
            eps,
            weight_decay,
            amsgrad,
            step: 0,
            exp_avg: HashMap::new(),
            exp_avg_sq: HashMap::new(),
            max_exp_avg_sq: HashMap::new(),
        }
    }
    
    /// Create Adam with default parameters
    pub fn default() -> Self {
        Adam::new(0.001, (0.9, 0.999), 1e-8, 0.0, false)
    }
}

impl Optimizer for Adam {
    fn step(&mut self) {
        self.step += 1;
        
        for (i, param) in self.parameters.iter_mut().enumerate() {
            // Simplified Adam update
            // In real implementation, we would:
            // 1. Get gradient
            // 2. Apply weight decay if needed
            // 3. Update biased first moment estimate: m = beta1 * m + (1 - beta1) * g
            // 4. Update biased second moment estimate: v = beta2 * v + (1 - beta2) * g^2
            // 5. Compute bias-corrected estimates: m_hat = m / (1 - beta1^t), v_hat = v / (1 - beta2^t)
            // 6. Update parameters: param = param - lr * m_hat / (sqrt(v_hat) + eps)
            
            // Initialize state if needed
            if !self.exp_avg.contains_key(&i) {
                self.exp_avg.insert(i, Tensor::zeros(param.shape().to_vec(), false));
                self.exp_avg_sq.insert(i, Tensor::zeros(param.shape().to_vec(), false));
                if self.amsgrad {
                    self.max_exp_avg_sq.insert(i, Tensor::zeros(param.shape().to_vec(), false));
                }
            }
            
            // Placeholder for actual update logic
            let grad = Tensor::rand(param.shape().to_vec(), false);  // Placeholder
            
            // Update would happen here in real implementation
        }
    }
    
    fn zero_grad(&mut self) {
        for param in &mut self.parameters {
            // Reset gradients
        }
    }
    
    fn add_parameters(&mut self, parameters: Vec<Tensor>) {
        self.parameters.extend(parameters);
    }
    
    fn get_lr(&self) -> f32 {
        self.lr
    }
    
    fn set_lr(&mut self, lr: f32) {
        self.lr = lr;
    }
}

/// RMSprop optimizer
#[derive(Debug)]
pub struct RMSprop {
    parameters: Vec<Tensor>,
    lr: f32,
    alpha: f32,
    eps: f32,
    weight_decay: f32,
    momentum: f32,
    centered: bool,
    
    // State
    square_avg: HashMap<usize, Tensor>,
    momentum_buffer: HashMap<usize, Tensor>,
    grad_avg: HashMap<usize, Tensor>,  // For centered version
}

impl RMSprop {
    /// Create a new RMSprop optimizer
    pub fn new(
        lr: f32,
        alpha: f32,
        eps: f32,
        weight_decay: f32,
        momentum: f32,
        centered: bool,
    ) -> Self {
        RMSprop {
            parameters: Vec::new(),
            lr,
            alpha,
            eps,
            weight_decay,
            momentum,
            centered,
            square_avg: HashMap::new(),
            momentum_buffer: HashMap::new(),
            grad_avg: HashMap::new(),
        }
    }
    
    /// Create RMSprop with default parameters
    pub fn default() -> Self {
        RMSprop::new(0.01, 0.99, 1e-8, 0.0, 0.0, false)
    }
}

impl Optimizer for RMSprop {
    fn step(&mut self) {
        for (i, param) in self.parameters.iter_mut().enumerate() {
            // Simplified RMSprop update
            // In real implementation:
            // 1. square_avg = alpha * square_avg + (1 - alpha) * grad^2
            // 2. If centered: grad_avg = alpha * grad_avg + (1 - alpha) * grad
            // 3. If momentum > 0: buf = momentum * buf + lr * grad / sqrt(square_avg - grad_avg^2 + eps)
            //    param = param - buf
            // 4. Else: param = param - lr * grad / sqrt(square_avg + eps)
            
            // Initialize state if needed
            if !self.square_avg.contains_key(&i) {
                self.square_avg.insert(i, Tensor::zeros(param.shape().to_vec(), false));
                if self.centered {
                    self.grad_avg.insert(i, Tensor::zeros(param.shape().to_vec(), false));
                }
                if self.momentum > 0.0 {
                    self.momentum_buffer.insert(i, Tensor::zeros(param.shape().to_vec(), false));
                }
            }
            
            // Placeholder for actual update
        }
    }
    
    fn zero_grad(&mut self) {
        for param in &mut self.parameters {
            // Reset gradients
        }
    }
    
    fn add_parameters(&mut self, parameters: Vec<Tensor>) {
        self.parameters.extend(parameters);
    }
    
    fn get_lr(&self) -> f32 {
        self.lr
    }
    
    fn set_lr(&mut self, lr: f32) {
        self.lr = lr;
    }
}

/// Adagrad optimizer
#[derive(Debug)]
pub struct Adagrad {
    parameters: Vec<Tensor>,
    lr: f32,
    lr_decay: f32,
    weight_decay: f32,
    eps: f32,
    
    // State
    sum: HashMap<usize, Tensor>,  // Sum of squared gradients
}

impl Adagrad {
    /// Create a new Adagrad optimizer
    pub fn new(lr: f32, lr_decay: f32, weight_decay: f32, eps: f32) -> Self {
        Adagrad {
            parameters: Vec::new(),
            lr,
            lr_decay,
            weight_decay,
            eps,
            sum: HashMap::new(),
        }
    }
    
    /// Create Adagrad with default parameters
    pub fn default() -> Self {
        Adagrad::new(0.01, 0.0, 0.0, 1e-10)
    }
}

impl Optimizer for Adagrad {
    fn step(&mut self) {
        for (i, param) in self.parameters.iter_mut().enumerate() {
            // Adagrad update:
            // sum = sum + grad^2
            // param = param - lr * grad / (sqrt(sum) + eps)
            
            if !self.sum.contains_key(&i) {
                self.sum.insert(i, Tensor::zeros(param.shape().to_vec(), false));
            }
            
            // Placeholder for actual update
        }
    }
    
    fn zero_grad(&mut self) {
        for param in &mut self.parameters {
            // Reset gradients
        }
    }
    
    fn add_parameters(&mut self, parameters: Vec<Tensor>) {
        self.parameters.extend(parameters);
    }
    
    fn get_lr(&self) -> f32 {
        self.lr
    }
    
    fn set_lr(&mut self, lr: f32) {
        self.lr = lr;
    }
}

/// Adadelta optimizer
#[derive(Debug)]
pub struct Adadelta {
    parameters: Vec<Tensor>,
    lr: f32,
    rho: f32,
    eps: f32,
    weight_decay: f32,
    
    // State
    square_avg: HashMap<usize, Tensor>,  // Running average of squared gradients
    acc_delta: HashMap<usize, Tensor>,   // Running average of squared updates
}

impl Adadelta {
    /// Create a new Adadelta optimizer
    pub fn new(lr: f32, rho: f32, eps: f32, weight_decay: f32) -> Self {
        Adadelta {
            parameters: Vec::new(),
            lr,
            rho,
            eps,
            weight_decay,
            square_avg: HashMap::new(),
            acc_delta: HashMap::new(),
        }
    }
    
    /// Create Adadelta with default parameters
    pub fn default() -> Self {
        Adadelta::new(1.0, 0.9, 1e-6, 0.0)
    }
}

impl Optimizer for Adadelta {
    fn step(&mut self) {
        for (i, param) in self.parameters.iter_mut().enumerate() {
            // Adadelta update:
            // square_avg = rho * square_avg + (1 - rho) * grad^2
            // delta = sqrt(acc_delta + eps) / sqrt(square_avg + eps) * grad
            // param = param - delta
            // acc_delta = rho * acc_delta + (1 - rho) * delta^2
            
            if !self.square_avg.contains_key(&i) {
                self.square_avg.insert(i, Tensor::zeros(param.shape().to_vec(), false));
                self.acc_delta.insert(i, Tensor::zeros(param.shape().to_vec(), false));
            }
            
            // Placeholder for actual update
        }
    }
    
    fn zero_grad(&mut self) {
        for param in &mut self.parameters {
            // Reset gradients
        }
    }
    
    fn add_parameters(&mut self, parameters: Vec<Tensor>) {
        self.parameters.extend(parameters);
    }
    
    fn get_lr(&self) -> f32 {
        self.lr
    }
    
    fn set_lr(&mut self, lr: f32) {
        self.lr = lr;
    }
}

/// Learning rate scheduler trait
pub trait LRScheduler {
    /// Get current learning rate
    fn get_lr(&self) -> f32;
    
    /// Step the scheduler (usually called after each epoch)
    fn step(&mut self);
    
    /// Get number of steps taken
    fn get_steps(&self) -> usize;
}

/// Step learning rate scheduler
#[derive(Debug)]
pub struct StepLR {
    initial_lr: f32,
    step_size: usize,
    gamma: f32,
    steps: usize,
}

impl StepLR {
    /// Create a new step learning rate scheduler
    pub fn new(initial_lr: f32, step_size: usize, gamma: f32) -> Self {
        StepLR {
            initial_lr,
            step_size,
            gamma,
            steps: 0,
        }
    }
}

impl LRScheduler for StepLR {
    fn get_lr(&self) -> f32 {
        let factor = (self.steps / self.step_size) as u32;
        self.initial_lr * self.gamma.powi(factor as i32)
    }
    
    fn step(&mut self) {
        self.steps += 1;
    }
    
    fn get_steps(&self) -> usize {
        self.steps
    }
}

/// Exponential learning rate scheduler
#[derive(Debug)]
pub struct ExponentialLR {
    initial_lr: f32,
    gamma: f32,
    steps: usize,
}

impl ExponentialLR {
    /// Create a new exponential learning rate scheduler
    pub fn new(initial_lr: f32, gamma: f32) -> Self {
        ExponentialLR {
            initial_lr,
            gamma,
            steps: 0,
        }
    }
}

impl LRScheduler for ExponentialLR {
    fn get_lr(&self) -> f32 {
        self.initial_lr * self.gamma.powi(self.steps as i32)
    }
    
    fn step(&mut self) {
        self.steps += 1;
    }
    
    fn get_steps(&self) -> usize {
        self.steps
    }
}

/// Cosine annealing learning rate scheduler
#[derive(Debug)]
pub struct CosineAnnealingLR {
    initial_lr: f32,
    t_max: usize,
    eta_min: f32,
    steps: usize,
}

impl CosineAnnealingLR {
    /// Create a new cosine annealing learning rate scheduler
    pub fn new(initial_lr: f32, t_max: usize, eta_min: f32) -> Self {
        CosineAnnealingLR {
            initial_lr,
            t_max,
            eta_min,
            steps: 0,
        }
    }
}

impl LRScheduler for CosineAnnealingLR {
    fn get_lr(&self) -> f32 {
        use std::f32::consts::PI;
        
        if self.steps >= self.t_max {
            self.eta_min
        } else {
            let cos_factor = (PI * self.steps as f32 / self.t_max as f32).cos();
            self.eta_min + 0.5 * (self.initial_lr - self.eta_min) * (1.0 + cos_factor)
        }
    }
    
    fn step(&mut self) {
        self.steps += 1;
    }
    
    fn get_steps(&self) -> usize {
        self.steps
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_sgd_creation() {
        let sgd = SGD::default();
        assert_eq!(sgd.get_lr(), 0.01);
    }
}