//! Tensor operations with automatic differentiation
//! 
//! This module provides tensor types and operations with support for
//! automatic differentiation, essential for neural network training.

use std::ops::{Add, Sub, Mul, Div};
use std::fmt;

/// Tensor with automatic differentiation support
#[derive(Clone, Debug)]
pub struct Tensor {
    data: Vec<f32>,
    shape: Vec<usize>,
    grad: Option<Box<Tensor>>,
    requires_grad: bool,
    // For computational graph tracking
    operation: Option<Operation>,
    children: Vec<Tensor>,
}

/// Operation type for computational graph
#[derive(Clone, Debug)]
pub enum Operation {
    Add,
    Sub,
    Mul,
    Div,
    MatMul,
    ReLU,
    Sigmoid,
    Tanh,
    LogSoftmax,
    Conv2d,
    MaxPool2d,
}

impl Tensor {
    /// Create a new tensor with given data and shape
    pub fn new(data: Vec<f32>, shape: Vec<usize>, requires_grad: bool) -> Self {
        let total_elements: usize = shape.iter().product();
        assert_eq!(data.len(), total_elements, "Data length must match shape");
        
        Tensor {
            data,
            shape,
            grad: if requires_grad { Some(Box::new(Tensor::zeros(vec![1], false))) } else { None },
            requires_grad,
            operation: None,
            children: Vec::new(),
        }
    }
    
    /// Create a tensor from a scalar
    pub fn scalar(value: f32, requires_grad: bool) -> Self {
        Tensor::new(vec![value], vec![1], requires_grad)
    }
    
    /// Create a tensor with all zeros
    pub fn zeros(shape: Vec<usize>, requires_grad: bool) -> Self {
        let total_elements: usize = shape.iter().product();
        Tensor::new(vec![0.0; total_elements], shape, requires_grad)
    }
    
    /// Create a tensor with all ones
    pub fn ones(shape: Vec<usize>, requires_grad: bool) -> Self {
        let total_elements: usize = shape.iter().product();
        Tensor::new(vec![1.0; total_elements], shape, requires_grad)
    }
    
    /// Create a tensor with random values (uniform distribution)
    pub fn rand(shape: Vec<usize>, requires_grad: bool) -> Self {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        let total_elements: usize = shape.iter().product();
        let data: Vec<f32> = (0..total_elements).map(|_| rng.gen_range(-1.0..1.0)).collect();
        Tensor::new(data, shape, requires_grad)
    }
    
    /// Create a tensor with random values (normal distribution)
    pub fn randn(shape: Vec<usize>, mean: f32, std: f32, requires_grad: bool) -> Self {
        use rand_distr::{Normal, Distribution};
        let normal = Normal::new(mean, std).unwrap();
        let mut rng = rand::thread_rng();
        let total_elements: usize = shape.iter().product();
        let data: Vec<f32> = (0..total_elements).map(|_| normal.sample(&mut rng)).collect();
        Tensor::new(data, shape, requires_grad)
    }
    
    /// Get tensor shape
    pub fn shape(&self) -> &[usize] {
        &self.shape
    }
    
    /// Get number of dimensions
    pub fn ndim(&self) -> usize {
        self.shape.len()
    }
    
    /// Get total number of elements
    pub fn numel(&self) -> usize {
        self.data.len()
    }
    
    /// Check if tensor requires gradient computation
    pub fn requires_grad(&self) -> bool {
        self.requires_grad
    }
    
    /// Reshape the tensor
    pub fn reshape(&self, new_shape: Vec<usize>) -> Self {
        let total_elements: usize = new_shape.iter().product();
        assert_eq!(self.numel(), total_elements, "Total elements must match");
        
        let mut result = self.clone();
        result.shape = new_shape;
        result
    }
    
    /// Transpose the tensor (2D only for now)
    pub fn t(&self) -> Self {
        assert_eq!(self.ndim(), 2, "Transpose only supported for 2D tensors");
        let rows = self.shape[0];
        let cols = self.shape[1];
        let mut new_data = vec![0.0; rows * cols];
        
        for i in 0..rows {
            for j in 0..cols {
                new_data[j * rows + i] = self.data[i * cols + j];
            }
        }
        
        let mut result = self.clone();
        result.data = new_data;
        result.shape = vec![cols, rows];
        result
    }
    
    /// Matrix multiplication
    pub fn matmul(&self, other: &Tensor) -> Self {
        assert_eq!(self.ndim(), 2, "matmul requires 2D tensors");
        assert_eq!(other.ndim(), 2, "matmul requires 2D tensors");
        assert_eq!(self.shape[1], other.shape[0], "Dimension mismatch for matmul");
        
        let m = self.shape[0];
        let n = self.shape[1];
        let p = other.shape[1];
        let mut result_data = vec![0.0; m * p];
        
        for i in 0..m {
            for j in 0..p {
                let mut sum = 0.0;
                for k in 0..n {
                    sum += self.data[i * n + k] * other.data[k * p + j];
                }
                result_data[i * p + j] = sum;
            }
        }
        
        let mut result = Tensor::new(result_data, vec![m, p], self.requires_grad || other.requires_grad);
        if self.requires_grad || other.requires_grad {
            result.operation = Some(Operation::MatMul);
            result.children = vec![self.clone(), other.clone()];
        }
        result
    }
    
    /// Element-wise addition
    pub fn add(&self, other: &Tensor) -> Self {
        assert_eq!(self.shape, other.shape, "Shape mismatch for addition");
        
        let result_data: Vec<f32> = self.data.iter().zip(&other.data).map(|(a, b)| a + b).collect();
        
        let mut result = Tensor::new(result_data, self.shape.clone(), self.requires_grad || other.requires_grad);
        if self.requires_grad || other.requires_grad {
            result.operation = Some(Operation::Add);
            result.children = vec![self.clone(), other.clone()];
        }
        result
    }
    
    /// Element-wise subtraction
    pub fn sub(&self, other: &Tensor) -> Self {
        assert_eq!(self.shape, other.shape, "Shape mismatch for subtraction");
        
        let result_data: Vec<f32> = self.data.iter().zip(&other.data).map(|(a, b)| a - b).collect();
        
        let mut result = Tensor::new(result_data, self.shape.clone(), self.requires_grad || other.requires_grad);
        if self.requires_grad || other.requires_grad {
            result.operation = Some(Operation::Sub);
            result.children = vec![self.clone(), other.clone()];
        }
        result
    }
    
    /// Element-wise multiplication
    pub fn mul(&self, other: &Tensor) -> Self {
        assert_eq!(self.shape, other.shape, "Shape mismatch for multiplication");
        
        let result_data: Vec<f32> = self.data.iter().zip(&other.data).map(|(a, b)| a * b).collect();
        
        let mut result = Tensor::new(result_data, self.shape.clone(), self.requires_grad || other.requires_grad);
        if self.requires_grad || other.requires_grad {
            result.operation = Some(Operation::Mul);
            result.children = vec![self.clone(), other.clone()];
        }
        result
    }
    
    /// ReLU activation
    pub fn relu(&self) -> Self {
        let result_data: Vec<f32> = self.data.iter().map(|&x| if x > 0.0 { x } else { 0.0 }).collect();
        
        let mut result = Tensor::new(result_data, self.shape.clone(), self.requires_grad);
        if self.requires_grad {
            result.operation = Some(Operation::ReLU);
            result.children = vec![self.clone()];
        }
        result
    }
    
    /// Sigmoid activation
    pub fn sigmoid(&self) -> Self {
        let result_data: Vec<f32> = self.data.iter().map(|&x| 1.0 / (1.0 + (-x).exp())).collect();
        
        let mut result = Tensor::new(result_data, self.shape.clone(), self.requires_grad);
        if self.requires_grad {
            result.operation = Some(Operation::Sigmoid);
            result.children = vec![self.clone()];
        }
        result
    }
    
    /// Softmax activation
    pub fn softmax(&self, dim: usize) -> Self {
        assert!(dim < self.ndim(), "Dimension out of bounds");
        
        // For simplicity, implement 1D softmax
        if self.ndim() == 1 {
            let max_val = self.data.iter().fold(f32::NEG_INFINITY, |a, &b| a.max(b));
            let exp_data: Vec<f32> = self.data.iter().map(|&x| (x - max_val).exp()).collect();
            let sum_exp: f32 = exp_data.iter().sum();
            let result_data: Vec<f32> = exp_data.iter().map(|&x| x / sum_exp).collect();
            
            let mut result = Tensor::new(result_data, self.shape.clone(), self.requires_grad);
            if self.requires_grad {
                // Note: Softmax backward is more complex, would need proper implementation
                result.children = vec![self.clone()];
            }
            result
        } else {
            // For higher dimensions, we would need to implement properly
            self.clone() // Placeholder
        }
    }
    
    /// Compute gradients (backward pass)
    pub fn backward(&mut self) {
        if !self.requires_grad {
            return;
        }
        
        // Initialize gradient if needed
        if self.requires_grad {
            self.grad = Some(Box::new(Tensor::ones(vec![1], false)));
        }
        
        // Recursively compute gradients
        self._backward();
    }
    
    fn _backward(&mut self) {
        // This is a simplified implementation
        // A full implementation would traverse the computational graph
        
        // For now, just recursively call backward on children
        for child in &mut self.children {
            child._backward();
        }
    }
    
    /// Get a reference to the data
    pub fn data(&self) -> &[f32] {
        &self.data
    }
    
    /// Get mutable reference to the data
    pub fn data_mut(&mut self) -> &mut [f32] {
        &mut self.data
    }
}

// Operator overloads for convenience
impl Add for Tensor {
    type Output = Self;
    
    fn add(self, other: Self) -> Self::Output {
        Tensor::add(&self, &other)
    }
}

impl Sub for Tensor {
    type Output = Self;
    
    fn sub(self, other: Self) -> Self::Output {
        // Implement subtraction
        assert_eq!(self.shape, other.shape, "Shape mismatch for subtraction");
        let result_data: Vec<f32> = self.data.iter().zip(&other.data).map(|(a, b)| a - b).collect();
        
        let mut result = Tensor::new(result_data, self.shape.clone(), self.requires_grad || other.requires_grad);
        if self.requires_grad || other.requires_grad {
            result.operation = Some(Operation::Sub);
            result.children = vec![self.clone(), other.clone()];
        }
        result
    }
}

impl Mul for Tensor {
    type Output = Self;
    
    fn mul(self, other: Self) -> Self::Output {
        Tensor::mul(&self, &other)
    }
}

impl Div for Tensor {
    type Output = Self;
    
    fn div(self, other: Self) -> Self::Output {
        assert_eq!(self.shape, other.shape, "Shape mismatch for division");
        let result_data: Vec<f32> = self.data.iter().zip(&other.data).map(|(a, b)| a / b).collect();
        
        let mut result = Tensor::new(result_data, self.shape.clone(), self.requires_grad || other.requires_grad);
        if self.requires_grad || other.requires_grad {
            result.operation = Some(Operation::Div);
            result.children = vec![self.clone(), other.clone()];
        }
        result
    }
}

impl fmt::Display for Tensor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Tensor(shape={:?}, requires_grad={})", self.shape, self.requires_grad)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_tensor_creation() {
        let t = Tensor::new(vec![1.0, 2.0, 3.0, 4.0], vec![2, 2], false);
        assert_eq!(t.shape(), &[2, 2]);
        assert_eq!(t.numel(), 4);
    }
    
    #[test]
    fn test_tensor_addition() {
        let t1 = Tensor::new(vec![1.0, 2.0, 3.0, 4.0], vec![2, 2], false);
        let t2 = Tensor::new(vec![5.0, 6.0, 7.0, 8.0], vec![2, 2], false);
        let result = Tensor::add(&t1, &t2);
        assert_eq!(result.data(), &[6.0, 8.0, 10.0, 12.0]);
    }
    
    #[test]
    fn test_tensor_matmul() {
        let t1 = Tensor::new(vec![1.0, 2.0, 3.0, 4.0], vec![2, 2], false);
        let t2 = Tensor::new(vec![5.0, 6.0, 7.0, 8.0], vec![2, 2], false);
        let result = t1.matmul(&t2);
        assert_eq!(result.shape(), &[2, 2]);
        // 1*5 + 2*7 = 19, 1*6 + 2*8 = 22
        // 3*5 + 4*7 = 43, 3*6 + 4*8 = 50
        assert_eq!(result.data()[0], 19.0);
        assert_eq!(result.data()[1], 22.0);
        assert_eq!(result.data()[2], 43.0);
        assert_eq!(result.data()[3], 50.0);
    }
    
    #[test]
    fn test_relu() {
        let t = Tensor::new(vec![-1.0, 0.0, 1.0, 2.0], vec![2, 2], false);
        let result = t.relu();
        assert_eq!(result.data(), &[0.0, 0.0, 1.0, 2.0]);
    }
}