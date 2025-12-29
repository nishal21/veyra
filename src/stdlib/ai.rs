//! Veyra AI & Machine Learning Library
//! 
//! High-performance tensor operations, neural networks, and ML utilities.
//! Designed to compete with PyTorch, TensorFlow, and other ML frameworks.

use std::ops::{Add, Sub, Mul, Div};

/// N-dimensional Tensor - the core data structure for ML
#[derive(Debug, Clone)]
pub struct Tensor {
    data: Vec<f64>,
    shape: Vec<usize>,
    strides: Vec<usize>,
    requires_grad: bool,
    grad: Option<Box<Tensor>>,
}

impl Tensor {
    /// Create a new tensor from data and shape
    pub fn new(data: Vec<f64>, shape: Vec<usize>) -> Self {
        let strides = compute_strides(&shape);
        Self {
            data,
            shape,
            strides,
            requires_grad: false,
            grad: None,
        }
    }
    
    /// Create tensor filled with zeros
    pub fn zeros(shape: &[usize]) -> Self {
        let size: usize = shape.iter().product();
        Self::new(vec![0.0; size], shape.to_vec())
    }
    
    /// Create tensor filled with ones
    pub fn ones(shape: &[usize]) -> Self {
        let size: usize = shape.iter().product();
        Self::new(vec![1.0; size], shape.to_vec())
    }
    
    /// Create random tensor (uniform 0-1)
    pub fn rand(shape: &[usize]) -> Self {
        use std::time::{SystemTime, UNIX_EPOCH};
        let size: usize = shape.iter().product();
        let seed = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
        
        let data: Vec<f64> = (0..size)
            .map(|i| {
                let a: u128 = 6364136223846793005;
                let c: u128 = 1442695040888963407;
                let m: u128 = 1 << 64;
                let result = (a.wrapping_mul(seed + i as u128).wrapping_add(c)) % m;
                (result as f64) / (m as f64)
            })
            .collect();
        
        Self::new(data, shape.to_vec())
    }
    
    /// Create random tensor (normal distribution)
    pub fn randn(shape: &[usize]) -> Self {
        let uniform = Self::rand(shape);
        // Box-Muller transform for normal distribution
        let data: Vec<f64> = uniform.data.chunks(2)
            .flat_map(|chunk| {
                if chunk.len() == 2 {
                    let u1 = chunk[0].max(1e-10);
                    let u2 = chunk[1];
                    let r = (-2.0 * u1.ln()).sqrt();
                    let theta = 2.0 * std::f64::consts::PI * u2;
                    vec![r * theta.cos(), r * theta.sin()]
                } else {
                    vec![chunk[0]]
                }
            })
            .take(uniform.data.len())
            .collect();
        
        Self::new(data, shape.to_vec())
    }
    
    /// Create identity matrix
    pub fn eye(n: usize) -> Self {
        let mut data = vec![0.0; n * n];
        for i in 0..n {
            data[i * n + i] = 1.0;
        }
        Self::new(data, vec![n, n])
    }
    
    /// Get tensor shape
    pub fn shape(&self) -> &[usize] {
        &self.shape
    }
    
    /// Get total number of elements
    pub fn numel(&self) -> usize {
        self.data.len()
    }
    
    /// Get element at index
    pub fn get(&self, indices: &[usize]) -> f64 {
        let idx = self.compute_index(indices);
        self.data[idx]
    }
    
    /// Set element at index
    pub fn set(&mut self, indices: &[usize], value: f64) {
        let idx = self.compute_index(indices);
        self.data[idx] = value;
    }
    
    fn compute_index(&self, indices: &[usize]) -> usize {
        indices.iter()
            .zip(self.strides.iter())
            .map(|(i, s)| i * s)
            .sum()
    }
    
    /// Reshape tensor
    pub fn reshape(&self, new_shape: Vec<usize>) -> Self {
        let new_size: usize = new_shape.iter().product();
        assert_eq!(new_size, self.data.len(), "Shape mismatch in reshape");
        Self::new(self.data.clone(), new_shape)
    }
    
    /// Transpose 2D tensor
    pub fn transpose(&self) -> Self {
        assert_eq!(self.shape.len(), 2, "Transpose only for 2D tensors");
        let (rows, cols) = (self.shape[0], self.shape[1]);
        let mut data = vec![0.0; rows * cols];
        
        for i in 0..rows {
            for j in 0..cols {
                data[j * rows + i] = self.data[i * cols + j];
            }
        }
        
        Self::new(data, vec![cols, rows])
    }
    
    /// Matrix multiplication
    pub fn matmul(&self, other: &Tensor) -> Self {
        assert_eq!(self.shape.len(), 2, "matmul requires 2D tensors");
        assert_eq!(other.shape.len(), 2, "matmul requires 2D tensors");
        assert_eq!(self.shape[1], other.shape[0], "Matrix dimension mismatch");
        
        let (m, k) = (self.shape[0], self.shape[1]);
        let n = other.shape[1];
        let mut result = vec![0.0; m * n];
        
        for i in 0..m {
            for j in 0..n {
                let mut sum = 0.0;
                for l in 0..k {
                    sum += self.data[i * k + l] * other.data[l * n + j];
                }
                result[i * n + j] = sum;
            }
        }
        
        Self::new(result, vec![m, n])
    }
    
    /// Element-wise operations
    pub fn add(&self, other: &Tensor) -> Self {
        self.elementwise(other, |a, b| a + b)
    }
    
    pub fn sub(&self, other: &Tensor) -> Self {
        self.elementwise(other, |a, b| a - b)
    }
    
    pub fn mul(&self, other: &Tensor) -> Self {
        self.elementwise(other, |a, b| a * b)
    }
    
    pub fn div(&self, other: &Tensor) -> Self {
        self.elementwise(other, |a, b| a / b)
    }
    
    fn elementwise<F>(&self, other: &Tensor, op: F) -> Self 
    where F: Fn(f64, f64) -> f64
    {
        assert_eq!(self.shape, other.shape, "Shape mismatch");
        let data: Vec<f64> = self.data.iter()
            .zip(other.data.iter())
            .map(|(a, b)| op(*a, *b))
            .collect();
        Self::new(data, self.shape.clone())
    }
    
    /// Scalar operations
    pub fn add_scalar(&self, scalar: f64) -> Self {
        Self::new(self.data.iter().map(|x| x + scalar).collect(), self.shape.clone())
    }
    
    pub fn mul_scalar(&self, scalar: f64) -> Self {
        Self::new(self.data.iter().map(|x| x * scalar).collect(), self.shape.clone())
    }
    
    /// Reduction operations
    pub fn sum(&self) -> f64 {
        self.data.iter().sum()
    }
    
    pub fn mean(&self) -> f64 {
        self.sum() / self.data.len() as f64
    }
    
    pub fn max(&self) -> f64 {
        self.data.iter().cloned().fold(f64::NEG_INFINITY, f64::max)
    }
    
    pub fn min(&self) -> f64 {
        self.data.iter().cloned().fold(f64::INFINITY, f64::min)
    }
    
    pub fn argmax(&self) -> usize {
        self.data.iter()
            .enumerate()
            .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap())
            .map(|(i, _)| i)
            .unwrap_or(0)
    }
    
    /// Activation functions
    pub fn relu(&self) -> Self {
        Self::new(self.data.iter().map(|x| x.max(0.0)).collect(), self.shape.clone())
    }
    
    pub fn sigmoid(&self) -> Self {
        Self::new(self.data.iter().map(|x| 1.0 / (1.0 + (-x).exp())).collect(), self.shape.clone())
    }
    
    pub fn tanh(&self) -> Self {
        Self::new(self.data.iter().map(|x| x.tanh()).collect(), self.shape.clone())
    }
    
    pub fn softmax(&self) -> Self {
        let max = self.max();
        let exp: Vec<f64> = self.data.iter().map(|x| (x - max).exp()).collect();
        let sum: f64 = exp.iter().sum();
        Self::new(exp.iter().map(|x| x / sum).collect(), self.shape.clone())
    }
    
    pub fn leaky_relu(&self, alpha: f64) -> Self {
        Self::new(
            self.data.iter().map(|x| if *x > 0.0 { *x } else { alpha * x }).collect(),
            self.shape.clone()
        )
    }
    
    /// Layer operations
    pub fn dropout(&self, p: f64) -> Self {
        let mask = Self::rand(&self.shape);
        let data: Vec<f64> = self.data.iter()
            .zip(mask.data.iter())
            .map(|(x, m)| if *m > p { x / (1.0 - p) } else { 0.0 })
            .collect();
        Self::new(data, self.shape.clone())
    }
    
    pub fn batch_norm(&self, gamma: f64, beta: f64, eps: f64) -> Self {
        let mean = self.mean();
        let variance = self.data.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / self.data.len() as f64;
        let data: Vec<f64> = self.data.iter()
            .map(|x| gamma * (x - mean) / (variance + eps).sqrt() + beta)
            .collect();
        Self::new(data, self.shape.clone())
    }
}

fn compute_strides(shape: &[usize]) -> Vec<usize> {
    let mut strides = vec![1; shape.len()];
    for i in (0..shape.len() - 1).rev() {
        strides[i] = strides[i + 1] * shape[i + 1];
    }
    strides
}

/// Neural Network Layer trait
pub trait Layer {
    fn forward(&self, input: &Tensor) -> Tensor;
    fn parameters(&self) -> Vec<&Tensor>;
}

/// Dense (fully connected) layer
pub struct Dense {
    weights: Tensor,
    bias: Tensor,
}

impl Dense {
    pub fn new(input_size: usize, output_size: usize) -> Self {
        // Xavier initialization
        let scale = (2.0 / (input_size + output_size) as f64).sqrt();
        let weights = Tensor::randn(&[input_size, output_size]).mul_scalar(scale);
        let bias = Tensor::zeros(&[output_size]);
        Self { weights, bias }
    }
}

impl Layer for Dense {
    fn forward(&self, input: &Tensor) -> Tensor {
        input.matmul(&self.weights).add(&self.bias)
    }
    
    fn parameters(&self) -> Vec<&Tensor> {
        vec![&self.weights, &self.bias]
    }
}

/// Sequential model
pub struct Sequential {
    layers: Vec<Box<dyn Layer>>,
}

impl Sequential {
    pub fn new() -> Self {
        Self { layers: Vec::new() }
    }
    
    pub fn add<L: Layer + 'static>(mut self, layer: L) -> Self {
        self.layers.push(Box::new(layer));
        self
    }
    
    pub fn forward(&self, input: &Tensor) -> Tensor {
        let mut x = input.clone();
        for layer in &self.layers {
            x = layer.forward(&x);
        }
        x
    }
}

impl Default for Sequential {
    fn default() -> Self {
        Self::new()
    }
}

/// Loss functions
pub mod loss {
    use super::Tensor;
    
    /// Mean Squared Error
    pub fn mse(predictions: &Tensor, targets: &Tensor) -> f64 {
        let diff = predictions.sub(targets);
        diff.data.iter().map(|x| x * x).sum::<f64>() / diff.data.len() as f64
    }
    
    /// Cross Entropy Loss
    pub fn cross_entropy(predictions: &Tensor, targets: &Tensor) -> f64 {
        let eps = 1e-10;
        -predictions.data.iter()
            .zip(targets.data.iter())
            .map(|(p, t)| t * (p + eps).ln())
            .sum::<f64>() / predictions.data.len() as f64
    }
    
    /// Binary Cross Entropy
    pub fn binary_cross_entropy(predictions: &Tensor, targets: &Tensor) -> f64 {
        let eps = 1e-10;
        -predictions.data.iter()
            .zip(targets.data.iter())
            .map(|(p, t)| t * (p + eps).ln() + (1.0 - t) * (1.0 - p + eps).ln())
            .sum::<f64>() / predictions.data.len() as f64
    }
}

/// Optimizers
pub mod optim {
    use super::Tensor;
    
    /// Stochastic Gradient Descent
    pub struct SGD {
        learning_rate: f64,
        momentum: f64,
        velocities: Vec<Tensor>,
    }
    
    impl SGD {
        pub fn new(learning_rate: f64) -> Self {
            Self {
                learning_rate,
                momentum: 0.0,
                velocities: Vec::new(),
            }
        }
        
        pub fn with_momentum(mut self, momentum: f64) -> Self {
            self.momentum = momentum;
            self
        }
        
        pub fn step(&mut self, params: &mut [Tensor], grads: &[Tensor]) {
            if self.velocities.is_empty() {
                self.velocities = grads.iter()
                    .map(|g| Tensor::zeros(&g.shape))
                    .collect();
            }
            
            for ((param, grad), velocity) in params.iter_mut().zip(grads).zip(&mut self.velocities) {
                // v = momentum * v - lr * grad
                *velocity = velocity.mul_scalar(self.momentum)
                    .sub(&grad.mul_scalar(self.learning_rate));
                // param = param + v
                *param = param.add(velocity);
            }
        }
    }
    
    /// Adam optimizer
    pub struct Adam {
        learning_rate: f64,
        beta1: f64,
        beta2: f64,
        epsilon: f64,
        m: Vec<Tensor>,
        v: Vec<Tensor>,
        t: usize,
    }
    
    impl Adam {
        pub fn new(learning_rate: f64) -> Self {
            Self {
                learning_rate,
                beta1: 0.9,
                beta2: 0.999,
                epsilon: 1e-8,
                m: Vec::new(),
                v: Vec::new(),
                t: 0,
            }
        }
        
        pub fn step(&mut self, params: &mut [Tensor], grads: &[Tensor]) {
            self.t += 1;
            
            if self.m.is_empty() {
                self.m = grads.iter().map(|g| Tensor::zeros(&g.shape)).collect();
                self.v = grads.iter().map(|g| Tensor::zeros(&g.shape)).collect();
            }
            
            let bias_correction1 = 1.0 - self.beta1.powi(self.t as i32);
            let bias_correction2 = 1.0 - self.beta2.powi(self.t as i32);
            
            for (i, (param, grad)) in params.iter_mut().zip(grads).enumerate() {
                // m = beta1 * m + (1 - beta1) * grad
                self.m[i] = self.m[i].mul_scalar(self.beta1)
                    .add(&grad.mul_scalar(1.0 - self.beta1));
                
                // v = beta2 * v + (1 - beta2) * grad^2
                let grad_sq = grad.mul(grad);
                self.v[i] = self.v[i].mul_scalar(self.beta2)
                    .add(&grad_sq.mul_scalar(1.0 - self.beta2));
                
                // Bias-corrected estimates
                let m_hat = self.m[i].mul_scalar(1.0 / bias_correction1);
                let v_hat = self.v[i].mul_scalar(1.0 / bias_correction2);
                
                // Update: param = param - lr * m_hat / (sqrt(v_hat) + eps)
                let update: Vec<f64> = m_hat.data.iter()
                    .zip(v_hat.data.iter())
                    .map(|(m, v)| self.learning_rate * m / (v.sqrt() + self.epsilon))
                    .collect();
                
                let update_tensor = Tensor::new(update, param.shape.clone());
                *param = param.sub(&update_tensor);
            }
        }
    }
}

/// Data utilities
pub mod data {
    use super::Tensor;
    
    /// Split data into batches
    pub fn batch(data: &Tensor, batch_size: usize) -> Vec<Tensor> {
        let total = data.shape()[0];
        let num_batches = (total + batch_size - 1) / batch_size;
        
        (0..num_batches)
            .map(|i| {
                let start = i * batch_size;
                let end = (start + batch_size).min(total);
                let size = end - start;
                let row_size = data.numel() / total;
                
                let batch_data: Vec<f64> = data.data[start * row_size..end * row_size].to_vec();
                let mut shape = data.shape.clone();
                shape[0] = size;
                Tensor::new(batch_data, shape)
            })
            .collect()
    }
    
    /// Shuffle data
    pub fn shuffle(data: &mut Tensor) {
        use std::time::{SystemTime, UNIX_EPOCH};
        let seed = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
        
        let n = data.shape()[0];
        let row_size = data.numel() / n;
        
        for i in (1..n).rev() {
            let j = ((seed.wrapping_mul(i as u128)) % (i as u128 + 1)) as usize;
            // Swap rows i and j
            for k in 0..row_size {
                data.data.swap(i * row_size + k, j * row_size + k);
            }
        }
    }
    
    /// Train/test split
    pub fn train_test_split(data: &Tensor, test_ratio: f64) -> (Tensor, Tensor) {
        let n = data.shape()[0];
        let test_size = (n as f64 * test_ratio) as usize;
        let train_size = n - test_size;
        let row_size = data.numel() / n;
        
        let train_data = data.data[..train_size * row_size].to_vec();
        let test_data = data.data[train_size * row_size..].to_vec();
        
        let mut train_shape = data.shape.clone();
        train_shape[0] = train_size;
        let mut test_shape = data.shape.clone();
        test_shape[0] = test_size;
        
        (Tensor::new(train_data, train_shape), Tensor::new(test_data, test_shape))
    }
    
    /// One-hot encoding
    pub fn one_hot(labels: &[usize], num_classes: usize) -> Tensor {
        let mut data = vec![0.0; labels.len() * num_classes];
        for (i, &label) in labels.iter().enumerate() {
            data[i * num_classes + label] = 1.0;
        }
        Tensor::new(data, vec![labels.len(), num_classes])
    }
}

/// Metrics
pub mod metrics {
    use super::Tensor;
    
    /// Classification accuracy
    pub fn accuracy(predictions: &Tensor, targets: &Tensor) -> f64 {
        let n = predictions.shape()[0];
        let num_classes = predictions.shape()[1];
        
        let correct: usize = (0..n)
            .filter(|&i| {
                let pred_slice = &predictions.data[i * num_classes..(i + 1) * num_classes];
                let target_slice = &targets.data[i * num_classes..(i + 1) * num_classes];
                
                let pred_class = pred_slice.iter()
                    .enumerate()
                    .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap())
                    .map(|(i, _)| i)
                    .unwrap();
                
                let target_class = target_slice.iter()
                    .enumerate()
                    .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap())
                    .map(|(i, _)| i)
                    .unwrap();
                
                pred_class == target_class
            })
            .count();
        
        correct as f64 / n as f64
    }
    
    /// R² score
    pub fn r2_score(predictions: &Tensor, targets: &Tensor) -> f64 {
        let mean = targets.mean();
        let ss_res: f64 = predictions.data.iter()
            .zip(targets.data.iter())
            .map(|(p, t)| (t - p).powi(2))
            .sum();
        let ss_tot: f64 = targets.data.iter()
            .map(|t| (t - mean).powi(2))
            .sum();
        
        1.0 - ss_res / ss_tot
    }
}
