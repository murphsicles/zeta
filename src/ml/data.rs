//! Data loading and preprocessing utilities
//! 
//! This module provides data loading, preprocessing, and augmentation
//! utilities for machine learning pipelines.

use super::tensor::Tensor;
use std::path::Path;
use std::fs::File;
use std::io::BufReader;

/// Dataset trait for data loading
pub trait Dataset {
    /// Get the number of samples in the dataset
    fn len(&self) -> usize;
    
    /// Check if dataset is empty
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
    
    /// Get a sample by index
    fn get(&self, index: usize) -> Option<(Tensor, Tensor)>;  // (features, label)
    
    /// Get all samples (for small datasets)
    fn get_all(&self) -> Vec<(Tensor, Tensor)>;
}

/// In-memory dataset
#[derive(Debug)]
pub struct MemoryDataset {
    features: Vec<Tensor>,
    labels: Vec<Tensor>,
    name: String,
}

impl MemoryDataset {
    /// Create a new in-memory dataset
    pub fn new(features: Vec<Tensor>, labels: Vec<Tensor>, name: &str) -> Self {
        assert_eq!(features.len(), labels.len(), "Features and labels must have same length");
        MemoryDataset {
            features,
            labels,
            name: name.to_string(),
        }
    }
    
    /// Create a dataset from raw data
    pub fn from_raw_data(
        features_data: Vec<Vec<f32>>,
        labels_data: Vec<Vec<f32>>,
        feature_shape: Vec<usize>,
        label_shape: Vec<usize>,
        name: &str,
    ) -> Self {
        let features: Vec<Tensor> = features_data
            .into_iter()
            .map(|data| Tensor::new(data, feature_shape.clone(), false))
            .collect();
        
        let labels: Vec<Tensor> = labels_data
            .into_iter()
            .map(|data| Tensor::new(data, label_shape.clone(), false))
            .collect();
        
        MemoryDataset::new(features, labels, name)
    }
    
    /// Split dataset into train and test sets
    pub fn train_test_split(&self, test_ratio: f32, shuffle: bool) -> (MemoryDataset, MemoryDataset) {
        assert!(test_ratio > 0.0 && test_ratio < 1.0, "Test ratio must be between 0 and 1");
        
        let mut indices: Vec<usize> = (0..self.len()).collect();
        if shuffle {
            use rand::seq::SliceRandom;
            use rand::thread_rng;
            indices.shuffle(&mut thread_rng());
        }
        
        let split_idx = (self.len() as f32 * (1.0 - test_ratio)) as usize;
        let train_indices = &indices[..split_idx];
        let test_indices = &indices[split_idx..];
        
        let train_features: Vec<Tensor> = train_indices.iter().map(|&i| self.features[i].clone()).collect();
        let train_labels: Vec<Tensor> = train_indices.iter().map(|&i| self.labels[i].clone()).collect();
        
        let test_features: Vec<Tensor> = test_indices.iter().map(|&i| self.features[i].clone()).collect();
        let test_labels: Vec<Tensor> = test_indices.iter().map(|&i| self.labels[i].clone()).collect();
        
        (
            MemoryDataset::new(train_features, train_labels, &format!("{}_train", self.name)),
            MemoryDataset::new(test_features, test_labels, &format!("{}_test", self.name)),
        )
    }
}

impl Dataset for MemoryDataset {
    fn len(&self) -> usize {
        self.features.len()
    }
    
    fn get(&self, index: usize) -> Option<(Tensor, Tensor)> {
        if index < self.len() {
            Some((self.features[index].clone(), self.labels[index].clone()))
        } else {
            None
        }
    }
    
    fn get_all(&self) -> Vec<(Tensor, Tensor)> {
        self.features.iter()
            .zip(&self.labels)
            .map(|(f, l)| (f.clone(), l.clone()))
            .collect()
    }
}

/// DataLoader for batching and shuffling
pub struct DataLoader {
    dataset: Box<dyn Dataset>,
    batch_size: usize,
    shuffle: bool,
    current_index: usize,
    indices: Vec<usize>,
}

impl DataLoader {
    /// Create a new DataLoader
    pub fn new(dataset: Box<dyn Dataset>, batch_size: usize, shuffle: bool) -> Self {
        let indices: Vec<usize> = (0..dataset.len()).collect();
        
        DataLoader {
            dataset,
            batch_size,
            shuffle,
            current_index: 0,
            indices,
        }
    }
    
    /// Reset the DataLoader for a new epoch
    pub fn reset(&mut self) {
        self.current_index = 0;
        if self.shuffle {
            use rand::seq::SliceRandom;
            use rand::thread_rng;
            self.indices.shuffle(&mut thread_rng());
        }
    }
    
    /// Get the next batch
    pub fn next_batch(&mut self) -> Option<(Vec<Tensor>, Vec<Tensor>)> {
        if self.current_index >= self.dataset.len() {
            return None;
        }
        
        let end_idx = std::cmp::min(self.current_index + self.batch_size, self.dataset.len());
        let batch_indices = &self.indices[self.current_index..end_idx];
        
        let mut batch_features = Vec::new();
        let mut batch_labels = Vec::new();
        
        for &idx in batch_indices {
            if let Some((features, labels)) = self.dataset.get(idx) {
                batch_features.push(features);
                batch_labels.push(labels);
            }
        }
        
        self.current_index = end_idx;
        
        if batch_features.is_empty() {
            None
        } else {
            Some((batch_features, batch_labels))
        }
    }
    
    /// Iterate over all batches
    pub fn iter(&mut self) -> DataLoaderIter<'_> {
        self.reset();
        DataLoaderIter { loader: self }
    }
    
    /// Get number of batches
    pub fn num_batches(&self) -> usize {
        (self.dataset.len() + self.batch_size - 1) / self.batch_size
    }
}

/// Iterator for DataLoader
pub struct DataLoaderIter<'a> {
    loader: &'a mut DataLoader,
}

impl<'a> Iterator for DataLoaderIter<'a> {
    type Item = (Vec<Tensor>, Vec<Tensor>);
    
    fn next(&mut self) -> Option<Self::Item> {
        self.loader.next_batch()
    }
}

/// Data preprocessing utilities
pub struct Preprocessor {
    mean: Option<Vec<f32>>,
    std: Option<Vec<f32>>,
    min: Option<Vec<f32>>,
    max: Option<Vec<f32>>,
    feature_range: Option<(f32, f32)>,
}

impl Preprocessor {
    /// Create a new preprocessor
    pub fn new() -> Self {
        Preprocessor {
            mean: None,
            std: None,
            min: None,
            max: None,
            feature_range: None,
        }
    }
    
    /// Fit the preprocessor to data (compute statistics)
    pub fn fit(&mut self, data: &[Tensor]) {
        if data.is_empty() {
            return;
        }
        
        let num_features = data[0].numel();
        let mut sum = vec![0.0; num_features];
        let mut sum_sq = vec![0.0; num_features];
        let mut min = vec![f32::INFINITY; num_features];
        let mut max = vec![f32::NEG_INFINITY; num_features];
        
        let mut count = 0;
        for tensor in data {
            let data_slice = tensor.data();
            for (i, &value) in data_slice.iter().enumerate() {
                sum[i] += value;
                sum_sq[i] += value * value;
                min[i] = min[i].min(value);
                max[i] = max[i].max(value);
            }
            count += 1;
        }
        
        let count_f = count as f32;
        let mean: Vec<f32> = sum.iter().map(|&s| s / count_f).collect();
        let variance: Vec<f32> = sum_sq.iter()
            .zip(&mean)
            .map(|(&ss, &m)| ss / count_f - m * m)
            .collect();
        let std: Vec<f32> = variance.iter().map(|&v| v.sqrt()).collect();
        
        self.mean = Some(mean);
        self.std = Some(std);
        self.min = Some(min);
        self.max = Some(max);
    }
    
    /// Standardize data (zero mean, unit variance)
    pub fn standardize(&self, data: &Tensor) -> Tensor {
        let mean = self.mean.as_ref().expect("Preprocessor not fitted");
        let std = self.std.as_ref().expect("Preprocessor not fitted");
        
        assert_eq!(data.numel(), mean.len(), "Feature dimension mismatch");
        
        let standardized_data: Vec<f32> = data.data()
            .iter()
            .zip(mean)
            .zip(std)
            .map(|((&x, &m), &s)| (x - m) / s)
            .collect();
        
        Tensor::new(standardized_data, data.shape().to_vec(), data.requires_grad())
    }
    
    /// Normalize data to [0, 1] range
    pub fn normalize(&self, data: &Tensor) -> Tensor {
        let min = self.min.as_ref().expect("Preprocessor not fitted");
        let max = self.max.as_ref().expect("Preprocessor not fitted");
        
        assert_eq!(data.numel(), min.len(), "Feature dimension mismatch");
        
        let normalized_data: Vec<f32> = data.data()
            .iter()
            .zip(min)
            .zip(max)
            .map(|((&x, &min_val), &max_val)| {
                if max_val > min_val {
                    (x - min_val) / (max_val - min_val)
                } else {
                    0.0
                }
            })
            .collect();
        
        Tensor::new(normalized_data, data.shape().to_vec(), data.requires_grad())
    }
    
    /// Normalize data to custom range
    pub fn normalize_range(&self, data: &Tensor, range: (f32, f32)) -> Tensor {
        let (new_min, new_max) = range;
        let normalized = self.normalize(data);
        
        let scaled_data: Vec<f32> = normalized.data()
            .iter()
            .map(|&x| x * (new_max - new_min) + new_min)
            .collect();
        
        Tensor::new(scaled_data, data.shape().to_vec(), data.requires_grad())
    }
}

/// Data augmentation for images
pub struct ImageAugmentation {
    rotation_range: f32,  // Degrees
    width_shift_range: f32,
    height_shift_range: f32,
    shear_range: f32,
    zoom_range: (f32, f32),
    horizontal_flip: bool,
    vertical_flip: bool,
    fill_mode: FillMode,
}

#[derive(Debug, Clone, Copy)]
pub enum FillMode {
    Constant(f32),
    Nearest,
    Reflect,
    Wrap,
}

impl ImageAugmentation {
    /// Create a new image augmentation pipeline
    pub fn new() -> Self {
        ImageAugmentation {
            rotation_range: 0.0,
            width_shift_range: 0.0,
            height_shift_range: 0.0,
            shear_range: 0.0,
            zoom_range: (1.0, 1.0),
            horizontal_flip: false,
            vertical_flip: false,
            fill_mode: FillMode::Constant(0.0),
        }
    }
    
    /// Apply augmentation to an image tensor
    pub fn apply(&self, image: &Tensor) -> Tensor {
        // Simplified implementation
        // Real implementation would apply transformations using interpolation
        image.clone()
    }
    
    /// Configure rotation
    pub fn with_rotation(mut self, degrees: f32) -> Self {
        self.rotation_range = degrees;
        self
    }
    
    /// Configure horizontal shift
    pub fn with_width_shift(mut self, fraction: f32) -> Self {
        self.width_shift_range = fraction;
        self
    }
    
    /// Configure vertical shift
    pub fn with_height_shift(mut self, fraction: f32) -> Self {
        self.height_shift_range = fraction;
        self
    }
    
    /// Configure shear
    pub fn with_shear(mut self, shear: f32) -> Self {
        self.shear_range = shear;
        self
    }
    
    /// Configure zoom
    pub fn with_zoom(mut self, min_zoom: f32, max_zoom: f32) -> Self {
        self.zoom_range = (min_zoom, max_zoom);
        self
    }
    
    /// Configure horizontal flip
    pub fn with_horizontal_flip(mut self, enable: bool) -> Self {
        self.horizontal_flip = enable;
        self
    }
    
    /// Configure vertical flip
    pub fn with_vertical_flip(mut self, enable: bool) -> Self {
        self.vertical_flip = enable;
        self
    }
    
    /// Configure fill mode
    pub fn with_fill_mode(mut self, fill_mode: FillMode) -> Self {
        self.fill_mode = fill_mode;
        self
    }
}

/// CSV dataset loader
pub struct CSVDataset {
    data: Vec<Vec<f32>>,
    labels: Vec<f32>,
    feature_names: Vec<String>,
    label_name: String,
}

impl CSVDataset {
    /// Load dataset from CSV file
    pub fn from_file<P: AsRef<Path>>(path: P, has_header: bool, label_column: Option<usize>) -> std::io::Result<Self> {
        let file = File::open(path)?;
        let mut reader = csv::Reader::from_reader(BufReader::new(file));
        
        let mut data = Vec::new();
        let mut labels = Vec::new();
        let mut feature_names = Vec::new();
        
        // Read headers if present
        if has_header {
            if let Ok(headers) = reader.headers() {
                for header in headers.iter() {
                    feature_names.push(header.to_string());
                }
            }
        }
        
        // Read records
        for result in reader.records() {
            let record = result?;
            let mut row = Vec::new();
            let mut label = 0.0;
            
            for (i, field) in record.iter().enumerate() {
                if let Some(label_col) = label_column {
                    if i == label_col {
                        label = field.parse().unwrap_or(0.0);
                    } else {
                        row.push(field.parse().unwrap_or(0.0));
                    }
                } else {
                    row.push(field.parse().unwrap_or(0.0));
                }
            }
            
            data.push(row);
            if label_column.is_some() {
                labels.push(label);
            }
        }
        
        Ok(CSVDataset {
            data,
            labels,
            feature_names,
            label_name: if label_column.is_some() { "label".to_string() } else { "".to_string() },
        })
    }
    
    /// Convert to MemoryDataset
    pub fn to_memory_dataset(&self, name: &str) -> MemoryDataset {
        let features: Vec<Tensor> = self.data
            .iter()
            .map(|row| Tensor::new(row.clone(), vec![row.len()], false))
            .collect();
        
        let labels: Vec<Tensor> = self.labels
            .iter()
            .map(|&label| Tensor::new(vec![label], vec![1], false))
            .collect();
        
        MemoryDataset::new(features, labels, name)
    }
}

/// Synthetic dataset generators
pub struct SyntheticData {
    // Configuration for generating synthetic data
}

impl SyntheticData {
    /// Generate classification data (moons dataset)
    pub fn make_moons(n_samples: usize, noise: f32, shuffle: bool) -> (Vec<Vec<f32>>, Vec<f32>) {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        
        let n_samples_out = n_samples / 2;
        let n_samples_in = n_samples - n_samples_out;
        
        let mut data = Vec::new();
        let mut labels = Vec::new();
        
        // Generate outer circle
        for i in 0..n_samples_out {
            let angle = std::f32::consts::PI * i as f32 / (n_samples_out as f32);
            let x = angle.cos();
            let y = angle.sin();
            
            data.push(vec![x + rng.gen_range(-noise..noise), y + rng.gen_range(-noise..noise)]);
            labels.push(0.0);
        }
        
        // Generate inner circle
        for i in 0..n_samples_in {
            let angle = std::f32::consts::PI * i as f32 / (n_samples_in as f32);
            let x = 1.0 - angle.cos();
            let y = 1.0 - angle.sin() - 0.5;
            
            data.push(vec![x + rng.gen_range(-noise..noise), y + rng.gen_range(-noise..noise)]);
            labels.push(1.0);
        }
        
        // Shuffle if requested
        if shuffle {
            use rand::seq::SliceRandom;
            let mut indices: Vec<usize> = (0..n_samples).collect();
            indices.shuffle(&mut rng);
            
            let shuffled_data: Vec<Vec<f32>> = indices.iter().map(|&i| data[i].clone()).collect();
            let shuffled_labels: Vec<f32> = indices.iter().map(|&i| labels[i]).collect();
            
            (shuffled_data, shuffled_labels)
        } else {
            (data, labels)
        }
    }
}