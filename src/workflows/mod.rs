//! Professional development workflows for Zeta
//!
//! This module provides tools and templates for professional development:
//! - CI/CD integration templates
//! - Documentation generation
//! - Performance profiling tools
//! - Code quality checks

mod ci_cd;
mod documentation;
mod profiling;
mod quality;

pub use ci_cd::{CICDTemplate, CICDProvider, generate_ci_config};
pub use documentation::{DocumentationGenerator, DocFormat, generate_docs};
pub use profiling::{Profiler, ProfileResult, ProfileMetric, run_profiler};
pub use quality::{CodeQualityChecker, QualityIssue, check_code_quality};

/// Workflow error type
#[derive(Debug)]
pub enum WorkflowError {
    /// Template generation error
    TemplateError(String),
    /// Documentation generation error
    DocumentationError(String),
    /// Profiling error
    ProfilingError(String),
    /// Quality check error
    QualityError(String),
    /// File system error
    FileSystemError(std::io::Error),
}

impl std::fmt::Display for WorkflowError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WorkflowError::TemplateError(msg) => write!(f, "Template error: {}", msg),
            WorkflowError::DocumentationError(msg) => write!(f, "Documentation error: {}", msg),
            WorkflowError::ProfilingError(msg) => write!(f, "Profiling error: {}", msg),
            WorkflowError::QualityError(msg) => write!(f, "Quality check error: {}", msg),
            WorkflowError::FileSystemError(err) => write!(f, "File system error: {}", err),
        }
    }
}

impl std::error::Error for WorkflowError {}

impl From<std::io::Error> for WorkflowError {
    fn from(err: std::io::Error) -> Self {
        WorkflowError::FileSystemError(err)
    }
}

/// Workflow result type
pub type WorkflowResult<T> = Result<T, WorkflowError>;

/// Professional workflow manager
pub struct WorkflowManager {
    /// Project root path
    project_root: std::path::PathBuf,
    /// CI/CD templates
    ci_cd_templates: Vec<CICDTemplate>,
    /// Documentation generator
    doc_generator: DocumentationGenerator,
    /// Profiler
    profiler: Profiler,
    /// Quality checker
    quality_checker: CodeQualityChecker,
}

impl WorkflowManager {
    /// Create a new workflow manager
    pub fn new(project_root: &std::path::Path) -> Self {
        Self {
            project_root: project_root.to_path_buf(),
            ci_cd_templates: vec![
                CICDTemplate::github_actions(),
                CICDTemplate::gitlab_ci(),
                CICDTemplate::jenkins(),
            ],
            doc_generator: DocumentationGenerator::new(),
            profiler: Profiler::new(),
            quality_checker: CodeQualityChecker::new(),
        }
    }
    
    /// Setup CI/CD for the project
    pub fn setup_ci_cd(&self, provider: CICDProvider) -> WorkflowResult<()> {
        let template = self.ci_cd_templates
            .iter()
            .find(|t| t.provider == provider)
            .ok_or_else(|| WorkflowError::TemplateError(
                format!("No template for provider: {:?}", provider)
            ))?;
        
        generate_ci_config(&self.project_root, template)
    }
    
    /// Generate documentation
    pub fn generate_documentation(&self, format: DocFormat) -> WorkflowResult<()> {
        generate_docs(&self.project_root, &self.doc_generator, format)
    }
    
    /// Run performance profiling
    pub fn run_profiling(&self, target: &str) -> WorkflowResult<Vec<ProfileResult>> {
        run_profiler(&self.project_root, &self.profiler, target)
    }
    
    /// Run code quality checks
    pub fn run_quality_checks(&self) -> WorkflowResult<Vec<QualityIssue>> {
        check_code_quality(&self.project_root, &self.quality_checker)
    }
    
    /// Create professional project structure
    pub fn create_project_structure(&self) -> WorkflowResult<()> {
        let dirs = vec![
            "src",
            "tests",
            "docs",
            "examples",
            "benchmarks",
            ".github/workflows",
            ".gitlab",
            "scripts",
        ];
        
        for dir in dirs {
            let path = self.project_root.join(dir);
            std::fs::create_dir_all(&path)?;
        }
        
        // Create basic files
        let files = vec![
            ("README.md", "# Project\n\nWelcome to the project!"),
            ("CONTRIBUTING.md", "# Contributing\n\nThank you for considering contributing!"),
            ("CODE_OF_CONDUCT.md", "# Code of Conduct\n\nBe respectful to everyone."),
        ];
        
        for (filename, content) in files {
            let path = self.project_root.join(filename);
            if !path.exists() {
                std::fs::write(&path, content)?;
            }
        }
        
        Ok(())
    }
}