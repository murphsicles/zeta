//! Code quality checking tools

use std::path::Path;
use std::collections::HashMap;
use crate::workflows::WorkflowError;

/// Quality issue severity
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IssueSeverity {
    /// Critical issue - must fix
    Critical,
    /// High severity issue
    High,
    /// Medium severity issue
    Medium,
    /// Low severity issue
    Low,
    /// Informational issue
    Info,
}

/// Quality issue
#[derive(Debug, Clone)]
pub struct QualityIssue {
    /// Issue type
    pub issue_type: String,
    /// Severity
    pub severity: IssueSeverity,
    /// File path
    pub file: String,
    /// Line number (1-based)
    pub line: Option<u32>,
    /// Column number (1-based)
    pub column: Option<u32>,
    /// Message
    pub message: String,
    /// Suggested fix
    pub suggestion: Option<String>,
}

impl QualityIssue {
    /// Create a new quality issue
    pub fn new(
        issue_type: &str,
        severity: IssueSeverity,
        file: &str,
        message: &str,
    ) -> Self {
        Self {
            issue_type: issue_type.to_string(),
            severity,
            file: file.to_string(),
            line: None,
            column: None,
            message: message.to_string(),
            suggestion: None,
        }
    }
    
    /// Set line number
    pub fn with_line(mut self, line: u32) -> Self {
        self.line = Some(line);
        self
    }
    
    /// Set column number
    pub fn with_column(mut self, column: u32) -> Self {
        self.column = Some(column);
        self
    }
    
    /// Set suggestion
    pub fn with_suggestion(mut self, suggestion: &str) -> Self {
        self.suggestion = Some(suggestion.to_string());
        self
    }
    
    /// Format as string
    pub fn format(&self) -> String {
        let severity_str = match self.severity {
            IssueSeverity::Critical => "CRITICAL",
            IssueSeverity::High => "HIGH",
            IssueSeverity::Medium => "MEDIUM",
            IssueSeverity::Low => "LOW",
            IssueSeverity::Info => "INFO",
        };
        
        let location = if let Some(line) = self.line {
            if let Some(col) = self.column {
                format!("{}:{}:{}", self.file, line, col)
            } else {
                format!("{}:{}", self.file, line)
            }
        } else {
            self.file.clone()
        };
        
        let mut result = format!("{}: {} [{}]", location, self.message, severity_str);
        
        if let Some(suggestion) = &self.suggestion {
            result.push_str(&format!("\n  Suggestion: {}", suggestion));
        }
        
        result
    }
}

/// Code quality checker
pub struct CodeQualityChecker {
    /// Enabled checks
    pub enabled_checks: HashMap<String, bool>,
    /// Custom rules
    pub custom_rules: Vec<Box<dyn Fn(&Path) -> Vec<QualityIssue>>>,
}

impl CodeQualityChecker {
    /// Create a new code quality checker
    pub fn new() -> Self {
        let mut enabled_checks = HashMap::new();
        
        // Default checks
        enabled_checks.insert("unused_variables".to_string(), true);
        enabled_checks.insert("dead_code".to_string(), true);
        enabled_checks.insert("complex_functions".to_string(), true);
        enabled_checks.insert("long_files".to_string(), true);
        enabled_checks.insert("todo_comments".to_string(), true);
        enabled_checks.insert("debug_code".to_string(), true);
        enabled_checks.insert("security_issues".to_string(), true);
        enabled_checks.insert("performance_issues".to_string(), true);
        
        Self {
            enabled_checks,
            custom_rules: Vec::new(),
        }
    }
    
    /// Enable a check
    pub fn enable_check(&mut self, check_name: &str) {
        self.enabled_checks.insert(check_name.to_string(), true);
    }
    
    /// Disable a check
    pub fn disable_check(&mut self, check_name: &str) {
        self.enabled_checks.insert(check_name.to_string(), false);
    }
    
    /// Add custom rule
    pub fn add_custom_rule<F>(&mut self, rule: F)
    where
        F: Fn(&Path) -> Vec<QualityIssue> + 'static,
    {
        self.custom_rules.push(Box::new(rule));
    }
    
    /// Check a file for quality issues
    pub fn check_file(&self, file_path: &Path) -> Vec<QualityIssue> {
        let mut issues = Vec::new();
        
        if !file_path.exists() {
            return issues;
        }
        
        let content = match std::fs::read_to_string(file_path) {
            Ok(content) => content,
            Err(_) => return issues,
        };
        
        let file_str = file_path.to_string_lossy();
        
        // Check for TODO comments
        if *self.enabled_checks.get("todo_comments").unwrap_or(&true) {
            for (line_num, line) in content.lines().enumerate() {
                if line.to_lowercase().contains("todo") || line.to_lowercase().contains("fixme") {
                    let todo_text = line.trim();
                    issues.push(
                        QualityIssue::new(
                            "todo_comment",
                            IssueSeverity::Medium,
                            &file_str,
                            &format!("TODO/FIXME comment found: {}", todo_text),
                        )
                        .with_line((line_num + 1) as u32)
                        .with_suggestion("Address the TODO comment or remove it"),
                    );
                }
            }
        }
        
        // Check for debug code
        if *self.enabled_checks.get("debug_code").unwrap_or(&true) {
            for (line_num, line) in content.lines().enumerate() {
                let line_lower = line.to_lowercase();
                if line_lower.contains("println!")
                    || line_lower.contains("dbg!")
                    || line_lower.contains("eprintln!")
                {
                    issues.push(
                        QualityIssue::new(
                            "debug_code",
                            IssueSeverity::Low,
                            &file_str,
                            "Debug print statement found",
                        )
                        .with_line((line_num + 1) as u32)
                        .with_suggestion("Remove debug statements before production"),
                    );
                }
            }
        }
        
        // Check for long files
        if *self.enabled_checks.get("long_files").unwrap_or(&true) {
            let line_count = content.lines().count();
            if line_count > 1000 {
                issues.push(
                    QualityIssue::new(
                        "long_file",
                        IssueSeverity::Medium,
                        &file_str,
                        &format!("File is too long ({} lines)", line_count),
                    )
                    .with_suggestion("Consider splitting into smaller modules"),
                );
            }
        }
        
        // Check for complex functions (simplified)
        if *self.enabled_checks.get("complex_functions").unwrap_or(&true) {
            let mut in_function = false;
            let mut function_start = 0;
            let mut brace_count = 0;
            
            for (line_num, line) in content.lines().enumerate() {
                if line.trim().starts_with("fn ") && !in_function {
                    in_function = true;
                    function_start = line_num;
                    brace_count = 0;
                }
                
                if in_function {
                    brace_count += line.matches('{').count();
                    brace_count -= line.matches('}').count();
                    
                    if brace_count == 0 && line_num > function_start {
                        let function_length = line_num - function_start + 1;
                        if function_length > 50 {
                            issues.push(
                                QualityIssue::new(
                                    "complex_function",
                                    IssueSeverity::Medium,
                                    &file_str,
                                    &format!("Function is too long ({} lines)", function_length),
                                )
                                .with_line((function_start + 1) as u32)
                                .with_suggestion("Consider breaking into smaller functions"),
                            );
                        }
                        in_function = false;
                    }
                }
            }
        }
        
        // Run custom rules
        for rule in &self.custom_rules {
            issues.extend(rule(file_path));
        }
        
        issues
    }
    
    /// Check a directory recursively
    pub fn check_directory(&self, dir_path: &Path) -> Vec<QualityIssue> {
        let mut issues = Vec::new();
        
        if !dir_path.exists() || !dir_path.is_dir() {
            return issues;
        }
        
        let entries = match std::fs::read_dir(dir_path) {
            Ok(entries) => entries,
            Err(_) => return issues,
        };
        
        for entry in entries.flatten() {
            let path = entry.path();
            
            if path.is_dir() {
                // Skip hidden directories and target directory
                if let Some(name) = path.file_name() {
                    let name_str = name.to_string_lossy();
                    if !name_str.starts_with('.') && name_str != "target" {
                        issues.extend(self.check_directory(&path));
                    }
                }
            } else if path.is_file() {
                // Check Rust and Zeta files
                if let Some(ext) = path.extension() {
                    if ext == "rs" || ext == "z" {
                        issues.extend(self.check_file(&path));
                    }
                }
            }
        }
        
        issues
    }
}

/// Check code quality
pub fn check_code_quality(
    project_root: &Path,
    checker: &CodeQualityChecker,
) -> Result<Vec<QualityIssue>, WorkflowError> {
    let issues = checker.check_directory(project_root);
    
    // Sort by severity and file
    let mut sorted_issues = issues.clone();
    sorted_issues.sort_by(|a, b| {
        b.severity.cmp(&a.severity)
            .then(a.file.cmp(&b.file))
            .then(a.line.cmp(&b.line))
    });
    
    // Generate report
    let report_dir = project_root.join("reports");
    std::fs::create_dir_all(&report_dir)?;
    
    let timestamp = chrono::Local::now().format("%Y%m%d_%H%M%S");
    let report_path = report_dir.join(format!("quality_report_{}.md", timestamp));
    
    let mut report = String::new();
    report.push_str("# Code Quality Report\n\n");
    
    // Summary
    let critical_count = issues.iter().filter(|i| i.severity == IssueSeverity::Critical).count();
    let high_count = issues.iter().filter(|i| i.severity == IssueSeverity::High).count();
    let medium_count = issues.iter().filter(|i| i.severity == IssueSeverity::Medium).count();
    let low_count = issues.iter().filter(|i| i.severity == IssueSeverity::Low).count();
    let info_count = issues.iter().filter(|i| i.severity == IssueSeverity::Info).count();
    
    report.push_str(&format!("## Summary\n\n"));
    report.push_str(&format!("- Total Issues: {}\n", issues.len()));
    report.push_str(&format!("- Critical: {}\n", critical_count));
    report.push_str(&format!("- High: {}\n", high_count));
    report.push_str(&format!("- Medium: {}\n", medium_count));
    report.push_str(&format!("- Low: {}\n", low_count));
    report.push_str(&format!("- Info: {}\n\n", info_count));
    
    // Issues by severity
    for severity in &[
        IssueSeverity::Critical,
        IssueSeverity::High,
        IssueSeverity::Medium,
        IssueSeverity::Low,
        IssueSeverity::Info,
    ] {
        let severity_issues: Vec<_> = sorted_issues.iter()
            .filter(|i| i.severity == *severity)
            .collect();
        
        if !severity_issues.is_empty() {
            let severity_name = match severity {
                IssueSeverity::Critical => "Critical",
                IssueSeverity::High => "High",
                IssueSeverity::Medium => "Medium",
                IssueSeverity::Low => "Low",
                IssueSeverity::Info => "Info",
            };
            
            report.push_str(&format!("## {} Issues\n\n", severity_name));
            
            for issue in severity_issues {
                report.push_str(&format!("- {}\n", issue.format()));
            }
            
            report.push_str("\n");
        }
    }
    
    std::fs::write(report_path, report)?;
    
    println!("Code quality check completed. Found {} issues.", issues.len());
    println!("Report saved to reports/ directory.");
    
    Ok(issues)
}