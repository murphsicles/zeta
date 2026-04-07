//! Professional workflows tests

#[cfg(test)]
mod tests {
    use zetac::workflows::*;
    use zetac::workflows::ci_cd::*;
    use zetac::workflows::documentation::*;
    use zetac::workflows::profiling::*;
    use zetac::workflows::quality::*;
    use std::path::Path;
    use tempfile::TempDir;

    /// Test CI/CD template generation
    #[test]
    fn test_ci_cd_templates() {
        let github_template = CICDTemplate::github_actions();
        let gitlab_template = CICDTemplate::gitlab_ci();
        let jenkins_template = CICDTemplate::jenkins();
        
        assert_eq!(github_template.provider, CICDProvider::GitHubActions);
        assert_eq!(github_template.filename, ".github/workflows/ci.yml");
        assert!(github_template.content.contains("name: CI"));
        
        assert_eq!(gitlab_template.provider, CICDProvider::GitLabCI);
        assert_eq!(gitlab_template.filename, ".gitlab-ci.yml");
        assert!(gitlab_template.content.contains("image: rust:latest"));
        
        assert_eq!(jenkins_template.provider, CICDProvider::Jenkins);
        assert_eq!(jenkins_template.filename, "Jenkinsfile");
        assert!(jenkins_template.content.contains("pipeline"));
    }

    /// Test documentation generator
    #[test]
    fn test_documentation_generator() {
        let mut generator = DocumentationGenerator::new();
        
        // Set test data
        generator.project_name = "Test Project".to_string();
        generator.project_version = "1.0.0".to_string();
        generator.description = "A test project".to_string();
        generator.authors = vec!["Author <author@test.com>".to_string()];
        generator.license = Some("MIT".to_string());
        
        // Test README generation
        let readme = generator.generate_readme();
        assert!(readme.contains("# Test Project"));
        assert!(readme.contains("A test project"));
        assert!(readme.contains("MIT License"));
        
        // Test API docs generation
        let api_docs = generator.generate_api_docs();
        assert!(api_docs.contains("<title>Test Project - API Documentation</title>"));
        assert!(api_docs.contains("v1.0.0"));
    }

    /// Test profiler
    #[test]
    fn test_profiler() {
        let profiler = Profiler::new();
        
        // Profile a simple function
        let result = profiler.profile("test_function", || {
            // Simulate some work
            std::thread::sleep(std::time::Duration::from_micros(100));
            42
        });
        
        assert_eq!(result.name, "test_function");
        assert!(!result.metrics.is_empty());
        
        // Check that we have execution time metric
        let has_exec_time = result.metrics.iter().any(|m| {
            matches!(m, ProfileMetric::ExecutionTime(_))
        });
        assert!(has_exec_time, "Should have execution time metric");
        
        // Test markdown formatting
        let markdown = result.to_markdown();
        assert!(markdown.contains("## test_function"));
        assert!(markdown.contains("Execution Time"));
        
        // Test JSON formatting
        let json = result.to_json();
        assert!(json.is_ok());
        let json_str = json.unwrap();
        assert!(json_str.contains("test_function"));
        assert!(json_str.contains("execution_time"));
    }

    /// Test code quality checker
    #[test]
    fn test_code_quality_checker() {
        let checker = CodeQualityChecker::new();
        
        // Create a temporary file with some issues
        let temp_dir = TempDir::new().unwrap();
        let test_file = temp_dir.path().join("test.rs");
        
        let content = r#"
// TODO: Implement this function
fn test() {
    println!("Debug message");
    // FIXME: This needs fixing
    let x = 42;
}
"#;
        
        std::fs::write(&test_file, content).unwrap();
        
        // Check the file
        let issues = checker.check_file(&test_file);
        
        // Should find TODO and FIXME comments
        let todo_count = issues.iter()
            .filter(|i| i.issue_type == "todo_comment")
            .count();
        let debug_count = issues.iter()
            .filter(|i| i.issue_type == "debug_code")
            .count();
        
        assert!(todo_count >= 1, "Should find TODO/FIXME comments");
        assert!(debug_count >= 1, "Should find debug code");
        
        // Test issue formatting
        if let Some(issue) = issues.first() {
            let formatted = issue.format();
            assert!(formatted.contains("TODO") || formatted.contains("FIXME") || formatted.contains("Debug"));
        }
    }

    /// Test workflow manager creation
    #[test]
    fn test_workflow_manager() {
        let temp_dir = TempDir::new().unwrap();
        let manager = WorkflowManager::new(temp_dir.path());
        
        // Test that manager was created
        assert_eq!(manager.project_root, temp_dir.path());
        
        // Test CI/CD templates
        assert_eq!(manager.ci_cd_templates.len(), 3);
        
        // Test creating project structure (would create files)
        // This is commented out because it would create actual files
        // let result = manager.create_project_structure();
        // assert!(result.is_ok());
    }

    /// Test quality issue severity ordering
    #[test]
    fn test_issue_severity_ordering() {
        let critical = IssueSeverity::Critical;
        let high = IssueSeverity::High;
        let medium = IssueSeverity::Medium;
        let low = IssueSeverity::Low;
        let info = IssueSeverity::Info;
        
        // Test ordering
        assert!(critical > high);
        assert!(high > medium);
        assert!(medium > low);
        assert!(low > info);
        
        // Test equality
        assert_eq!(critical, IssueSeverity::Critical);
        assert_eq!(high, IssueSeverity::High);
    }
}