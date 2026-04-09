//! Documentation generation tools

use std::path::Path;
use crate::workflows::WorkflowError;

/// Documentation format
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocFormat {
    /// HTML documentation
    Html,
    /// Markdown documentation
    Markdown,
    /// PDF documentation
    Pdf,
    /// Man pages
    Man,
}

/// Documentation generator
#[derive(Debug, Clone)]
pub struct DocumentationGenerator {
    /// Project name
    pub project_name: String,
    /// Project version
    pub project_version: String,
    /// Authors
    pub authors: Vec<String>,
    /// Description
    pub description: String,
    /// Repository URL
    pub repository: Option<String>,
    /// License
    pub license: Option<String>,
}

impl DocumentationGenerator {
    /// Create a new documentation generator
    pub fn new() -> Self {
        Self {
            project_name: "My Project".to_string(),
            project_version: "0.1.0".to_string(),
            authors: vec!["Author Name <author@example.com>".to_string()],
            description: "A fantastic project".to_string(),
            repository: None,
            license: None,
        }
    }
    
    /// Load from Cargo.toml
    pub fn load_from_cargo_toml(&mut self, path: &Path) -> Result<(), WorkflowError> {
        let content = std::fs::read_to_string(path)?;
        
        // Simple TOML parsing for demonstration
        // In real implementation, use toml crate
        for line in content.lines() {
            let line = line.trim();
            
            if line.starts_with("name = ") {
                if let Some(name) = line.strip_prefix("name = ") {
                    self.project_name = name.trim_matches('"').to_string();
                }
            } else if line.starts_with("version = ") {
                if let Some(version) = line.strip_prefix("version = ") {
                    self.project_version = version.trim_matches('"').to_string();
                }
            } else if line.starts_with("authors = ") {
                if let Some(authors_str) = line.strip_prefix("authors = ") {
                    // Parse array of authors
                    let authors_clean = authors_str.trim_matches(&['[', ']', '"'][..]);
                    self.authors = authors_clean.split(',')
                        .map(|s| s.trim().trim_matches('"').to_string())
                        .collect();
                }
            } else if line.starts_with("description = ") {
                if let Some(desc) = line.strip_prefix("description = ") {
                    self.description = desc.trim_matches('"').to_string();
                }
            } else if line.starts_with("repository = ") {
                if let Some(repo) = line.strip_prefix("repository = ") {
                    self.repository = Some(repo.trim_matches('"').to_string());
                }
            } else if line.starts_with("license = ") {
                if let Some(license) = line.strip_prefix("license = ") {
                    self.license = Some(license.trim_matches('"').to_string());
                }
            }
        }
        
        Ok(())
    }
    
    /// Generate README.md
    pub fn generate_readme(&self) -> String {
        let project_lower = self.project_name.to_lowercase();
        let description = if self.description.is_empty() { "" } else { &self.description };
        let license = self.license.as_deref().unwrap_or("MIT");
        let authors = self.authors.join("\n");
        
        format!(
            r#"# {}

{}

[![Build Status](https://img.shields.io/github/actions/workflow/status/{}/ci.yml)](https://github.com/{}/actions)
[![Version](https://img.shields.io/crates/v/{})](https://crates.io/crates/{})
[![License](https://img.shields.io/crates/l/{})](LICENSE)

## Description

{}

## Installation

```bash
cargo add {}
```

## Usage

```rust
// Add usage example here
```

## Features

- Feature 1
- Feature 2
- Feature 3

## Documentation

Full documentation is available at [docs.rs/{}](https://docs.rs/{}).

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for details.

## License

This project is licensed under the {} License - see the [LICENSE](LICENSE) file for details.

## Authors

{}

"#,
            self.project_name,
            description,
            project_lower,
            project_lower,
            project_lower,
            project_lower,
            project_lower,
            self.description,
            project_lower,
            project_lower,
            project_lower,
            license,
            authors
        )
    }
    
    /// Generate API documentation
    pub fn generate_api_docs(&self) -> String {
        format!(
            r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{} - API Documentation</title>
    <style>
        body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif; }}
        .container {{ max-width: 1200px; margin: 0 auto; padding: 20px; }}
        header {{ border-bottom: 2px solid #eaeaea; padding-bottom: 20px; margin-bottom: 40px; }}
        h1 {{ color: #333; }}
        .version {{ color: #666; font-size: 0.8em; }}
        .module {{ margin-bottom: 40px; padding: 20px; border: 1px solid #eaeaea; border-radius: 5px; }}
        .function {{ margin-bottom: 20px; padding: 15px; background: #f9f9f9; border-radius: 3px; }}
        .signature {{ font-family: 'Courier New', monospace; color: #0366d6; }}
        .description {{ margin-top: 10px; color: #555; }}
    </style>
</head>
<body>
    <div class="container">
        <header>
            <h1>{} <span class="version">v{}</span></h1>
            <p>{}</p>
            {}
        </header>
        
        <section>
            <h2>Modules</h2>
            
            <div class="module">
                <h3>main</h3>
                <p>Main module containing primary functionality.</p>
                
                <div class="function">
                    <div class="signature">fn main() -> Result<(), Error></div>
                    <div class="description">
                        Entry point of the application.
                    </div>
                </div>
            </div>
        </section>
        
        <footer>
            <p>Generated on {} | {} License</p>
        </footer>
    </div>
</body>
</html>"#,
            self.project_name,
            self.project_name,
            self.project_version,
            self.description,
            if let Some(repo) = &self.repository {
                format!("<p>Repository: <a href=\"{}\">{}</a></p>", repo, repo)
            } else {
                String::new()
            },
            chrono::Local::now().format("%Y-%m-%d %H:%M:%S"),
            self.license.as_deref().unwrap_or("MIT")
        )
    }
}

/// Generate documentation
pub fn generate_docs(
    project_root: &Path,
    generator: &DocumentationGenerator,
    format: DocFormat,
) -> Result<(), WorkflowError> {
    match format {
        DocFormat::Html => {
            let docs_dir = project_root.join("docs");
            std::fs::create_dir_all(&docs_dir)?;
            
            let html = generator.generate_api_docs();
            let html_path = docs_dir.join("index.html");
            std::fs::write(html_path, html)?;
            
            println!("Generated HTML documentation in docs/");
        }
        DocFormat::Markdown => {
            let readme = generator.generate_readme();
            let readme_path = project_root.join("README.md");
            std::fs::write(readme_path, readme)?;
            
            println!("Generated README.md");
        }
        DocFormat::Pdf => {
            println!("PDF generation not yet implemented");
        }
        DocFormat::Man => {
            println!("Man page generation not yet implemented");
        }
    }
    
    Ok(())
}