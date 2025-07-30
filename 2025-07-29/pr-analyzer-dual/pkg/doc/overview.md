---
Title: PR Analyzer Dual - Tool Overview
Slug: overview
Short: Comprehensive GitHub pull request analysis tool with dual output modes for both human consumption and automation
Topics:
  - pull-requests
  - github
  - analysis
  - tree-sitter
  - go
Commands:
  - get
  - analyze
Flags: []
IsTopLevel: true
IsTemplate: false
ShowPerDefault: true
SectionType: GeneralTopic
---

# PR Analyzer Dual Overview

## Purpose and Vision

The `pr-analyzer-dual` tool transforms GitHub pull request analysis by providing deep code insights through tree-sitter parsing and dual output modes. Built with the glazed framework, it bridges the gap between human-readable reports and automation-ready structured data, making pull request analysis both accessible and programmable.

Whether you're conducting manual code reviews, building automated workflows, or analyzing code patterns across pull requests, pr-analyzer-dual provides the flexibility and depth you need.

## Key Capabilities

### GitHub Integration
The tool seamlessly connects with GitHub's API to access pull request data, handling authentication, rate limiting, and API versioning automatically. It retrieves commits, diffs, file history, and metadata without requiring complex API knowledge.

### Advanced Code Analysis
Using tree-sitter for Go code parsing, pr-analyzer-dual goes beyond simple text processing to understand code structure at the function level. It identifies changed functions, tracks modifications, and provides context-aware analysis of code changes.

### Dual Output Architecture
Every command supports two distinct output modes through the glazed framework's dual command pattern:

- **Human-readable mode**: Rich markdown output with formatting, context, and narrative structure for direct consumption
- **Structured mode**: JSON, CSV, YAML, and table formats for automation, scripting, and integration with other tools

This architecture ensures that the same powerful analysis engine serves both interactive users and automated systems.

## Command Architecture

The tool organizes functionality into two primary command groups:

### Get Commands
Information retrieval commands that extract data from GitHub pull requests:
- `get commits` - Pull request commit history with author and date information
- `get context` - File-level change analysis with function statistics
- `get diff` - Unified diff content for the entire pull request
- `get file-history` - Commit history for specific files

### Analyze Commands  
Deep analysis commands that use tree-sitter parsing for Go code insights:
- `analyze functions` - Function-level change detection with optional body inclusion
- `analyze function-history` - Temporal analysis of how specific functions evolved

## Technical Foundation

### Tree-sitter Integration
The tool leverages tree-sitter's robust Go parser to analyze code structure, enabling precise function identification, signature extraction, and change detection. This provides far more accurate results than regex-based approaches.

### GitHub API Client
A well-architected GitHub client handles authentication (both authenticated and anonymous), rate limiting, error handling, and API response processing, ensuring reliable operation even with large repositories.

### Glazed Framework Integration
Built on the glazed framework, the tool inherits powerful features like field selection, filtering, multiple output formats, and middleware processing, making it highly flexible and integration-friendly.

## Authentication and Setup

The tool supports both authenticated and anonymous GitHub API access. For authenticated access, set the `GITHUB_TOKEN` environment variable with a personal access token. Authentication increases rate limits and provides access to private repositories.

```bash
export GITHUB_TOKEN=your_github_token_here
```

For public repositories and light usage, anonymous access works without any setup, though it has lower rate limits.

## Common Use Cases

- **Code Review Enhancement**: Generate detailed function-level change reports to supplement pull request reviews
- **Automation Integration**: Extract structured data for CI/CD pipelines, code quality tools, or custom analysis scripts  
- **Development Metrics**: Track function modification patterns, commit frequencies, and change impact across teams
- **Documentation Generation**: Create automated change logs or release notes based on function-level changes
- **Security Analysis**: Identify which functions were modified in security-related pull requests for focused review
