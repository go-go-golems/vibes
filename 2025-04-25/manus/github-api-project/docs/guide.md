# GitHub Project Management Tool Guide

## Table of Contents

1. [Introduction](#introduction)
2. [Installation](#installation)
3. [YAML DSL Reference](#yaml-dsl-reference)
4. [Command Reference](#command-reference)
5. [Usage Examples](#usage-examples)
6. [Best Practices](#best-practices)
7. [Limitations and Future Enhancements](#limitations-and-future-enhancements)

## Introduction

The GitHub Project Management Tool (ghpm) is a command-line utility that allows you to manage GitHub projects and issues using a YAML-based Domain Specific Language (DSL). With this tool, you can:

- Create and update projects
- Create issues and sub-issues with hierarchical relationships
- Update issue statuses
- Add assignees to issues
- Add comments to issues
- Manage project columns

The tool uses GitHub's GraphQL API to interact with GitHub, providing a powerful and flexible way to manage your projects programmatically.

## Installation

### Prerequisites

- Go 1.18 or later
- GitHub Personal Access Token with the following scopes:
  - `project` (for full access to projects)
  - `repo` (for access to repositories and issues)

### Building from Source

1. Clone the repository:
   ```bash
   git clone https://github.com/your-username/github-api-project.git
   cd github-api-project
   ```

2. Build the application:
   ```bash
   go build -o ghpm ./cmd/ghpm
   ```

3. (Optional) Move the binary to a directory in your PATH:
   ```bash
   sudo mv ghpm /usr/local/bin/
   ```

### Setting Up Authentication

The tool requires a GitHub Personal Access Token for authentication. You can provide this token in two ways:

1. Set it as an environment variable:
   ```bash
   export GITHUB_TOKEN=your_token_here
   ```

2. Include it directly in your YAML configuration file:
   ```yaml
   config:
     auth:
       token: "your_token_here"
   ```

For security reasons, using the environment variable is recommended.

## YAML DSL Reference

The YAML DSL is designed to be intuitive and closely mirror GitHub's project structure. Below is a comprehensive reference of the DSL structure.

### Top-Level Structure

```yaml
# Configuration section (required)
config:
  auth:
    token: "${GITHUB_TOKEN}"  # Environment variable or direct token
  organization: "my-org"      # GitHub organization or username
  
# Project definition (optional)
project:
  name: "My Project"
  id: "PVT_kwDOABCD123"  # Optional, for updates to existing project
  description: "Project description"
  public: true
  columns:
    - name: "To Do"
      id: "f75ad846"  # Optional, for updates to existing column
    - name: "In Progress"
      id: "47fc9ee4"
    - name: "Done"
      id: "98236657"
  
# Issues definition (optional)
issues:
  - title: "Main Feature"
    id: "I_kwDOABCD123"  # Optional, for updates to existing issue
    body: "Implement the main feature"
    status: "To Do"  # Maps to column name
    assignees:
      - "username1"
    labels:
      - name: "enhancement"
        color: "a2eeef"
    sub_issues:
      - title: "Sub-task 1"
        body: "Implement sub-task 1"
        status: "To Do"
    comments:
      - body: "This is a comment"
```

### Field Definitions

#### Config Section

- `auth.token`: GitHub Personal Access Token for authentication
- `organization`: GitHub organization or username

#### Project Section

- `name`: Project name
- `id`: Optional project ID for updates to existing projects
- `description`: Project description
- `public`: Boolean indicating if the project is public
- `columns`: List of columns (status values) for the project
  - `name`: Column name
  - `id`: Optional column ID for updates to existing columns

#### Issues Section

- `title`: Issue title
- `id`: Optional issue ID for updates to existing issues
- `body`: Issue description
- `status`: Current status (maps to column name)
- `assignees`: List of GitHub usernames to assign
- `labels`: List of labels
  - `name`: Label name
  - `color`: Label color (hex code without #)
- `sub_issues`: List of sub-issues (recursive structure)
- `comments`: List of comments to add to the issue
  - `body`: Comment text

### ID Management

The DSL supports two modes of operation:

1. **Creation Mode**: When IDs are not provided, new resources are created.
2. **Update Mode**: When IDs are provided, existing resources are updated.

After creation, the tool will update the YAML file with the IDs of the created resources, allowing for subsequent updates.

## Command Reference

The tool provides several commands to help you manage your GitHub projects.

### Initialize a New Configuration

```bash
ghpm init [directory]
```

Creates a new directory (if it doesn't exist) and initializes a template YAML configuration file.

**Options:**
- `directory`: Optional. The directory where the configuration file will be created. Defaults to the current directory.

### Validate a Configuration

```bash
ghpm validate [file]
```

Validates a YAML configuration file without applying any changes.

**Options:**
- `file`: Required. The path to the YAML configuration file.

### Apply a Configuration

```bash
ghpm apply [file]
```

Applies a YAML configuration file to create or update GitHub projects and issues.

**Options:**
- `file`: Required. The path to the YAML configuration file.

## Usage Examples

### Creating a New Project with Issues

```yaml
config:
  auth:
    token: "${GITHUB_TOKEN}"
  organization: "my-org"
  
project:
  name: "New Project"
  description: "A new project"
  public: true
  columns:
    - name: "To Do"
    - name: "In Progress"
    - name: "Done"
  
issues:
  - title: "First Issue"
    body: "First issue description"
    status: "To Do"
```

### Updating an Existing Project

```yaml
config:
  auth:
    token: "${GITHUB_TOKEN}"
  organization: "my-org"
  
project:
  id: "PVT_kwDOABCD123"  # Existing project ID
  name: "Updated Project Name"
  
issues:
  - id: "I_kwDOABCD123"  # Existing issue ID
    status: "In Progress"  # Update status
    assignees:
      - "new-assignee"  # Update assignees
```

### Creating Sub-Issues for an Existing Issue

```yaml
config:
  auth:
    token: "${GITHUB_TOKEN}"
  organization: "my-org"
  
issues:
  - id: "I_kwDOABCD123"  # Existing parent issue ID
    sub_issues:
      - title: "New Sub-Issue"
        body: "New sub-issue description"
        status: "To Do"
```

## Best Practices

### Organization and Structure

1. **Use Environment Variables for Tokens**: Avoid hardcoding tokens in your YAML files.
2. **Version Control Your YAML Files**: Track changes to your project structure over time.
3. **Split Large Projects**: For very large projects, consider splitting your configuration into multiple files.

### Workflow Integration

1. **CI/CD Integration**: Incorporate the tool into your CI/CD pipelines to automate project management.
2. **Scheduled Updates**: Use cron jobs or scheduled workflows to update project statuses.
3. **Backup Before Updates**: Always create backups of your YAML files before applying updates.

### Naming Conventions

1. **Consistent Status Names**: Ensure status names in your YAML match exactly with column names in GitHub.
2. **Descriptive Issue Titles**: Use clear, descriptive titles for issues to improve searchability.
3. **Hierarchical Structure**: Use a consistent approach to structuring parent-child relationships.

## Limitations and Future Enhancements

### Current Limitations

1. **Label Management**: The current version has limited support for label management.
2. **Repository Integration**: The tool currently focuses on project management and does not handle repository creation or management.
3. **Milestone Support**: Milestones are not currently supported.

### Planned Enhancements

1. **Full Label Support**: Implement complete label creation and management.
2. **Milestone Integration**: Add support for creating and managing milestones.
3. **Repository Management**: Extend the tool to handle repository creation and configuration.
4. **Advanced Filtering**: Add support for filtering issues based on various criteria.
5. **Batch Operations**: Enhance support for batch operations on multiple issues.

## Conclusion

The GitHub Project Management Tool provides a powerful way to manage your GitHub projects using a simple YAML-based DSL. By leveraging the power of GitHub's GraphQL API, it enables you to automate project management tasks and maintain a consistent project structure.

Whether you're setting up a new project, updating existing issues, or managing complex hierarchical relationships between issues, this tool can help streamline your workflow and improve your productivity.
