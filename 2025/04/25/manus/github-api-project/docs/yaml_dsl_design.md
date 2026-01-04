# YAML DSL Design for GitHub Project Management

This document outlines the design of a YAML DSL (Domain Specific Language) for managing GitHub projects and issues. The DSL is designed to be used with a Golang implementation that interacts with GitHub's GraphQL API.

## Design Goals

1. **Intuitive Structure**: The DSL should be easy to read and write, with a clear structure that mirrors GitHub's project organization.
2. **Complete Functionality**: Support all key operations: creating projects, issues, sub-issues, updating statuses, adding comments, and managing labels and assignees.
3. **Idempotent Operations**: The DSL should support both initialization and updates, with the ability to reference existing resources.
4. **Hierarchical Representation**: Support for parent-child relationships between issues.
5. **Batch Operations**: Allow for bulk creation and updates of multiple issues.

## YAML Structure

### Top-Level Structure

```yaml
# Top-level configuration
config:
  auth:
    token: "${GITHUB_TOKEN}"  # Environment variable or direct token
  organization: "my-org"      # GitHub organization or username
  
# Project definition
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
  
# Issues definition
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
      - name: "bug"
        color: "d73a4a"
    sub_issues:
      - title: "Sub-task 1"
        id: "I_kwDOABCD456"  # Optional, for updates to existing sub-issue
        body: "Implement sub-task 1"
        status: "To Do"
        assignees:
          - "username2"
        labels:
          - name: "documentation"
            color: "0075ca"
      - title: "Sub-task 2"
        body: "Implement sub-task 2"
        status: "In Progress"
        sub_issues:
          - title: "Sub-sub-task 1"
            body: "Implement sub-sub-task 1"
            status: "To Do"
  
  - title: "Another Issue"
    body: "Another issue description"
    status: "In Progress"
    comments:
      - body: "This is a comment"
      - body: "This is another comment"
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

## Usage Patterns

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

## ID Management

The DSL supports two modes of operation:

1. **Creation Mode**: When IDs are not provided, new resources are created.
2. **Update Mode**: When IDs are provided, existing resources are updated.

After creation, the tool will update the YAML file with the IDs of the created resources, allowing for subsequent updates.

## Status Mapping

The DSL maps status names to project columns. When updating an issue's status, the tool will:

1. Find the column ID that corresponds to the status name
2. Move the issue to that column

## Error Handling

The tool will validate the YAML against a schema before execution and provide clear error messages for:

- Invalid YAML syntax
- Missing required fields
- Invalid field values
- API errors from GitHub

## Extensions

The DSL can be extended to support additional features:

- Milestones
- Project fields (custom fields)
- Issue templates
- Automated workflows
