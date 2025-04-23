# YAML DSL for GitHub Project Management

This document describes the YAML Domain Specific Language (DSL) for managing GitHub projects and issues.

## Overview

The YAML DSL provides a declarative way to define and manage GitHub projects, issues, and their relationships. It allows you to:

- Create and update projects
- Define project columns (status values)
- Create issues and sub-issues with hierarchical relationships
- Set issue statuses, assignees, and labels
- Add comments to issues

## Schema Definition

The YAML DSL follows this schema:

```yaml
# Configuration section
config:
  auth:
    token: string  # GitHub Personal Access Token
  organization: string  # GitHub organization or username

# Project definition
project:
  name: string  # Project name
  id: string  # Optional, for updates to existing projects
  description: string  # Project description
  public: boolean  # Whether the project is public
  columns:  # Project columns (status values)
    - name: string  # Column name
      id: string  # Optional, for updates to existing columns

# Issues definition
issues:
  - title: string  # Issue title
    id: string  # Optional, for updates to existing issues
    body: string  # Issue description
    status: string  # Current status (maps to column name)
    assignees:  # List of GitHub usernames to assign
      - string
    labels:  # List of labels
      - name: string  # Label name
        color: string  # Label color (hex code without #)
    sub_issues:  # List of sub-issues (recursive structure)
      - # Same structure as parent issues
    comments:  # List of comments to add to the issue
      - body: string  # Comment text
```

## Field Descriptions

### Config Section

- `auth.token`: GitHub Personal Access Token for authentication. Can use environment variable syntax `${GITHUB_TOKEN}`.
- `organization`: GitHub organization or username where the project will be created or updated.

### Project Section

- `name`: Project name.
- `id`: Optional project ID for updates to existing projects. If not provided, a new project will be created.
- `description`: Project description.
- `public`: Boolean indicating if the project is public.
- `columns`: List of columns (status values) for the project.
  - `name`: Column name.
  - `id`: Optional column ID for updates to existing columns.

### Issues Section

- `title`: Issue title.
- `id`: Optional issue ID for updates to existing issues. If not provided, a new issue will be created.
- `body`: Issue description.
- `status`: Current status (maps to column name).
- `assignees`: List of GitHub usernames to assign to the issue.
- `labels`: List of labels to apply to the issue.
  - `name`: Label name.
  - `color`: Label color (hex code without #).
- `sub_issues`: List of sub-issues (recursive structure).
- `comments`: List of comments to add to the issue.
  - `body`: Comment text.

## ID Management

The DSL supports two modes of operation:

1. **Creation Mode**: When IDs are not provided, new resources are created.
2. **Update Mode**: When IDs are provided, existing resources are updated.

After creation, the tool will update the YAML file with the IDs of the created resources, allowing for subsequent updates.

## Environment Variables

The DSL supports environment variable substitution in the format `${VAR_NAME}`. This is particularly useful for sensitive information like authentication tokens.

## Validation Rules

The DSL enforces the following validation rules:

1. The `config` section is required and must include `organization` and `auth.token`.
2. Either `project.name` or `project.id` must be provided if the `project` section is included.
3. Either `title` or `id` must be provided for each issue.
4. Sub-issues can be nested up to 8 levels deep.

## Error Handling

The tool will validate the YAML file before applying any changes. If validation fails, an error message will be displayed indicating the specific validation error.

## Examples

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
    assignees:
      - "username"
    labels:
      - name: "bug"
        color: "d73a4a"
    sub_issues:
      - title: "Sub-task"
        body: "Description of the sub-task"
        status: "To Do"
    comments:
      - body: "This is a comment"
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
```
