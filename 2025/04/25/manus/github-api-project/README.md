# GitHub Project Management Tool

A command-line tool for managing GitHub projects and issues using a YAML-based Domain Specific Language (DSL).

## Features

- Create and update projects
- Create issues and sub-issues with hierarchical relationships
- Update issue statuses
- Add assignees to issues
- Add comments to issues
- Manage project columns

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

## Quick Start

1. Initialize a new project:
   ```bash
   ghpm init my-project
   ```

2. Edit the configuration file:
   ```bash
   nano my-project/github-project.yaml
   ```

3. Set your GitHub token as an environment variable:
   ```bash
   export GITHUB_TOKEN=your_token_here
   ```

4. Validate the configuration:
   ```bash
   ghpm validate my-project/github-project.yaml
   ```

5. Apply the configuration:
   ```bash
   ghpm apply my-project/github-project.yaml
   ```

## Documentation

- [User Guide](docs/guide.md) - Comprehensive guide to using the tool
- [YAML DSL Reference](docs/yaml_dsl.md) - Detailed reference for the YAML DSL
- [CLI Reference](docs/cli_reference.md) - Command-line interface reference
- [API Reference](docs/api_reference.md) - GitHub API reference

## Example Configuration

```yaml
config:
  auth:
    token: "${GITHUB_TOKEN}"
  organization: "my-org"
  
project:
  name: "My Project"
  description: "Project description"
  public: true
  columns:
    - name: "To Do"
    - name: "In Progress"
    - name: "Done"
  
issues:
  - title: "Main Feature"
    body: "Implement the main feature"
    status: "To Do"
    sub_issues:
      - title: "Sub-task 1"
        body: "Implement sub-task 1"
        status: "To Do"
    comments:
      - body: "This is a comment"
```

## License

MIT

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
