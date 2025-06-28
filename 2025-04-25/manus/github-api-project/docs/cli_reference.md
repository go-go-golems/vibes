# GitHub Project Management Tool - CLI Reference

This document provides a detailed reference for the command-line interface (CLI) of the GitHub Project Management tool.

## Command Overview

The GitHub Project Management tool provides the following commands:

```
ghpm - GitHub Project Manager

Usage:
  ghpm [command]

Available Commands:
  apply       Apply a YAML configuration file
  help        Help about any command
  init        Initialize a new YAML configuration file
  validate    Validate a YAML configuration file

Flags:
  -h, --help   help for ghpm
```

## Command Details

### Initialize Command

```
ghpm init [directory]
```

Creates a new directory (if it doesn't exist) and initializes a template YAML configuration file.

**Arguments:**
- `directory`: Optional. The directory where the configuration file will be created. Defaults to the current directory.

**Example:**
```bash
ghpm init my-project
```

This will create a directory called `my-project` (if it doesn't exist) and initialize a template YAML configuration file called `github-project.yaml` in that directory.

**Output:**
```
Created configuration file: my-project/github-project.yaml
Edit this file with your project details, then run:
  ghpm validate my-project/github-project.yaml
  ghpm apply my-project/github-project.yaml
```

### Validate Command

```
ghpm validate [file]
```

Validates a YAML configuration file without applying any changes.

**Arguments:**
- `file`: Required. The path to the YAML configuration file.

**Example:**
```bash
ghpm validate my-project/github-project.yaml
```

**Output:**
If the file is valid:
```
Configuration file is valid
```

If the file is invalid:
```
Error: validation failed: [error message]
```

### Apply Command

```
ghpm apply [file]
```

Applies a YAML configuration file to create or update GitHub projects and issues.

**Arguments:**
- `file`: Required. The path to the YAML configuration file.

**Example:**
```bash
ghpm apply my-project/github-project.yaml
```

**Output:**
The command will output progress information as it creates or updates resources:
```
Creating project: My Project
Project created with ID: PVT_kwDOABCD123
Creating issue: Main Feature
Issue created with ID: I_kwDOABCD123
Added comment to issue
Creating issue: Sub-task 1
Issue created with ID: I_kwDOABCD456
Configuration applied successfully
```

After successful execution, the YAML file will be updated with the IDs of the created resources.

### Help Command

```
ghpm help [command]
```

Displays help information about the specified command.

**Arguments:**
- `command`: Optional. The command to get help for. If not specified, general help information is displayed.

**Example:**
```bash
ghpm help apply
```

**Output:**
```
Apply a YAML configuration file to create or update GitHub projects and issues.

Usage:
  ghpm apply [file] [flags]

Flags:
  -h, --help   help for apply
```

## Environment Variables

The tool recognizes the following environment variables:

- `GITHUB_TOKEN`: GitHub Personal Access Token for authentication. This can be used instead of specifying the token in the YAML file.

**Example:**
```bash
export GITHUB_TOKEN=ghp_your_token_here
ghpm apply my-project/github-project.yaml
```

## Exit Codes

The tool uses the following exit codes:

- `0`: Success
- `1`: Error (with error message displayed)

## Examples

### Complete Workflow Example

1. Initialize a new project:
   ```bash
   ghpm init my-project
   ```

2. Edit the configuration file:
   ```bash
   nano my-project/github-project.yaml
   ```

3. Validate the configuration:
   ```bash
   ghpm validate my-project/github-project.yaml
   ```

4. Apply the configuration:
   ```bash
   export GITHUB_TOKEN=ghp_your_token_here
   ghpm apply my-project/github-project.yaml
   ```

5. Update the configuration and apply again:
   ```bash
   nano my-project/github-project.yaml
   ghpm apply my-project/github-project.yaml
   ```

## Troubleshooting

### Common Errors

1. **Authentication Error**:
   ```
   Error: failed to get organization ID: failed to get organization ID: Could not resolve to an Organization with the login of 'org-name'.
   ```
   Solution: Check that your GitHub token has the correct permissions and that the organization name is correct.

2. **Validation Error**:
   ```
   Error: validation failed: organization is required in config section
   ```
   Solution: Ensure your YAML file includes all required fields.

3. **User Not Found Error**:
   ```
   Error: failed to get user ID for username: failed to get user ID: Could not resolve to a User with the login of 'username'.
   ```
   Solution: Verify that the username exists on GitHub.

### Debugging Tips

1. Validate your YAML file before applying it:
   ```bash
   ghpm validate my-project/github-project.yaml
   ```

2. Check your GitHub token permissions:
   - Ensure it has the `project` and `repo` scopes
   - Verify it hasn't expired

3. Check your organization/username:
   - Ensure you have the correct permissions in the organization
   - Verify the organization name is spelled correctly

4. For issues with specific API calls, refer to the [API Reference](api_reference.md) for details on the expected format and parameters.
