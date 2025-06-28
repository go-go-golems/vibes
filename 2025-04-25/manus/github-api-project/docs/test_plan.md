# Test Plan for GitHub Project Management Tool

This document outlines the testing approach for the GitHub Project Management tool.

## Prerequisites

- GitHub Personal Access Token with the following scopes:
  - `project` (for full access to projects)
  - `repo` (for access to repositories and issues)
- A GitHub organization or user account where you have permissions to create projects

## Test Cases

### 1. Configuration Validation

- **Test Case 1.1**: Validate a valid YAML configuration file
  - Command: `ghpm validate example.yaml`
  - Expected Result: "Configuration file is valid"

- **Test Case 1.2**: Validate an invalid YAML configuration file
  - Create a test file with missing required fields
  - Command: `ghpm validate invalid.yaml`
  - Expected Result: Error message indicating validation failure

### 2. Project Creation and Management

- **Test Case 2.1**: Create a new project
  - Modify example.yaml with appropriate organization/username
  - Command: `ghpm apply example.yaml`
  - Expected Result: Project created successfully with ID updated in YAML file

- **Test Case 2.2**: Update an existing project
  - Modify the project name in the YAML file (with ID preserved)
  - Command: `ghpm apply example.yaml`
  - Expected Result: Project updated successfully

### 3. Issue Management

- **Test Case 3.1**: Create issues and sub-issues
  - Use the example.yaml with hierarchical issue structure
  - Command: `ghpm apply example.yaml`
  - Expected Result: Issues and sub-issues created with proper parent-child relationships

- **Test Case 3.2**: Update issue status
  - Modify the status of an existing issue in the YAML file
  - Command: `ghpm apply example.yaml`
  - Expected Result: Issue status updated in GitHub

- **Test Case 3.3**: Add assignees to issues
  - Ensure valid GitHub usernames are specified in the YAML file
  - Command: `ghpm apply example.yaml`
  - Expected Result: Assignees added to the issues

- **Test Case 3.4**: Add comments to issues
  - Include comments in the YAML file for issues
  - Command: `ghpm apply example.yaml`
  - Expected Result: Comments added to the issues

### 4. CLI Functionality

- **Test Case 4.1**: Initialize a new configuration
  - Command: `ghpm init test-dir`
  - Expected Result: New directory created with template YAML file

## Test Execution

Once we have the GitHub Personal Access Token, we'll execute these tests in sequence:

1. Set the token as an environment variable:
   ```
   export GITHUB_TOKEN=your_token_here
   ```

2. Run the validation tests to ensure the configuration is valid

3. Run the project creation and management tests

4. Run the issue management tests

5. Run the CLI functionality tests

## Expected Results

After successful testing, we should have:
- A new GitHub project created with the specified structure
- Issues and sub-issues created with proper relationships
- Issues assigned to users and with appropriate statuses
- Comments added to issues
- The YAML file updated with all the IDs for future updates

## Troubleshooting

If any tests fail, we'll:
1. Check the error messages for specific API issues
2. Verify the GitHub token has the correct permissions
3. Ensure the organization/username has the necessary access rights
4. Check the GraphQL API documentation for any changes or limitations
