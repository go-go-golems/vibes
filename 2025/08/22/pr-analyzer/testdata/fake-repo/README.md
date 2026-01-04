# Test Project

This is a test project for demonstrating the PR analyzer tool.

## Structure

- `frontend/` - React frontend application
- `backend/` - Go backend API
- `database/` - Database schemas and migrations
- `docs/` - Documentation
- `config/` - Configuration files
- `tests/` - Test files

## PR Analyzer Tool Validation

This repository was created to test the PR analyzer tool with the following merge commits:

1. **Frontend Improvements** (9fae05f): Added Header component and modern styling
   - Languages: CSS (3 files), JavaScript (2 files)
   - Systems: frontend only
   - Cross-system rate: 0%

2. **Backend API Improvements** (dae4140): Added service layer and database migrations
   - Languages: Go (3 files), SQL (1 file)
   - Systems: backend, database
   - Cross-system rate: 0% (each commit touched single system)

3. **Full-stack Integration** (f76cb9d): Added API client, Docker setup, and documentation
   - Languages: Markdown, YAML, JavaScript, Go
   - Systems: config, docs, frontend, tests
   - Cross-system rate: 100% (single commit touched multiple systems)

The tool successfully demonstrates:
- Language-based statistics with percentage calculations
- Cross-subsystem analysis with co-occurrence matrix
- Custom categorization using glob patterns
- Multiple output formats (table, JSON, YAML)
- Exclude patterns for filtering files

