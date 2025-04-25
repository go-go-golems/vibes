# Go Development Guidelines

## Project Setup

- Install the latest go compiler (see https://go.dev/doc/install)
- Initialize modules with a properly chosen module path
- Place code in logically named packages
- Organize files using standard Go project structure
- Set up a git repository for version control
- Always use absolute paths when running commands or writing files

## Code Quality

- Run `go fmt` before committing code
- Apply golangci-lint linting tools to ensure code quality
- Use `var _ Interface = &Foo{}` pattern to verify interface implementations
- Use context.Context argument when appropriate
- Use "defaults" package name instead of "default" (reserved in Go)

## Dependencies

- Use github.com/pkg/errors for wrapping errors
- Use errgroup when starting goroutines
- For CLI applications, use cobra
- For HTTP servers, use either standard net/http or echo
- Use viper for configuration with YAML as the primary format

## Testing

- Write comprehensive tests for algorithms and business logic
- Use table-driven tests when testing multiple input/output combinations
- Implement mocks when testing external dependencies
- Aim for high test coverage of critical code paths

## Best Practices

- Handle file paths cross-platform compatible way
- Ensure you're using the correct Go compiler version
- Document your code with clear comments
- Handle errors explicitly
- Use consistent code formatting
