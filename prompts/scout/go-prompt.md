# Go Development Guidelines

## Project Setup

- Install the latest go compiler (see https://go.dev/doc/install)
- Initialize modules with a properly chosen module path
- Use go.mod and do go mod tidy after every step
- Place code in logically named packages
- Organize files using standard Go project structure
- Always use absolute paths when running commands or writing files
- Use the standard pkg/ cmd/ internal/ directories

## Source control (IMPORTANT)

- Set up a git repository for version control
- ALWAYS USE git to manage your code
- always add every file to git, even if you think you already added it
- Call git to commit after every step
- at regular intervals, run "git add ." to add everything to make sure we have it all

## Code Quality

- Run `go fmt` before committing code
- Apply golangci-lint linting tools to ensure code quality
- Use `var _ Interface = &Foo{}` pattern to verify interface implementations
- Use context.Context argument when appropriate
- Use "defaults" package name instead of "default" (reserved in Go)
- Use zerolog for logging and be extremely generous with logging so that we can better debug issues. Default log level should be debug.

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
