# problems

- filepaths
- installing the right go compiler
- create and initializing the module + package name
- add tests for algorithms
- use mocks in the tests when necessary
- use table driven tests when possible
- use cobra for cli apps
- use either plain httpServer or echo for http apps
- use viper for configuration and use yaml for most cases
- use errGroup

When implementing go interfaces, use the var \_ Interface = &Foo{} to make sure the interface is always implemented correctly.
Always use a context argument when appropriate.
Use the "defaults" package name, instead of "default" package name, as it's reserved in go.
Use github.com/pkg/errors for wrapping errors.
When starting goroutines, use errgroup.

- use a git repo to manage the files
- always cd into the absolute path before running commands / writing files
- use lint, also run go fmt
