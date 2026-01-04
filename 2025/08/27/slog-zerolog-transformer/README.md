# Go slog to zerolog AST Transformer

This project demonstrates the development of an Abstract Syntax Tree (AST) transformer that converts Go applications from using the standard library's `log/slog` package to the third-party `github.com/rs/zerolog` logging library.

## Project Structure

```
slog-zerolog-transformer/
├── README.md                    # This file
├── slog-demo/                   # Original slog demonstration application
│   ├── main.go                  # Demo app using slog
│   ├── go.mod                   # Go module file
│   └── slog-demo                # Compiled binary
├── slog-demo-copy/              # Transformed application using zerolog
│   ├── main.go                  # Demo app converted to use zerolog
│   ├── go.mod                   # Go module file with zerolog dependency
│   └── zerolog-demo             # Compiled binary
├── transformer/                 # AST transformer tool
│   ├── main.go                  # Transformer implementation
│   ├── go.mod                   # Go module file
│   └── transformer              # Compiled binary
├── outputs/                     # Log outputs and analysis
│   ├── slog-output.log          # Output from slog demo
│   ├── zerolog-output.log       # Output from zerolog demo
│   ├── comparison.diff          # Diff between outputs
│   └── analysis.md              # Analysis of differences
├── docs/                        # Documentation
│   └── comprehensive-report.md  # Detailed technical report
└── todo.md                      # Project progress tracking
```

## Quick Start

### Prerequisites

- Go 1.23.4 or later
- Unix-like environment (Linux, macOS)

### Running the Original slog Demo

```bash
cd slog-demo
go run main.go
```

### Running the Transformed zerolog Demo

```bash
cd slog-demo-copy
go run main.go
```

### Using the AST Transformer

```bash
cd transformer
go build -o transformer .
./transformer -input <source_directory> -output <target_directory>
```

## Key Features Demonstrated

### slog Features
- Basic logging with different levels (Debug, Info, Warn, Error)
- Structured logging with key-value pairs
- Context-aware logging
- Grouped logging
- Custom error types
- Handler configuration

### zerolog Features
- Fluent interface with method chaining
- Colorized console output
- Zero-allocation logging design
- Type-specific logging methods
- Performance-optimized output

## Transformation Capabilities

The AST transformer handles:
- ✅ Basic log level conversions (Info, Debug, Warn, Error)
- ✅ Structured data key-value pairs
- ✅ Import statement transformations
- ✅ Simple handler configurations
- ✅ Error logging patterns

### Limitations
- ❌ Complex handler configurations
- ❌ Context-aware logging (context information is lost)
- ❌ Advanced slog features (Groups, Attributes)
- ❌ Import aliases and dot imports
- ❌ Custom handler implementations

## Output Comparison

### slog Output Format
```
time=2025-08-27T12:58:09.833-04:00 level=INFO msg="Application starting" version=1.0.0
```

### zerolog Output Format
```
[90m1:01PM[0m [32mINF[0m [1mApplication starting[0m [36mversion=[0m1.0.0
```

## Performance Characteristics

- **slog**: Standard library performance, machine-readable output
- **zerolog**: Zero-allocation design, human-readable colorized output

## Documentation

See `docs/comprehensive-report.md` for a detailed technical analysis including:
- Implementation methodology
- AST transformation techniques
- Comparative analysis of outputs
- Performance considerations
- Limitations and future improvements

## Building from Source

All components can be built using standard Go commands:

```bash
# Build slog demo
cd slog-demo && go build -o slog-demo .

# Build zerolog demo
cd slog-demo-copy && go build -o zerolog-demo .

# Build transformer
cd transformer && go build -o transformer .
```

## Dependencies

### slog-demo
- Go standard library only

### slog-demo-copy (zerolog version)
- `github.com/rs/zerolog v1.34.0`

### transformer
- Go standard library only (uses go/ast, go/parser, go/format)

## License

This project is provided as-is for educational and demonstration purposes.

## Author

Manus AI - August 27, 2025

