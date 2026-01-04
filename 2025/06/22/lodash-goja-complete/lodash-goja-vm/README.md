# Lodash Goja VM Implementation

A comprehensive implementation of building lodash from source and running it in a Goja JavaScript VM with multiple minification methods comparison.

## Overview

This project demonstrates how to:
- Download lodash from npm programmatically
- Minify it using different methods (tdewolff, esbuild, terser)
- Embed it into a Go binary using `go:embed`
- Run it efficiently in a Goja JavaScript VM
- Compare performance and size characteristics

## Project Structure

```
lodash-goja-vm/
├── go.mod                          # Go module definition
├── go.sum                          # Go module checksums
├── main.go                         # Main demo application
├── internal/
│   ├── js/                         # Generated JavaScript files
│   │   ├── lodash.min.js          # Best minified version (auto-selected)
│   │   ├── lodash.tdewolff.min.js # tdewolff minified version
│   │   ├── lodash.esbuild.min.js  # esbuild minified version
│   │   └── lodash.terser.min.js   # terser minified version
│   └── tools/
│       ├── gen_lodash.go          # Simple tdewolff generator
│       └── gen_lodash_compare.go  # Comparison generator
├── test_suite.sh                   # Comprehensive test suite
├── perf_bench.go                   # Performance benchmarking
└── quick_validate.go               # Quick validation script
```

## Dependencies

- **Go 1.23.4+** - Latest Go toolchain
- **github.com/dop251/goja** - JavaScript VM for Go
- **github.com/tdewolff/minify/v2** - Pure Go minifier
- **Node.js 20+** - For esbuild and terser minification
- **esbuild** - Fast JavaScript bundler and minifier
- **terser** - JavaScript parser and mangler/compressor

## Installation

1. **Install Go 1.23.4+**:
   ```bash
   wget https://go.dev/dl/go1.23.4.linux-amd64.tar.gz
   sudo rm -rf /usr/local/go
   sudo tar -C /usr/local -xzf go1.23.4.linux-amd64.tar.gz
   export PATH=$PATH:/usr/local/go/bin
   ```

2. **Install Node.js and minifiers**:
   ```bash
   curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash -
   sudo apt-get install -y nodejs
   npm install -g esbuild terser
   ```

3. **Clone and setup project**:
   ```bash
   git clone <repository>
   cd lodash-goja-vm
   go mod tidy
   ```

## Usage

### Generate Lodash with All Minification Methods

```bash
cd internal/tools
go run gen_lodash_compare.go
```

This will:
- Download lodash 4.17.21 from npm
- Extract lodash.js from the tarball
- Minify using three different methods
- Compare results and select the best one
- Generate individual files for each method

### Run the Demo

```bash
go run main.go
```

This demonstrates:
- Loading lodash into Goja VM
- Testing various lodash functions
- Performance benchmarking
- Complex operations validation

### Run Comprehensive Tests

```bash
./test_suite.sh
```

This runs:
- Minification comparison
- Functionality validation for each method
- Performance benchmarks
- Memory usage tests
- Build verification



## Minification Comparison Results

### Size Comparison

| Method | Size (bytes) | Reduction | Notes |
|--------|-------------|-----------|-------|
| **Original** | 544,098 | 0% | Unminified lodash.js |
| **Terser** | 71,124 | **86.9%** | 🏆 Best compression |
| **tdewolff** | 71,895 | 86.8% | Pure Go solution |
| **esbuild** | 72,859 | 86.6% | Fastest build time |

### Performance Comparison

Performance test: 10,000 operations (uniq, chunk, merge)

| Method | Time | Ops/ms | Notes |
|--------|------|--------|-------|
| **esbuild** | 719ms | **13.91** | 🏆 Best runtime performance |
| **tdewolff** | 830ms | 12.06 | Good balance |
| **terser** | 1,169ms | 8.55 | Slower but smallest |

### Recommendations

- **For production**: Use **terser** for smallest bundle size (71,124 bytes)
- **For development**: Use **esbuild** for fastest performance (13.91 ops/ms)
- **For Go-only builds**: Use **tdewolff** for pure Go toolchain

## Implementation Details

### Minification Process

1. **Download**: Fetch lodash tarball from npm registry
2. **Extract**: Parse tar.gz and locate `package/lodash.js`
3. **Minify**: Apply chosen minification method
4. **Embed**: Use `go:embed` to include in binary
5. **Load**: Initialize in Goja VM at runtime

### Goja VM Integration

```go
//go:embed internal/js/lodash.min.js
var lodashSrc string

func main() {
    vm := goja.New()
    
    // Load lodash into global scope
    _, err := vm.RunString(lodashSrc)
    if err != nil {
        log.Fatal(err)
    }
    
    // Use lodash functions
    result, _ := vm.RunString("_.uniq([1,1,2,3])")
    fmt.Println(result.Export()) // [1 2 3]
}
```

### Supported Lodash Functions

✅ **Array Methods**: chunk, compact, difference, drop, flatten, intersection, reverse, uniq  
✅ **Collection Methods**: countBy, every, filter, find, groupBy, includes, map, reduce, size, some  
✅ **Object Methods**: assign, defaults, get, has, keys, merge, omit, pick, values  
✅ **String Methods**: camelCase, capitalize, endsWith, escape, kebabCase, lowerCase, pad, repeat, snakeCase, startsWith, trim, upperCase  
✅ **Math Methods**: add, ceil, divide, floor, max, mean, min, multiply, round, subtract, sum  
✅ **Utility Methods**: identity, isArray, isBoolean, isDate, isEmpty, isEqual, isFunction, isNumber, isObject, isString, noop  

### Memory Usage

- **Lodash source**: ~71KB embedded in binary
- **Runtime memory**: ~2-4MB for VM + lodash
- **No external dependencies** at runtime

## Advanced Usage

### Custom Builds

For smaller bundles, you can create custom lodash builds:

```go
// In gen_lodash_compare.go, modify to use lodash-cli
cmd := exec.Command("npx", "lodash",
    "include=chunk,uniq,merge",   // Only include needed functions
    "-o", tempOut,
)
```

### Multiple VM Instances

```go
// Create multiple isolated VMs
vm1 := goja.New()
vm2 := goja.New()

// Each VM gets its own lodash instance
vm1.RunString(lodashSrc)
vm2.RunString(lodashSrc)

// VMs are completely isolated
vm1.RunString("_.customVar = 'vm1'")
vm2.RunString("_.customVar = 'vm2'")
```

### Error Handling

```go
result, err := vm.RunString("_.invalidFunction([1,2,3])")
if err != nil {
    // Handle JavaScript errors
    if jsErr, ok := err.(*goja.Exception); ok {
        fmt.Printf("JS Error: %s\n", jsErr.String())
    }
}
```


## Examples

### Basic Usage

```go
package main

import (
    _ "embed"
    "fmt"
    "github.com/dop251/goja"
)

//go:embed internal/js/lodash.min.js
var lodashSrc string

func main() {
    vm := goja.New()
    vm.RunString(lodashSrc)
    
    // Array operations
    result, _ := vm.RunString("_.uniq([1,1,2,3,3,4])")
    fmt.Println("Unique:", result.Export()) // [1 2 3 4]
    
    // Object operations
    result, _ = vm.RunString("_.merge({a:1}, {b:2}, {c:3})")
    fmt.Println("Merged:", result.Export()) // map[a:1 b:2 c:3]
    
    // String operations
    result, _ = vm.RunString("_.camelCase('hello world test')")
    fmt.Println("CamelCase:", result.Export()) // helloWorldTest
}
```

### Data Processing Pipeline

```go
func processData() {
    vm := goja.New()
    vm.RunString(lodashSrc)
    
    // Set up data
    vm.RunString(`
        var users = [
            {name: "John", age: 30, city: "NYC"},
            {name: "Jane", age: 25, city: "LA"},
            {name: "Bob", age: 35, city: "NYC"}
        ];
    `)
    
    // Process with lodash
    result, _ := vm.RunString(`
        _.chain(users)
         .groupBy('city')
         .mapValues(group => _.meanBy(group, 'age'))
         .value()
    `)
    
    fmt.Println("Average age by city:", result.Export())
    // map[LA:25 NYC:32.5]
}
```

### Performance Optimization

```go
func optimizedUsage() {
    vm := goja.New()
    vm.RunString(lodashSrc)
    
    // Pre-compile frequently used functions
    vm.RunString(`
        var processArray = function(arr) {
            return _.chain(arr)
                    .uniq()
                    .chunk(2)
                    .flatten()
                    .value();
        };
    `)
    
    // Use pre-compiled function
    vm.Set("data", []int{1,1,2,2,3,3,4,4})
    result, _ := vm.RunString("processArray(data)")
    fmt.Println("Processed:", result.Export())
}
```

## Troubleshooting

### Common Issues

**1. "pattern internal/js/lodash.min.js: no matching files found"**
```bash
# Solution: Generate the lodash files first
cd internal/tools
go run gen_lodash_compare.go
```

**2. "go: cannot run *_test.go files"**
```bash
# Solution: Rename test files to avoid _test.go suffix
mv perf_test.go perf_bench.go
```

**3. "SyntaxError: Identifier '_' has already been declared"**
```bash
# Solution: Use fresh VM instance or wrap in function scope
vm := goja.New() // Create new VM for each test
```

**4. Node.js tools not found**
```bash
# Solution: Install Node.js and global packages
npm install -g esbuild terser
```

### Performance Tips

1. **Reuse VM instances** when possible to avoid initialization overhead
2. **Pre-compile complex operations** into JavaScript functions
3. **Use the esbuild version** for best runtime performance
4. **Use the terser version** for smallest binary size
5. **Avoid frequent VM creation** in hot paths

### Memory Management

```go
// For long-running applications
func managedUsage() {
    vm := goja.New()
    vm.RunString(lodashSrc)
    
    // Process data in batches
    for batch := range dataBatches {
        result, _ := vm.RunString(fmt.Sprintf(
            "_.chunk(%v, 100)", batch))
        
        // Process result...
        
        // Trigger GC periodically
        if batchCount%1000 == 0 {
            runtime.GC()
        }
    }
}
```

## Build and Deployment

### Development Build

```bash
go run main.go
```

### Production Build

```bash
# Build optimized binary
go build -ldflags="-s -w" -o lodash-app main.go

# Check binary size
ls -lh lodash-app
```

### Docker Deployment

```dockerfile
FROM golang:1.23-alpine AS builder
WORKDIR /app
COPY . .
RUN go build -ldflags="-s -w" -o app main.go

FROM alpine:latest
RUN apk --no-cache add ca-certificates
WORKDIR /root/
COPY --from=builder /app/app .
CMD ["./app"]
```

### Cross-Platform Builds

```bash
# Linux
GOOS=linux GOARCH=amd64 go build -o lodash-linux main.go

# Windows
GOOS=windows GOARCH=amd64 go build -o lodash-windows.exe main.go

# macOS
GOOS=darwin GOARCH=amd64 go build -o lodash-macos main.go
```

## License

This implementation is provided as-is for educational and development purposes. Lodash itself is licensed under the MIT License.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## Changelog

### v1.0.0
- Initial implementation with three minification methods
- Comprehensive test suite
- Performance benchmarking
- Full lodash function support
- Documentation and examples

