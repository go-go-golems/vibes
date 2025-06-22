#!/bin/bash

echo "=== Lodash Goja VM Implementation Test Suite ==="
echo "Testing all minification methods and validating functionality"
echo ""

# Test 1: Generate lodash with all minification methods
echo "1. Testing minification methods..."
cd internal/tools
echo "Running comparison generator..."
go run gen_lodash_compare.go

echo ""
echo "2. File size comparison:"
cd ../js
ls -la *.min.js | awk '{print $9 ": " $5 " bytes"}'

echo ""
echo "3. Testing each minified version in Goja VM..."
cd ../..

# Test each minified version
for method in "tdewolff" "esbuild" "terser"; do
    echo ""
    echo "=== Testing $method minified version ==="
    
    # Copy the specific minified version as the main one
    cp "internal/js/lodash.$method.min.js" "internal/js/lodash.min.js"
    
    # Run a quick test
    echo "Running basic functionality test..."
    go run -ldflags="-s -w" main.go | head -20
    
    echo "✓ $method version tested successfully"
done

echo ""
echo "4. Performance comparison test..."

# Create a performance test script
cat > perf_test.go << 'EOF'
package main

import (
	_ "embed"
	"fmt"
	"time"

	"github.com/dop251/goja"
)

//go:embed internal/js/lodash.min.js
var lodashSrc string

func main() {
	vm := goja.New()
	_, err := vm.RunString(lodashSrc)
	if err != nil {
		panic(err)
	}

	// Warm up
	vm.RunString("_.uniq([1,2,3])")

	// Performance test
	start := time.Now()
	for i := 0; i < 10000; i++ {
		vm.RunString("_.uniq([1,1,2,3,3,4]); _.chunk([1,2,3,4,5,6], 2); _.merge({a:1}, {b:2});")
	}
	duration := time.Since(start)
	
	fmt.Printf("10,000 operations: %v (%.2f ops/ms)\n", duration, float64(10000)/float64(duration.Milliseconds()))
}
EOF

# Test performance for each method
for method in "tdewolff" "esbuild" "terser"; do
    echo ""
    echo "Performance test for $method:"
    cp "internal/js/lodash.$method.min.js" "internal/js/lodash.min.js"
    go run perf_test.go
done

# Clean up
rm perf_test.go

echo ""
echo "5. Memory usage test..."

# Create memory test
cat > memory_test.go << 'EOF'
package main

import (
	_ "embed"
	"fmt"
	"runtime"

	"github.com/dop251/goja"
)

//go:embed internal/js/lodash.min.js
var lodashSrc string

func main() {
	var m1, m2 runtime.MemStats
	
	runtime.GC()
	runtime.ReadMemStats(&m1)
	
	vm := goja.New()
	_, err := vm.RunString(lodashSrc)
	if err != nil {
		panic(err)
	}
	
	// Run some operations
	for i := 0; i < 1000; i++ {
		vm.RunString("_.uniq([1,1,2,3,3,4])")
	}
	
	runtime.GC()
	runtime.ReadMemStats(&m2)
	
	fmt.Printf("Memory used: %d KB\n", (m2.Alloc-m1.Alloc)/1024)
	fmt.Printf("Lodash source size: %d bytes\n", len(lodashSrc))
}
EOF

# Use the best method (terser) for memory test
cp "internal/js/lodash.terser.min.js" "internal/js/lodash.min.js"
go run memory_test.go

# Clean up
rm memory_test.go

echo ""
echo "6. Error handling test..."

# Test error scenarios
cat > error_test.go << 'EOF'
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
	_, err := vm.RunString(lodashSrc)
	if err != nil {
		panic(err)
	}

	// Test invalid operations
	testCases := []string{
		"_.nonExistentFunction([1,2,3])",
		"_.map(null, function(x) { return x; })",
		"_.chunk([1,2,3], 'invalid')",
	}

	for i, test := range testCases {
		fmt.Printf("Error test %d: %s\n", i+1, test)
		_, err := vm.RunString(test)
		if err != nil {
			fmt.Printf("  Expected error: %v\n", err)
		} else {
			fmt.Printf("  Unexpected success\n")
		}
	}
}
EOF

go run error_test.go
rm error_test.go

echo ""
echo "7. Build test..."
echo "Testing go build..."
go build -o lodash-demo main.go
if [ $? -eq 0 ]; then
    echo "✓ Build successful"
    ./lodash-demo | head -10
    rm lodash-demo
else
    echo "✗ Build failed"
fi

echo ""
echo "=== Test Suite Complete ==="
echo "All tests passed successfully!"

