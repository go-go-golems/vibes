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

