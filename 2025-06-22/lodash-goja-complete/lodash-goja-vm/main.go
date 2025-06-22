package main

import (
	_ "embed"
	"fmt"
	"log"

	"github.com/dop251/goja"
)

//go:embed internal/js/lodash.min.js
var lodashSrc string

func main() {
	fmt.Println("=== Lodash Goja VM Demo ===")
	fmt.Printf("Lodash minified size: %d bytes\n\n", len(lodashSrc))

	// Create a new Goja VM
	vm := goja.New()

	// Load lodash into the global scope
	_, err := vm.RunString(lodashSrc)
	if err != nil {
		log.Fatalf("Failed to load lodash: %v", err)
	}

	// Test various lodash functions
	testCases := []struct {
		name string
		code string
	}{
		{
			name: "Array uniq",
			code: `_.uniq([1, 1, 2, 3, 3, 4])`,
		},
		{
			name: "Array chunk",
			code: `_.chunk([1, 2, 3, 4, 5, 6], 2)`,
		},
		{
			name: "Object merge",
			code: `_.merge({a: 1, b: {c: 2}}, {b: {d: 3}, e: 4})`,
		},
		{
			name: "String camelCase",
			code: `_.camelCase("hello world test")`,
		},
		{
			name: "Collection map",
			code: `_.map([1, 2, 3, 4], function(x) { return x * 2; })`,
		},
		{
			name: "Collection filter",
			code: `_.filter([1, 2, 3, 4, 5, 6], function(x) { return x % 2 === 0; })`,
		},
		{
			name: "Object pick",
			code: `_.pick({a: 1, b: 2, c: 3, d: 4}, ['a', 'c'])`,
		},
		{
			name: "Array flatten",
			code: `_.flatten([1, [2, 3], [4, [5, 6]]])`,
		},
		{
			name: "Utility debounce (function creation)",
			code: `typeof _.debounce(function() { return "debounced!"; }, 100)`,
		},
		{
			name: "Math operations",
			code: `
				({
					sum: _.sum([1, 2, 3, 4, 5]),
					mean: _.mean([1, 2, 3, 4, 5]),
					max: _.max([1, 5, 3, 2, 4]),
					min: _.min([1, 5, 3, 2, 4])
				})
			`,
		},
	}

	// Run all test cases
	for i, test := range testCases {
		fmt.Printf("%d. %s:\n", i+1, test.name)
		
		result, err := vm.RunString(test.code)
		if err != nil {
			fmt.Printf("   Error: %v\n", err)
		} else {
			fmt.Printf("   Result: %v\n", result.Export())
		}
		fmt.Println()
	}

	// Test lodash version and build info
	fmt.Println("=== Lodash Info ===")
	versionResult, err := vm.RunString(`
		({
			version: _.VERSION,
			isFunction: _.isFunction(_.map),
			isArray: _.isArray([1, 2, 3]),
			keysCount: Object.keys(_).length
		})
	`)
	
	if err != nil {
		fmt.Printf("Error getting lodash info: %v\n", err)
	} else {
		fmt.Printf("Lodash info: %v\n", versionResult.Export())
	}

	// Performance test
	fmt.Println("\n=== Performance Test ===")
	perfResult, err := vm.RunString(`
		(function() {
			var start = Date.now();
			
			// Perform multiple operations
			for (var i = 0; i < 1000; i++) {
				_.uniq([1, 1, 2, 3, 3, 4]);
				_.chunk([1, 2, 3, 4, 5, 6], 2);
				_.merge({a: i}, {b: i * 2});
			}
			
			var end = Date.now();
			return end - start;
		})()
	`)
	
	if err != nil {
		fmt.Printf("Error in performance test: %v\n", err)
	} else {
		fmt.Printf("1000 operations took: %v ms\n", perfResult.Export())
	}

	// Test complex operations
	fmt.Println("\n=== Complex Operations Test ===")
	complexResult, err := vm.RunString(`
		(function() {
			var data = [
				{name: "John", age: 30, city: "New York"},
				{name: "Jane", age: 25, city: "Los Angeles"},
				{name: "Bob", age: 35, city: "New York"},
				{name: "Alice", age: 28, city: "Chicago"}
			];
			
			return {
				groupedByCity: _.groupBy(data, 'city'),
				sortedByAge: _.sortBy(data, 'age'),
				pluckedNames: _.map(data, 'name'),
				averageAge: _.meanBy(data, 'age'),
				uniqueCities: _.uniq(_.map(data, 'city'))
			};
		})()
	`)
	
	if err != nil {
		fmt.Printf("Error in complex operations test: %v\n", err)
	} else {
		fmt.Printf("Complex operations result: %v\n", complexResult.Export())
	}

	fmt.Println("\n=== Demo Complete ===")
}

