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
fmt.Println("=== Comprehensive Lodash Validation ===")

vm := goja.New()
_, err := vm.RunString(lodashSrc)
if err != nil {
panic(err)
}

// Test all major lodash categories
tests := []struct {
category string
tests    []string
}{
{
"Array Methods",
[]string{
"_.chunk([1,2,3,4,5], 2)",
"_.compact([0, 1, false, 2, '', 3])",
"_.difference([2, 1], [2, 3])",
"_.drop([1, 2, 3], 1)",
"_.flatten([1, [2, [3, [4]], 5]])",
"_.intersection([2, 1], [2, 3])",
"_.reverse([1, 2, 3])",
"_.uniq([2, 1, 2])",
},
},
{
"Collection Methods",
[]string{
"_.countBy(['one', 'two', 'three'], 'length')",
"_.every([true, 1, null, 'yes'], Boolean)",
"_.filter([1, 2, 3, 4], function(n) { return n % 2 == 0; })",
"_.find([1, 2, 3, 4], function(n) { return n % 2 == 0; })",
"_.groupBy([6.1, 4.2, 6.3], Math.floor)",
"_.includes([1, 2, 3], 1)",
"_.map([4, 8], function(n) { return n * n; })",
"_.reduce([1, 2], function(sum, n) { return sum + n; }, 0)",
"_.size([1, 2, 3])",
"_.some([null, 0, 'yes', false], Boolean)",
},
},
{
"Object Methods",
[]string{
"_.assign({a: 1}, {b: 2}, {c: 3})",
"_.defaults({a: 1}, {b: 2}, {a: 3})",
"_.get({a: {b: 2}}, 'a.b')",
"_.has({a: {b: 2}}, 'a')",
"_.keys({a: 1, b: 2, c: 3})",
"_.merge({a: 1}, {b: {c: 2}}, {b: {d: 3}})",
"_.omit({a: 1, b: 2, c: 3}, ['a', 'c'])",
"_.pick({a: 1, b: 2, c: 3}, ['a', 'c'])",
"_.values({a: 1, b: 2, c: 3})",
},
},
{
"String Methods",
[]string{
"_.camelCase('Foo Bar')",
"_.capitalize('FRED')",
"_.endsWith('abc', 'c')",
"_.escape('fred, barney, & pebbles')",
"_.kebabCase('Foo Bar')",
"_.lowerCase('--Foo-Bar--')",
"_.pad('abc', 8)",
"_.repeat('*', 3)",
"_.snakeCase('Foo Bar')",
"_.startsWith('abc', 'a')",
"_.trim('  abc  ')",
"_.upperCase('--foo-bar--')",
},
},
{
"Math Methods",
[]string{
"_.add(6, 4)",
"_.ceil(4.006)",
"_.divide(6, 4)",
"_.floor(4.006)",
"_.max([4, 2, 8, 6])",
"_.mean([4, 2, 8, 6])",
"_.min([4, 2, 8, 6])",
"_.multiply(6, 4)",
"_.round(4.006)",
"_.subtract(6, 4)",
"_.sum([4, 2, 8, 6])",
},
},
{
"Utility Methods",
[]string{
"_.identity(42)",
"_.isArray([])",
"_.isBoolean(false)",
"_.isDate(new Date)",
"_.isEmpty(null)",
"_.isEqual([1, 2], [1, 2])",
"_.isFunction(function() {})",
"_.isNumber(3)",
"_.isObject({})",
"_.isString('abc')",
"typeof _.noop()",
},
},
}

successCount := 0
totalCount := 0

for _, category := range tests {
fmt.Printf("\n=== %s ===\n", category.category)
for _, test := range category.tests {
totalCount++
result, err := vm.RunString(test)
if err != nil {
fmt.Printf("❌ %s: ERROR - %v\n", test, err)
} else {
fmt.Printf("✅ %s: %v\n", test, result.Export())
successCount++
}
}
}

fmt.Printf("\n=== Summary ===\n")
fmt.Printf("Total tests: %d\n", totalCount)
fmt.Printf("Successful: %d\n", successCount)
fmt.Printf("Failed: %d\n", totalCount-successCount)
fmt.Printf("Success rate: %.1f%%\n", float64(successCount)/float64(totalCount)*100)

// Memory usage
var m runtime.MemStats
runtime.GC()
runtime.ReadMemStats(&m)
fmt.Printf("Memory usage: %d KB\n", m.Alloc/1024)
fmt.Printf("Lodash size: %d bytes\n", len(lodashSrc))
}
