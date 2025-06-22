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

tests := []string{
"_.uniq([1,1,2,3])",
"_.chunk([1,2,3,4], 2)",
"_.merge({a:1}, {b:2})",
"_.camelCase(\"hello world\")",
"_.sum([1,2,3,4,5])",
}

fmt.Println("=== Quick Validation ===")
for i, test := range tests {
result, err := vm.RunString(test)
if err != nil {
fmt.Printf("%d. %s: ERROR - %v\n", i+1, test, err)
} else {
fmt.Printf("%d. %s: %v\n", i+1, test, result.Export())
}
}
fmt.Printf("Lodash size: %d bytes\n", len(lodashSrc))
}
