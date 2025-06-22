package main

import (
	"fmt"
)

//go:build generate
// +build generate

// 1️⃣ Type-check (no JS emitted)
//go:generate npx tsc --project ../web/tsconfig.json --noEmit
// 2️⃣ Produce CommonJS bundle that Goja can load
//go:generate npx esbuild ../web/app.ts --bundle --format=cjs --platform=node --target=es2019 --outfile=../web/app.js
// 3️⃣ Export Go structs/interfaces to TypeScript
//go:generate tygo generate --packages=github.com/example/myvm/internal/handlers --output=../web/types

func main() {
	fmt.Println("This file is only used for go:generate directives")
	// This function is never executed
}
