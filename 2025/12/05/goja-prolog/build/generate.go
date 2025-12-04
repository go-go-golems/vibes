//go:build generate
// +build generate

package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// 1️⃣ Type-check (no JS emitted)
//go:generate npx tsc --project ../web/tsconfig.json --noEmit
// 2️⃣ Produce CommonJS bundle that Goja can load (to temp location)
//go:generate npx esbuild ../web/app.ts --bundle --format=cjs --platform=node --target=es2019 --outfile=../assets/prolog-ts.js --sourcemap=inline
// 3️⃣ Copy bundle to embeddable location
//go:generate sh -c "mkdir -p ../cmd/prolog-repl/assets && cp ../assets/prolog-ts.js ../cmd/prolog-repl/assets/prolog-ts.js"

func main() {
	if len(os.Args) > 1 && os.Args[1] == "copy-bundle" {
		// Get the directory of this file
		buildDir, err := os.Getwd()
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error getting current directory: %v\n", err)
			os.Exit(1)
		}

		// Paths relative to build directory
		source := filepath.Join(buildDir, "../assets/prolog-ts.js")
		dest := filepath.Join(buildDir, "../cmd/prolog-repl/assets/prolog-ts.js")

		// Ensure destination directory exists
		destDir := filepath.Dir(dest)
		if err := os.MkdirAll(destDir, 0755); err != nil {
			fmt.Fprintf(os.Stderr, "Error creating destination directory: %v\n", err)
			os.Exit(1)
		}

		// Copy file using cp command
		cmd := exec.Command("cp", source, dest)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			fmt.Fprintf(os.Stderr, "Error copying bundle: %v\n", err)
			os.Exit(1)
		}

		fmt.Printf("Copied bundle to %s\n", dest)
	} else {
		fmt.Println("This file is only used for go:generate directives")
		// This function is never executed
	}
}

