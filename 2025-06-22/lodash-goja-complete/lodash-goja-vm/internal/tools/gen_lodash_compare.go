//go:build ignore
// +build ignore

//go:generate go run .

package main

import (
	"archive/tar"
	"compress/gzip"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/tdewolff/minify/v2"
	"github.com/tdewolff/minify/v2/js"
)

const (
	version = "4.17.21"
	tgzURL  = "https://registry.npmjs.org/lodash/-/lodash-" + version + ".tgz"
	baseDir = "../js"
)

func main() {
	fmt.Printf("Downloading lodash %s from npm...\n", version)
	
	// Download and extract lodash.js
	src, err := downloadAndExtractLodash()
	if err != nil {
		panic(err)
	}

	fmt.Printf("Original size: %d bytes\n", len(src))

	// Create output directory
	if err := os.MkdirAll(baseDir, 0o755); err != nil {
		panic(fmt.Errorf("failed to create output directory: %w", err))
	}

	// Write original file for Node.js tools
	originalFile := filepath.Join(baseDir, "lodash.original.js")
	if err := os.WriteFile(originalFile, src, 0o644); err != nil {
		panic(fmt.Errorf("failed to write original file: %w", err))
	}

	// Test different minification methods
	results := make(map[string]int)

	// 1. tdewolff/minify (Go)
	fmt.Println("\n=== Testing tdewolff/minify (Go) ===")
	minified1, err := minifyWithTdewolff(src)
	if err != nil {
		fmt.Printf("tdewolff/minify failed: %v\n", err)
	} else {
		outFile1 := filepath.Join(baseDir, "lodash.tdewolff.min.js")
		if err := os.WriteFile(outFile1, minified1, 0o644); err != nil {
			fmt.Printf("Failed to write tdewolff output: %v\n", err)
		} else {
			results["tdewolff"] = len(minified1)
			fmt.Printf("tdewolff size: %d bytes (%.1f%% reduction)\n", 
				len(minified1), 
				float64(len(src)-len(minified1))/float64(len(src))*100)
		}
	}

	// 2. esbuild
	fmt.Println("\n=== Testing esbuild ===")
	minified2, err := minifyWithEsbuild(originalFile)
	if err != nil {
		fmt.Printf("esbuild failed: %v\n", err)
	} else {
		results["esbuild"] = len(minified2)
		fmt.Printf("esbuild size: %d bytes (%.1f%% reduction)\n", 
			len(minified2), 
			float64(len(src)-len(minified2))/float64(len(src))*100)
	}

	// 3. terser
	fmt.Println("\n=== Testing terser ===")
	minified3, err := minifyWithTerser(originalFile)
	if err != nil {
		fmt.Printf("terser failed: %v\n", err)
	} else {
		results["terser"] = len(minified3)
		fmt.Printf("terser size: %d bytes (%.1f%% reduction)\n", 
			len(minified3), 
			float64(len(src)-len(minified3))/float64(len(src))*100)
	}

	// Find the best result and copy it as the main minified file
	fmt.Println("\n=== Comparison Results ===")
	bestMethod := ""
	bestSize := len(src)
	
	for method, size := range results {
		reduction := float64(len(src)-size)/float64(len(src))*100
		fmt.Printf("%s: %d bytes (%.1f%% reduction)\n", method, size, reduction)
		if size < bestSize {
			bestSize = size
			bestMethod = method
		}
	}

	if bestMethod != "" {
		fmt.Printf("\nBest method: %s\n", bestMethod)
		
		// Copy the best result as the main minified file
		srcFile := filepath.Join(baseDir, fmt.Sprintf("lodash.%s.min.js", bestMethod))
		dstFile := filepath.Join(baseDir, "lodash.min.js")
		
		data, err := os.ReadFile(srcFile)
		if err == nil {
			os.WriteFile(dstFile, data, 0o644)
			fmt.Printf("Copied %s result to lodash.min.js\n", bestMethod)
		}
	}

	// Clean up original file
	os.Remove(originalFile)
}

func downloadAndExtractLodash() ([]byte, error) {
	resp, err := http.Get(tgzURL)
	if err != nil {
		return nil, fmt.Errorf("failed to download lodash: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("failed to download lodash: HTTP %d", resp.StatusCode)
	}

	gzr, err := gzip.NewReader(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to create gzip reader: %w", err)
	}
	defer gzr.Close()
	
	tr := tar.NewReader(gzr)

	for {
		hdr, err := tr.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, fmt.Errorf("failed to read tar entry: %w", err)
		}
		
		if filepath.Base(hdr.Name) == "lodash.js" && filepath.Dir(hdr.Name) == "package" {
			return io.ReadAll(tr)
		}
	}
	
	return nil, fmt.Errorf("lodash.js not found in tarball")
}

func minifyWithTdewolff(src []byte) ([]byte, error) {
	m := minify.New()
	m.AddFunc("application/javascript", js.Minify)
	return m.Bytes("application/javascript", src)
}

func minifyWithEsbuild(inputFile string) ([]byte, error) {
	outputFile := filepath.Join(baseDir, "lodash.esbuild.min.js")
	
	cmd := exec.Command("esbuild", inputFile, 
		"--minify", 
		"--target=es5",
		"--outfile="+outputFile)
	
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("esbuild command failed: %w", err)
	}
	
	return os.ReadFile(outputFile)
}

func minifyWithTerser(inputFile string) ([]byte, error) {
	outputFile := filepath.Join(baseDir, "lodash.terser.min.js")
	
	cmd := exec.Command("terser", inputFile, 
		"--compress", 
		"--mangle",
		"--output", outputFile)
	
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("terser command failed: %w", err)
	}
	
	return os.ReadFile(outputFile)
}

