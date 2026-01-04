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
	"path/filepath"

	"github.com/tdewolff/minify/v2"
	"github.com/tdewolff/minify/v2/js"
)

const (
	version = "4.17.21"
	tgzURL  = "https://registry.npmjs.org/lodash/-/lodash-" + version + ".tgz"
	outFile = "../js/lodash.min.js" // relative to this file
)

func main() {
	fmt.Printf("Downloading lodash %s from npm...\n", version)
	
	// 1. Download the official tarball once per generate.
	resp, err := http.Get(tgzURL)
	if err != nil {
		panic(fmt.Errorf("failed to download lodash: %w", err))
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		panic(fmt.Errorf("failed to download lodash: HTTP %d", resp.StatusCode))
	}

	fmt.Println("Extracting lodash.js from tarball...")
	
	// 2. Find package/lodash.js inside the .tgz
	gzr, err := gzip.NewReader(resp.Body)
	if err != nil {
		panic(fmt.Errorf("failed to create gzip reader: %w", err))
	}
	defer gzr.Close()
	
	tr := tar.NewReader(gzr)

	var src []byte
	for {
		hdr, err := tr.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			panic(fmt.Errorf("failed to read tar entry: %w", err))
		}
		
		// Look for lodash.js in the package directory
		if filepath.Base(hdr.Name) == "lodash.js" && filepath.Dir(hdr.Name) == "package" {
			fmt.Printf("Found lodash.js in tarball (size: %d bytes)\n", hdr.Size)
			src, err = io.ReadAll(tr)
			if err != nil {
				panic(fmt.Errorf("failed to read lodash.js: %w", err))
			}
			break
		}
	}
	
	if len(src) == 0 {
		panic("lodash.js not found in tarball")
	}

	fmt.Printf("Original size: %d bytes\n", len(src))
	fmt.Println("Minifying with tdewolff/minify...")
	
	// 3. Minify with tdewolff/minify
	m := minify.New()
	m.AddFunc("application/javascript", js.Minify)

	minified, err := m.Bytes("application/javascript", src)
	if err != nil {
		panic(fmt.Errorf("failed to minify lodash: %w", err))
	}

	fmt.Printf("Minified size: %d bytes (%.1f%% reduction)\n", 
		len(minified), 
		float64(len(src)-len(minified))/float64(len(src))*100)

	// 4. Write output
	if err := os.MkdirAll(filepath.Dir(outFile), 0o755); err != nil {
		panic(fmt.Errorf("failed to create output directory: %w", err))
	}
	
	if err := os.WriteFile(outFile, minified, 0o644); err != nil {
		panic(fmt.Errorf("failed to write minified file: %w", err))
	}

	fmt.Printf("Successfully generated %s\n", outFile)
}

