package output

import (
	"encoding/json"
	"fmt"
	"os"
	"sync"

	"gopkg.in/yaml.v3"
)

type OutputMode string

const (
	OutputModeOverwrite OutputMode = "overwrite"
	OutputModeAppend    OutputMode = "append"
	OutputModeMerge     OutputMode = "merge"
)

type WriteOptions struct {
	Mode    OutputMode
	Format  string // envrc|json|yaml
	Verbose bool
}

var outputLocks = struct {
	mu    sync.Mutex
	locks map[string]*sync.Mutex
}{locks: make(map[string]*sync.Mutex)}

func lockForPath(path string) func() {
	outputLocks.mu.Lock()
	m, ok := outputLocks.locks[path]
	if !ok {
		m = &sync.Mutex{}
		outputLocks.locks[path] = m
	}
	outputLocks.mu.Unlock()
	m.Lock()
	return func() { m.Unlock() }
}

// Write writes content to path according to options, performing merges for json/yaml
func Write(path string, content []byte, opts WriteOptions) error {
	// stdout special-case
	if path == "-" {
		fmt.Print(string(content))
		return nil
	}

	// Ensure output directory exists
	if dir := dirOf(path); dir != "" && dir != "." {
		if err := os.MkdirAll(dir, 0755); err != nil {
			return fmt.Errorf("failed to create output directory %s: %w", dir, err)
		}
	}

	if opts.Verbose {
		fmt.Fprintf(os.Stderr, "[output] acquiring lock for %s\n", path)
	}
	unlock := lockForPath(path)
	if opts.Verbose {
		fmt.Fprintf(os.Stderr, "[output] acquired lock for %s\n", path)
	}
	defer unlock()

	// Determine effective mode: JSON always merges
	effectiveMode := opts.Mode
	if opts.Format == "json" {
		effectiveMode = OutputModeMerge
	}

	if opts.Verbose {
		fmt.Fprintf(os.Stderr, "[output] write start path=%s mode=%s format=%s size=%d\n", path, effectiveMode, opts.Format, len(content))
	}
	switch effectiveMode {
	case OutputModeOverwrite:
		err := os.WriteFile(path, content, 0644)
		if opts.Verbose {
			fmt.Fprintf(os.Stderr, "[output] overwrite wrote %d bytes to %s (err=%v)\n", len(content), path, err)
		}
		return err
	case OutputModeAppend:
		// YAML special-case: append as a new document
		if opts.Format == "yaml" {
			var prefix []byte
			if fi, err := os.Stat(path); err == nil && fi.Size() > 0 {
				prefix = []byte("\n---\n")
			}
			f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644)
			if err != nil {
				return fmt.Errorf("failed to open output file %s: %w", path, err)
			}
			defer f.Close()
			if len(prefix) > 0 {
				if _, err := f.Write(prefix); err != nil {
					return fmt.Errorf("failed to write YAML doc separator to %s: %w", path, err)
				}
			}
			if _, err := f.Write(content); err != nil {
				return fmt.Errorf("failed to append to %s: %w", path, err)
			}
			if opts.Verbose {
				fmt.Fprintf(os.Stderr, "[output] append(yaml-doc) wrote %d bytes to %s\n", len(content), path)
			}
			return nil
		}
		// Default append behavior for other formats
		f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644)
		if err != nil {
			return fmt.Errorf("failed to open output file %s: %w", path, err)
		}
		defer f.Close()
		if _, err := f.Write(content); err != nil {
			return fmt.Errorf("failed to append to %s: %w", path, err)
		}
		if opts.Verbose {
			fmt.Fprintf(os.Stderr, "[output] append wrote %d bytes to %s\n", len(content), path)
		}
		return nil
	case OutputModeMerge:
		switch opts.Format {
		case "json":
			var existing map[string]interface{}
			if b, err := os.ReadFile(path); err == nil && len(b) > 0 {
				_ = json.Unmarshal(b, &existing)
			}
			if existing == nil {
				existing = map[string]interface{}{}
			}
			var next map[string]interface{}
			if err := json.Unmarshal(content, &next); err != nil {
				return fmt.Errorf("failed to parse generated JSON for merge: %w", err)
			}
			for k, v := range next {
				existing[k] = v
			}
			buf, err := json.MarshalIndent(existing, "", "  ")
			if err != nil {
				return fmt.Errorf("failed to marshal merged JSON: %w", err)
			}
			err = os.WriteFile(path, buf, 0644)
			if opts.Verbose {
				fmt.Fprintf(os.Stderr, "[output] merge(json) wrote %d bytes to %s (err=%v)\n", len(buf), path, err)
			}
			return err
		case "yaml":
			var existing map[string]interface{}
			if b, err := os.ReadFile(path); err == nil && len(b) > 0 {
				_ = yaml.Unmarshal(b, &existing)
			}
			if existing == nil {
				existing = map[string]interface{}{}
			}
			var next map[string]interface{}
			if err := yaml.Unmarshal(content, &next); err != nil {
				return fmt.Errorf("failed to parse generated YAML for merge: %w", err)
			}
			for k, v := range next {
				existing[k] = v
			}
			buf, err := yaml.Marshal(existing)
			if err != nil {
				return fmt.Errorf("failed to marshal merged YAML: %w", err)
			}
			err = os.WriteFile(path, buf, 0644)
			if opts.Verbose {
				fmt.Fprintf(os.Stderr, "[output] merge(yaml) wrote %d bytes to %s (err=%v)\n", len(buf), path, err)
			}
			return err
		default:
			f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644)
			if err != nil {
				return fmt.Errorf("failed to open output file %s: %w", path, err)
			}
			defer f.Close()
			if _, err := f.Write(content); err != nil {
				return fmt.Errorf("failed to append to %s: %w", path, err)
			}
			if opts.Verbose {
				fmt.Fprintf(os.Stderr, "[output] merge(default-append) wrote %d bytes to %s\n", len(content), path)
			}
			return nil
		}
	default:
		return fmt.Errorf("unknown output mode: %s", effectiveMode)
	}
}

func dirOf(path string) string {
	// minimal implementation to avoid importing filepath just for Dir
	// find last '/'
	last := -1
	for i := len(path) - 1; i >= 0; i-- {
		if path[i] == '/' {
			last = i
			break
		}
	}
	if last <= 0 {
		return "."
	}
	return path[:last]
}
