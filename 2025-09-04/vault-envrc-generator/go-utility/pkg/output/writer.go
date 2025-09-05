package output

import (
    "bytes"
    "encoding/json"
    "fmt"
    "os"
    "sort"
    "sync"

    "github.com/rs/zerolog/log"
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
    SortKeys bool
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

    log.Debug().Str("path", path).Msg("acquiring output lock")
    unlock := lockForPath(path)
    log.Debug().Str("path", path).Msg("acquired output lock")
    defer unlock()

	// Determine effective mode: JSON always merges
	effectiveMode := opts.Mode
    if opts.Format == "json" {
        effectiveMode = OutputModeMerge
    }

    log.Debug().Str("path", path).Str("mode", string(effectiveMode)).Str("format", opts.Format).Int("size", len(content)).Msg("write start")
	switch effectiveMode {
	case OutputModeOverwrite:
		err := os.WriteFile(path, content, 0644)
        log.Debug().Str("path", path).Int("bytes", len(content)).Err(err).Msg("overwrite written")
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
            log.Debug().Str("path", path).Int("bytes", len(content)).Msg("append yaml-doc")
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
        log.Debug().Str("path", path).Int("bytes", len(content)).Msg("append written")
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
            if opts.SortKeys {
                keys := make([]string, 0, len(existing))
                for k := range existing { keys = append(keys, k) }
                sort.Strings(keys)
                type omap struct{ keys []string; m map[string]interface{} }
                // inline orderedMap implementation to avoid import cycles
                var o = omap{keys: keys, m: existing}
                // custom marshal
                var bld bytes.Buffer
                bld.WriteByte('{')
                for i, k := range o.keys {
                    kb, _ := json.Marshal(k)
                    vb, err := json.Marshal(o.m[k]); if err != nil { return fmt.Errorf("failed to marshal ordered json value: %w", err) }
                    if i == 0 { bld.WriteByte('\n') } else { bld.WriteByte(','); bld.WriteByte('\n') }
                    bld.WriteString("  ")
                    bld.Write(kb)
                    bld.WriteString(": ")
                    bld.Write(vb)
                }
                if len(o.keys) > 0 { bld.WriteByte('\n') }
                bld.WriteByte('}')
                if err := os.WriteFile(path, bld.Bytes(), 0644); err != nil {
                    return err
                }
            log.Debug().Str("path", path).Int("bytes", bld.Len()).Msg("merge json ordered written")
            return nil
        } else {
            buf, err := json.MarshalIndent(existing, "", "  ")
            if err != nil { return fmt.Errorf("failed to marshal merged JSON: %w", err) }
            if err := os.WriteFile(path, buf, 0644); err != nil { return err }
            log.Debug().Str("path", path).Int("bytes", len(buf)).Msg("merge json written")
            return nil
        }
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
            if opts.SortKeys {
                keys := make([]string, 0, len(existing))
                for k := range existing { keys = append(keys, k) }
                sort.Strings(keys)
                // Build ordered YAML node
                node := &yaml.Node{Kind: yaml.MappingNode}
                for _, k := range keys {
                    keyNode := &yaml.Node{Kind: yaml.ScalarNode, Tag: "!!str", Value: k}
                    var valueDoc yaml.Node
                    b, err := yaml.Marshal(existing[k])
                    if err != nil { return fmt.Errorf("failed to marshal yaml value: %w", err) }
                    if err := yaml.Unmarshal(b, &valueDoc); err != nil { return fmt.Errorf("failed to unmarshal yaml value to node: %w", err) }
                    var valueNode *yaml.Node
                    if len(valueDoc.Content) == 0 { valueNode = &yaml.Node{Kind: yaml.ScalarNode, Tag: "!!null", Value: "~"} } else { valueNode = valueDoc.Content[0] }
                    node.Content = append(node.Content, keyNode, valueNode)
                }
                buf, err := yaml.Marshal(node)
                if err != nil { return fmt.Errorf("failed to marshal ordered YAML: %w", err) }
                if err := os.WriteFile(path, buf, 0644); err != nil { return err }
                log.Debug().Str("path", path).Int("bytes", len(buf)).Msg("merge yaml ordered written")
                return nil
            } else {
                buf, err := yaml.Marshal(existing)
                if err != nil { return fmt.Errorf("failed to marshal merged YAML: %w", err) }
                if err := os.WriteFile(path, buf, 0644); err != nil { return err }
                log.Debug().Str("path", path).Int("bytes", len(buf)).Msg("merge yaml written")
                return nil
            }
		default:
			f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644)
			if err != nil {
				return fmt.Errorf("failed to open output file %s: %w", path, err)
			}
			defer f.Close()
			if _, err := f.Write(content); err != nil {
				return fmt.Errorf("failed to append to %s: %w", path, err)
			}
            log.Debug().Str("path", path).Int("bytes", len(content)).Msg("merge default append")
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
