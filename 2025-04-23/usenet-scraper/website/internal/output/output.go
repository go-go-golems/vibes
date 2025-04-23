package output

import (
	"encoding/csv"
	"encoding/json"
	"encoding/xml"
	"fmt"
	"os"
	"path/filepath"
	"sync"

	"github.com/usenet-scraper/internal/config"
	"github.com/usenet-scraper/internal/parser"
)

// Writer represents an output writer
type Writer struct {
	config    config.OutputConfig
	file      *os.File
	encoder   interface{}
	buffer    []map[string]interface{}
	mu        sync.Mutex
	csvWriter *csv.Writer
	csvHeader []string
}

// NewWriter creates a new output writer
func NewWriter(cfg config.OutputConfig) (*Writer, error) {
	// Create the output directory if it doesn't exist
	dir := filepath.Dir(cfg.File)
	if err := os.MkdirAll(dir, 0755); err != nil {
		return nil, fmt.Errorf("failed to create output directory: %w", err)
	}

	// Open the output file
	flag := os.O_CREATE | os.O_WRONLY
	if cfg.Append {
		flag |= os.O_APPEND
	} else {
		flag |= os.O_TRUNC
	}

	file, err := os.OpenFile(cfg.File, flag, 0644)
	if err != nil {
		return nil, fmt.Errorf("failed to open output file: %w", err)
	}

	w := &Writer{
		config: cfg,
		file:   file,
		buffer: make([]map[string]interface{}, 0, cfg.BatchSize),
	}

	// Initialize the encoder based on the output format
	switch cfg.Format {
	case "json":
		// For JSON, we'll buffer and write in batches
		if !cfg.Append {
			// Write the opening bracket for a JSON array
			if _, err := file.Write([]byte("[\n")); err != nil {
				file.Close()
				return nil, fmt.Errorf("failed to write JSON header: %w", err)
			}
		}
	case "csv":
		w.csvWriter = csv.NewWriter(file)
		// If fields are specified, use them as the CSV header
		if len(cfg.Fields) > 0 {
			w.csvHeader = cfg.Fields
			if !cfg.Append {
				if err := w.csvWriter.Write(w.csvHeader); err != nil {
					file.Close()
					return nil, fmt.Errorf("failed to write CSV header: %w", err)
				}
				w.csvWriter.Flush()
			}
		}
	case "xml":
		// For XML, write the header
		if !cfg.Append {
			if _, err := file.Write([]byte("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<posts>\n")); err != nil {
				file.Close()
				return nil, fmt.Errorf("failed to write XML header: %w", err)
			}
		}
	case "sqlite":
		// For SQLite, we would initialize a database connection
		// This is a simplified implementation
		return nil, fmt.Errorf("SQLite output format not implemented in this demo")
	default:
		file.Close()
		return nil, fmt.Errorf("unsupported output format: %s", cfg.Format)
	}

	return w, nil
}

// Write writes a post to the output
func (w *Writer) Write(post *parser.Post) error {
	w.mu.Lock()
	defer w.mu.Unlock()

	// Extract the fields to include in the output
	fields := make(map[string]interface{})
	if len(w.config.Fields) > 0 {
		// Include only the specified fields
		for _, field := range w.config.Fields {
			if value, ok := post.Fields[field]; ok {
				fields[field] = value
			}
		}
	} else {
		// Include all fields
		for field, value := range post.Fields {
			fields[field] = value
		}
	}

	// Add to the buffer
	w.buffer = append(w.buffer, fields)

	// Flush if the buffer is full
	if len(w.buffer) >= w.config.BatchSize {
		return w.Flush()
	}

	return nil
}

// Flush flushes the buffer to the output file
func (w *Writer) Flush() error {
	if len(w.buffer) == 0 {
		return nil
	}

	switch w.config.Format {
	case "json":
		return w.flushJSON()
	case "csv":
		return w.flushCSV()
	case "xml":
		return w.flushXML()
	default:
		return fmt.Errorf("unsupported output format: %s", w.config.Format)
	}
}

// flushJSON flushes the buffer to a JSON file
func (w *Writer) flushJSON() error {
	for i, fields := range w.buffer {
		// Convert to JSON
		var data []byte
		var err error
		if w.config.Pretty {
			data, err = json.MarshalIndent(fields, "  ", "  ")
		} else {
			data, err = json.Marshal(fields)
		}
		if err != nil {
			return fmt.Errorf("failed to marshal JSON: %w", err)
		}

		// Write a comma if this is not the first item
		if w.config.Append || i > 0 {
			if _, err := w.file.Write([]byte(",\n")); err != nil {
				return fmt.Errorf("failed to write JSON separator: %w", err)
			}
		}

		// Write the JSON data
		if _, err := w.file.Write(data); err != nil {
			return fmt.Errorf("failed to write JSON data: %w", err)
		}
	}

	// Clear the buffer
	w.buffer = w.buffer[:0]
	return nil
}

// flushCSV flushes the buffer to a CSV file
func (w *Writer) flushCSV() error {
	// If header is not set yet, use the first record's keys
	if len(w.csvHeader) == 0 && len(w.buffer) > 0 {
		for field := range w.buffer[0] {
			w.csvHeader = append(w.csvHeader, field)
		}
		if err := w.csvWriter.Write(w.csvHeader); err != nil {
			return fmt.Errorf("failed to write CSV header: %w", err)
		}
	}

	// Write each record
	for _, fields := range w.buffer {
		record := make([]string, len(w.csvHeader))
		for i, field := range w.csvHeader {
			if value, ok := fields[field]; ok {
				record[i] = fmt.Sprintf("%v", value)
			}
		}
		if err := w.csvWriter.Write(record); err != nil {
			return fmt.Errorf("failed to write CSV record: %w", err)
		}
	}

	// Flush the CSV writer
	w.csvWriter.Flush()
	if err := w.csvWriter.Error(); err != nil {
		return fmt.Errorf("CSV writer error: %w", err)
	}

	// Clear the buffer
	w.buffer = w.buffer[:0]
	return nil
}

// flushXML flushes the buffer to an XML file
func (w *Writer) flushXML() error {
	for _, fields := range w.buffer {
		// Convert to XML
		data, err := xml.MarshalIndent(struct {
			XMLName xml.Name
			Fields  map[string]interface{}
		}{
			XMLName: xml.Name{Local: "post"},
			Fields:  fields,
		}, "  ", "  ")
		if err != nil {
			return fmt.Errorf("failed to marshal XML: %w", err)
		}

		// Write the XML data
		if _, err := w.file.Write(data); err != nil {
			return fmt.Errorf("failed to write XML data: %w", err)
		}
		if _, err := w.file.Write([]byte("\n")); err != nil {
			return fmt.Errorf("failed to write newline: %w", err)
		}
	}

	// Clear the buffer
	w.buffer = w.buffer[:0]
	return nil
}

// Close closes the output writer
func (w *Writer) Close() error {
	// Flush any remaining data
	if err := w.Flush(); err != nil {
		return err
	}

	// Write the closing tags/brackets
	switch w.config.Format {
	case "json":
		if _, err := w.file.Write([]byte("\n]")); err != nil {
			return fmt.Errorf("failed to write JSON footer: %w", err)
		}
	case "xml":
		if _, err := w.file.Write([]byte("</posts>")); err != nil {
			return fmt.Errorf("failed to write XML footer: %w", err)
		}
	}

	// Close the file
	return w.file.Close()
}
