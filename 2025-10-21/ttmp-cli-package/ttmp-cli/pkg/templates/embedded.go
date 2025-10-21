package templates

import (
	"embed"
	"fmt"
	"text/template"
)

//go:embed *.tmpl
var templatesFS embed.FS

// GetTemplate loads and parses a template by name
func GetTemplate(name string) (*template.Template, error) {
	content, err := templatesFS.ReadFile(name + ".tmpl")
	if err != nil {
		return nil, fmt.Errorf("template not found: %s", name)
	}

	tmpl, err := template.New(name).Parse(string(content))
	if err != nil {
		return nil, fmt.Errorf("failed to parse template: %w", err)
	}

	return tmpl, nil
}

// ExecuteTemplate executes a template with the given data
func ExecuteTemplate(name string, data interface{}) (string, error) {
	tmpl, err := GetTemplate(name)
	if err != nil {
		return "", err
	}

	var buf []byte
	writer := &bufferWriter{buf: &buf}
	if err := tmpl.Execute(writer, data); err != nil {
		return "", fmt.Errorf("failed to execute template: %w", err)
	}

	return string(buf), nil
}

type bufferWriter struct {
	buf *[]byte
}

func (w *bufferWriter) Write(p []byte) (n int, err error) {
	*w.buf = append(*w.buf, p...)
	return len(p), nil
}

