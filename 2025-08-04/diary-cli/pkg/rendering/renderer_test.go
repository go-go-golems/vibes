package rendering

import (
	"bytes"
	"testing"
	"time"

	"diary-cli/pkg/config"
	"diary-cli/pkg/types"
)

func TestNewRenderer_DefaultTemplates(t *testing.T) {
	cfg := config.DefaultConfig()
	renderer, err := NewRenderer(cfg)
	if err != nil {
		tFatalf(t, "NewRenderer() error = %v, wantErr %v", err, false)
	}

	if renderer.templates.Lookup("default.md.tmpl") == nil {
		t.Errorf("Default template 'default.md.tmpl' not found")
	}
	if renderer.templates.Lookup("markdown.md.tmpl") == nil {
		t.Errorf("Default template 'markdown.md.tmpl' not found")
	}
	if renderer.templates.Lookup("task.md.tmpl") == nil {
		t.Errorf("Default template 'task.md.tmpl' not found")
	}
}

func TestNewRenderer_WithUserOverrides(t *testing.T) {
	cfg := config.DefaultConfig()
	userTemplate := `User override for {{ .Type }}`
	cfg.Rendering = &config.RenderingConfig{
		Templates: map[string]string{
			"default.md.tmpl": userTemplate,
		},
	}

	renderer, err := NewRenderer(cfg)
	if err != nil {
		tFatalf(t, "NewRenderer() with overrides error = %v, wantErr %v", err, false)
	}

	entry := &types.DiaryEntry{Type: "til", Content: "test"}
	output, err := renderer.Render("default.md.tmpl", entry)
	if err != nil {
		tFatalf(t, "Render() with override error = %v", err)
	}

	expected := `User override for til`
	if output != expected {
		t.Errorf("Render() with override got = %q, want %q", output, expected)
	}
}

func TestRenderer_Render(t *testing.T) {
	cfg := config.DefaultConfig()
	renderer, _ := NewRenderer(cfg)

	testDate := time.Date(2025, 1, 15, 10, 30, 0, 0, time.UTC)
	entry := &types.DiaryEntry{
		Type:    types.EntryTypeTIL,
		Title:   "Test Title",
		Content: "This is a test.",
		Date:    testDate,
		Tags:    []string{"go", "testing"},
	}

	tests := []struct {
		name         string
		templateName string
		entry        *types.DiaryEntry
		expected     string
	}{
		{
			name:         "Default Template",
			templateName: "default.md.tmpl",
			entry:        entry,
			expected:     "## Til: This is a test.\n\nThis is a test.\n\n*Added: 2025-01-15 10:30*\n",
		},
		{
			name:         "Markdown Template",
			templateName: "markdown.md.tmpl",
			entry:        entry,
			expected:     "## Til: This is a test.\n**Type:** til  \n**Date:** 2025-01-15 10:30  \n**Tags:** go, testing  \n---\nThis is a test.\n",
		},
		{
			name:         "Task Template",
			templateName: "task.md.tmpl",
			entry:        entry,
			expected:     "- [ ] **TIL**: This is a test. #toProcess #til\n  - Added: 2025-01-15 10:30\n",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var b bytes.Buffer
			err := renderer.templates.ExecuteTemplate(&b, tt.templateName, tt.entry)
			if err != nil {
				tFatalf(t, "ExecuteTemplate() error = %v", err)
			}
			if got := b.String(); got != tt.expected {
				t.Errorf("Render() got = %q, want %q", got, tt.expected)
			}
		})
	}
}

func tFatalf(t *testing.T, format string, args ...interface{}) {
	t.Helper()
	t.Fatalf(format, args...)
}
