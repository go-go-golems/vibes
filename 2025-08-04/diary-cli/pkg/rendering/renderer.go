package rendering

import (
	"bytes"
	"fmt"
	"strings"
	"text/template"

	"diary-cli/pkg/config"
)

// Built-in template strings
var builtIns = map[string]string{
	"default.md.tmpl": defaultTemplate,
	"markdown.md.tmpl": markdownTemplate,
	"task.md.tmpl": taskTemplate,
}

// Default template strings
const defaultTemplate = `## {{ title (string .Type) }}: {{ .Content }}
{{ if .SubtitleSlug }}### {{ .SubtitleSlug }}{{ end }}
{{ if .Title }}{{ .Content }}
{{ end }}
*Added: {{ .Date.Format "2006-01-02 15:04" }}*
`

const markdownTemplate = `## {{ title (string .Type) }}: {{ .Content }}
**Type:** {{ .Type }}  
**Date:** {{ .Date.Format "2006-01-02 15:04" }}  
{{ if .Tags }}**Tags:** {{ join .Tags ", " }}  {{ end }}
---
{{ .Content }}
`

const taskTemplate = `- [ ] **{{ upper (string .Type) }}**: {{ .Content }} #toProcess #{{ .Type }}
  - Added: {{ .Date.Format "2006-01-02 15:04" }}
`

// Renderer handles template rendering for diary entries
type Renderer struct {
	cfg       *config.Config
	templates *template.Template
}

// NewRenderer creates a new renderer with built-in templates and user overrides
func NewRenderer(cfg *config.Config) (*Renderer, error) {
	r := &Renderer{cfg: cfg}
	
	// 1. Parse built-ins
	base := template.New("base").Funcs(funcMap())
	for name, src := range builtIns {
		_, err := base.New(name).Parse(src)
		if err != nil {
			return nil, fmt.Errorf("failed to parse template %s: %w", name, err)
		}
	}
	
	// 2. User-supplied overrides from config
	if cfg.Rendering != nil {
		for name, src := range cfg.Rendering.Templates {
			_, err := base.New(name).Parse(src)
			if err != nil {
				return nil, fmt.Errorf("failed to parse user template %s: %w", name, err)
			}
		}
	}
	
	r.templates = base
	return r, nil
}

// Render renders a template with the given data
func (r *Renderer) Render(tmpl string, data any) (string, error) {
	var b bytes.Buffer
	err := r.templates.ExecuteTemplate(&b, tmpl, data)
	return b.String(), err
}

// funcMap provides template functions
func funcMap() template.FuncMap {
	return template.FuncMap{
		"title": func(s string) string {
			return strings.Title(s)
		},
		"upper": func(s string) string {
			return strings.ToUpper(s)
		},
		"join": func(slice []string, sep string) string {
			return strings.Join(slice, sep)
		},
		"string": func(v interface{}) string {
			return fmt.Sprint(v)
		},
	}
} 