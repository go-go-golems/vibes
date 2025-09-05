package vault

import (
	"bytes"
	"strings"
	"text/template"
)

// RenderTemplateString renders s using Go templates with TemplateContext
func RenderTemplateString(s string, tctx TemplateContext) (string, error) {
	if !strings.Contains(s, "{{") {
		return s, nil
	}
	tmpl, err := template.New("path").Option("missingkey=error").Parse(s)
	if err != nil {
		return "", err
	}
	var buf bytes.Buffer
	if err := tmpl.Execute(&buf, tctx); err != nil {
		return "", err
	}
	return buf.String(), nil
}
