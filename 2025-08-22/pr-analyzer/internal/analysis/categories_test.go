package analysis

import (
	"reflect"
	"sort"
	"testing"
)

func normalize(xs []string) []string {
	out := append([]string(nil), xs...)
	sort.Strings(out)
	return out
}

func TestCategoryMatcher_CategorizeFile(t *testing.T) {
	cm := NewCategoryMatcher()
	cm.AddCategory("frontend", []string{"frontend/**", "web/**", "*.css", "*.jsx", "*.tsx"})
	cm.AddCategory("backend", []string{"backend/**", "server/**", "*.go"})
	cm.AddCategory("docs", []string{"docs/**", "*.md"})
	cm.AddCategory("config", []string{"config/**", "*.yaml", "*.yml"})

	cm.AddExcludePattern("vendor/**")
	cm.AddExcludePattern("*.lock")

	tests := []struct {
		name     string
		file     string
		expect   []string
	}{
		{
			name:   "frontend directory match",
			file:   "frontend/src/app.jsx",
			expect: []string{"frontend"},
		},
		{
			name:   "frontend by extension (css)",
			file:   "styles/main.css",
			expect: []string{"frontend"},
		},
		{
			name:   "backend directory match",
			file:   "backend/api/handler.go",
			expect: []string{"backend"},
		},
		{
			name:   "docs by basename pattern",
			file:   "README.md",
			expect: []string{"docs"},
		},
		{
			name:   "docs in nested path by basename pattern",
			file:   "docs/guides/intro.md",
			expect: []string{"docs"},
		},
		{
			name:   "multiple categories (backend dir and yaml config)",
			file:   "backend/api/openapi.yaml",
			expect: []string{"backend", "config"},
		},
		{
			name:   "uncategorized when no match",
			file:   "misc/build.gradle",
			expect: []string{"uncategorized"},
		},
		{
			name:   "exclude vendor path",
			file:   "vendor/github.com/pkg/errors/err.go",
			expect: []string{},
		},
		{
			name:   "exclude by extension",
			file:   "package.lock",
			expect: []string{},
		},
		{
			name:   "path normalization on windows-style slashes",
			file:   "frontend\\App.tsx",
			expect: []string{"frontend"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := cm.CategorizeFile(tt.file)
			if !reflect.DeepEqual(normalize(got), normalize(tt.expect)) {
				t.Fatalf("expected %v, got %v", tt.expect, got)
			}
		})
	}
}
