package analysis

import (
	"path/filepath"
	"strings"
)

// CategoryMatcher handles file categorization using glob patterns
type CategoryMatcher struct {
	categories map[string][]string // category name -> glob patterns
	excludes   []string            // exclude patterns
}

// NewCategoryMatcher creates a new category matcher
func NewCategoryMatcher() *CategoryMatcher {
	return &CategoryMatcher{
		categories: make(map[string][]string),
		excludes:   []string{},
	}
}

// AddCategory adds a category with its glob patterns
func (cm *CategoryMatcher) AddCategory(name string, patterns []string) {
	cm.categories[name] = patterns
}

// AddExcludePattern adds a pattern to exclude from categorization
func (cm *CategoryMatcher) AddExcludePattern(pattern string) {
	cm.excludes = append(cm.excludes, pattern)
}

// CategorizeFile determines which categories a file belongs to
func (cm *CategoryMatcher) CategorizeFile(filename string) []string {
	// Check if file should be excluded
	for _, exclude := range cm.excludes {
		if matched, _ := filepath.Match(exclude, filename); matched {
			return []string{}
		}
		// Also check if any parent directory matches the exclude pattern
		if cm.matchesPath(filename, exclude) {
			return []string{}
		}
	}
	
	var categories []string
	for category, patterns := range cm.categories {
		for _, pattern := range patterns {
			if cm.matchesPath(filename, pattern) {
				categories = append(categories, category)
				break // Don't add the same category multiple times
			}
		}
	}
	
	// If no categories matched, assign to "uncategorized"
	if len(categories) == 0 {
		categories = append(categories, "uncategorized")
	}
	
	return categories
}

// matchesPath checks if a file path matches a glob pattern
// Supports both simple filename matching and directory path matching
func (cm *CategoryMatcher) matchesPath(filename, pattern string) bool {
	// Direct match
	if matched, _ := filepath.Match(pattern, filename); matched {
		return true
	}
	
	// Check if pattern contains directory separators
	if strings.Contains(pattern, "/") {
		// For patterns like "frontend/**", "backend/api/**"
		if strings.HasSuffix(pattern, "/**") {
			prefix := strings.TrimSuffix(pattern, "/**")
			return strings.HasPrefix(filename, prefix+"/") || filename == prefix
		}
		
		// For patterns like "frontend/*.js"
		if matched, _ := filepath.Match(pattern, filename); matched {
			return true
		}
		
		// Check each component of the path
		dir := filepath.Dir(filename)
		for dir != "." && dir != "/" {
			if matched, _ := filepath.Match(pattern, filepath.Join(dir, filepath.Base(filename))); matched {
				return true
			}
			dir = filepath.Dir(dir)
		}
	}
	
	return false
}

// GetCategories returns all defined categories
func (cm *CategoryMatcher) GetCategories() map[string][]string {
	result := make(map[string][]string)
	for k, v := range cm.categories {
		result[k] = make([]string, len(v))
		copy(result[k], v)
	}
	return result
}

// ParseCategoriesString parses a comma-separated string of category definitions
// Format: "name1:pattern1,pattern2;name2:pattern3,pattern4"
func ParseCategoriesString(categoriesStr string) map[string][]string {
	categories := make(map[string][]string)
	
	if categoriesStr == "" {
		return categories
	}
	
	// Split by semicolon to get each category definition
	categoryDefs := strings.Split(categoriesStr, ";")
	
	for _, categoryDef := range categoryDefs {
		categoryDef = strings.TrimSpace(categoryDef)
		if categoryDef == "" {
			continue
		}
		
		// Split by colon to separate name from patterns
		parts := strings.SplitN(categoryDef, ":", 2)
		if len(parts) != 2 {
			continue
		}
		
		name := strings.TrimSpace(parts[0])
		patternsStr := strings.TrimSpace(parts[1])
		
		// Split patterns by comma
		patterns := strings.Split(patternsStr, ",")
		for i, pattern := range patterns {
			patterns[i] = strings.TrimSpace(pattern)
		}
		
		categories[name] = patterns
	}
	
	return categories
}

// GetDefaultCategories returns a set of common default categories
func GetDefaultCategories() map[string][]string {
	return map[string][]string{
		"frontend": {
			"frontend/**",
			"web/**",
			"ui/**",
			"client/**",
			"*.html",
			"*.css",
			"*.js",
			"*.jsx",
			"*.ts",
			"*.tsx",
			"*.vue",
			"*.svelte",
		},
		"backend": {
			"backend/**",
			"server/**",
			"api/**",
			"services/**",
			"*.go",
			"*.py",
			"*.java",
			"*.rb",
			"*.php",
			"*.rs",
		},
		"database": {
			"database/**",
			"db/**",
			"migrations/**",
			"*.sql",
			"*.db",
		},
		"config": {
			"config/**",
			"configs/**",
			"*.yaml",
			"*.yml",
			"*.json",
			"*.toml",
			"*.ini",
			"*.conf",
			"*.cfg",
		},
		"docs": {
			"docs/**",
			"documentation/**",
			"*.md",
			"*.rst",
			"*.txt",
			"README*",
		},
		"tests": {
			"test/**",
			"tests/**",
			"*_test.*",
			"*Test.*",
			"*.test.*",
		},
		"build": {
			"build/**",
			"scripts/**",
			"Makefile",
			"*.mk",
			"Dockerfile*",
			"docker-compose*",
			"*.sh",
			"*.bat",
			"*.ps1",
		},
	}
}

