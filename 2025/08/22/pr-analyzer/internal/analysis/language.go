package analysis

import (
	"path/filepath"
	"strings"
)

// LanguageDetector handles programming language detection
type LanguageDetector struct {
	extensionMap map[string]string
}

// NewLanguageDetector creates a new language detector with default mappings
func NewLanguageDetector() *LanguageDetector {
	return &LanguageDetector{
		extensionMap: getDefaultLanguageMap(),
	}
}

// DetectLanguage detects the programming language of a file based on its extension
func (ld *LanguageDetector) DetectLanguage(filename string) string {
	ext := strings.ToLower(filepath.Ext(filename))
	if lang, exists := ld.extensionMap[ext]; exists {
		return lang
	}
	
	// Check for special cases without extensions
	base := strings.ToLower(filepath.Base(filename))
	switch base {
	case "dockerfile":
		return "Docker"
	case "makefile":
		return "Makefile"
	case "readme", "readme.md", "readme.txt":
		return "Documentation"
	}
	
	return "Other"
}

// AddLanguageMapping adds a custom language mapping
func (ld *LanguageDetector) AddLanguageMapping(extension, language string) {
	ld.extensionMap[strings.ToLower(extension)] = language
}

// getDefaultLanguageMap returns the default file extension to language mapping
func getDefaultLanguageMap() map[string]string {
	return map[string]string{
		// Go
		".go":   "Go",
		".mod":  "Go",
		".sum":  "Go",
		
		// JavaScript/TypeScript
		".js":   "JavaScript",
		".jsx":  "JavaScript",
		".ts":   "TypeScript",
		".tsx":  "TypeScript",
		".mjs":  "JavaScript",
		".cjs":  "JavaScript",
		
		// Python
		".py":   "Python",
		".pyx":  "Python",
		".pyi":  "Python",
		".pyw":  "Python",
		
		// Java
		".java": "Java",
		".class": "Java",
		".jar":  "Java",
		
		// C/C++
		".c":    "C",
		".h":    "C",
		".cpp":  "C++",
		".cxx":  "C++",
		".cc":   "C++",
		".hpp":  "C++",
		".hxx":  "C++",
		
		// C#
		".cs":   "C#",
		".csx":  "C#",
		
		// Web
		".html": "HTML",
		".htm":  "HTML",
		".css":  "CSS",
		".scss": "SCSS",
		".sass": "SASS",
		".less": "LESS",
		
		// Shell
		".sh":   "Shell",
		".bash": "Shell",
		".zsh":  "Shell",
		".fish": "Shell",
		
		// Ruby
		".rb":   "Ruby",
		".rbw":  "Ruby",
		
		// PHP
		".php":  "PHP",
		".phtml": "PHP",
		
		// Rust
		".rs":   "Rust",
		
		// Swift
		".swift": "Swift",
		
		// Kotlin
		".kt":   "Kotlin",
		".kts":  "Kotlin",
		
		// Scala
		".scala": "Scala",
		".sc":   "Scala",
		
		// Configuration
		".json": "JSON",
		".yaml": "YAML",
		".yml":  "YAML",
		".toml": "TOML",
		".xml":  "XML",
		".ini":  "INI",
		".conf": "Config",
		".cfg":  "Config",
		
		// Documentation
		".md":   "Markdown",
		".rst":  "reStructuredText",
		".txt":  "Text",
		".doc":  "Document",
		".docx": "Document",
		".pdf":  "Document",
		
		// SQL
		".sql":  "SQL",
		
		// Docker
		".dockerfile": "Docker",
		
		// Terraform
		".tf":   "Terraform",
		".tfvars": "Terraform",
		
		// Other
		".gitignore": "Git",
		".gitattributes": "Git",
	}
}

