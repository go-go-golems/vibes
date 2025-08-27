package detector

import (
	"fmt"
	"path/filepath"
	"strings"
	"text/template"

	"github.com/user/git-precommit-guard/pkg/config"
)

// MimeDetector detects files based on MIME type restrictions
type MimeDetector struct{}

// NewMimeDetector creates a new MIME detector
func NewMimeDetector() *MimeDetector {
	return &MimeDetector{}
}

// Name returns the detector name
func (d *MimeDetector) Name() string {
	return "mime_detection"
}

// Check performs MIME type detection on a file
func (d *MimeDetector) Check(fileInfo *FileInfo, cfg *config.Config) (*DetectionResult, error) {
	rule := cfg.Rules.MimeDetection
	if !rule.Enabled {
		return nil, nil
	}

	// Apply directory-specific overrides
	ruleConfig := cfg.ApplyMimeOverride(fileInfo.Path)

	mimeType := fileInfo.MimeType
	if mimeType == "" {
		// If MIME type is not available, try to detect from file extension
		mimeType = d.detectMimeFromExtension(fileInfo.Path)
	}

	// Check if MIME type is explicitly allowed
	if d.isAllowedType(mimeType, ruleConfig.AllowedTypes) {
		return &DetectionResult{
			FilePath: fileInfo.Path,
			RuleName: d.Name(),
			Severity: rule.Severity,
			Passed:   true,
			Message:  fmt.Sprintf("File MIME type (%s) is allowed", mimeType),
			Details: map[string]string{
				"mime_type": mimeType,
				"status":    "allowed",
			},
		}, nil
	}

	// Check if MIME type is blocked
	if d.isBlockedType(mimeType, ruleConfig.BlockedTypes) {
		message := d.parseMessageTemplate(ruleConfig.Message, fileInfo, mimeType)
		return &DetectionResult{
			FilePath: fileInfo.Path,
			RuleName: d.Name(),
			Severity: rule.Severity,
			Passed:   false,
			Message:  message,
			Details: map[string]string{
				"mime_type": mimeType,
				"status":    "blocked",
			},
		}, nil
	}

	// MIME type is neither explicitly allowed nor blocked
	return &DetectionResult{
		FilePath: fileInfo.Path,
		RuleName: d.Name(),
		Severity: rule.Severity,
		Passed:   true,
		Message:  fmt.Sprintf("File MIME type (%s) is not restricted", mimeType),
		Details: map[string]string{
			"mime_type": mimeType,
			"status":    "neutral",
		},
	}, nil
}

// isAllowedType checks if a MIME type matches any allowed patterns
func (d *MimeDetector) isAllowedType(mimeType string, allowedTypes []string) bool {
	for _, allowedType := range allowedTypes {
		if d.matchesMimePattern(mimeType, allowedType) {
			return true
		}
	}
	return false
}

// isBlockedType checks if a MIME type matches any blocked patterns
func (d *MimeDetector) isBlockedType(mimeType string, blockedTypes []string) bool {
	for _, blockedType := range blockedTypes {
		if d.matchesMimePattern(mimeType, blockedType) {
			return true
		}
	}
	return false
}

// matchesMimePattern checks if a MIME type matches a pattern (supports wildcards)
func (d *MimeDetector) matchesMimePattern(mimeType, pattern string) bool {
	// Exact match
	if mimeType == pattern {
		return true
	}

	// Wildcard pattern (e.g., "text/*")
	if strings.HasSuffix(pattern, "/*") {
		prefix := strings.TrimSuffix(pattern, "/*")
		return strings.HasPrefix(mimeType, prefix+"/")
	}

	return false
}

// detectMimeFromExtension attempts to detect MIME type from file extension
func (d *MimeDetector) detectMimeFromExtension(filePath string) string {
	ext := strings.ToLower(filepath.Ext(filePath))
	
	// Common MIME type mappings
	mimeMap := map[string]string{
		".txt":  "text/plain",
		".md":   "text/markdown",
		".json": "application/json",
		".xml":  "application/xml",
		".yaml": "application/yaml",
		".yml":  "application/yaml",
		".html": "text/html",
		".css":  "text/css",
		".js":   "application/javascript",
		".py":   "text/x-python",
		".go":   "text/x-go",
		".java": "text/x-java-source",
		".c":    "text/x-c",
		".cpp":  "text/x-c++",
		".h":    "text/x-c",
		".hpp":  "text/x-c++",
		".sh":   "application/x-sh",
		".exe":  "application/x-msdownload",
		".dll":  "application/x-msdownload",
		".so":   "application/x-sharedlib",
		".jar":  "application/java-archive",
		".zip":  "application/zip",
		".tar":  "application/x-tar",
		".gz":   "application/gzip",
		".pdf":  "application/pdf",
		".png":  "image/png",
		".jpg":  "image/jpeg",
		".jpeg": "image/jpeg",
		".gif":  "image/gif",
		".svg":  "image/svg+xml",
	}

	if mimeType, exists := mimeMap[ext]; exists {
		return mimeType
	}

	return "application/octet-stream" // Default for unknown types
}

// parseMessageTemplate parses the message template with MIME type information
func (d *MimeDetector) parseMessageTemplate(messageTemplate string, fileInfo *FileInfo, mimeType string) string {
	tmpl, err := template.New("message").Parse(messageTemplate)
	if err != nil {
		return messageTemplate // Return original if parsing fails
	}

	data := struct {
		File     string
		MimeType string
	}{
		File:     fileInfo.Path,
		MimeType: mimeType,
	}

	var result strings.Builder
	if err := tmpl.Execute(&result, data); err != nil {
		return messageTemplate // Return original if execution fails
	}

	return result.String()
}

