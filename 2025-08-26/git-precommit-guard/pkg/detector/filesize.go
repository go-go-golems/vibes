package detector

import (
	"fmt"
	"strings"
	"text/template"

	"github.com/user/git-precommit-guard/pkg/config"
)

// FileSizeDetector detects files that exceed size limits
type FileSizeDetector struct{}

// NewFileSizeDetector creates a new file size detector
func NewFileSizeDetector() *FileSizeDetector {
	return &FileSizeDetector{}
}

// Name returns the detector name
func (d *FileSizeDetector) Name() string {
	return "file_size"
}

// Check performs file size detection on a file
func (d *FileSizeDetector) Check(fileInfo *FileInfo, cfg *config.Config) (*DetectionResult, error) {
	rule := cfg.Rules.FileSize
	if !rule.Enabled {
		return nil, nil
	}

	// Apply directory-specific overrides
	ruleConfig := cfg.ApplyFileSizeOverride(fileInfo.Path)

	fileSizeMB := float64(fileInfo.Size) / (1024 * 1024)
	maxSizeMB := float64(ruleConfig.MaxSizeMB)
	warnSizeMB := float64(ruleConfig.WarnSizeMB)

	// Check if file exceeds maximum size limit
	if fileSizeMB > maxSizeMB {
		message := d.parseMessageTemplate(ruleConfig.Message, fileInfo, fileSizeMB, maxSizeMB)
		return &DetectionResult{
			FilePath: fileInfo.Path,
			RuleName: d.Name(),
			Severity: rule.Severity,
			Passed:   false,
			Message:  message,
			Details: map[string]string{
				"file_size_mb":  fmt.Sprintf("%.2f", fileSizeMB),
				"max_size_mb":   fmt.Sprintf("%.2f", maxSizeMB),
				"file_size_bytes": fmt.Sprintf("%d", fileInfo.Size),
			},
		}, nil
	}

	// Check if file exceeds warning threshold
	if fileSizeMB > warnSizeMB {
		message := fmt.Sprintf("File %s (%.2fMB) exceeds warning threshold (%.2fMB)", 
			fileInfo.Path, fileSizeMB, warnSizeMB)
		return &DetectionResult{
			FilePath: fileInfo.Path,
			RuleName: d.Name(),
			Severity: "warning",
			Passed:   true, // It's a warning, not a failure
			Message:  message,
			Details: map[string]string{
				"file_size_mb":  fmt.Sprintf("%.2f", fileSizeMB),
				"warn_size_mb":  fmt.Sprintf("%.2f", warnSizeMB),
				"file_size_bytes": fmt.Sprintf("%d", fileInfo.Size),
			},
		}, nil
	}

	// File size is within acceptable limits
	return &DetectionResult{
		FilePath: fileInfo.Path,
		RuleName: d.Name(),
		Severity: rule.Severity,
		Passed:   true,
		Message:  fmt.Sprintf("File size (%.2fMB) is within limits", fileSizeMB),
		Details: map[string]string{
			"file_size_mb":  fmt.Sprintf("%.2f", fileSizeMB),
			"max_size_mb":   fmt.Sprintf("%.2f", maxSizeMB),
		},
	}, nil
}

// parseMessageTemplate parses the message template with file size information
func (d *FileSizeDetector) parseMessageTemplate(messageTemplate string, fileInfo *FileInfo, sizeMB, maxSizeMB float64) string {
	tmpl, err := template.New("message").Parse(messageTemplate)
	if err != nil {
		return messageTemplate // Return original if parsing fails
	}

	data := struct {
		File      string
		SizeMB    string
		MaxSizeMB string
	}{
		File:      fileInfo.Path,
		SizeMB:    fmt.Sprintf("%.2f", sizeMB),
		MaxSizeMB: fmt.Sprintf("%.2f", maxSizeMB),
	}

	var result strings.Builder
	if err := tmpl.Execute(&result, data); err != nil {
		return messageTemplate // Return original if execution fails
	}

	return result.String()
}

