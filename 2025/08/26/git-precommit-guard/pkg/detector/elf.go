package detector

import (
	"encoding/hex"
	"fmt"
	"os/exec"
	"regexp"
	"strings"
	"text/template"

	"github.com/user/git-precommit-guard/pkg/config"
)

// ElfDetector detects ELF binary files
type ElfDetector struct{}

// NewElfDetector creates a new ELF detector
func NewElfDetector() *ElfDetector {
	return &ElfDetector{}
}

// Name returns the detector name
func (d *ElfDetector) Name() string {
	return "elf_detection"
}

// Check performs ELF binary detection on a file
func (d *ElfDetector) Check(fileInfo *FileInfo, cfg *config.Config) (*DetectionResult, error) {
	rule := cfg.Rules.ElfDetection
	if !rule.Enabled {
		return nil, nil
	}

	// Apply directory-specific overrides
	ruleConfig := cfg.ApplyElfOverride(fileInfo.Path)

	// Check ELF magic number
	if d.hasElfMagic(fileInfo.Content, ruleConfig.ElfMagic) {
		return d.createResult(fileInfo, rule, ruleConfig, "ELF magic number detected"), nil
	}

	// Check file command output
	if d.matchesFileCommand(fileInfo.Path, ruleConfig.FilePatterns) {
		return d.createResult(fileInfo, rule, ruleConfig, "ELF file detected by file command"), nil
	}

	// Check MIME type
	if d.matchesMimeType(fileInfo.MimeType, ruleConfig.MimeTypes) {
		return d.createResult(fileInfo, rule, ruleConfig, "ELF MIME type detected"), nil
	}

	// File passed all ELF checks
	return &DetectionResult{
		FilePath: fileInfo.Path,
		RuleName: d.Name(),
		Severity: rule.Severity,
		Passed:   true,
		Message:  "File is not an ELF binary",
	}, nil
}

// hasElfMagic checks if the file starts with ELF magic number
func (d *ElfDetector) hasElfMagic(content []byte, elfMagic string) bool {
	if len(content) < 4 {
		return false
	}

	// Convert hex string to bytes
	expectedMagic, err := hex.DecodeString(elfMagic)
	if err != nil {
		return false
	}

	if len(expectedMagic) > len(content) {
		return false
	}

	// Compare the first bytes
	for i, b := range expectedMagic {
		if content[i] != b {
			return false
		}
	}

	return true
}

// matchesFileCommand runs the file command and checks patterns
func (d *ElfDetector) matchesFileCommand(filePath string, patterns []string) bool {
	cmd := exec.Command("file", filePath)
	output, err := cmd.Output()
	if err != nil {
		return false
	}

	fileOutput := string(output)
	
	for _, pattern := range patterns {
		matched, err := regexp.MatchString(pattern, fileOutput)
		if err != nil {
			continue
		}
		if matched {
			return true
		}
	}

	return false
}

// matchesMimeType checks if the MIME type matches ELF types
func (d *ElfDetector) matchesMimeType(mimeType string, elfMimeTypes []string) bool {
	for _, elfType := range elfMimeTypes {
		if mimeType == elfType {
			return true
		}
	}
	return false
}

// createResult creates a detection result for ELF files
func (d *ElfDetector) createResult(fileInfo *FileInfo, rule config.ElfDetectionRule, ruleConfig config.ElfDetectionConfig, reason string) *DetectionResult {
	// Parse message template
	message := d.parseMessageTemplate(ruleConfig.Message, fileInfo)

	return &DetectionResult{
		FilePath: fileInfo.Path,
		RuleName: d.Name(),
		Severity: rule.Severity,
		Passed:   false,
		Message:  message,
		Details: map[string]string{
			"reason":    reason,
			"file_size": fmt.Sprintf("%d", fileInfo.Size),
			"mime_type": fileInfo.MimeType,
		},
	}
}

// parseMessageTemplate parses the message template with file information
func (d *ElfDetector) parseMessageTemplate(messageTemplate string, fileInfo *FileInfo) string {
	tmpl, err := template.New("message").Parse(messageTemplate)
	if err != nil {
		return messageTemplate // Return original if parsing fails
	}

	data := struct {
		File string
		Size int64
	}{
		File: fileInfo.Path,
		Size: fileInfo.Size,
	}

	var result strings.Builder
	if err := tmpl.Execute(&result, data); err != nil {
		return messageTemplate // Return original if execution fails
	}

	return result.String()
}

