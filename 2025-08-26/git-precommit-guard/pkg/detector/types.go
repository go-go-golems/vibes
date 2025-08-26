package detector

import (
	"github.com/user/git-precommit-guard/pkg/config"
)

// DetectionResult represents the result of a file detection check
type DetectionResult struct {
	FilePath    string            `json:"file_path"`
	RuleName    string            `json:"rule_name"`
	Severity    string            `json:"severity"`
	Passed      bool              `json:"passed"`
	Message     string            `json:"message"`
	Details     map[string]string `json:"details,omitempty"`
}

// FileInfo contains information about a file being checked
type FileInfo struct {
	Path     string
	Size     int64
	MimeType string
	Content  []byte // First few bytes for magic number detection
}

// Detector interface for different types of file detection
type Detector interface {
	Name() string
	Check(fileInfo *FileInfo, config *config.Config) (*DetectionResult, error)
}

// DetectorManager manages all file detectors
type DetectorManager struct {
	detectors []Detector
	config    *config.Config
}

// NewDetectorManager creates a new detector manager
func NewDetectorManager(cfg *config.Config) *DetectorManager {
	dm := &DetectorManager{
		config: cfg,
	}

	// Register all detectors
	dm.detectors = []Detector{
		NewElfDetector(),
		NewFileSizeDetector(),
		NewMimeDetector(),
	}

	return dm
}

// CheckFile runs all enabled detectors on a file
func (dm *DetectorManager) CheckFile(fileInfo *FileInfo) ([]*DetectionResult, error) {
	var results []*DetectionResult

	for _, detector := range dm.detectors {
		// Skip if globally excluded
		if dm.config.IsPathExcluded(fileInfo.Path) {
			continue
		}

		// Check if this detector is enabled for this path
		if !dm.config.IsRuleEnabledForPath(detector.Name(), fileInfo.Path) {
			continue
		}

		result, err := detector.Check(fileInfo, dm.config)
		if err != nil {
			return nil, err
		}

		if result != nil {
			results = append(results, result)
		}
	}

	return results, nil
}

