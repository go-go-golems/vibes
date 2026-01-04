package analysis

import (
	"regexp"
	"strings"
	"time"

	"github.com/procmon/procmon/pkg/monitor"
)

// ProgramAnalyzer provides enhanced analysis for well-known programs
type ProgramAnalyzer struct {
	analyzers map[ProgramType]ProgramAnalyzerFunc
}

// ProgramType represents different types of well-known programs
type ProgramType string

const (
	ProgramTypeFirefox     ProgramType = "firefox"
	ProgramTypeChrome      ProgramType = "chrome"
	ProgramTypeChromium    ProgramType = "chromium"
	ProgramTypeVSCode      ProgramType = "vscode"
	ProgramTypeDocker      ProgramType = "docker"
	ProgramTypeKubernetes  ProgramType = "kubernetes"
	ProgramTypeDatabase    ProgramType = "database"
	ProgramTypeWebServer   ProgramType = "webserver"
	ProgramTypeJava        ProgramType = "java"
	ProgramTypePython      ProgramType = "python"
	ProgramTypeNode        ProgramType = "node"
	ProgramTypeUnknown     ProgramType = "unknown"
)

// ProgramAnalyzerFunc analyzes a process and returns enhanced information
type ProgramAnalyzerFunc func(process *monitor.ProcessInfo) *ProgramAnalysis

// ProgramAnalysis represents enhanced analysis for well-known programs
type ProgramAnalysis struct {
	ProgramType     ProgramType      `json:"program_type"`
	Version         string           `json:"version"`
	ThreadRoles     []ThreadRole     `json:"thread_roles"`
	Metrics         ProgramMetrics   `json:"metrics"`
	Insights        []PerformanceInsight `json:"insights"`
	Recommendations []Recommendation `json:"recommendations"`
}

// ThreadRole represents the identified role of a thread within a program
type ThreadRole struct {
	TID         int32      `json:"tid"`
	Role        string     `json:"role"`
	Description string     `json:"description"`
	Importance  Importance `json:"importance"`
	ExpectedCPU float64    `json:"expected_cpu"`
	ExpectedMem uint64     `json:"expected_memory"`
}

type Importance string

const (
	ImportanceCritical Importance = "critical"
	ImportanceHigh     Importance = "high"
	ImportanceMedium   Importance = "medium"
	ImportanceLow      Importance = "low"
)

// ProgramMetrics represents program-specific performance metrics
type ProgramMetrics struct {
	ResponseTime time.Duration              `json:"response_time"`
	Throughput   float64                    `json:"throughput"`
	ErrorRate    float64                    `json:"error_rate"`
	Custom       map[string]interface{}     `json:"custom"`
}

// PerformanceInsight represents an automated performance insight
type PerformanceInsight struct {
	Type        InsightType `json:"type"`
	Severity    Severity    `json:"severity"`
	Title       string      `json:"title"`
	Description string      `json:"description"`
	Evidence    []string    `json:"evidence"`
	DetectedAt  time.Time   `json:"detected_at"`
	Duration    time.Duration `json:"duration"`
}

type InsightType string

const (
	InsightTypePerformance InsightType = "performance"
	InsightTypeResource    InsightType = "resource"
	InsightTypeStability   InsightType = "stability"
	InsightTypeSecurity    InsightType = "security"
)

type Severity string

const (
	SeverityInfo     Severity = "info"
	SeverityWarning  Severity = "warning"
	SeverityError    Severity = "error"
	SeverityCritical Severity = "critical"
)

// Recommendation represents an automated recommendation for optimization
type Recommendation struct {
	Type           RecommendationType `json:"type"`
	Priority       Priority           `json:"priority"`
	Title          string             `json:"title"`
	Description    string             `json:"description"`
	Action         string             `json:"action"`
	ExpectedImpact ImpactEstimate     `json:"expected_impact"`
	Implementation string             `json:"implementation"`
	Difficulty     Difficulty         `json:"difficulty"`
}

type RecommendationType string

const (
	RecommendationTypeConfiguration RecommendationType = "configuration"
	RecommendationTypeResource      RecommendationType = "resource"
	RecommendationTypeUpgrade       RecommendationType = "upgrade"
	RecommendationTypeOptimization  RecommendationType = "optimization"
)

type Priority string

const (
	PriorityLow      Priority = "low"
	PriorityMedium   Priority = "medium"
	PriorityHigh     Priority = "high"
	PriorityCritical Priority = "critical"
)

type Difficulty string

const (
	DifficultyEasy     Difficulty = "easy"
	DifficultyModerate Difficulty = "moderate"
	DifficultyHard     Difficulty = "hard"
	DifficultyExpert   Difficulty = "expert"
)

// ImpactEstimate represents the estimated impact of implementing a recommendation
type ImpactEstimate struct {
	CPUImprovement    float64 `json:"cpu_improvement"`
	MemoryImprovement float64 `json:"memory_improvement"`
	PowerImprovement  float64 `json:"power_improvement"`
	Confidence        float64 `json:"confidence"`
}

// NewProgramAnalyzer creates a new program analyzer
func NewProgramAnalyzer() *ProgramAnalyzer {
	analyzer := &ProgramAnalyzer{
		analyzers: make(map[ProgramType]ProgramAnalyzerFunc),
	}
	
	// Register built-in analyzers
	analyzer.registerBuiltinAnalyzers()
	
	return analyzer
}

// registerBuiltinAnalyzers registers analyzers for well-known programs
func (pa *ProgramAnalyzer) registerBuiltinAnalyzers() {
	pa.analyzers[ProgramTypeFirefox] = pa.analyzeFirefox
	pa.analyzers[ProgramTypeChrome] = pa.analyzeChrome
	pa.analyzers[ProgramTypeChromium] = pa.analyzeChromium
	pa.analyzers[ProgramTypeVSCode] = pa.analyzeVSCode
	pa.analyzers[ProgramTypeDocker] = pa.analyzeDocker
	pa.analyzers[ProgramTypeJava] = pa.analyzeJava
	pa.analyzers[ProgramTypePython] = pa.analyzePython
	pa.analyzers[ProgramTypeNode] = pa.analyzeNode
}

// AnalyzeProcess analyzes a process and returns enhanced information
func (pa *ProgramAnalyzer) AnalyzeProcess(process *monitor.ProcessInfo) *ProgramAnalysis {
	programType := pa.identifyProgramType(process)
	
	// Get analyzer for this program type
	analyzer, exists := pa.analyzers[programType]
	if !exists {
		return pa.analyzeGeneric(process, programType)
	}
	
	return analyzer(process)
}

// identifyProgramType identifies the type of program based on process information
func (pa *ProgramAnalyzer) identifyProgramType(process *monitor.ProcessInfo) ProgramType {
	name := strings.ToLower(process.Name)
	cmdline := strings.ToLower(process.CommandLine)
	
	// Firefox
	if strings.Contains(name, "firefox") || strings.Contains(cmdline, "firefox") {
		return ProgramTypeFirefox
	}
	
	// Chrome
	if strings.Contains(name, "chrome") && !strings.Contains(name, "chromium") {
		return ProgramTypeChrome
	}
	
	// Chromium
	if strings.Contains(name, "chromium") {
		return ProgramTypeChromium
	}
	
	// VS Code
	if strings.Contains(name, "code") && (strings.Contains(cmdline, "vscode") || strings.Contains(cmdline, "code")) {
		return ProgramTypeVSCode
	}
	
	// Docker
	if strings.Contains(name, "docker") || strings.Contains(cmdline, "docker") {
		return ProgramTypeDocker
	}
	
	// Java applications
	if strings.Contains(name, "java") || strings.Contains(cmdline, "java") {
		return ProgramTypeJava
	}
	
	// Python applications
	if strings.Contains(name, "python") || strings.Contains(cmdline, "python") {
		return ProgramTypePython
	}
	
	// Node.js applications
	if strings.Contains(name, "node") || strings.Contains(cmdline, "node") {
		return ProgramTypeNode
	}
	
	// Database servers
	if strings.Contains(name, "mysql") || strings.Contains(name, "postgres") || 
	   strings.Contains(name, "mongodb") || strings.Contains(name, "redis") {
		return ProgramTypeDatabase
	}
	
	// Web servers
	if strings.Contains(name, "nginx") || strings.Contains(name, "apache") || 
	   strings.Contains(name, "httpd") {
		return ProgramTypeWebServer
	}
	
	return ProgramTypeUnknown
}

// analyzeFirefox provides specialized analysis for Firefox
func (pa *ProgramAnalyzer) analyzeFirefox(process *monitor.ProcessInfo) *ProgramAnalysis {
	analysis := &ProgramAnalysis{
		ProgramType: ProgramTypeFirefox,
		Version:     pa.extractVersionFromCmdline(process.CommandLine, `firefox.*?(\d+\.\d+)`),
		ThreadRoles: pa.analyzeFirefoxThreads(process),
		Metrics:     ProgramMetrics{Custom: make(map[string]interface{})},
	}
	
	// Add Firefox-specific insights
	analysis.Insights = pa.generateFirefoxInsights(process)
	analysis.Recommendations = pa.generateFirefoxRecommendations(process, analysis)
	
	return analysis
}

// analyzeFirefoxThreads analyzes Firefox thread roles
func (pa *ProgramAnalyzer) analyzeFirefoxThreads(process *monitor.ProcessInfo) []ThreadRole {
	var roles []ThreadRole
	
	for _, thread := range process.Threads {
		role := ThreadRole{
			TID:         thread.TID,
			Role:        pa.identifyFirefoxThreadRole(thread.Name),
			Description: pa.getFirefoxThreadDescription(thread.Name),
			Importance:  pa.getFirefoxThreadImportance(thread.Name),
		}
		roles = append(roles, role)
	}
	
	return roles
}

// identifyFirefoxThreadRole identifies the role of a Firefox thread
func (pa *ProgramAnalyzer) identifyFirefoxThreadRole(threadName string) string {
	name := strings.ToLower(threadName)
	
	switch {
	case strings.Contains(name, "main"):
		return "Main Thread"
	case strings.Contains(name, "compositor"):
		return "Compositor"
	case strings.Contains(name, "dom worker"):
		return "DOM Worker"
	case strings.Contains(name, "js helper"):
		return "JavaScript Helper"
	case strings.Contains(name, "imgdecoder"):
		return "Image Decoder"
	case strings.Contains(name, "media"):
		return "Media Processing"
	case strings.Contains(name, "network"):
		return "Network I/O"
	case strings.Contains(name, "cache"):
		return "Cache Management"
	case strings.Contains(name, "timer"):
		return "Timer Thread"
	default:
		return "Worker Thread"
	}
}

// getFirefoxThreadDescription returns a description for a Firefox thread role
func (pa *ProgramAnalyzer) getFirefoxThreadDescription(threadName string) string {
	role := pa.identifyFirefoxThreadRole(threadName)
	
	descriptions := map[string]string{
		"Main Thread":      "Primary UI and JavaScript execution thread",
		"Compositor":       "Handles graphics composition and rendering",
		"DOM Worker":       "Processes DOM operations and parsing",
		"JavaScript Helper": "Assists with JavaScript execution",
		"Image Decoder":    "Decodes and processes images",
		"Media Processing": "Handles audio and video processing",
		"Network I/O":      "Manages network requests and responses",
		"Cache Management": "Handles browser cache operations",
		"Timer Thread":     "Manages timers and scheduled tasks",
		"Worker Thread":    "General purpose worker thread",
	}
	
	if desc, exists := descriptions[role]; exists {
		return desc
	}
	return "General purpose thread"
}

// getFirefoxThreadImportance returns the importance level of a Firefox thread
func (pa *ProgramAnalyzer) getFirefoxThreadImportance(threadName string) Importance {
	role := pa.identifyFirefoxThreadRole(threadName)
	
	switch role {
	case "Main Thread":
		return ImportanceCritical
	case "Compositor":
		return ImportanceHigh
	case "DOM Worker", "JavaScript Helper":
		return ImportanceHigh
	case "Network I/O", "Media Processing":
		return ImportanceMedium
	default:
		return ImportanceLow
	}
}

// analyzeChrome provides specialized analysis for Chrome
func (pa *ProgramAnalyzer) analyzeChrome(process *monitor.ProcessInfo) *ProgramAnalysis {
	analysis := &ProgramAnalysis{
		ProgramType: ProgramTypeChrome,
		Version:     pa.extractVersionFromCmdline(process.CommandLine, `chrome.*?(\d+\.\d+\.\d+)`),
		ThreadRoles: pa.analyzeChromeThreads(process),
		Metrics:     ProgramMetrics{Custom: make(map[string]interface{})},
	}
	
	// Add Chrome-specific insights
	analysis.Insights = pa.generateChromeInsights(process)
	analysis.Recommendations = pa.generateChromeRecommendations(process, analysis)
	
	return analysis
}

// analyzeChromeThreads analyzes Chrome thread roles
func (pa *ProgramAnalyzer) analyzeChromeThreads(process *monitor.ProcessInfo) []ThreadRole {
	var roles []ThreadRole
	
	for _, thread := range process.Threads {
		role := ThreadRole{
			TID:         thread.TID,
			Role:        pa.identifyChromeThreadRole(thread.Name),
			Description: pa.getChromeThreadDescription(thread.Name),
			Importance:  pa.getChromeThreadImportance(thread.Name),
		}
		roles = append(roles, role)
	}
	
	return roles
}

// identifyChromeThreadRole identifies the role of a Chrome thread
func (pa *ProgramAnalyzer) identifyChromeThreadRole(threadName string) string {
	name := strings.ToLower(threadName)
	
	switch {
	case strings.Contains(name, "main"):
		return "Main Thread"
	case strings.Contains(name, "io"):
		return "I/O Thread"
	case strings.Contains(name, "compositor"):
		return "Compositor"
	case strings.Contains(name, "renderer"):
		return "Renderer"
	case strings.Contains(name, "gpu"):
		return "GPU Process"
	case strings.Contains(name, "network"):
		return "Network Service"
	case strings.Contains(name, "audio"):
		return "Audio Service"
	case strings.Contains(name, "video"):
		return "Video Decoder"
	case strings.Contains(name, "worker"):
		return "Service Worker"
	default:
		return "Utility Thread"
	}
}

// getChromeThreadDescription returns a description for a Chrome thread role
func (pa *ProgramAnalyzer) getChromeThreadDescription(threadName string) string {
	role := pa.identifyChromeThreadRole(threadName)
	
	descriptions := map[string]string{
		"Main Thread":     "Primary browser UI thread",
		"I/O Thread":      "Handles file and network I/O operations",
		"Compositor":      "Manages graphics composition",
		"Renderer":        "Renders web page content",
		"GPU Process":     "Handles GPU-accelerated operations",
		"Network Service": "Manages network requests",
		"Audio Service":   "Processes audio streams",
		"Video Decoder":   "Decodes video content",
		"Service Worker":  "Handles background tasks",
		"Utility Thread": "General purpose utility operations",
	}
	
	if desc, exists := descriptions[role]; exists {
		return desc
	}
	return "General purpose thread"
}

// getChromeThreadImportance returns the importance level of a Chrome thread
func (pa *ProgramAnalyzer) getChromeThreadImportance(threadName string) Importance {
	role := pa.identifyChromeThreadRole(threadName)
	
	switch role {
	case "Main Thread":
		return ImportanceCritical
	case "Renderer", "Compositor":
		return ImportanceHigh
	case "I/O Thread", "GPU Process":
		return ImportanceHigh
	case "Network Service", "Audio Service":
		return ImportanceMedium
	default:
		return ImportanceLow
	}
}

// analyzeChromium provides specialized analysis for Chromium (similar to Chrome)
func (pa *ProgramAnalyzer) analyzeChromium(process *monitor.ProcessInfo) *ProgramAnalysis {
	// Chromium analysis is very similar to Chrome
	analysis := pa.analyzeChrome(process)
	analysis.ProgramType = ProgramTypeChromium
	return analysis
}

// analyzeVSCode provides specialized analysis for Visual Studio Code
func (pa *ProgramAnalyzer) analyzeVSCode(process *monitor.ProcessInfo) *ProgramAnalysis {
	analysis := &ProgramAnalysis{
		ProgramType: ProgramTypeVSCode,
		Version:     pa.extractVersionFromCmdline(process.CommandLine, `code.*?(\d+\.\d+\.\d+)`),
		ThreadRoles: pa.analyzeVSCodeThreads(process),
		Metrics:     ProgramMetrics{Custom: make(map[string]interface{})},
	}
	
	analysis.Insights = pa.generateVSCodeInsights(process)
	analysis.Recommendations = pa.generateVSCodeRecommendations(process, analysis)
	
	return analysis
}

// analyzeVSCodeThreads analyzes VS Code thread roles
func (pa *ProgramAnalyzer) analyzeVSCodeThreads(process *monitor.ProcessInfo) []ThreadRole {
	var roles []ThreadRole
	
	for _, thread := range process.Threads {
		role := ThreadRole{
			TID:         thread.TID,
			Role:        pa.identifyVSCodeThreadRole(thread.Name),
			Description: pa.getVSCodeThreadDescription(thread.Name),
			Importance:  pa.getVSCodeThreadImportance(thread.Name),
		}
		roles = append(roles, role)
	}
	
	return roles
}

// identifyVSCodeThreadRole identifies the role of a VS Code thread
func (pa *ProgramAnalyzer) identifyVSCodeThreadRole(threadName string) string {
	name := strings.ToLower(threadName)
	
	switch {
	case strings.Contains(name, "main"):
		return "Main Process"
	case strings.Contains(name, "renderer"):
		return "Renderer Process"
	case strings.Contains(name, "extension"):
		return "Extension Host"
	case strings.Contains(name, "language"):
		return "Language Server"
	case strings.Contains(name, "typescript"):
		return "TypeScript Service"
	case strings.Contains(name, "git"):
		return "Git Service"
	case strings.Contains(name, "search"):
		return "Search Service"
	case strings.Contains(name, "file"):
		return "File Watcher"
	default:
		return "Worker Process"
	}
}

// getVSCodeThreadDescription returns a description for a VS Code thread role
func (pa *ProgramAnalyzer) getVSCodeThreadDescription(threadName string) string {
	role := pa.identifyVSCodeThreadRole(threadName)
	
	descriptions := map[string]string{
		"Main Process":      "Primary VS Code process",
		"Renderer Process":  "Handles UI rendering",
		"Extension Host":    "Runs VS Code extensions",
		"Language Server":   "Provides language intelligence",
		"TypeScript Service": "TypeScript language support",
		"Git Service":       "Git integration and operations",
		"Search Service":    "File and text search operations",
		"File Watcher":      "Monitors file system changes",
		"Worker Process":    "General background tasks",
	}
	
	if desc, exists := descriptions[role]; exists {
		return desc
	}
	return "General purpose process"
}

// getVSCodeThreadImportance returns the importance level of a VS Code thread
func (pa *ProgramAnalyzer) getVSCodeThreadImportance(threadName string) Importance {
	role := pa.identifyVSCodeThreadRole(threadName)
	
	switch role {
	case "Main Process":
		return ImportanceCritical
	case "Renderer Process", "Extension Host":
		return ImportanceHigh
	case "Language Server", "TypeScript Service":
		return ImportanceMedium
	default:
		return ImportanceLow
	}
}

// Placeholder implementations for other analyzers
func (pa *ProgramAnalyzer) analyzeDocker(process *monitor.ProcessInfo) *ProgramAnalysis {
	return pa.analyzeGeneric(process, ProgramTypeDocker)
}

func (pa *ProgramAnalyzer) analyzeJava(process *monitor.ProcessInfo) *ProgramAnalysis {
	return pa.analyzeGeneric(process, ProgramTypeJava)
}

func (pa *ProgramAnalyzer) analyzePython(process *monitor.ProcessInfo) *ProgramAnalysis {
	return pa.analyzeGeneric(process, ProgramTypePython)
}

func (pa *ProgramAnalyzer) analyzeNode(process *monitor.ProcessInfo) *ProgramAnalysis {
	return pa.analyzeGeneric(process, ProgramTypeNode)
}

// analyzeGeneric provides basic analysis for unknown or unsupported programs
func (pa *ProgramAnalyzer) analyzeGeneric(process *monitor.ProcessInfo, programType ProgramType) *ProgramAnalysis {
	return &ProgramAnalysis{
		ProgramType: programType,
		ThreadRoles: pa.analyzeGenericThreads(process),
		Metrics:     ProgramMetrics{Custom: make(map[string]interface{})},
		Insights:    []PerformanceInsight{},
		Recommendations: []Recommendation{},
	}
}

// analyzeGenericThreads provides basic thread analysis
func (pa *ProgramAnalyzer) analyzeGenericThreads(process *monitor.ProcessInfo) []ThreadRole {
	var roles []ThreadRole
	
	for _, thread := range process.Threads {
		role := ThreadRole{
			TID:         thread.TID,
			Role:        "Worker Thread",
			Description: "General purpose thread",
			Importance:  ImportanceMedium,
		}
		roles = append(roles, role)
	}
	
	return roles
}

// extractVersionFromCmdline extracts version information from command line using regex
func (pa *ProgramAnalyzer) extractVersionFromCmdline(cmdline, pattern string) string {
	re, err := regexp.Compile(pattern)
	if err != nil {
		return "unknown"
	}
	
	matches := re.FindStringSubmatch(cmdline)
	if len(matches) > 1 {
		return matches[1]
	}
	
	return "unknown"
}

// Placeholder implementations for insight and recommendation generators
func (pa *ProgramAnalyzer) generateFirefoxInsights(process *monitor.ProcessInfo) []PerformanceInsight {
	return []PerformanceInsight{}
}

func (pa *ProgramAnalyzer) generateFirefoxRecommendations(process *monitor.ProcessInfo, analysis *ProgramAnalysis) []Recommendation {
	return []Recommendation{}
}

func (pa *ProgramAnalyzer) generateChromeInsights(process *monitor.ProcessInfo) []PerformanceInsight {
	return []PerformanceInsight{}
}

func (pa *ProgramAnalyzer) generateChromeRecommendations(process *monitor.ProcessInfo, analysis *ProgramAnalysis) []Recommendation {
	return []Recommendation{}
}

func (pa *ProgramAnalyzer) generateVSCodeInsights(process *monitor.ProcessInfo) []PerformanceInsight {
	return []PerformanceInsight{}
}

func (pa *ProgramAnalyzer) generateVSCodeRecommendations(process *monitor.ProcessInfo, analysis *ProgramAnalysis) []Recommendation {
	return []Recommendation{}
}

