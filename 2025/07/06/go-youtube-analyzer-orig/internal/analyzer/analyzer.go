package analyzer

import (
	"context"
	"fmt"
	"time"

	"github.com/user/youtube-analyzer-go/internal/config"
	"github.com/user/youtube-analyzer-go/internal/logger"
	"github.com/user/youtube-analyzer-go/pkg/gemini"
	"github.com/user/youtube-analyzer-go/pkg/models"
)

// Analyzer orchestrates the video analysis process
type Analyzer struct {
	config     *config.Config
	logger     *logger.Logger
	gemini     *gemini.Client
	sessionID  string
	startTime  time.Time
	steps      []models.AnalysisStep
	apiCalls   []models.APICallLog
	stepCount  int
}

// New creates a new analyzer instance
func New(cfg *config.Config, log *logger.Logger, sessionID string) (*Analyzer, error) {
	// Initialize Gemini client
	geminiClient, err := gemini.New(cfg, log)
	if err != nil {
		return nil, fmt.Errorf("failed to initialize Gemini client: %w", err)
	}

	analyzer := &Analyzer{
		config:    cfg,
		logger:    log,
		gemini:    geminiClient,
		sessionID: sessionID,
		startTime: time.Now(),
		steps:     make([]models.AnalysisStep, 0),
		apiCalls:  make([]models.APICallLog, 0),
		stepCount: 0,
	}

	analyzer.logStep("Analyzer initialized", "success", map[string]interface{}{
		"session_id": sessionID,
		"mode":       cfg.Mode,
		"model":      cfg.GetModelName(),
	})

	return analyzer, nil
}

// Close closes the analyzer and cleans up resources
func (a *Analyzer) Close() error {
	if a.gemini != nil {
		return a.gemini.Close()
	}
	return nil
}

// AnalyzeVideo performs the complete video analysis
func (a *Analyzer) AnalyzeVideo(videoURL string, progressCallback models.ProgressCallback) (*models.AnalysisResult, error) {
	defer a.Close()

	a.logStep("Starting video analysis", "processing", map[string]interface{}{
		"video_url": videoURL,
	})

	if progressCallback != nil {
		progressCallback("Initializing analysis", 10)
	}

	// Step 1: Validate video URL
	if err := a.validateVideoURL(videoURL); err != nil {
		a.logStep("Video URL validation failed", "error", map[string]interface{}{
			"error": err.Error(),
		})
		return nil, err
	}

	if progressCallback != nil {
		progressCallback("Video URL validated", 20)
	}

	// Step 2: Prepare analysis context
	a.logStep("Preparing analysis context", "processing", map[string]interface{}{
		"mode":  a.config.Mode,
		"model": a.config.GetModelName(),
	})

	if progressCallback != nil {
		progressCallback("Context prepared", 30)
	}

	// Step 3: Execute Gemini analysis
	a.logStep("Executing Gemini video analysis", "processing", map[string]interface{}{
		"api_endpoint": "gemini_video_analysis",
	})

	if progressCallback != nil {
		progressCallback("Calling Gemini API", 40)
	}

	ctx := context.Background()
	analysis, err := a.gemini.AnalyzeVideo(ctx, videoURL)
	if err != nil {
		a.logStep("Gemini analysis failed", "error", map[string]interface{}{
			"error": err.Error(),
		})
		return nil, fmt.Errorf("video analysis failed: %w", err)
	}

	if progressCallback != nil {
		progressCallback("Analysis completed", 70)
	}

	// Step 4: Process and validate results
	a.logStep("Processing analysis results", "processing", map[string]interface{}{
		"response_length": len(analysis.RawResponse),
		"technologies":    len(analysis.Technologies),
		"timestamps":      len(analysis.KeyTimestamps),
	})

	if progressCallback != nil {
		progressCallback("Results processed", 85)
	}

	// Step 5: Finalize results
	result := &models.AnalysisResult{
		SessionID:   a.sessionID,
		VideoURL:    videoURL,
		Mode:        a.config.Mode,
		Model:       a.config.GetModelName(),
		Timestamp:   a.startTime,
		TotalSteps:  len(a.steps),
		APICalls:    len(a.apiCalls),
		TotalTime:   time.Since(a.startTime).Seconds(),
		Analysis:    analysis,
		Steps:       a.steps,
		APICallLogs: a.apiCalls,
		Metadata: map[string]interface{}{
			"go_version":    "1.21",
			"cli_version":   "1.0.0",
			"analysis_type": "technical_video_analysis",
		},
	}

	a.logStep("Analysis completed successfully", "success", map[string]interface{}{
		"total_time":    result.TotalTime,
		"total_steps":   result.TotalSteps,
		"api_calls":     result.APICalls,
		"technical_score": analysis.TechnicalScore,
		"viral_potential": analysis.ViralPotential,
	})

	if progressCallback != nil {
		progressCallback("Analysis complete", 100)
	}

	return result, nil
}

// validateVideoURL validates the provided video URL
func (a *Analyzer) validateVideoURL(videoURL string) error {
	a.logStep("Validating video URL", "processing", map[string]interface{}{
		"url": videoURL,
	})

	if videoURL == "" {
		return fmt.Errorf("video URL cannot be empty")
	}

	// Check for YouTube URL patterns
	validPatterns := []string{
		"youtube.com/watch",
		"youtu.be/",
		"youtube.com/embed/",
		"youtube.com/v/",
	}

	isValid := false
	for _, pattern := range validPatterns {
		if contains(videoURL, pattern) {
			isValid = true
			break
		}
	}

	if !isValid {
		return fmt.Errorf("invalid YouTube URL format")
	}

	a.logStep("Video URL validation successful", "success", map[string]interface{}{
		"url_type": "youtube",
	})

	return nil
}

// logStep logs a step in the analysis process
func (a *Analyzer) logStep(stepName, stepType string, details map[string]interface{}) {
	a.stepCount++
	
	step := models.AnalysisStep{
		StepNumber:  a.stepCount,
		StepName:    stepName,
		StepType:    stepType,
		Timestamp:   time.Now(),
		ElapsedTime: time.Since(a.startTime).Seconds(),
		Details:     details,
	}

	a.steps = append(a.steps, step)
	a.logger.Step(a.stepCount, stepName, stepType, details)
}

// contains checks if a string contains a substring (case-insensitive)
func contains(s, substr string) bool {
	return len(s) >= len(substr) && 
		   (s == substr || 
		    (len(s) > len(substr) && 
		     findSubstring(s, substr)))
}

// findSubstring finds a substring in a string
func findSubstring(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}

