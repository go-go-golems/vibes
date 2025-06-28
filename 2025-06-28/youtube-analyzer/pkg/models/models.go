package models

import (
	"time"
)

// AnalysisResult represents the complete analysis result
type AnalysisResult struct {
	SessionID   string                 `json:"session_id"`
	VideoURL    string                 `json:"video_url"`
	Mode        string                 `json:"analysis_mode"`
	Model       string                 `json:"model_used"`
	Timestamp   time.Time              `json:"timestamp"`
	TotalSteps  int                    `json:"total_steps"`
	APICalls    int                    `json:"api_calls_made"`
	TotalTime   float64                `json:"total_time_seconds"`
	Analysis    *TechnicalAnalysis     `json:"analysis,omitempty"`
	Steps       []AnalysisStep         `json:"steps"`
	APICallLogs []APICallLog           `json:"api_call_logs"`
	Metadata    map[string]interface{} `json:"metadata,omitempty"`
}

// TechnicalAnalysis represents the AI analysis results
type TechnicalAnalysis struct {
	Summary              string                 `json:"summary"`
	TechnicalScore       float64                `json:"technical_score"`
	ViralPotential       float64                `json:"viral_potential"`
	TargetAudience       string                 `json:"target_audience"`
	Technologies         []string               `json:"technologies_identified"`
	KeyTimestamps        []Timestamp            `json:"key_timestamps"`
	SocialMediaTips      []string               `json:"social_media_optimization"`
	TechnicalAccuracy    string                 `json:"technical_accuracy_assessment"`
	EducationalValue     string                 `json:"educational_value"`
	CodeQuality          string                 `json:"code_quality_assessment"`
	DeveloperRelevance   string                 `json:"developer_relevance"`
	PlatformRecommendations map[string]string   `json:"platform_recommendations"`
	RawResponse          string                 `json:"raw_ai_response"`
	AnalysisMetadata     map[string]interface{} `json:"analysis_metadata,omitempty"`
}

// Timestamp represents a key moment in the video
type Timestamp struct {
	Time        string `json:"time"`
	Description string `json:"description"`
	Importance  string `json:"importance"`
	Type        string `json:"type"` // "technical", "engagement", "educational", etc.
}

// AnalysisStep represents a single step in the analysis process
type AnalysisStep struct {
	StepNumber  int                    `json:"step_number"`
	StepName    string                 `json:"step_name"`
	StepType    string                 `json:"step_type"` // "info", "processing", "success", "error"
	Timestamp   time.Time              `json:"timestamp"`
	ElapsedTime float64                `json:"elapsed_time_seconds"`
	Details     map[string]interface{} `json:"details,omitempty"`
}

// APICallLog represents a logged API call
type APICallLog struct {
	CallNumber  int           `json:"call_number"`
	Model       string        `json:"model"`
	Operation   string        `json:"operation"`
	Timestamp   time.Time     `json:"timestamp"`
	Duration    time.Duration `json:"duration"`
	Success     bool          `json:"success"`
	RequestSize int           `json:"request_size_chars,omitempty"`
	ResponseSize int          `json:"response_size_chars,omitempty"`
	Error       string        `json:"error,omitempty"`
}

// ProgressCallback is a function type for progress updates
type ProgressCallback func(step string, progress int)

// VideoInfo represents basic video information
type VideoInfo struct {
	URL         string `json:"url"`
	Title       string `json:"title,omitempty"`
	Duration    string `json:"duration,omitempty"`
	Description string `json:"description,omitempty"`
	Channel     string `json:"channel,omitempty"`
}

// AnalysisConfig represents configuration for a specific analysis
type AnalysisConfig struct {
	Mode           string            `json:"mode"`
	Model          string            `json:"model"`
	MaxRetries     int               `json:"max_retries"`
	TimeoutSeconds int               `json:"timeout_seconds"`
	CustomPrompts  map[string]string `json:"custom_prompts,omitempty"`
}

