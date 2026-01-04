package types

import (
	"fmt"
	"time"
)

// ModelStatus represents the current status of a model
type ModelStatus string

const (
	ModelStatusUnknown   ModelStatus = "unknown"
	ModelStatusStarting  ModelStatus = "starting"
	ModelStatusLoading   ModelStatus = "loading"
	ModelStatusRunning   ModelStatus = "running"
	ModelStatusStopping  ModelStatus = "stopping"
	ModelStatusStopped   ModelStatus = "stopped"
	ModelStatusError     ModelStatus = "error"
	ModelStatusFailed    ModelStatus = "failed"
)

// Model represents a deployed vLLM model instance
type Model struct {
	Name         string            `yaml:"name" json:"name"`
	ModelID      string            `yaml:"model_id" json:"model_id"`
	PodName      string            `yaml:"pod_name" json:"pod_name"`
	Port         int               `yaml:"port" json:"port"`
	PID          int               `yaml:"pid,omitempty" json:"pid,omitempty"`
	Status       ModelStatus       `yaml:"status" json:"status"`
	GPUMemory    float64           `yaml:"gpu_memory" json:"gpu_memory"`
	GPUIDs       []int             `yaml:"gpu_ids,omitempty" json:"gpu_ids,omitempty"`
	AllGPUs      bool              `yaml:"all_gpus" json:"all_gpus"`
	VLLMArgs     []string          `yaml:"vllm_args,omitempty" json:"vllm_args,omitempty"`
	ToolParser   string            `yaml:"tool_parser,omitempty" json:"tool_parser,omitempty"`
	LogFile      string            `yaml:"log_file,omitempty" json:"log_file,omitempty"`
	StartedAt    *time.Time        `yaml:"started_at,omitempty" json:"started_at,omitempty"`
	StoppedAt    *time.Time        `yaml:"stopped_at,omitempty" json:"stopped_at,omitempty"`
	Metadata     map[string]string `yaml:"metadata,omitempty" json:"metadata,omitempty"`
	CreatedAt    time.Time         `yaml:"created_at" json:"created_at"`
	UpdatedAt    time.Time         `yaml:"updated_at" json:"updated_at"`
}

// ModelMetrics represents performance metrics for a model
type ModelMetrics struct {
	ModelName        string    `json:"model_name"`
	RequestCount     int64     `json:"request_count"`
	AvgLatency       float64   `json:"avg_latency_ms"`
	TokensPerSecond  float64   `json:"tokens_per_second"`
	MemoryUsage      int64     `json:"memory_usage_bytes"`
	GPUUtilization   float64   `json:"gpu_utilization_percent"`
	LastRequestTime  time.Time `json:"last_request_time"`
	ErrorCount       int64     `json:"error_count"`
	UptimeSeconds    int64     `json:"uptime_seconds"`
}

// ModelTemplate represents a reusable model configuration template
type ModelTemplate struct {
	Name        string            `yaml:"name" json:"name"`
	Description string            `yaml:"description,omitempty" json:"description,omitempty"`
	ModelID     string            `yaml:"model_id" json:"model_id"`
	GPUMemory   float64           `yaml:"gpu_memory" json:"gpu_memory"`
	AllGPUs     bool              `yaml:"all_gpus" json:"all_gpus"`
	VLLMArgs    []string          `yaml:"vllm_args,omitempty" json:"vllm_args,omitempty"`
	ToolParser  string            `yaml:"tool_parser,omitempty" json:"tool_parser,omitempty"`
	Tags        []string          `yaml:"tags,omitempty" json:"tags,omitempty"`
	Metadata    map[string]string `yaml:"metadata,omitempty" json:"metadata,omitempty"`
}

// ModelConfig represents the configuration for all models
type ModelConfig struct {
	Models    map[string]*Model         `yaml:"models" json:"models"`
	Templates map[string]*ModelTemplate `yaml:"templates" json:"templates"`
}

// Validate validates the model configuration
func (m *Model) Validate() error {
	if m.Name == "" {
		return ErrInvalidModelName
	}
	if m.ModelID == "" {
		return ErrInvalidModelID
	}
	if m.PodName == "" {
		return ErrInvalidPodName
	}
	if m.GPUMemory <= 0 || m.GPUMemory > 1.0 {
		return ErrInvalidGPUMemory
	}
	return nil
}

// IsRunning returns true if the model is currently running
func (m *Model) IsRunning() bool {
	return m.Status == ModelStatusRunning
}

// IsHealthy returns true if the model is in a healthy state
func (m *Model) IsHealthy() bool {
	return m.Status == ModelStatusRunning || m.Status == ModelStatusLoading
}

// GetDisplayName returns a human-readable name for the model
func (m *Model) GetDisplayName() string {
	if m.Name != "" {
		return m.Name
	}
	return m.ModelID
}

// GetAPIEndpoint returns the API endpoint URL for the model
func (m *Model) GetAPIEndpoint(host string) string {
	if host == "" {
		host = "localhost"
	}
	return fmt.Sprintf("http://%s:%d/v1", host, m.Port)
}

// GetUptime returns the uptime duration of the model
func (m *Model) GetUptime() time.Duration {
	if m.StartedAt == nil {
		return 0
	}
	if m.StoppedAt != nil {
		return m.StoppedAt.Sub(*m.StartedAt)
	}
	return time.Since(*m.StartedAt)
}

// Validate validates the model template
func (t *ModelTemplate) Validate() error {
	if t.Name == "" {
		return ErrInvalidTemplateName
	}
	if t.ModelID == "" {
		return ErrInvalidModelID
	}
	if t.GPUMemory <= 0 || t.GPUMemory > 1.0 {
		return ErrInvalidGPUMemory
	}
	return nil
}

