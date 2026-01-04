package types

import (
	"errors"
	"fmt"
)

// Common errors
var (
	// Pod errors
	ErrPodNotFound      = errors.New("pod not found")
	ErrPodAlreadyExists = errors.New("pod already exists")
	ErrInvalidPodName   = errors.New("invalid pod name")
	ErrInvalidPodHost   = errors.New("invalid pod host")
	ErrInvalidPodUser   = errors.New("invalid pod user")
	ErrInvalidSSHCommand = errors.New("invalid SSH command")
	ErrPodOffline       = errors.New("pod is offline")
	ErrPodConnectionFailed = errors.New("failed to connect to pod")

	// Model errors
	ErrModelNotFound       = errors.New("model not found")
	ErrModelAlreadyExists  = errors.New("model already exists")
	ErrInvalidModelName    = errors.New("invalid model name")
	ErrInvalidModelID      = errors.New("invalid model ID")
	ErrInvalidGPUMemory    = errors.New("invalid GPU memory allocation")
	ErrModelStartFailed    = errors.New("failed to start model")
	ErrModelStopFailed     = errors.New("failed to stop model")
	ErrModelNotRunning     = errors.New("model is not running")

	// Template errors
	ErrTemplateNotFound     = errors.New("template not found")
	ErrTemplateAlreadyExists = errors.New("template already exists")
	ErrInvalidTemplateName  = errors.New("invalid template name")

	// Configuration errors
	ErrConfigNotFound      = errors.New("configuration not found")
	ErrInvalidConfig       = errors.New("invalid configuration")
	ErrConfigLoadFailed    = errors.New("failed to load configuration")
	ErrConfigSaveFailed    = errors.New("failed to save configuration")

	// SSH errors
	ErrSSHConnectionFailed = errors.New("SSH connection failed")
	ErrSSHAuthFailed       = errors.New("SSH authentication failed")
	ErrSSHTunnelFailed     = errors.New("SSH tunnel failed")
	ErrCommandFailed       = errors.New("command execution failed")

	// API errors
	ErrAPIConnectionFailed = errors.New("API connection failed")
	ErrAPIRequestFailed    = errors.New("API request failed")
	ErrInvalidAPIResponse  = errors.New("invalid API response")
)

// PodError represents a pod-specific error
type PodError struct {
	PodName string
	Op      string
	Err     error
}

func (e *PodError) Error() string {
	return fmt.Sprintf("pod %s: %s: %v", e.PodName, e.Op, e.Err)
}

func (e *PodError) Unwrap() error {
	return e.Err
}

// ModelError represents a model-specific error
type ModelError struct {
	ModelName string
	Op        string
	Err       error
}

func (e *ModelError) Error() string {
	return fmt.Sprintf("model %s: %s: %v", e.ModelName, e.Op, e.Err)
}

func (e *ModelError) Unwrap() error {
	return e.Err
}

// SSHError represents an SSH-specific error
type SSHError struct {
	Host string
	Op   string
	Err  error
}

func (e *SSHError) Error() string {
	return fmt.Sprintf("ssh %s: %s: %v", e.Host, e.Op, e.Err)
}

func (e *SSHError) Unwrap() error {
	return e.Err
}

// ConfigError represents a configuration-specific error
type ConfigError struct {
	Path string
	Op   string
	Err  error
}

func (e *ConfigError) Error() string {
	return fmt.Sprintf("config %s: %s: %v", e.Path, e.Op, e.Err)
}

func (e *ConfigError) Unwrap() error {
	return e.Err
}

// NewPodError creates a new pod error
func NewPodError(podName, op string, err error) *PodError {
	return &PodError{
		PodName: podName,
		Op:      op,
		Err:     err,
	}
}

// NewModelError creates a new model error
func NewModelError(modelName, op string, err error) *ModelError {
	return &ModelError{
		ModelName: modelName,
		Op:        op,
		Err:       err,
	}
}

// NewSSHError creates a new SSH error
func NewSSHError(host, op string, err error) *SSHError {
	return &SSHError{
		Host: host,
		Op:   op,
		Err:  err,
	}
}

// NewConfigError creates a new configuration error
func NewConfigError(path, op string, err error) *ConfigError {
	return &ConfigError{
		Path: path,
		Op:   op,
		Err:  err,
	}
}

