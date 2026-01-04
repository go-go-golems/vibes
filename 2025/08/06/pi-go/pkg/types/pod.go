package types

import (
	"fmt"
	"time"
)

// PodStatus represents the current status of a pod
type PodStatus string

const (
	PodStatusUnknown      PodStatus = "unknown"
	PodStatusConnecting   PodStatus = "connecting"
	PodStatusOnline       PodStatus = "online"
	PodStatusOffline      PodStatus = "offline"
	PodStatusError        PodStatus = "error"
	PodStatusMaintenance  PodStatus = "maintenance"
)

// Pod represents a GPU pod configuration
type Pod struct {
	Name         string            `yaml:"name" json:"name"`
	Host         string            `yaml:"host" json:"host"`
	Port         int               `yaml:"port" json:"port"`
	User         string            `yaml:"user" json:"user"`
	SSHCommand   string            `yaml:"ssh_command" json:"ssh_command"`
	StoragePath  string            `yaml:"storage_path" json:"storage_path"`
	UseSudo      bool              `yaml:"use_sudo" json:"use_sudo"`
	Status       PodStatus         `yaml:"status" json:"status"`
	LastSeen     *time.Time        `yaml:"last_seen,omitempty" json:"last_seen,omitempty"`
	Capabilities PodCapabilities   `yaml:"capabilities" json:"capabilities"`
	Metadata     map[string]string `yaml:"metadata,omitempty" json:"metadata,omitempty"`
	CreatedAt    time.Time         `yaml:"created_at" json:"created_at"`
	UpdatedAt    time.Time         `yaml:"updated_at" json:"updated_at"`
}

// PodCapabilities represents the capabilities of a pod
type PodCapabilities struct {
	GPUCount     int      `yaml:"gpu_count" json:"gpu_count"`
	GPUMemory    []int64  `yaml:"gpu_memory" json:"gpu_memory"`
	GPUTypes     []string `yaml:"gpu_types" json:"gpu_types"`
	TotalMemory  int64    `yaml:"total_memory" json:"total_memory"`
	CPUCores     int      `yaml:"cpu_cores" json:"cpu_cores"`
	Architecture string   `yaml:"architecture" json:"architecture"`
	OS           string   `yaml:"os" json:"os"`
	OSVersion    string   `yaml:"os_version" json:"os_version"`
	CUDAVersion  string   `yaml:"cuda_version,omitempty" json:"cuda_version,omitempty"`
	PythonPath   string   `yaml:"python_path,omitempty" json:"python_path,omitempty"`
	VLLMVersion  string   `yaml:"vllm_version,omitempty" json:"vllm_version,omitempty"`
}

// PodConfig represents the configuration for all pods
type PodConfig struct {
	Pods   map[string]*Pod `yaml:"pods" json:"pods"`
	Active string          `yaml:"active" json:"active"`
}

// Validate validates the pod configuration
func (p *Pod) Validate() error {
	if p.Name == "" {
		return ErrInvalidPodName
	}
	if p.Host == "" {
		return ErrInvalidPodHost
	}
	if p.User == "" {
		return ErrInvalidPodUser
	}
	if p.SSHCommand == "" {
		return ErrInvalidSSHCommand
	}
	return nil
}

// IsOnline returns true if the pod is currently online
func (p *Pod) IsOnline() bool {
	return p.Status == PodStatusOnline
}

// IsHealthy returns true if the pod is in a healthy state
func (p *Pod) IsHealthy() bool {
	return p.Status == PodStatusOnline || p.Status == PodStatusMaintenance
}

// GetDisplayName returns a human-readable name for the pod
func (p *Pod) GetDisplayName() string {
	if p.Name != "" {
		return p.Name
	}
	return p.Host
}

// GetConnectionString returns the SSH connection string for the pod
func (p *Pod) GetConnectionString() string {
	if p.SSHCommand != "" {
		return p.SSHCommand
	}
	if p.Port != 0 && p.Port != 22 {
		return fmt.Sprintf("ssh %s@%s -p %d", p.User, p.Host, p.Port)
	}
	return fmt.Sprintf("ssh %s@%s", p.User, p.Host)
}

