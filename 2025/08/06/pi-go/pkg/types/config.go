package types

import (
	"fmt"
	"time"
)

// Config represents the main application configuration
type Config struct {
	Global   GlobalConfig `yaml:"global" json:"global"`
	Pods     PodConfig    `yaml:"pods" json:"pods"`
	Models   ModelConfig  `yaml:"models" json:"models"`
	SSH      SSHConfig    `yaml:"ssh" json:"ssh"`
	TUI      TUIConfig    `yaml:"tui" json:"tui"`
	Logging  LoggingConfig `yaml:"logging" json:"logging"`
	Version  string       `yaml:"version" json:"version"`
	UpdatedAt time.Time   `yaml:"updated_at" json:"updated_at"`
}

// GlobalConfig represents global application settings
type GlobalConfig struct {
	DefaultStoragePath string        `yaml:"default_storage_path" json:"default_storage_path"`
	DefaultUser        string        `yaml:"default_user" json:"default_user"`
	DefaultGPUMemory   float64       `yaml:"default_gpu_memory" json:"default_gpu_memory"`
	DefaultTimeout     time.Duration `yaml:"default_timeout" json:"default_timeout"`
	AutoCleanup        bool          `yaml:"auto_cleanup" json:"auto_cleanup"`
	CheckInterval      time.Duration `yaml:"check_interval" json:"check_interval"`
	MaxRetries         int           `yaml:"max_retries" json:"max_retries"`
	RetryDelay         time.Duration `yaml:"retry_delay" json:"retry_delay"`
}

// SSHConfig represents SSH-specific configuration
type SSHConfig struct {
	DefaultPort        int           `yaml:"default_port" json:"default_port"`
	ConnectTimeout     time.Duration `yaml:"connect_timeout" json:"connect_timeout"`
	KeepAlive          time.Duration `yaml:"keep_alive" json:"keep_alive"`
	MaxConnections     int           `yaml:"max_connections" json:"max_connections"`
	TunnelPortRange    PortRange     `yaml:"tunnel_port_range" json:"tunnel_port_range"`
	PreferredKeyTypes  []string      `yaml:"preferred_key_types" json:"preferred_key_types"`
	StrictHostKeyCheck bool          `yaml:"strict_host_key_check" json:"strict_host_key_check"`
	UserKnownHostsFile string        `yaml:"user_known_hosts_file" json:"user_known_hosts_file"`
}

// PortRange represents a range of ports
type PortRange struct {
	Start int `yaml:"start" json:"start"`
	End   int `yaml:"end" json:"end"`
}

// TUIConfig represents TUI-specific configuration
type TUIConfig struct {
	Theme           string        `yaml:"theme" json:"theme"`
	RefreshInterval time.Duration `yaml:"refresh_interval" json:"refresh_interval"`
	ShowHelp        bool          `yaml:"show_help" json:"show_help"`
	EnableMouse     bool          `yaml:"enable_mouse" json:"enable_mouse"`
	PanelLayout     PanelLayout   `yaml:"panel_layout" json:"panel_layout"`
	Colors          ColorScheme   `yaml:"colors" json:"colors"`
}

// PanelLayout represents the TUI panel layout configuration
type PanelLayout struct {
	Overview PanelConfig `yaml:"overview" json:"overview"`
	Pods     PanelConfig `yaml:"pods" json:"pods"`
	Models   PanelConfig `yaml:"models" json:"models"`
	Logs     PanelConfig `yaml:"logs" json:"logs"`
}

// PanelConfig represents configuration for a single panel
type PanelConfig struct {
	Enabled bool `yaml:"enabled" json:"enabled"`
	Width   int  `yaml:"width" json:"width"`
	Height  int  `yaml:"height" json:"height"`
	X       int  `yaml:"x" json:"x"`
	Y       int  `yaml:"y" json:"y"`
}

// ColorScheme represents the color scheme for the TUI
type ColorScheme struct {
	Primary     string `yaml:"primary" json:"primary"`
	Secondary   string `yaml:"secondary" json:"secondary"`
	Success     string `yaml:"success" json:"success"`
	Warning     string `yaml:"warning" json:"warning"`
	Error       string `yaml:"error" json:"error"`
	Background  string `yaml:"background" json:"background"`
	Foreground  string `yaml:"foreground" json:"foreground"`
	Border      string `yaml:"border" json:"border"`
	Highlight   string `yaml:"highlight" json:"highlight"`
}

// LoggingConfig represents logging configuration
type LoggingConfig struct {
	Level      string `yaml:"level" json:"level"`
	Format     string `yaml:"format" json:"format"`
	Output     string `yaml:"output" json:"output"`
	File       string `yaml:"file,omitempty" json:"file,omitempty"`
	MaxSize    int    `yaml:"max_size" json:"max_size"`
	MaxBackups int    `yaml:"max_backups" json:"max_backups"`
	MaxAge     int    `yaml:"max_age" json:"max_age"`
	Compress   bool   `yaml:"compress" json:"compress"`
}

// DefaultConfig returns a default configuration
func DefaultConfig() *Config {
	return &Config{
		Global: GlobalConfig{
			DefaultStoragePath: "~/.cache/huggingface",
			DefaultUser:        "root",
			DefaultGPUMemory:   0.8,
			DefaultTimeout:     30 * time.Second,
			AutoCleanup:        true,
			CheckInterval:      30 * time.Second,
			MaxRetries:         3,
			RetryDelay:         5 * time.Second,
		},
		SSH: SSHConfig{
			DefaultPort:    22,
			ConnectTimeout: 30 * time.Second,
			KeepAlive:      30 * time.Second,
			MaxConnections: 10,
			TunnelPortRange: PortRange{
				Start: 8001,
				End:   8100,
			},
			PreferredKeyTypes:  []string{"ed25519", "ecdsa", "rsa"},
			StrictHostKeyCheck: false,
			UserKnownHostsFile: "~/.ssh/known_hosts",
		},
		TUI: TUIConfig{
			Theme:           "default",
			RefreshInterval: 2 * time.Second,
			ShowHelp:        true,
			EnableMouse:     true,
			PanelLayout: PanelLayout{
				Overview: PanelConfig{Enabled: true, Width: 50, Height: 10, X: 0, Y: 0},
				Pods:     PanelConfig{Enabled: true, Width: 50, Height: 15, X: 0, Y: 10},
				Models:   PanelConfig{Enabled: true, Width: 50, Height: 15, X: 50, Y: 0},
				Logs:     PanelConfig{Enabled: true, Width: 100, Height: 10, X: 0, Y: 25},
			},
			Colors: ColorScheme{
				Primary:     "#00ff00",
				Secondary:   "#0080ff",
				Success:     "#00ff00",
				Warning:     "#ffff00",
				Error:       "#ff0000",
				Background:  "#000000",
				Foreground:  "#ffffff",
				Border:      "#808080",
				Highlight:   "#ffff00",
			},
		},
		Logging: LoggingConfig{
			Level:      "info",
			Format:     "text",
			Output:     "stdout",
			MaxSize:    100,
			MaxBackups: 3,
			MaxAge:     28,
			Compress:   true,
		},
		Version:   "1.0.0",
		UpdatedAt: time.Now(),
	}
}

// Validate validates the configuration
func (c *Config) Validate() error {
	if c.Global.DefaultGPUMemory <= 0 || c.Global.DefaultGPUMemory > 1.0 {
		return fmt.Errorf("invalid default GPU memory: %f", c.Global.DefaultGPUMemory)
	}
	
	if c.SSH.TunnelPortRange.Start <= 0 || c.SSH.TunnelPortRange.End <= c.SSH.TunnelPortRange.Start {
		return fmt.Errorf("invalid tunnel port range: %d-%d", c.SSH.TunnelPortRange.Start, c.SSH.TunnelPortRange.End)
	}
	
	if c.TUI.RefreshInterval <= 0 {
		return fmt.Errorf("invalid TUI refresh interval: %v", c.TUI.RefreshInterval)
	}
	
	return nil
}

// GetActivePod returns the currently active pod
func (c *Config) GetActivePod() *Pod {
	if c.Pods.Active == "" {
		return nil
	}
	return c.Pods.Pods[c.Pods.Active]
}

// SetActivePod sets the active pod
func (c *Config) SetActivePod(name string) error {
	if _, exists := c.Pods.Pods[name]; !exists {
		return ErrPodNotFound
	}
	c.Pods.Active = name
	c.UpdatedAt = time.Now()
	return nil
}

// AddPod adds a new pod to the configuration
func (c *Config) AddPod(pod *Pod) error {
	if c.Pods.Pods == nil {
		c.Pods.Pods = make(map[string]*Pod)
	}
	
	if _, exists := c.Pods.Pods[pod.Name]; exists {
		return ErrPodAlreadyExists
	}
	
	if err := pod.Validate(); err != nil {
		return err
	}
	
	pod.CreatedAt = time.Now()
	pod.UpdatedAt = time.Now()
	c.Pods.Pods[pod.Name] = pod
	
	// Set as active if it's the first pod
	if c.Pods.Active == "" {
		c.Pods.Active = pod.Name
	}
	
	c.UpdatedAt = time.Now()
	return nil
}

// RemovePod removes a pod from the configuration
func (c *Config) RemovePod(name string) error {
	if _, exists := c.Pods.Pods[name]; !exists {
		return ErrPodNotFound
	}
	
	delete(c.Pods.Pods, name)
	
	// Clear active pod if it was the removed pod
	if c.Pods.Active == name {
		c.Pods.Active = ""
		// Set another pod as active if available
		for podName := range c.Pods.Pods {
			c.Pods.Active = podName
			break
		}
	}
	
	c.UpdatedAt = time.Now()
	return nil
}

// AddModel adds a new model to the configuration
func (c *Config) AddModel(model *Model) error {
	if c.Models.Models == nil {
		c.Models.Models = make(map[string]*Model)
	}
	
	if _, exists := c.Models.Models[model.Name]; exists {
		return ErrModelAlreadyExists
	}
	
	if err := model.Validate(); err != nil {
		return err
	}
	
	model.CreatedAt = time.Now()
	model.UpdatedAt = time.Now()
	c.Models.Models[model.Name] = model
	c.UpdatedAt = time.Now()
	return nil
}

// RemoveModel removes a model from the configuration
func (c *Config) RemoveModel(name string) error {
	if _, exists := c.Models.Models[name]; !exists {
		return ErrModelNotFound
	}
	
	delete(c.Models.Models, name)
	c.UpdatedAt = time.Now()
	return nil
}

