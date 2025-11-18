package config

import (
	"fmt"
	"os"
	"strings"

	"gopkg.in/yaml.v3"
)

type AppConfig struct {
	Global   GlobalConfig   `yaml:"global"`
	Services []ServiceConfig `yaml:"services"`
}

type GlobalConfig struct {
	WorkingDirectory string `yaml:"working_directory"`
	LogBufferSize    int    `yaml:"log_buffer_size"`
}

type ServiceConfig struct {
	Name             string   `yaml:"name"`
	Ports            []int    `yaml:"ports"`
	Port             int      `yaml:"port"`
	BinaryPath       string   `yaml:"binary_path"`
	WorkingDirectory string   `yaml:"working_directory"`
	Args             string   `yaml:"args"` // Can be string or list, handled in UnmarshalYAML
	ArgsList         []string `yaml:"-"`    // Parsed args list
	EnvVars          []string `yaml:"env_vars"`
	LogBufferSize    int      `yaml:"log_buffer_size"`
}

// UnmarshalYAML custom unmarshaler to handle args as both string and list
func (sc *ServiceConfig) UnmarshalYAML(unmarshal func(interface{}) error) error {
	// Define a temporary struct to capture raw YAML
	type rawServiceConfig struct {
		Name             string   `yaml:"name"`
		Ports            []int    `yaml:"ports"`
		Port             int      `yaml:"port"`
		BinaryPath       string   `yaml:"binary_path"`
		WorkingDirectory string   `yaml:"working_directory"`
		Args             interface{} `yaml:"args"` // Use interface{} to handle both string and list
		EnvVars          []string `yaml:"env_vars"`
		LogBufferSize    int      `yaml:"log_buffer_size"`
	}

	var raw rawServiceConfig
	if err := unmarshal(&raw); err != nil {
		return err
	}

	sc.Name = raw.Name
	sc.Ports = raw.Ports
	sc.Port = raw.Port
	sc.BinaryPath = raw.BinaryPath
	sc.WorkingDirectory = raw.WorkingDirectory
	sc.EnvVars = raw.EnvVars
	sc.LogBufferSize = raw.LogBufferSize

	// Handle args: can be string or list
	if raw.Args != nil {
		switch v := raw.Args.(type) {
		case string:
			// Split string into args list
			sc.Args = v
			sc.ArgsList = strings.Fields(v)
		case []interface{}:
			// Convert list to string slice
			argsList := make([]string, 0, len(v))
			for _, item := range v {
				if str, ok := item.(string); ok {
					argsList = append(argsList, str)
				}
			}
			sc.ArgsList = argsList
			// Also store as space-separated string for reference
			sc.Args = strings.Join(argsList, " ")
		case []string:
			// Direct string slice
			sc.ArgsList = v
			sc.Args = strings.Join(v, " ")
		}
	}

	return nil
}

// Normalize normalizes ports (merges Port into Ports, deduplicates)
func (sc *ServiceConfig) Normalize() {
	portSet := make(map[int]bool)
	
	// Add ports from Ports field
	for _, p := range sc.Ports {
		portSet[p] = true
	}
	
	// Add port from Port field if set
	if sc.Port > 0 {
		portSet[sc.Port] = true
	}
	
	// Rebuild Ports list
	sc.Ports = make([]int, 0, len(portSet))
	for p := range portSet {
		sc.Ports = append(sc.Ports, p)
	}
	
	// Clear Port field after normalization
	sc.Port = 0
}

// Load loads and parses a YAML configuration file
func Load(path string) (*AppConfig, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("failed to read config file: %w", err)
	}

	var cfg AppConfig
	if err := yaml.Unmarshal(data, &cfg); err != nil {
		return nil, fmt.Errorf("failed to parse YAML: %w", err)
	}

	// Normalize all service configs
	for i := range cfg.Services {
		cfg.Services[i].Normalize()
	}

	// Validate configuration
	if err := cfg.Validate(); err != nil {
		return nil, fmt.Errorf("validation failed: %w", err)
	}

	return &cfg, nil
}

// Validate validates the configuration
func (ac *AppConfig) Validate() error {
	if len(ac.Services) == 0 {
		return fmt.Errorf("at least one service must be defined")
	}

	serviceNames := make(map[string]bool)
	allPorts := make(map[int]string) // port -> service name

	for i, svc := range ac.Services {
		// Validate required fields
		if svc.Name == "" {
			return fmt.Errorf("service[%d]: name is required", i)
		}
		if len(svc.Ports) == 0 {
			return fmt.Errorf("service[%d] (%s): at least one port is required", i, svc.Name)
		}
		if svc.BinaryPath == "" {
			return fmt.Errorf("service[%d] (%s): binary_path is required", i, svc.Name)
		}

		// Check for duplicate service names
		if serviceNames[svc.Name] {
			return fmt.Errorf("duplicate service name: %s", svc.Name)
		}
		serviceNames[svc.Name] = true

		// Validate ports
		for _, port := range svc.Ports {
			if port < 1 || port > 65535 {
				return fmt.Errorf("service[%d] (%s): invalid port %d (must be 1-65535)", i, svc.Name, port)
			}

			// Check for duplicate ports across services
			if existingSvc, exists := allPorts[port]; exists {
				return fmt.Errorf("port %d is used by both '%s' and '%s'", port, existingSvc, svc.Name)
			}
			allPorts[port] = svc.Name
		}

		// Validate working directory if set
		if svc.WorkingDirectory != "" {
			if info, err := os.Stat(svc.WorkingDirectory); err != nil {
				// Warn but don't fail - might be created later
				// Could be enhanced to check if parent exists
			} else if !info.IsDir() {
				return fmt.Errorf("service[%d] (%s): working_directory '%s' is not a directory", i, svc.Name, svc.WorkingDirectory)
			}
		}

		// Validate log buffer size if set
		if svc.LogBufferSize < 0 {
			return fmt.Errorf("service[%d] (%s): log_buffer_size must be non-negative", i, svc.Name)
		}
	}

	// Validate global config
	if ac.Global.LogBufferSize < 0 {
		return fmt.Errorf("global.log_buffer_size must be non-negative")
	}

	return nil
}

// GetServiceWorkingDirectory returns the working directory for a service with fallback
func (ac *AppConfig) GetServiceWorkingDirectory(svc *ServiceConfig) string {
	if svc.WorkingDirectory != "" {
		return svc.WorkingDirectory
	}
	if ac.Global.WorkingDirectory != "" {
		return ac.Global.WorkingDirectory
	}
	return "." // Default to current directory
}

// GetServiceLogBufferSize returns the log buffer size for a service with fallback
func (ac *AppConfig) GetServiceLogBufferSize(svc *ServiceConfig) int {
	if svc.LogBufferSize > 0 {
		return svc.LogBufferSize
	}
	if ac.Global.LogBufferSize > 0 {
		return ac.Global.LogBufferSize
	}
	return 1000 // Default
}

// GetGlobalLogBufferSize returns the global log buffer size with fallback
func (ac *AppConfig) GetGlobalLogBufferSize() int {
	if ac.Global.LogBufferSize > 0 {
		return ac.Global.LogBufferSize
	}
	return 10000 // Default
}

