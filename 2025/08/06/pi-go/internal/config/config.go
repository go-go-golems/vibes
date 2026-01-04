package config

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/pi-go/pi/pkg/types"
	"github.com/spf13/viper"
	"gopkg.in/yaml.v3"
)

const (
	ConfigFileName = "pi-go"
	ConfigFileType = "yaml"
)

var (
	configPaths = []string{
		".",
		"$HOME/.config/pi-go",
		"$HOME/.pi-go",
		"/etc/pi-go",
	}
)

// Load loads the configuration from various sources
func Load() (*types.Config, error) {
	v := viper.New()
	
	// Set configuration file name and type
	v.SetConfigName(ConfigFileName)
	v.SetConfigType(ConfigFileType)
	
	// Add configuration paths
	for _, path := range configPaths {
		v.AddConfigPath(os.ExpandEnv(path))
	}
	
	// Set environment variable prefix
	v.SetEnvPrefix("PI")
	v.SetEnvKeyReplacer(strings.NewReplacer(".", "_", "-", "_"))
	v.AutomaticEnv()
	
	// Set defaults
	setDefaults(v)
	
	// Try to read configuration file
	if err := v.ReadInConfig(); err != nil {
		if _, ok := err.(viper.ConfigFileNotFoundError); !ok {
			return nil, fmt.Errorf("failed to read config file: %w", err)
		}
		// Config file not found is OK, we'll use defaults
	}
	
	// Start with default config and override with viper values
	config := types.DefaultConfig()
	
	// Unmarshal viper values into config struct
	if err := v.Unmarshal(config); err != nil {
		return nil, fmt.Errorf("failed to unmarshal config: %w", err)
	}
	
	// Validate configuration
	if err := config.Validate(); err != nil {
		return nil, fmt.Errorf("invalid configuration: %w", err)
	}
	
	// Initialize maps if nil
	if config.Pods.Pods == nil {
		config.Pods.Pods = make(map[string]*types.Pod)
	}
	if config.Models.Models == nil {
		config.Models.Models = make(map[string]*types.Model)
	}
	if config.Models.Templates == nil {
		config.Models.Templates = make(map[string]*types.ModelTemplate)
	}
	
	return config, nil
}

// Save saves the configuration to the default location
func Save(config *types.Config) error {
	configDir := os.ExpandEnv("$HOME/.config/pi-go")
	if err := os.MkdirAll(configDir, 0755); err != nil {
		return fmt.Errorf("failed to create config directory: %w", err)
	}
	
	configFile := filepath.Join(configDir, ConfigFileName+"."+ConfigFileType)
	
	// Update timestamp
	config.UpdatedAt = time.Now()
	
	// Marshal to YAML
	data, err := yaml.Marshal(config)
	if err != nil {
		return fmt.Errorf("failed to marshal config: %w", err)
	}
	
	// Write to file
	if err := os.WriteFile(configFile, data, 0644); err != nil {
		return fmt.Errorf("failed to write config file: %w", err)
	}
	
	return nil
}

// GetConfigPath returns the path to the configuration file
func GetConfigPath() string {
	configDir := os.ExpandEnv("$HOME/.config/pi-go")
	return filepath.Join(configDir, ConfigFileName+"."+ConfigFileType)
}

// setDefaults sets default configuration values
func setDefaults(v *viper.Viper) {
	// Global defaults
	v.SetDefault("global.default_storage_path", "~/.cache/huggingface")
	v.SetDefault("global.default_user", "root")
	v.SetDefault("global.default_gpu_memory", 0.8)
	v.SetDefault("global.default_timeout", "30s")
	v.SetDefault("global.auto_cleanup", true)
	v.SetDefault("global.check_interval", "30s")
	v.SetDefault("global.max_retries", 3)
	v.SetDefault("global.retry_delay", "5s")
	
	// SSH defaults
	v.SetDefault("ssh.default_port", 22)
	v.SetDefault("ssh.connect_timeout", "30s")
	v.SetDefault("ssh.keep_alive", "30s")
	v.SetDefault("ssh.max_connections", 10)
	v.SetDefault("ssh.tunnel_port_range.start", 8001)
	v.SetDefault("ssh.tunnel_port_range.end", 8100)
	v.SetDefault("ssh.preferred_key_types", []string{"ed25519", "ecdsa", "rsa"})
	v.SetDefault("ssh.strict_host_key_check", false)
	v.SetDefault("ssh.user_known_hosts_file", "~/.ssh/known_hosts")
	
	// TUI defaults
	v.SetDefault("tui.theme", "default")
	v.SetDefault("tui.refresh_interval", "2s")
	v.SetDefault("tui.show_help", true)
	v.SetDefault("tui.enable_mouse", true)
	
	// Logging defaults
	v.SetDefault("logging.level", "info")
	v.SetDefault("logging.format", "text")
	v.SetDefault("logging.output", "stdout")
	v.SetDefault("logging.max_size", 100)
	v.SetDefault("logging.max_backups", 3)
	v.SetDefault("logging.max_age", 28)
	v.SetDefault("logging.compress", true)
	
	// Version
	v.SetDefault("version", "1.0.0")
}

