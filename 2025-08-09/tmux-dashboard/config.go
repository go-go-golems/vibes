package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"text/template"

	"gopkg.in/yaml.v3"
)

// Config represents the main YAML configuration structure
type Config struct {
	Version int               `yaml:"version"`
	Session string            `yaml:"session"`
	Include []string          `yaml:"include,omitempty"`
	Vars    map[string]string `yaml:"vars,omitempty"`
	Tabs    []Tab             `yaml:"tabs"`
}

// Tab represents a tmux window configuration
type Tab struct {
	Name   string `yaml:"name"`
	Layout string `yaml:"layout,omitempty"`
	Panes  []Pane `yaml:"panes"`
}

// Pane represents a tmux pane configuration
type Pane struct {
	Cmd     string            `yaml:"cmd"`
	Refresh int               `yaml:"refresh,omitempty"`
	Env     map[string]string `yaml:"env,omitempty"`
}

// ValidLayouts contains the allowed tmux layouts
var ValidLayouts = map[string]bool{
	"tiled":           true,
	"even-vertical":   true,
	"even-horizontal": true,
	"main-vertical":   true,
	"main-horizontal": true,
}

// LoadConfig loads and parses a YAML configuration file
func LoadConfig(filename string) (*Config, error) {
	data, err := os.ReadFile(filename)
	if err != nil {
		return nil, fmt.Errorf("failed to read config file: %w", err)
	}

	var config Config
	if err := yaml.Unmarshal(data, &config); err != nil {
		return nil, fmt.Errorf("failed to parse YAML: %w", err)
	}

	return &config, nil
}

// LoadConfigWithIncludes loads a config file and processes all includes
func LoadConfigWithIncludes(filename string) (*Config, error) {
	baseDir := filepath.Dir(filename)
	config, err := LoadConfig(filename)
	if err != nil {
		return nil, err
	}

	// Process includes
	var allTabs []Tab
	tabNames := make(map[string]bool)

	// First, process included files
	for _, includePath := range config.Include {
		// Resolve relative paths
		if !filepath.IsAbs(includePath) {
			includePath = filepath.Join(baseDir, includePath)
		}

		includedConfig, err := LoadConfigWithIncludes(includePath)
		if err != nil {
			return nil, fmt.Errorf("failed to load included file %s: %w", includePath, err)
		}

		// Add tabs from included file
		for _, tab := range includedConfig.Tabs {
			if tabNames[tab.Name] {
				return nil, fmt.Errorf("duplicate tab name: %s", tab.Name)
			}
			allTabs = append(allTabs, tab)
			tabNames[tab.Name] = true
		}
	}

	// Then add tabs from current file
	for _, tab := range config.Tabs {
		if tabNames[tab.Name] {
			return nil, fmt.Errorf("duplicate tab name: %s", tab.Name)
		}
		allTabs = append(allTabs, tab)
		tabNames[tab.Name] = true
	}

	config.Tabs = allTabs
	return config, nil
}

// Validate validates the configuration
func (c *Config) Validate() error {
	// Check version
	if c.Version != 2 {
		return fmt.Errorf("unsupported version: %d (expected 2)", c.Version)
	}

	// Check session name
	if c.Session == "" {
		return fmt.Errorf("session name is required")
	}

	// Check tabs
	if len(c.Tabs) == 0 {
		return fmt.Errorf("at least one tab is required")
	}

	tabNames := make(map[string]bool)
	for _, tab := range c.Tabs {
		// Check tab name
		if tab.Name == "" {
			return fmt.Errorf("tab name is required")
		}
		if tabNames[tab.Name] {
			return fmt.Errorf("duplicate tab name: %s", tab.Name)
		}
		tabNames[tab.Name] = true

		// Check layout
		if tab.Layout != "" && !ValidLayouts[tab.Layout] {
			return fmt.Errorf("invalid layout '%s' for tab '%s'", tab.Layout, tab.Name)
		}

		// Check panes
		if len(tab.Panes) == 0 {
			return fmt.Errorf("tab '%s' must have at least one pane", tab.Name)
		}

		for i, pane := range tab.Panes {
			if pane.Cmd == "" {
				return fmt.Errorf("pane %d in tab '%s' must have a command", i, tab.Name)
			}
			if pane.Refresh < 0 {
				return fmt.Errorf("pane %d in tab '%s' has invalid refresh interval: %d", i, tab.Name, pane.Refresh)
			}
		}
	}

	return nil
}

// SubstituteVars performs template variable substitution
func (c *Config) SubstituteVars(vars map[string]string) error {
	// Merge vars with config vars (config vars have lower precedence)
	allVars := make(map[string]string)
	for k, v := range c.Vars {
		allVars[k] = v
	}
	for k, v := range vars {
		allVars[k] = v
	}

	// Substitute variables in all command strings
	for tabIdx := range c.Tabs {
		for paneIdx := range c.Tabs[tabIdx].Panes {
			cmd := c.Tabs[tabIdx].Panes[paneIdx].Cmd
			substituted, err := substituteTemplate(cmd, allVars)
			if err != nil {
				return fmt.Errorf("failed to substitute variables in tab '%s', pane %d: %w", 
					c.Tabs[tabIdx].Name, paneIdx, err)
			}
			c.Tabs[tabIdx].Panes[paneIdx].Cmd = substituted
		}
	}

	return nil
}

// substituteTemplate performs template substitution using Go's text/template
func substituteTemplate(tmpl string, vars map[string]string) (string, error) {
	t, err := template.New("cmd").Parse(tmpl)
	if err != nil {
		return "", fmt.Errorf("failed to parse template: %w", err)
	}

	var buf strings.Builder
	if err := t.Execute(&buf, vars); err != nil {
		return "", fmt.Errorf("failed to execute template: %w", err)
	}

	return buf.String(), nil
}

