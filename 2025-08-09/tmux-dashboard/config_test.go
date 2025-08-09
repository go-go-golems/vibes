package main

import (
	"os"
	"path/filepath"
	"testing"
)

func TestLoadConfig(t *testing.T) {
	// Create a temporary config file
	configContent := `version: 2
session: "test-session"
vars:
  host: "localhost"
  port: "8080"
tabs:
  - name: "overview"
    layout: tiled
    panes:
      - cmd: "uptime"
        refresh: 5
      - cmd: "df -h"
        refresh: 10
  - name: "logs"
    layout: even-vertical
    panes:
      - cmd: "tail -f /var/log/syslog"
`

	tmpDir := t.TempDir()
	configFile := filepath.Join(tmpDir, "test-config.yml")
	
	if err := os.WriteFile(configFile, []byte(configContent), 0644); err != nil {
		t.Fatalf("Failed to write test config: %v", err)
	}

	config, err := LoadConfig(configFile)
	if err != nil {
		t.Fatalf("Failed to load config: %v", err)
	}

	// Test basic fields
	if config.Version != 2 {
		t.Errorf("Expected version 2, got %d", config.Version)
	}

	if config.Session != "test-session" {
		t.Errorf("Expected session 'test-session', got '%s'", config.Session)
	}

	// Test vars
	if config.Vars["host"] != "localhost" {
		t.Errorf("Expected host 'localhost', got '%s'", config.Vars["host"])
	}

	// Test tabs
	if len(config.Tabs) != 2 {
		t.Errorf("Expected 2 tabs, got %d", len(config.Tabs))
	}

	if config.Tabs[0].Name != "overview" {
		t.Errorf("Expected first tab name 'overview', got '%s'", config.Tabs[0].Name)
	}

	if config.Tabs[0].Layout != "tiled" {
		t.Errorf("Expected first tab layout 'tiled', got '%s'", config.Tabs[0].Layout)
	}

	// Test panes
	if len(config.Tabs[0].Panes) != 2 {
		t.Errorf("Expected 2 panes in first tab, got %d", len(config.Tabs[0].Panes))
	}

	if config.Tabs[0].Panes[0].Cmd != "uptime" {
		t.Errorf("Expected first pane cmd 'uptime', got '%s'", config.Tabs[0].Panes[0].Cmd)
	}

	if config.Tabs[0].Panes[0].Refresh != 5 {
		t.Errorf("Expected first pane refresh 5, got %d", config.Tabs[0].Panes[0].Refresh)
	}
}

func TestConfigValidation(t *testing.T) {
	tests := []struct {
		name        string
		config      Config
		expectError bool
		errorMsg    string
	}{
		{
			name: "valid config",
			config: Config{
				Version: 2,
				Session: "test",
				Tabs: []Tab{
					{
						Name:   "tab1",
						Layout: "tiled",
						Panes: []Pane{
							{Cmd: "echo hello"},
						},
					},
				},
			},
			expectError: false,
		},
		{
			name: "invalid version",
			config: Config{
				Version: 1,
				Session: "test",
				Tabs: []Tab{
					{
						Name: "tab1",
						Panes: []Pane{
							{Cmd: "echo hello"},
						},
					},
				},
			},
			expectError: true,
			errorMsg:    "unsupported version",
		},
		{
			name: "missing session",
			config: Config{
				Version: 2,
				Tabs: []Tab{
					{
						Name: "tab1",
						Panes: []Pane{
							{Cmd: "echo hello"},
						},
					},
				},
			},
			expectError: true,
			errorMsg:    "session name is required",
		},
		{
			name: "invalid layout",
			config: Config{
				Version: 2,
				Session: "test",
				Tabs: []Tab{
					{
						Name:   "tab1",
						Layout: "invalid-layout",
						Panes: []Pane{
							{Cmd: "echo hello"},
						},
					},
				},
			},
			expectError: true,
			errorMsg:    "invalid layout",
		},
		{
			name: "duplicate tab names",
			config: Config{
				Version: 2,
				Session: "test",
				Tabs: []Tab{
					{
						Name: "tab1",
						Panes: []Pane{
							{Cmd: "echo hello"},
						},
					},
					{
						Name: "tab1",
						Panes: []Pane{
							{Cmd: "echo world"},
						},
					},
				},
			},
			expectError: true,
			errorMsg:    "duplicate tab name",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := tt.config.Validate()
			if tt.expectError {
				if err == nil {
					t.Errorf("Expected error containing '%s', got nil", tt.errorMsg)
				} else if !contains(err.Error(), tt.errorMsg) {
					t.Errorf("Expected error containing '%s', got '%s'", tt.errorMsg, err.Error())
				}
			} else {
				if err != nil {
					t.Errorf("Expected no error, got %v", err)
				}
			}
		})
	}
}

func TestSubstituteVars(t *testing.T) {
	config := &Config{
		Version: 2,
		Session: "test",
		Vars: map[string]string{
			"service": "my-app",
			"host":    "localhost",
		},
		Tabs: []Tab{
			{
				Name: "logs",
				Panes: []Pane{
					{Cmd: "journalctl -u {{.service}} -f"},
					{Cmd: "ssh {{.host}} 'tail -f /var/log/{{.service}}.log'"},
				},
			},
		},
	}

	vars := map[string]string{
		"host": "production-server", // Override config var
		"port": "8080",              // New var
	}

	err := config.SubstituteVars(vars)
	if err != nil {
		t.Fatalf("Failed to substitute vars: %v", err)
	}

	expectedCmds := []string{
		"journalctl -u my-app -f",
		"ssh production-server 'tail -f /var/log/my-app.log'",
	}

	for i, expectedCmd := range expectedCmds {
		actualCmd := config.Tabs[0].Panes[i].Cmd
		if actualCmd != expectedCmd {
			t.Errorf("Expected cmd '%s', got '%s'", expectedCmd, actualCmd)
		}
	}
}

func contains(s, substr string) bool {
	return len(s) >= len(substr) && (s == substr || 
		(len(s) > len(substr) && 
			(s[:len(substr)] == substr || 
			 s[len(s)-len(substr):] == substr ||
			 containsSubstring(s, substr))))
}

func containsSubstring(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}

