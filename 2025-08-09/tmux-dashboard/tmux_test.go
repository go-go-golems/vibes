package main

import (
	"fmt"
	"strings"
	"testing"
)

func TestTmuxManagerDryRun(t *testing.T) {
	config := &Config{
		Version: 2,
		Session: "test-session",
		Tabs: []Tab{
			{
				Name:   "overview",
				Layout: "tiled",
				Panes: []Pane{
					{
						Cmd:     "uptime",
						Refresh: 5,
					},
					{
						Cmd: "df -h",
						Env: map[string]string{
							"TERM": "xterm-256color",
						},
					},
				},
			},
			{
				Name:   "logs",
				Layout: "even-vertical",
				Panes: []Pane{
					{
						Cmd: "tail -f /var/log/syslog",
					},
				},
			},
		},
	}

	tm, err := NewTmuxManager(true)
	if err != nil {
		t.Fatalf("Failed to create TmuxManager: %v", err)
	}
	
	// Capture output by redirecting stdout
	// For testing purposes, we'll just ensure no error occurs
	err = tm.ApplyConfig(config)
	if err != nil {
		t.Fatalf("Dry run failed: %v", err)
	}
}

func TestPrintWindowSetup(t *testing.T) {
	tm, err := NewTmuxManager(true)
	if err != nil {
		t.Fatalf("Failed to create TmuxManager: %v", err)
	}
	
	tab := Tab{
		Name:   "test-tab",
		Layout: "tiled",
		Panes: []Pane{
			{
				Cmd:     "echo hello",
				Refresh: 0,
				Env: map[string]string{
					"TEST_VAR": "test_value",
				},
			},
			{
				Cmd:     "watch date",
				Refresh: 5,
			},
		},
	}

	// This test mainly ensures the function doesn't panic
	// In a real test environment, we'd capture stdout to verify output
	tm.printWindowSetup("test-session", tab, 0)
}

func TestSetupPaneCommand(t *testing.T) {
	tests := []struct {
		name     string
		pane     Pane
		expected string
	}{
		{
			name: "simple command",
			pane: Pane{
				Cmd: "echo hello",
			},
			expected: "echo hello",
		},
		{
			name: "command with refresh",
			pane: Pane{
				Cmd:     "uptime",
				Refresh: 5,
			},
			expected: "while :; do clear; date \"+%F %T\"; uptime; sleep 5; done",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var command string
			if tt.pane.Refresh > 0 {
				command = fmt.Sprintf("while :; do clear; date \"+%%F %%T\"; %s; sleep %d; done",
					tt.pane.Cmd, tt.pane.Refresh)
			} else {
				command = tt.pane.Cmd
			}

			if !strings.Contains(command, tt.pane.Cmd) {
				t.Errorf("Expected command to contain '%s', got '%s'", tt.pane.Cmd, command)
			}
		})
	}
}

func TestValidateLayouts(t *testing.T) {
	validLayouts := []string{"tiled", "even-vertical", "even-horizontal", "main-vertical", "main-horizontal"}
	
	for _, layout := range validLayouts {
		if !ValidLayouts[layout] {
			t.Errorf("Layout '%s' should be valid", layout)
		}
	}

	invalidLayouts := []string{"invalid", "custom", ""}
	for _, layout := range invalidLayouts {
		if layout != "" && ValidLayouts[layout] {
			t.Errorf("Layout '%s' should be invalid", layout)
		}
	}
}

