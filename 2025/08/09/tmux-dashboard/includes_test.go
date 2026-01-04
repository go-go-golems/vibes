package main

import (
	"os"
	"path/filepath"
	"testing"
)

func TestLoadConfigWithIncludes(t *testing.T) {
	tmpDir := t.TempDir()

	// Create included config file
	includedContent := `version: 2
session: "included-session"
tabs:
  - name: "network"
    layout: even-horizontal
    panes:
      - cmd: "netstat -tuln"
        refresh: 5
      - cmd: "ss -tuln"
        refresh: 5
  - name: "processes"
    panes:
      - cmd: "ps aux"
        refresh: 10
`

	includedFile := filepath.Join(tmpDir, "network.yml")
	if err := os.WriteFile(includedFile, []byte(includedContent), 0644); err != nil {
		t.Fatalf("Failed to write included config: %v", err)
	}

	// Create main config file with include
	mainContent := `version: 2
session: "main-session"
include:
  - network.yml
vars:
  service: "my-app"
tabs:
  - name: "overview"
    layout: tiled
    panes:
      - cmd: "uptime"
        refresh: 5
      - cmd: "df -h"
        refresh: 10
  - name: "logs"
    panes:
      - cmd: "journalctl -u {{.service}} -f"
`

	mainFile := filepath.Join(tmpDir, "main.yml")
	if err := os.WriteFile(mainFile, []byte(mainContent), 0644); err != nil {
		t.Fatalf("Failed to write main config: %v", err)
	}

	// Load config with includes
	config, err := LoadConfigWithIncludes(mainFile)
	if err != nil {
		t.Fatalf("Failed to load config with includes: %v", err)
	}

	// Should have 4 tabs total: 2 from included file + 2 from main file
	expectedTabs := []string{"network", "processes", "overview", "logs"}
	if len(config.Tabs) != len(expectedTabs) {
		t.Errorf("Expected %d tabs, got %d", len(expectedTabs), len(config.Tabs))
	}

	for i, expectedName := range expectedTabs {
		if i >= len(config.Tabs) {
			t.Errorf("Missing tab %d: %s", i, expectedName)
			continue
		}
		if config.Tabs[i].Name != expectedName {
			t.Errorf("Expected tab %d name '%s', got '%s'", i, expectedName, config.Tabs[i].Name)
		}
	}

	// Check that the session name from main file is used
	if config.Session != "main-session" {
		t.Errorf("Expected session 'main-session', got '%s'", config.Session)
	}

	// Check that vars from main file are preserved
	if config.Vars["service"] != "my-app" {
		t.Errorf("Expected service 'my-app', got '%s'", config.Vars["service"])
	}
}

func TestLoadConfigWithIncludesDuplicateTabNames(t *testing.T) {
	tmpDir := t.TempDir()

	// Create included config file with duplicate tab name
	includedContent := `version: 2
session: "included-session"
tabs:
  - name: "overview"  # This will conflict with main file
    panes:
      - cmd: "echo included"
`

	includedFile := filepath.Join(tmpDir, "duplicate.yml")
	if err := os.WriteFile(includedFile, []byte(includedContent), 0644); err != nil {
		t.Fatalf("Failed to write included config: %v", err)
	}

	// Create main config file
	mainContent := `version: 2
session: "main-session"
include:
  - duplicate.yml
tabs:
  - name: "overview"  # This will conflict with included file
    panes:
      - cmd: "echo main"
`

	mainFile := filepath.Join(tmpDir, "main.yml")
	if err := os.WriteFile(mainFile, []byte(mainContent), 0644); err != nil {
		t.Fatalf("Failed to write main config: %v", err)
	}

	// Should fail due to duplicate tab names
	_, err := LoadConfigWithIncludes(mainFile)
	if err == nil {
		t.Error("Expected error due to duplicate tab names, got nil")
	}
	if !contains(err.Error(), "duplicate tab name") {
		t.Errorf("Expected error about duplicate tab name, got: %v", err)
	}
}

func TestLoadConfigWithNestedIncludes(t *testing.T) {
	tmpDir := t.TempDir()

	// Create deeply nested include
	deepContent := `version: 2
session: "deep"
tabs:
  - name: "deep-tab"
    panes:
      - cmd: "echo deep"
`

	deepFile := filepath.Join(tmpDir, "deep.yml")
	if err := os.WriteFile(deepFile, []byte(deepContent), 0644); err != nil {
		t.Fatalf("Failed to write deep config: %v", err)
	}

	// Create middle include that includes deep
	middleContent := `version: 2
session: "middle"
include:
  - deep.yml
tabs:
  - name: "middle-tab"
    panes:
      - cmd: "echo middle"
`

	middleFile := filepath.Join(tmpDir, "middle.yml")
	if err := os.WriteFile(middleFile, []byte(middleContent), 0644); err != nil {
		t.Fatalf("Failed to write middle config: %v", err)
	}

	// Create main config that includes middle
	mainContent := `version: 2
session: "main"
include:
  - middle.yml
tabs:
  - name: "main-tab"
    panes:
      - cmd: "echo main"
`

	mainFile := filepath.Join(tmpDir, "main.yml")
	if err := os.WriteFile(mainFile, []byte(mainContent), 0644); err != nil {
		t.Fatalf("Failed to write main config: %v", err)
	}

	// Load config with nested includes
	config, err := LoadConfigWithIncludes(mainFile)
	if err != nil {
		t.Fatalf("Failed to load config with nested includes: %v", err)
	}

	// Should have 3 tabs: deep-tab, middle-tab, main-tab
	expectedTabs := []string{"deep-tab", "middle-tab", "main-tab"}
	if len(config.Tabs) != len(expectedTabs) {
		t.Errorf("Expected %d tabs, got %d", len(expectedTabs), len(config.Tabs))
	}

	for i, expectedName := range expectedTabs {
		if i >= len(config.Tabs) {
			t.Errorf("Missing tab %d: %s", i, expectedName)
			continue
		}
		if config.Tabs[i].Name != expectedName {
			t.Errorf("Expected tab %d name '%s', got '%s'", i, expectedName, config.Tabs[i].Name)
		}
	}
}

