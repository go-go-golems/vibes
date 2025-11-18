package config

import (
	"os"
	"path/filepath"
	"testing"
)

func TestLoad_ValidConfig(t *testing.T) {
	// Create a temporary config file
	tmpDir := t.TempDir()
	configPath := filepath.Join(tmpDir, "test.yaml")
	
	configContent := `
services:
  - name: "Test Service"
    ports: [8080]
    binary_path: "./test-binary"
    env_vars:
      - "TEST_VAR=value"
global:
  working_directory: "."
  log_buffer_size: 5000
`
	
	if err := os.WriteFile(configPath, []byte(configContent), 0644); err != nil {
		t.Fatalf("Failed to write test config: %v", err)
	}
	
	cfg, err := Load(configPath)
	if err != nil {
		t.Fatalf("Load failed: %v", err)
	}
	
	if len(cfg.Services) != 1 {
		t.Fatalf("Expected 1 service, got %d", len(cfg.Services))
	}
	
	svc := cfg.Services[0]
	if svc.Name != "Test Service" {
		t.Errorf("Expected name 'Test Service', got '%s'", svc.Name)
	}
	if len(svc.Ports) != 1 || svc.Ports[0] != 8080 {
		t.Errorf("Expected port 8080, got %v", svc.Ports)
	}
	if svc.BinaryPath != "./test-binary" {
		t.Errorf("Expected binary_path './test-binary', got '%s'", svc.BinaryPath)
	}
	if cfg.Global.LogBufferSize != 5000 {
		t.Errorf("Expected global log_buffer_size 5000, got %d", cfg.Global.LogBufferSize)
	}
}

func TestLoad_ArgsAsString(t *testing.T) {
	tmpDir := t.TempDir()
	configPath := filepath.Join(tmpDir, "test.yaml")
	
	configContent := `
services:
  - name: "Test Service"
    ports: [8080]
    binary_path: "./test-binary"
    args: "--port 8080 --debug"
`
	
	if err := os.WriteFile(configPath, []byte(configContent), 0644); err != nil {
		t.Fatalf("Failed to write test config: %v", err)
	}
	
	cfg, err := Load(configPath)
	if err != nil {
		t.Fatalf("Load failed: %v", err)
	}
	
	svc := cfg.Services[0]
	if len(svc.ArgsList) != 3 {
		t.Errorf("Expected 3 args, got %d: %v", len(svc.ArgsList), svc.ArgsList)
	}
	expected := []string{"--port", "8080", "--debug"}
	for i, arg := range expected {
		if i >= len(svc.ArgsList) || svc.ArgsList[i] != arg {
			t.Errorf("Expected arg[%d] '%s', got '%s'", i, arg, svc.ArgsList[i])
		}
	}
}

func TestLoad_ArgsAsList(t *testing.T) {
	tmpDir := t.TempDir()
	configPath := filepath.Join(tmpDir, "test.yaml")
	
	configContent := `
services:
  - name: "Test Service"
    ports: [8080]
    binary_path: "./test-binary"
    args: ["--port", "8080", "--debug"]
`
	
	if err := os.WriteFile(configPath, []byte(configContent), 0644); err != nil {
		t.Fatalf("Failed to write test config: %v", err)
	}
	
	cfg, err := Load(configPath)
	if err != nil {
		t.Fatalf("Load failed: %v", err)
	}
	
	svc := cfg.Services[0]
	if len(svc.ArgsList) != 3 {
		t.Errorf("Expected 3 args, got %d: %v", len(svc.ArgsList), svc.ArgsList)
	}
	expected := []string{"--port", "8080", "--debug"}
	for i, arg := range expected {
		if i >= len(svc.ArgsList) || svc.ArgsList[i] != arg {
			t.Errorf("Expected arg[%d] '%s', got '%s'", i, arg, svc.ArgsList[i])
		}
	}
}

func TestNormalize_PortToPorts(t *testing.T) {
	svc := ServiceConfig{
		Port:  8080,
		Ports: nil,
	}
	svc.Normalize()
	
	if len(svc.Ports) != 1 || svc.Ports[0] != 8080 {
		t.Errorf("Expected ports [8080], got %v", svc.Ports)
	}
	if svc.Port != 0 {
		t.Errorf("Expected Port to be cleared, got %d", svc.Port)
	}
}

func TestNormalize_MergePorts(t *testing.T) {
	svc := ServiceConfig{
		Port:  8080,
		Ports: []int{9090},
	}
	svc.Normalize()
	
	if len(svc.Ports) != 2 {
		t.Errorf("Expected 2 ports, got %d: %v", len(svc.Ports), svc.Ports)
	}
	// Check both ports are present
	has8080 := false
	has9090 := false
	for _, p := range svc.Ports {
		if p == 8080 {
			has8080 = true
		}
		if p == 9090 {
			has9090 = true
		}
	}
	if !has8080 || !has9090 {
		t.Errorf("Expected ports [8080, 9090], got %v", svc.Ports)
	}
}

func TestNormalize_DeduplicatePorts(t *testing.T) {
	svc := ServiceConfig{
		Port:  8080,
		Ports: []int{8080, 9090},
	}
	svc.Normalize()
	
	if len(svc.Ports) != 2 {
		t.Errorf("Expected 2 ports after deduplication, got %d: %v", len(svc.Ports), svc.Ports)
	}
}

func TestValidate_MissingName(t *testing.T) {
	cfg := &AppConfig{
		Services: []ServiceConfig{
			{
				Ports:      []int{8080},
				BinaryPath: "./test",
			},
		},
	}
	
	err := cfg.Validate()
	if err == nil {
		t.Error("Expected validation error for missing name")
	}
}

func TestValidate_MissingPorts(t *testing.T) {
	cfg := &AppConfig{
		Services: []ServiceConfig{
			{
				Name:       "Test",
				BinaryPath: "./test",
			},
		},
	}
	
	err := cfg.Validate()
	if err == nil {
		t.Error("Expected validation error for missing ports")
	}
}

func TestValidate_MissingBinaryPath(t *testing.T) {
	cfg := &AppConfig{
		Services: []ServiceConfig{
			{
				Name:  "Test",
				Ports: []int{8080},
			},
		},
	}
	
	err := cfg.Validate()
	if err == nil {
		t.Error("Expected validation error for missing binary_path")
	}
}

func TestValidate_InvalidPort(t *testing.T) {
	cfg := &AppConfig{
		Services: []ServiceConfig{
			{
				Name:       "Test",
				Ports:      []int{0},
				BinaryPath: "./test",
			},
		},
	}
	
	err := cfg.Validate()
	if err == nil {
		t.Error("Expected validation error for invalid port")
	}
	
	cfg.Services[0].Ports = []int{70000}
	err = cfg.Validate()
	if err == nil {
		t.Error("Expected validation error for port > 65535")
	}
}

func TestValidate_DuplicateServiceNames(t *testing.T) {
	cfg := &AppConfig{
		Services: []ServiceConfig{
			{
				Name:       "Test",
				Ports:      []int{8080},
				BinaryPath: "./test1",
			},
			{
				Name:       "Test",
				Ports:      []int{9090},
				BinaryPath: "./test2",
			},
		},
	}
	
	err := cfg.Validate()
	if err == nil {
		t.Error("Expected validation error for duplicate service names")
	}
}

func TestValidate_DuplicatePorts(t *testing.T) {
	cfg := &AppConfig{
		Services: []ServiceConfig{
			{
				Name:       "Test1",
				Ports:      []int{8080},
				BinaryPath: "./test1",
			},
			{
				Name:       "Test2",
				Ports:      []int{8080},
				BinaryPath: "./test2",
			},
		},
	}
	
	err := cfg.Validate()
	if err == nil {
		t.Error("Expected validation error for duplicate ports")
	}
}

func TestGetServiceWorkingDirectory(t *testing.T) {
	cfg := &AppConfig{
		Global: GlobalConfig{
			WorkingDirectory: "/global",
		},
		Services: []ServiceConfig{
			{
				WorkingDirectory: "/service",
			},
			{
				// No working directory
			},
		},
	}
	
	// Service with explicit working directory
	dir := cfg.GetServiceWorkingDirectory(&cfg.Services[0])
	if dir != "/service" {
		t.Errorf("Expected '/service', got '%s'", dir)
	}
	
	// Service without explicit working directory
	dir = cfg.GetServiceWorkingDirectory(&cfg.Services[1])
	if dir != "/global" {
		t.Errorf("Expected '/global', got '%s'", dir)
	}
	
	// No global working directory
	cfg.Global.WorkingDirectory = ""
	dir = cfg.GetServiceWorkingDirectory(&cfg.Services[1])
	if dir != "." {
		t.Errorf("Expected '.', got '%s'", dir)
	}
}

func TestGetServiceLogBufferSize(t *testing.T) {
	cfg := &AppConfig{
		Global: GlobalConfig{
			LogBufferSize: 5000,
		},
		Services: []ServiceConfig{
			{
				LogBufferSize: 2000,
			},
			{
				// No log buffer size
			},
		},
	}
	
	// Service with explicit log buffer size
	size := cfg.GetServiceLogBufferSize(&cfg.Services[0])
	if size != 2000 {
		t.Errorf("Expected 2000, got %d", size)
	}
	
	// Service without explicit log buffer size
	size = cfg.GetServiceLogBufferSize(&cfg.Services[1])
	if size != 5000 {
		t.Errorf("Expected 5000, got %d", size)
	}
	
	// No global log buffer size
	cfg.Global.LogBufferSize = 0
	size = cfg.GetServiceLogBufferSize(&cfg.Services[1])
	if size != 1000 {
		t.Errorf("Expected default 1000, got %d", size)
	}
}

func TestGetGlobalLogBufferSize(t *testing.T) {
	cfg := &AppConfig{
		Global: GlobalConfig{
			LogBufferSize: 5000,
		},
	}
	
	size := cfg.GetGlobalLogBufferSize()
	if size != 5000 {
		t.Errorf("Expected 5000, got %d", size)
	}
	
	cfg.Global.LogBufferSize = 0
	size = cfg.GetGlobalLogBufferSize()
	if size != 10000 {
		t.Errorf("Expected default 10000, got %d", size)
	}
}

