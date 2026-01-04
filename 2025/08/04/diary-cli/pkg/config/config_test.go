package config

import (
	"testing"
	"time"
)

func TestProcessPathTemplate(t *testing.T) {
	cfg := &Config{
		VaultPath:  "/home/user/obsidian-vault",
		LogsPath:   "Logs/YYYY-MM-DD",
		DateFormat: "2006-01-02",
	}

	testDate := time.Date(2025, 1, 15, 0, 0, 0, 0, time.UTC)

	tests := []struct {
		name     string
		path     string
		expected string
	}{
		{
			name:     "YYYY-MM-DD replacement",
			path:     "Logs/YYYY-MM-DD",
			expected: "Logs/2025-01-15",
		},
		{
			name:     "YYYY replacement",
			path:     "Logs/YYYY",
			expected: "Logs/2025",
		},
		{
			name:     "MM replacement",
			path:     "Logs/MM",
			expected: "Logs/01",
		},
		{
			name:     "DD replacement",
			path:     "Logs/DD",
			expected: "Logs/15",
		},
		{
			name:     "Complex path with multiple replacements",
			path:     "/home/user/obsidian-vault/YYYY/MM/DD/Logs",
			expected: "/home/user/obsidian-vault/2025/01/15/Logs",
		},
		{
			name:     "No template variables",
			path:     "/home/user/obsidian-vault/Logs",
			expected: "/home/user/obsidian-vault/Logs",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := cfg.processPathTemplate(tt.path, testDate)
			if result != tt.expected {
				t.Errorf("processPathTemplate(%q, %v) = %q, want %q", tt.path, testDate, result, tt.expected)
			}
		})
	}
}

func TestGetLogsDirForDate(t *testing.T) {
	cfg := &Config{
		VaultPath:  "/home/user/obsidian-vault",
		LogsPath:   "Logs/YYYY-MM-DD",
		DateFormat: "2006-01-02",
	}

	testDate := time.Date(2025, 1, 15, 0, 0, 0, 0, time.UTC)
	expected := "/home/user/obsidian-vault/Logs/2025-01-15"

	result := cfg.GetLogsDirForDate(testDate)
	if result != expected {
		t.Errorf("GetLogsDirForDate(%v) = %q, want %q", testDate, result, expected)
	}
}

func TestGetDateFile(t *testing.T) {
	cfg := &Config{
		VaultPath:  "/home/user/obsidian-vault",
		LogsPath:   "Logs/YYYY-MM-DD",
		DateFormat: "2006-01-02",
	}

	testDate := time.Date(2025, 1, 15, 0, 0, 0, 0, time.UTC)
	expected := "/home/user/obsidian-vault/Logs/2025-01-15/2025-01-15.md"

	result := cfg.GetDateFile(testDate)
	if result != expected {
		t.Errorf("GetDateFile(%v) = %q, want %q", testDate, result, expected)
	}
}

func TestGetDateFileFromString(t *testing.T) {
	cfg := &Config{
		VaultPath:  "/home/user/obsidian-vault",
		LogsPath:   "Logs/YYYY-MM-DD",
		DateFormat: "2006-01-02",
	}

	// Test valid date string
	dateStr := "2025-01-15"
	expected := "/home/user/obsidian-vault/Logs/2025-01-15/2025-01-15.md"

	result, err := cfg.GetDateFileFromString(dateStr)
	if err != nil {
		t.Errorf("GetDateFileFromString(%q) returned error: %v", dateStr, err)
	}
	if result != expected {
		t.Errorf("GetDateFileFromString(%q) = %q, want %q", dateStr, result, expected)
	}

	// Test invalid date string
	invalidDateStr := "invalid-date"
	_, err = cfg.GetDateFileFromString(invalidDateStr)
	if err == nil {
		t.Errorf("GetDateFileFromString(%q) should have returned an error", invalidDateStr)
	}
}

func TestGetTodayFile(t *testing.T) {
	cfg := &Config{
		VaultPath:  "/home/user/obsidian-vault",
		LogsPath:   "Logs/YYYY-MM-DD",
		DateFormat: "2006-01-02",
	}

	// Mock the current date for consistent testing
	originalGetCurrentDate := getCurrentDate
	defer func() { getCurrentDate = originalGetCurrentDate }()
	
	testDate := time.Date(2025, 1, 15, 0, 0, 0, 0, time.UTC)
	getCurrentDate = func() time.Time {
		return testDate
	}

	expected := "/home/user/obsidian-vault/Logs/2025-01-15/2025-01-15.md"

	result := cfg.GetTodayFile()
	if result != expected {
		t.Errorf("GetTodayFile() = %q, want %q", result, expected)
	}
} 