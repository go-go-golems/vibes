package model_test

import (
	"strings"
	"testing"

	tea "github.com/charmbracelet/bubbletea"

	"github.com/user/youtube-analyzer-go/internal/config"
	"github.com/user/youtube-analyzer-go/internal/logger"
	"github.com/user/youtube-analyzer-go/pkg/ui/keys"
	"github.com/user/youtube-analyzer-go/pkg/ui/model"
	"github.com/user/youtube-analyzer-go/pkg/ui/view"
)

// TestTUIComponents tests basic TUI component functionality
func TestTUIComponents(t *testing.T) {
	// Test configuration
	cfg := &config.Config{
		APIKey:   "test-key",
		Mode:     "quick",
		Model:    "gemini-2.5-flash",
		LogLevel: "info",
		Verbose:  false,
	}

	log := logger.New(cfg, "test")

	t.Run("KeyMaps", func(t *testing.T) {
		// Test basic key map
		keyMap := keys.NewKeyMap()
		if keyMap.Quit.Keys()[0] != "q" {
			t.Errorf("Expected quit key to be 'q', got %s", keyMap.Quit.Keys()[0])
		}

		// Test input key map
		inputKeyMap := keys.NewInputKeyMap()
		if inputKeyMap.Submit.Keys()[0] != "enter" {
			t.Errorf("Expected submit key to be 'enter', got %s", inputKeyMap.Submit.Keys()[0])
		}

		// Test results key map
		resultsKeyMap := keys.NewResultsKeyMap()
		if resultsKeyMap.ScrollUp.Keys()[0] != "u" {
			t.Errorf("Expected scroll up key to be 'u', got %s", resultsKeyMap.ScrollUp.Keys()[0])
		}
	})

	t.Run("Styles", func(t *testing.T) {
		// Test responsive helpers
		width := view.AdaptWidth(100)
		if width <= 0 {
			t.Error("AdaptWidth should return positive value")
		}

		height := view.AdaptHeight(50)
		if height <= 0 {
			t.Error("AdaptHeight should return positive value")
		}
	})

	t.Run("Models", func(t *testing.T) {
		// Test common state
		common := model.CommonState{
			Width:        80,
			Height:       24,
			GeminiClient: nil, // Not testing actual API calls
			Config:       cfg,
			Logger:       log,
		}

		// Test input model
		inputModel := model.NewInputModel(common)
		if inputModel.Common.Width != 80 {
			t.Errorf("Expected width 80, got %d", inputModel.Common.Width)
		}

		// Test loading model
		loadingModel := model.NewLoadingModel(common)
		if loadingModel.Common.Height != 24 {
			t.Errorf("Expected height 24, got %d", loadingModel.Common.Height)
		}

		// Test results model
		resultsModel := model.NewResultsModel(common)
		if resultsModel.Common.Config != cfg {
			t.Error("Results model should have correct config")
		}

		// Test error model
		errorModel := model.NewErrorModel(common)
		if errorModel.Common.Logger != log {
			t.Error("Error model should have correct logger")
		}
	})

	t.Run("MessageHandling", func(t *testing.T) {
		common := model.CommonState{
			Width:        80,
			Height:       24,
			GeminiClient: nil,
			Config:       cfg,
			Logger:       log,
		}

		inputModel := model.NewInputModel(common)

		// Test window size message
		windowMsg := tea.WindowSizeMsg{Width: 120, Height: 40}
		_, cmd := inputModel.Update(windowMsg)
		if cmd != nil {
			t.Error("Window size update should not return command")
		}

		// Test key message (escape)
		keyMsg := tea.KeyMsg{Type: tea.KeyEsc}
		_, cmd = inputModel.Update(keyMsg)
		if cmd == nil {
			t.Error("Escape key should return quit command")
		}
	})

	t.Run("URLValidation", func(t *testing.T) {
		validURLs := []string{
			"https://www.youtube.com/watch?v=dQw4w9WgXcQ",
			"https://youtu.be/dQw4w9WgXcQ",
			"https://youtube.com/embed/dQw4w9WgXcQ",
			"https://youtube.com/v/dQw4w9WgXcQ",
		}

		invalidURLs := []string{
			"https://vimeo.com/123456789",
			"https://example.com",
			"not-a-url",
			"",
		}

		for _, url := range validURLs {
			if !isValidYouTubeURL(url) {
				t.Errorf("URL should be valid: %s", url)
			}
		}

		for _, url := range invalidURLs {
			if isValidYouTubeURL(url) {
				t.Errorf("URL should be invalid: %s", url)
			}
		}
	})
}

// Helper function for URL validation (copied from input.go for testing)
func isValidYouTubeURL(url string) bool {
	url = strings.ToLower(url)
	return strings.Contains(url, "youtube.com/watch") ||
		strings.Contains(url, "youtu.be/") ||
		strings.Contains(url, "youtube.com/embed/") ||
		strings.Contains(url, "youtube.com/v/")
}

// TestTUIIntegration tests the integration between different TUI components
func TestTUIIntegration(t *testing.T) {
	cfg := &config.Config{
		APIKey:   "test-key",
		Mode:     "quick",
		LogLevel: "info",
	}

	log := logger.New(cfg, "integration-test")

	// Test main model creation (without actual API calls)
	t.Run("MainModelCreation", func(t *testing.T) {
		// We can't test with real Gemini client without API key
		// So we test the structure and initialization
		common := model.CommonState{
			Width:        80,
			Height:       24,
			GeminiClient: nil,
			Config:       cfg,
			Logger:       log,
		}

		mainModel := model.MainModel{
			Common:        common,
			CurrentScreen: model.ScreenInput,
			InputModel:    model.NewInputModel(common),
			LoadingModel:  model.NewLoadingModel(common),
			ResultsModel:  model.NewResultsModel(common),
			ErrorModel:    model.NewErrorModel(common),
		}

		if mainModel.CurrentScreen != model.ScreenInput {
			t.Error("Main model should start with input screen")
		}

		// Test screen transition messages
		screenChangeMsg := model.ScreenChangeMsg{
			Screen:   model.ScreenLoading,
			VideoURL: "https://www.youtube.com/watch?v=test",
		}

		_, cmd := mainModel.Update(screenChangeMsg)
		if cmd == nil {
			t.Error("Screen change should return command")
		}
	})
}
