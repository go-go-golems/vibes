package main

import (
	"fmt"
	"log"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	"github.com/user/youtube-analyzer-go/internal/config"
	"github.com/user/youtube-analyzer-go/internal/logger"
	"github.com/user/youtube-analyzer-go/pkg/gemini"
	uimodel "github.com/user/youtube-analyzer-go/pkg/ui/model"
)

var (
	apiKey   string
	mode     string
	model    string
	logLevel string
	verbose  bool
	videoURL string
)

func main() {
	if err := rootCmd.Execute(); err != nil {
		log.Fatal(err)
	}
}

var rootCmd = &cobra.Command{
	Use:   "tui",
	Short: "Interactive TUI for YouTube video analysis",
	Long: `A terminal user interface for analyzing YouTube videos using AI.

Features:
• Interactive URL input with validation
• Real-time streaming content generation
• Beautiful markdown rendering with glamour
• Keyboard shortcuts and help system
• Pre-fill URL via command line argument

Examples:
  # Launch TUI with interactive input
  youtube-analyzer tui --api-key YOUR_API_KEY
  
  # Launch TUI with pre-filled URL and start streaming immediately
  youtube-analyzer tui --api-key YOUR_API_KEY --video "https://youtube.com/watch?v=..."
  
  # Use comprehensive analysis mode
  youtube-analyzer tui --api-key YOUR_API_KEY --mode comprehensive --video "https://youtube.com/watch?v=..."`,
	RunE: runTUI,
}

func init() {
	rootCmd.Flags().StringVarP(&apiKey, "api-key", "k", "", "Google Gemini API key (required)")
	rootCmd.Flags().StringVarP(&mode, "mode", "m", "quick", "analysis mode: quick, comprehensive")
	rootCmd.Flags().StringVar(&model, "model", "", "Gemini model to use")
	rootCmd.Flags().StringVar(&logLevel, "log-level", "info", "log level (debug, info, warn, error)")
	rootCmd.Flags().BoolVarP(&verbose, "verbose", "v", false, "verbose output")
	rootCmd.Flags().StringVar(&videoURL, "video", "", "YouTube video URL to analyze (optional)")

	rootCmd.MarkFlagRequired("api-key")

	viper.BindPFlag("api-key", rootCmd.Flags().Lookup("api-key"))
	viper.BindPFlag("mode", rootCmd.Flags().Lookup("mode"))
	viper.BindPFlag("model", rootCmd.Flags().Lookup("model"))
	viper.BindPFlag("log-level", rootCmd.Flags().Lookup("log-level"))
	viper.BindPFlag("verbose", rootCmd.Flags().Lookup("verbose"))
	viper.BindPFlag("video", rootCmd.Flags().Lookup("video"))
}

func runTUI(cmd *cobra.Command, args []string) error {
	// Initialize configuration
	cfg := &config.Config{
		APIKey:   viper.GetString("api-key"),
		Mode:     viper.GetString("mode"),
		Model:    viper.GetString("model"),
		LogLevel: viper.GetString("log-level"),
		Verbose:  viper.GetBool("verbose"),
	}

	if err := cfg.Validate(); err != nil {
		return fmt.Errorf("configuration error: %w", err)
	}

	// Initialize logger
	log := logger.New(cfg, "tui")

	// Initialize Gemini client
	geminiClient, err := gemini.New(cfg, log)
	if err != nil {
		return fmt.Errorf("failed to initialize Gemini client: %w", err)
	}
	defer geminiClient.Close()

	// Create the main model
	inputVideoURL := viper.GetString("video")
	m := uimodel.NewMainModel(geminiClient, cfg, log, inputVideoURL)

	// Create tea program
	p := tea.NewProgram(m)

	// Set the program reference for streaming
	m.SetProgram(p)

	// Run the program
	if _, err := p.Run(); err != nil {
		return fmt.Errorf("error running program: %w", err)
	}

	return nil
}
