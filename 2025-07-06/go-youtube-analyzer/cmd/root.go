package cmd

import (
	"bufio"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/glamour"
	"github.com/charmbracelet/huh"
	"github.com/fatih/color"
	"github.com/schollz/progressbar/v3"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
	genai "google.golang.org/genai"
	"gopkg.in/yaml.v3"

	"github.com/user/youtube-analyzer-go/internal/analyzer"
	"github.com/user/youtube-analyzer-go/internal/config"
	"github.com/user/youtube-analyzer-go/internal/logger"
	"github.com/user/youtube-analyzer-go/pkg/gemini"
	"github.com/user/youtube-analyzer-go/pkg/models"
	uimodel "github.com/user/youtube-analyzer-go/pkg/ui/model"
)

var (
	cfgFile     string
	apiKey      string
	outputDir   string
	outputFile  string
	mode        string
	model       string
	verbose     bool
	quiet       bool
	jsonOutput  bool
	noColor     bool
	logLevel    string
	interactive bool
)

// rootCmd represents the base command when called without any subcommands
var rootCmd = &cobra.Command{
	Use:   "youtube-analyzer [YouTube URL]",
	Short: "AI-powered YouTube video analysis for technical content",
	Long: `🎬 YouTube Analyzer - AI-Powered Technical Video Analysis

A comprehensive CLI tool that analyzes YouTube videos using Google's Gemini AI,
specifically designed for technical developer content analysis.

Features:
• Real-time step tracking with detailed logging
• Technical content assessment and code quality analysis
• Developer audience targeting and skill level evaluation
• Social media optimization recommendations
• Comprehensive JSON output with timestamps
• Professional progress indicators and colored output

Examples:
  youtube-analyzer "https://www.youtube.com/watch?v=J3oJqan2Gv8" --api-key YOUR_KEY
  youtube-analyzer "https://youtu.be/VIDEO_ID" --mode comprehensive --output results.json
  youtube-analyzer URL --verbose --log-level debug --output-dir ./analysis`,
	Args: cobra.ExactArgs(1),
	RunE: runAnalysis,
}

// Execute adds all child commands to the root command and sets flags appropriately.
func Execute() error {
	return rootCmd.Execute()
}

func init() {
	cobra.OnInitialize(initConfig)

	// Global flags
	rootCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "config file (default is $HOME/.youtube-analyzer.yaml)")
	rootCmd.PersistentFlags().StringVar(&logLevel, "log-level", "info", "log level (debug, info, warn, error)")
	rootCmd.PersistentFlags().BoolVarP(&verbose, "verbose", "v", false, "verbose output")
	rootCmd.PersistentFlags().BoolVarP(&quiet, "quiet", "q", false, "quiet mode (minimal output)")
	rootCmd.PersistentFlags().BoolVar(&noColor, "no-color", false, "disable colored output")

	// Analysis flags
	rootCmd.Flags().StringVarP(&apiKey, "api-key", "k", "", "Google Gemini API key (or set GEMINI_API_KEY env var)")
	rootCmd.Flags().StringVarP(&mode, "mode", "m", "quick", "analysis mode: quick, comprehensive")
	rootCmd.Flags().StringVar(&model, "model", "", "Gemini model to use (default: gemini-2.5-flash for quick, gemini-2.5-pro for comprehensive)")
	rootCmd.Flags().StringVarP(&outputFile, "output", "o", "", "output file path (default: auto-generated)")
	rootCmd.Flags().StringVar(&outputDir, "output-dir", "./analysis_results", "output directory")
	rootCmd.Flags().BoolVarP(&jsonOutput, "json", "j", false, "output results in JSON format only")
	rootCmd.Flags().BoolVarP(&interactive, "interactive", "i", false, "interactive mode with request preview and confirmation")

	// Don't mark API key as required since we support environment variable fallback

	// Bind flags to viper
	viper.BindPFlag("api-key", rootCmd.Flags().Lookup("api-key"))
	viper.BindPFlag("mode", rootCmd.Flags().Lookup("mode"))
	viper.BindPFlag("model", rootCmd.Flags().Lookup("model"))
	viper.BindPFlag("output-dir", rootCmd.Flags().Lookup("output-dir"))
	viper.BindPFlag("log-level", rootCmd.PersistentFlags().Lookup("log-level"))
	viper.BindPFlag("verbose", rootCmd.PersistentFlags().Lookup("verbose"))
	viper.BindPFlag("quiet", rootCmd.PersistentFlags().Lookup("quiet"))

	// Add TUI command
	rootCmd.AddCommand(createTUICmd())
	
	// Add stream command
	rootCmd.AddCommand(createStreamCmd())
}

func initConfig() {
	if cfgFile != "" {
		viper.SetConfigFile(cfgFile)
	} else {
		home, err := os.UserHomeDir()
		cobra.CheckErr(err)

		viper.AddConfigPath(home)
		viper.AddConfigPath(".")
		viper.SetConfigType("yaml")
		viper.SetConfigName(".youtube-analyzer")
	}

	viper.AutomaticEnv()
	viper.SetEnvPrefix("YT_ANALYZER")
	viper.SetEnvKeyReplacer(strings.NewReplacer("-", "_"))

	if err := viper.ReadInConfig(); err == nil && verbose {
		fmt.Fprintf(os.Stderr, "Using config file: %s\n", viper.ConfigFileUsed())
	}
}

func runAnalysis(cmd *cobra.Command, args []string) error {
	videoURL := args[0]

	// Initialize configuration
	cfg := &config.Config{
		APIKey:    viper.GetString("api-key"),
		Mode:      viper.GetString("mode"),
		Model:     viper.GetString("model"),
		OutputDir: viper.GetString("output-dir"),
		LogLevel:  viper.GetString("log-level"),
		Verbose:   viper.GetBool("verbose"),
		Quiet:     viper.GetBool("quiet"),
		NoColor:   noColor,
	}

	// Fallback to environment variable if API key is not set
	if cfg.APIKey == "" {
		cfg.APIKey = os.Getenv("GEMINI_API_KEY")
	}

	// Validate configuration
	if err := cfg.Validate(); err != nil {
		return fmt.Errorf("configuration error: %w", err)
	}

	// Initialize colors
	if cfg.NoColor {
		color.NoColor = true
	}

	// Create output directory
	if err := os.MkdirAll(cfg.OutputDir, 0755); err != nil {
		return fmt.Errorf("failed to create output directory: %w", err)
	}

	// Generate session ID and output file
	sessionID := fmt.Sprintf("go_%s", time.Now().Format("20060102_150405"))
	if outputFile == "" {
		outputFile = filepath.Join(cfg.OutputDir, fmt.Sprintf("analysis_%s.json", sessionID))
	}

	// Initialize logger
	log := logger.New(cfg, sessionID)

	// Print header
	if !cfg.Quiet {
		printHeader(videoURL, sessionID, cfg.Mode)
	}

	// Initialize analyzer
	analyzer, err := analyzer.New(cfg, log, sessionID)
	if err != nil {
		return fmt.Errorf("failed to initialize analyzer: %w", err)
	}

	// Interactive mode - show request preview and ask for confirmation
	if interactive {
		if err := showInteractiveConfirmation(cfg, videoURL); err != nil {
			return err
		}
	}

	// Create progress bar
	var bar *progressbar.ProgressBar
	if !cfg.Quiet && !jsonOutput {
		bar = progressbar.NewOptions(100,
			progressbar.OptionSetDescription("🎬 Analyzing video..."),
			progressbar.OptionSetTheme(progressbar.Theme{
				Saucer:        "█",
				SaucerHead:    "█",
				SaucerPadding: "░",
				BarStart:      "▐",
				BarEnd:        "▌",
			}),
			progressbar.OptionShowCount(),
			progressbar.OptionShowIts(),
			progressbar.OptionSetWidth(50),
			progressbar.OptionSpinnerType(14),
		)
	}

	// Progress callback
	progressCallback := func(step string, progress int) {
		if bar != nil {
			bar.Set(progress)
			if progress == 100 {
				bar.Finish()
			}
		}
		if cfg.Verbose && !cfg.Quiet {
			log.Info(fmt.Sprintf("Step: %s (%d%%)", step, progress))
		}
	}

	// Run analysis
	result, err := analyzer.AnalyzeVideo(videoURL, progressCallback)
	if err != nil {
		return fmt.Errorf("analysis failed: %w", err)
	}

	// Save results
	if err := saveResults(result, outputFile, cfg, log); err != nil {
		return fmt.Errorf("failed to save results: %w", err)
	}

	// Print results
	if !cfg.Quiet {
		printResults(result, outputFile, cfg)
	}

	return nil
}

func printHeader(videoURL, sessionID, mode string) {
	cyan := color.New(color.FgCyan, color.Bold)
	yellow := color.New(color.FgYellow)

	fmt.Println()
	cyan.Println("🎬 YouTube Analyzer - AI-Powered Technical Video Analysis")
	fmt.Println(strings.Repeat("=", 60))
	fmt.Printf("📺 Video URL: %s\n", videoURL)
	fmt.Printf("🆔 Session ID: %s\n", sessionID)
	fmt.Printf("⚙️  Analysis Mode: %s\n", mode)
	yellow.Printf("🚀 Starting analysis...\n")
	fmt.Println()
}

func saveResults(result *models.AnalysisResult, outputFile string, cfg *config.Config, log *logger.Logger) error {
	// Create output directory if it doesn't exist
	if err := os.MkdirAll(filepath.Dir(outputFile), 0755); err != nil {
		return err
	}

	// Marshal to JSON with proper formatting
	data, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		return err
	}

	// Write to file
	if err := os.WriteFile(outputFile, data, 0644); err != nil {
		return err
	}

	log.Info(fmt.Sprintf("Results saved to: %s", outputFile))
	return nil
}

func printResults(result *models.AnalysisResult, outputFile string, cfg *config.Config) {
	green := color.New(color.FgGreen, color.Bold)
	blue := color.New(color.FgBlue)
	yellow := color.New(color.FgYellow)

	fmt.Println()
	green.Println("✅ Analysis Complete!")
	fmt.Println(strings.Repeat("=", 60))

	fmt.Printf("📊 Total Steps: %d\n", result.TotalSteps)
	fmt.Printf("🔗 API Calls: %d\n", result.APICalls)
	fmt.Printf("⏱️  Duration: %.2f seconds\n", result.TotalTime)
	fmt.Printf("📄 Output File: %s\n", outputFile)

	if result.Analysis != nil && result.Analysis.Summary != "" {
		fmt.Println()
		blue.Println("📝 Analysis Summary:")
		fmt.Println(strings.Repeat("-", 40))

		// Print first 200 characters of summary
		summary := result.Analysis.Summary
		if len(summary) > 200 {
			summary = summary[:200] + "..."
		}
		fmt.Println(summary)

		if result.Analysis.TechnicalScore > 0 {
			fmt.Printf("\n🎯 Technical Score: %.1f/10\n", result.Analysis.TechnicalScore)
		}
		if result.Analysis.ViralPotential > 0 {
			fmt.Printf("🚀 Viral Potential: %.1f/10\n", result.Analysis.ViralPotential)
		}
	}

	fmt.Println()
	yellow.Printf("💡 Use --json flag for machine-readable output\n")
	yellow.Printf("💡 Use --verbose flag for detailed step tracking\n")
	fmt.Println()
}

// RequestPreview represents the request structure for display
type RequestPreview struct {
	Model       string           `json:"model" yaml:"model"`
	VideoURL    string           `json:"video_url" yaml:"video_url"`
	Mode        string           `json:"mode" yaml:"mode"`
	OutputDir   string           `json:"output_directory" yaml:"output_directory"`
	Interactive bool             `json:"interactive" yaml:"interactive"`
	APICall     APICallStructure `json:"api_call" yaml:"api_call"`
}

// APICallStructure represents the actual genai API call structure
type APICallStructure struct {
	Method    string           `json:"method" yaml:"method"`
	ModelName string           `json:"model_name" yaml:"model_name"`
	Contents  []*genai.Content `json:"contents" yaml:"contents"`
	Options   interface{}      `json:"options" yaml:"options"`
}

// showInteractiveConfirmation shows the request preview and asks for confirmation
func showInteractiveConfirmation(cfg *config.Config, videoURL string) error {
	// Create a temporary gemini client to get the actual prompt that will be sent
	tempLogger := logger.New(cfg, "temp")
	geminiClient, err := gemini.New(cfg, tempLogger)
	if err != nil {
		return fmt.Errorf("failed to create gemini client for preview: %w", err)
	}
	defer geminiClient.Close()

	// Get the actual prompt using the same logic as the real analysis
	prompt := geminiClient.CreateTechnicalPrompt()

	// Create the exact genai.Content structure that will be sent
	modelName := cfg.GetModelName()
	contents := []*genai.Content{
		{
			Parts: []*genai.Part{
				genai.NewPartFromText(prompt),
				genai.NewPartFromURI(videoURL, "video/mp4"),
			},
			Role: "user",
		},
	}

	// Create API call structure for display
	apiCall := APICallStructure{
		Method:    "client.Models.GenerateContent",
		ModelName: modelName,
		Contents:  contents,
		Options:   nil,
	}

	// Create request preview
	preview := RequestPreview{
		Model:       modelName,
		VideoURL:    videoURL,
		Mode:        cfg.Mode,
		OutputDir:   cfg.OutputDir,
		Interactive: true,
		APICall:     apiCall,
	}

	// Convert to YAML for display
	yamlData, err := yaml.Marshal(preview)
	if err != nil {
		return fmt.Errorf("failed to marshal request preview: %w", err)
	}

	// Display the request
	cyan := color.New(color.FgCyan, color.Bold)
	yellow := color.New(color.FgYellow)
	green := color.New(color.FgGreen)
	blue := color.New(color.FgBlue)

	fmt.Println()
	cyan.Println("📋 Request Preview")
	fmt.Println(strings.Repeat("=", 50))
	fmt.Println()
	yellow.Println("The following request will be sent to Gemini:")
	fmt.Println()
	green.Println("API Call: client.Models.GenerateContent(ctx, modelName, contents, options)")
	fmt.Println()
	blue.Printf("Prompt length: %d characters\n", len(prompt))
	blue.Printf("First 200 chars: %s...\n", truncateForDisplay(prompt, 200))
	fmt.Println()
	fmt.Print(string(yamlData))
	fmt.Println()

	// Ask for confirmation
	var proceed bool
	form := huh.NewForm(
		huh.NewGroup(
			huh.NewConfirm().
				Title("Do you want to proceed with this analysis?").
				Description("This will send the request to Google's Gemini API and may incur costs.").
				Affirmative("Yes, proceed").
				Negative("No, cancel").
				Value(&proceed),
		),
	)

	if err := form.Run(); err != nil {
		return fmt.Errorf("interactive confirmation failed: %w", err)
	}

	if !proceed {
		fmt.Println()
		fmt.Println("❌ Analysis cancelled by user")
		return fmt.Errorf("analysis cancelled")
	}

	fmt.Println()
	fmt.Println("✅ Analysis confirmed. Starting...")
	fmt.Println()

	return nil
}

// truncateForDisplay truncates text for display purposes
func truncateForDisplay(text string, maxLength int) string {
	if len(text) <= maxLength {
		return text
	}
	return text[:maxLength] + "\n... [truncated - showing first " + fmt.Sprintf("%d", maxLength) + " characters of " + fmt.Sprintf("%d", len(text)) + " total]"
}

// createTUICmd creates the TUI command
func createTUICmd() *cobra.Command {
	var tuiAPIKey string
	var tuiMode string
	var tuiModel string
	var tuiLogLevel string
	var tuiVerbose bool

	tuiCmd := &cobra.Command{
		Use:   "tui",
		Short: "Launch the Terminal User Interface",
		Long: `🎬 YouTube Analyzer - Interactive Terminal UI

Launch an interactive terminal user interface for analyzing YouTube videos.
The TUI provides a user-friendly interface with real-time progress updates,
results display, and easy navigation between different analysis screens.

Features:
• Interactive URL input with validation
• Real-time progress display during analysis
• Comprehensive results viewer with scrolling
• Keyboard shortcuts and help system
• Error handling and retry options

Usage:
  youtube-analyzer tui --api-key YOUR_API_KEY
  youtube-analyzer tui --api-key YOUR_API_KEY --mode comprehensive
  
  Or set environment variable:
  export GEMINI_API_KEY="your-api-key"
  youtube-analyzer tui`,
		RunE: func(cmd *cobra.Command, args []string) error {
			return runTUI(tuiAPIKey, tuiMode, tuiModel, tuiLogLevel, tuiVerbose)
		},
	}

	tuiCmd.Flags().StringVarP(&tuiAPIKey, "api-key", "k", "", "Google Gemini API key (or set GEMINI_API_KEY env var)")
	tuiCmd.Flags().StringVarP(&tuiMode, "mode", "m", "quick", "analysis mode: quick, comprehensive")
	tuiCmd.Flags().StringVar(&tuiModel, "model", "", "Gemini model to use")
	tuiCmd.Flags().StringVar(&tuiLogLevel, "log-level", "info", "log level (debug, info, warn, error)")
	tuiCmd.Flags().BoolVarP(&tuiVerbose, "verbose", "v", false, "verbose output")

	// Remove required flag since we support environment variable fallback

	return tuiCmd
}

// runTUI runs the TUI application
func runTUI(apiKey, mode, model, logLevel string, verbose bool) error {
	// Fallback to environment variable if API key is not set
	if apiKey == "" {
		apiKey = os.Getenv("GEMINI_API_KEY")
	}

	// Initialize configuration
	cfg := &config.Config{
		APIKey:   apiKey,
		Mode:     mode,
		Model:    model,
		LogLevel: logLevel,
		Verbose:  verbose,
	}

	if err := cfg.Validate(); err != nil {
		return fmt.Errorf("configuration error: %w", err)
	}

	// Initialize logger
	logger := logger.New(cfg, "tui")

	// Initialize Gemini client
	geminiClient, err := gemini.New(cfg, logger)
	if err != nil {
		return fmt.Errorf("failed to initialize Gemini client: %w", err)
	}
	defer geminiClient.Close()

	// Create the main model (no initial video URL for regular TUI)
	m := uimodel.NewMainModel(geminiClient, cfg, logger, "")

	// Create tea program
	p := tea.NewProgram(m, tea.WithAltScreen())

	// Run the program
	if _, err := p.Run(); err != nil {
		return fmt.Errorf("error running TUI: %w", err)
	}

	return nil
}

func createStreamCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "stream",
		Short: "Stream text generation with Gemini",
		Long:  `Stream text generation with Gemini using simple prompts to demonstrate streaming functionality.`,
		RunE:  runStreamCmd,
	}

	cmd.Flags().StringP("prompt", "p", "", "Prompt to send to Gemini (if not provided, will prompt interactively)")
	cmd.Flags().StringP("model", "m", "gemini-2.0-flash-exp", "Gemini model to use")
	cmd.Flags().StringP("style", "s", "dark", "Glamour style (dark, light, auto)")

	return cmd
}

func runStreamCmd(cmd *cobra.Command, args []string) error {
	// Get flags
	prompt, _ := cmd.Flags().GetString("prompt")
	model, _ := cmd.Flags().GetString("model")
	style, _ := cmd.Flags().GetString("style")

	// Get config
	cfg := &config.Config{
		APIKey:   viper.GetString("api-key"),
		Model:    model,
		Mode:     "quick",
		LogLevel: viper.GetString("log-level"),
	}

	// Fallback to environment variable if API key is not set
	if cfg.APIKey == "" {
		cfg.APIKey = os.Getenv("GEMINI_API_KEY")
	}

	// Validate config
	if err := cfg.Validate(); err != nil {
		return fmt.Errorf("configuration error: %w", err)
	}

	// Get prompt if not provided
	if prompt == "" {
		fmt.Print("Enter your prompt: ")
		reader := bufio.NewReader(os.Stdin)
		input, err := reader.ReadString('\n')
		if err != nil {
			return fmt.Errorf("error reading input: %w", err)
		}
		prompt = strings.TrimSpace(input)
	}

	if prompt == "" {
		return fmt.Errorf("prompt cannot be empty")
	}

	// Initialize logger
	log := logger.New(cfg, "stream")

	// Create Gemini client
	client, err := gemini.New(cfg, log)
	if err != nil {
		return fmt.Errorf("failed to create Gemini client: %w", err)
	}
	defer client.Close()

	// Run streaming demo
	return runStreamingDemo(client, prompt, style)
}

func runStreamingDemo(client *gemini.Client, prompt, style string) error {
	fmt.Printf("🚀 Starting streaming demo with Gemini\n")
	fmt.Printf("📝 Prompt: %s\n", prompt)
	fmt.Printf("🎨 Style: %s\n", style)
	fmt.Printf("%s\n\n", strings.Repeat("=", 50))

	// Initialize glamour renderer
	renderer, err := glamour.NewTermRenderer(
		glamour.WithAutoStyle(),
		glamour.WithWordWrap(80),
	)
	if err != nil {
		return fmt.Errorf("failed to create glamour renderer: %w", err)
	}

	// Set up streaming callback
	var accumulatedContent strings.Builder
	startTime := time.Now()
	
	callback := func(content string) {
		accumulatedContent.WriteString(content)
		
		// Clear screen and show updated content
		fmt.Print("\033[H\033[2J") // Clear screen
		
		// Show header
		elapsed := time.Since(startTime)
		fmt.Printf("🔄 Streaming Response (%.1fs elapsed)\n", elapsed.Seconds())
		fmt.Printf("%s\n\n", strings.Repeat("=", 50))
		
		// Render markdown
		rendered, err := renderer.Render(accumulatedContent.String())
		if err != nil {
			// Fallback to plain text if rendering fails
			fmt.Print(accumulatedContent.String())
		} else {
			fmt.Print(rendered)
		}
		
		// Add streaming indicator
		fmt.Printf("\n%s\n", strings.Repeat(".", int(elapsed.Seconds())%4+1))
	}

	// Start streaming
	fmt.Printf("🎬 Starting streaming generation...\n\n")
	
	ctx := context.Background()
	response, err := client.GenerateContentStreaming(ctx, prompt, callback)
	if err != nil {
		return fmt.Errorf("streaming generation failed: %w", err)
	}

	// Final render
	fmt.Print("\033[H\033[2J") // Clear screen
	elapsed := time.Since(startTime)
	fmt.Printf("✅ Streaming Complete (%.1fs total)\n", elapsed.Seconds())
	fmt.Printf("%s\n\n", strings.Repeat("=", 50))
	
	// Final render of complete content
	rendered, err := renderer.Render(response)
	if err != nil {
		fmt.Print(response)
	} else {
		fmt.Print(rendered)
	}
	
	fmt.Printf("\n%s\n", strings.Repeat("=", 50))
	fmt.Printf("📊 Stats: %d characters, %.1fs duration\n", len(response), elapsed.Seconds())
	
	return nil
}
