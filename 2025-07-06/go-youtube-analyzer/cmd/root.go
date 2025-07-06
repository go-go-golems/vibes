package cmd

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

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
	"github.com/user/youtube-analyzer-go/pkg/models"
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
	rootCmd.Flags().StringVarP(&apiKey, "api-key", "k", "", "Google Gemini API key (required)")
	rootCmd.Flags().StringVarP(&mode, "mode", "m", "quick", "analysis mode: quick, comprehensive")
	rootCmd.Flags().StringVar(&model, "model", "", "Gemini model to use (default: gemini-2.5-flash for quick, gemini-2.5-pro for comprehensive)")
	rootCmd.Flags().StringVarP(&outputFile, "output", "o", "", "output file path (default: auto-generated)")
	rootCmd.Flags().StringVar(&outputDir, "output-dir", "./analysis_results", "output directory")
	rootCmd.Flags().BoolVarP(&jsonOutput, "json", "j", false, "output results in JSON format only")
	rootCmd.Flags().BoolVarP(&interactive, "interactive", "i", false, "interactive mode with request preview and confirmation")

	// Mark required flags
	rootCmd.MarkFlagRequired("api-key")

	// Bind flags to viper
	viper.BindPFlag("api-key", rootCmd.Flags().Lookup("api-key"))
	viper.BindPFlag("mode", rootCmd.Flags().Lookup("mode"))
	viper.BindPFlag("model", rootCmd.Flags().Lookup("model"))
	viper.BindPFlag("output-dir", rootCmd.Flags().Lookup("output-dir"))
	viper.BindPFlag("log-level", rootCmd.PersistentFlags().Lookup("log-level"))
	viper.BindPFlag("verbose", rootCmd.PersistentFlags().Lookup("verbose"))
	viper.BindPFlag("quiet", rootCmd.PersistentFlags().Lookup("quiet"))
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
	Method    string            `json:"method" yaml:"method"`
	ModelName string            `json:"model_name" yaml:"model_name"`
	Contents  []*genai.Content  `json:"contents" yaml:"contents"`
	Options   interface{}       `json:"options" yaml:"options"`
}

// showInteractiveConfirmation shows the request preview and asks for confirmation
func showInteractiveConfirmation(cfg *config.Config, videoURL string) error {
	// Create the actual prompt that will be sent
	prompt := createTechnicalPrompt(cfg.Mode)
	
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

// createTechnicalPrompt creates the technical analysis prompt (simplified version for preview)
func createTechnicalPrompt(mode string) string {
	basePrompt := `You are an expert technical video analyst specializing in developer content analysis for social media optimization.

Analyze this video with comprehensive focus on:

## 1. TECHNICAL CONTENT ASSESSMENT
- Programming languages, frameworks, and technologies discussed
- Code quality and best practices demonstrated  
- Technical accuracy and depth of explanations
- Educational value for developers
- Architecture patterns and design principles mentioned

## 2. DEVELOPER AUDIENCE ANALYSIS
- Target skill level (beginner, intermediate, advanced)
- Specific developer roles (frontend, backend, DevOps, full-stack, etc.)
- Technical concepts complexity and accessibility
- Prerequisites and assumed knowledge

## 3. SOCIAL MEDIA OPTIMIZATION FOR TECH COMMUNITY
- Viral potential in developer communities (score 1-10)
- Key moments that would engage technical audiences
- Shareable technical insights or "aha" moments
- Hook potential for different platforms:
  * Twitter/X: Technical threads and quick tips
  * LinkedIn: Professional development insights
  * YouTube Shorts: Quick coding demos
  * TikTok: Trending tech concepts
  * Reddit: Deep technical discussions

## 4. CONTENT STRUCTURE & ENGAGEMENT
- Introduction effectiveness and hook strength
- Technical demonstration quality and clarity
- Code examples and explanation effectiveness
- Pacing and information density
- Conclusion and call-to-action strength

## 5. TIMESTAMP ANALYSIS
Identify key moments with timestamps (MM:SS format):
- Technical concept introductions
- Code demonstration highlights
- "Aha" moments and insights
- Potential clip-worthy segments
- Engagement peaks and valleys

## 6. SCORING & RECOMMENDATIONS
Provide numerical scores (1-10) for:
- Technical Accuracy
- Educational Value  
- Viral Potential
- Code Quality (if applicable)
- Overall Developer Relevance

## OUTPUT FORMAT
Structure your response with clear sections and specific, actionable recommendations. Include:
- Executive summary (2-3 sentences)
- Technical assessment with specific technologies identified
- Key timestamps with descriptions
- Platform-specific content recommendations
- Viral potential analysis with reasoning
- Specific improvements for social media optimization

Focus on practical, actionable insights that would help optimize this content for maximum reach and engagement in developer communities.`

	if mode == "comprehensive" {
		basePrompt += `

## COMPREHENSIVE ANALYSIS ADDITIONS

## 7. COMPETITIVE ANALYSIS
- How this content compares to similar technical content
- Unique value propositions and differentiators
- Market positioning in tech education space
- Opportunities for improvement

## 8. ADVANCED TECHNICAL EVALUATION
- Performance considerations discussed
- Security implications and best practices
- Scalability and maintainability aspects
- Industry standards compliance
- Code review quality and thoroughness

## 9. CONTENT STRATEGY RECOMMENDATIONS
- Series potential and follow-up content ideas
- Cross-platform content adaptation strategies
- Community engagement optimization
- Long-term audience building recommendations

## 10. DETAILED METRICS PREDICTION
- Expected engagement rates by platform
- Audience retention predictions
- Share-ability factors analysis
- Comment and discussion potential`
	}

	return basePrompt
}

// truncateForDisplay truncates text for display purposes
func truncateForDisplay(text string, maxLength int) string {
	if len(text) <= maxLength {
		return text
	}
	return text[:maxLength] + "\n... [truncated - showing first " + fmt.Sprintf("%d", maxLength) + " characters of " + fmt.Sprintf("%d", len(text)) + " total]"
}
