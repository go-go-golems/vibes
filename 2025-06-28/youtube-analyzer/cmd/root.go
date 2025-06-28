package cmd

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/fatih/color"
	"github.com/schollz/progressbar/v3"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

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
	verbose     bool
	quiet       bool
	jsonOutput  bool
	noColor     bool
	logLevel    string
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
	rootCmd.Flags().StringVarP(&outputFile, "output", "o", "", "output file path (default: auto-generated)")
	rootCmd.Flags().StringVar(&outputDir, "output-dir", "./analysis_results", "output directory")
	rootCmd.Flags().BoolVarP(&jsonOutput, "json", "j", false, "output results in JSON format only")

	// Bind flags to viper
	viper.BindPFlag("api-key", rootCmd.Flags().Lookup("api-key"))
	viper.BindPFlag("mode", rootCmd.Flags().Lookup("mode"))
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

	// Print out a censored version of the two env variables for debugging
	if verbose {
		ytKey := os.Getenv("YT_ANALYZER_API_KEY")
		geminiKey := os.Getenv("GEMINI_API_KEY")
		censor := func(s string) string {
			if s == "" {
				return "(empty)"
			}
			if len(s) <= 6 {
				return "******"
			}
			return s[:4] + strings.Repeat("*", len(s)-6) + s[len(s)-2:]
		}
		fmt.Fprintf(os.Stderr, "[debug] YT_ANALYZER_API_KEY: %s\n", censor(ytKey))
		fmt.Fprintf(os.Stderr, "[debug] GEMINI_API_KEY: %s\n", censor(geminiKey))
	}

	// Support both YT_ANALYZER_API_KEY and GEMINI_API_KEY as env vars for the API key
	if viper.GetString("api-key") == "" {
		ytKey := os.Getenv("YT_ANALYZER_API_KEY")
		geminiKey := os.Getenv("GEMINI_API_KEY")
		if ytKey != "" {
			viper.Set("api-key", ytKey)
		} else if geminiKey != "" {
			viper.Set("api-key", geminiKey)
		}
	}
}

func runAnalysis(cmd *cobra.Command, args []string) error {
	videoURL := args[0]

	// Initialize configuration
	cfg := &config.Config{
		APIKey:    viper.GetString("api-key"),
		Mode:      viper.GetString("mode"),
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

