package cmd

import (
	"os"
	"strings"

	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"
)

var (
	repoPath     string
	configFile   string
	outputFormat string
	logLevel     string
	dbPathGlobal string
)

var rootCmd = &cobra.Command{
	Use:   "pr-analyzer",
	Short: "Analyze GitHub pull requests for language and cross-subsystem statistics",
	Long: `A CLI tool that analyzes GitHub pull requests to compute:
- Percentage of language-related changes (by lines of code)
- Cross-subsystem statistics (commits touching multiple subsystems)
- Custom categorization using glob patterns

Examples:
  pr-analyzer analyze --pr-branch feature/new-api
  pr-analyzer analyze --commit abc123def --output json
  pr-analyzer analyze --categories "frontend:frontend/**,backend:backend/**"`,
	PersistentPreRun: func(cmd *cobra.Command, args []string) {
		setLogLevel(logLevel)
		log.Debug().Str("repoPath", repoPath).Str("output", outputFormat).Msg("root PersistentPreRun")
	},
}

func Execute() error {
	return rootCmd.Execute()
}

func init() {
	rootCmd.PersistentFlags().StringVar(&repoPath, "repo-path", ".", "Path to git repository")
	rootCmd.PersistentFlags().StringVar(&repoPath, "repo", ".", "Path to git repository (alias of --repo-path)")
	rootCmd.PersistentFlags().StringVar(&configFile, "config", "", "Path to config file")
	rootCmd.PersistentFlags().StringVar(&outputFormat, "output", "table", "Output format: table, json, yaml")
	rootCmd.PersistentFlags().StringVar(&logLevel, "log-level", "info", "Log level: trace, debug, info, warn, error")
	rootCmd.PersistentFlags().StringVar(&dbPathGlobal, "db-path", "pr-analyzer.sqlite", "Path to sqlite database file")
}

func valueOrDash(s string) string { if s == "" { return "-" }; return s }

func checkError(err error) {
	if err != nil {
		log.Error().Err(err).Msg("error")
		os.Exit(1)
	}
}

// setLogLevel configures zerolog level from a string, defaults to info
func setLogLevel(level string) {
	switch strings.ToLower(strings.TrimSpace(level)) {
	case "trace":
		zerolog.SetGlobalLevel(zerolog.TraceLevel)
	case "debug":
		zerolog.SetGlobalLevel(zerolog.DebugLevel)
	case "info", "":
		zerolog.SetGlobalLevel(zerolog.InfoLevel)
	case "warn", "warning":
		zerolog.SetGlobalLevel(zerolog.WarnLevel)
	case "error":
		zerolog.SetGlobalLevel(zerolog.ErrorLevel)
	case "fatal":
		zerolog.SetGlobalLevel(zerolog.FatalLevel)
	case "panic":
		zerolog.SetGlobalLevel(zerolog.PanicLevel)
	default:
		log.Warn().Str("level", level).Msg("unknown log level, defaulting to info")
		zerolog.SetGlobalLevel(zerolog.InfoLevel)
	}
}

