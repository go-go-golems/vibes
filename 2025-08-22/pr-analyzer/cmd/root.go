package cmd

import (
	"os"
	"strings"
	"context"
	"fmt"
	"time"
	"database/sql"

	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"
	"pr-analyzer/internal/db"
)

var (
	repoPath     string
	configFile   string
	outputFormat string
	logLevel     string
	dbPathGlobal string
	filterSince  string
	filterAuthor string
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

	// DB command group
	dbCmd := &cobra.Command{Use: "db", Short: "SQLite database utilities"}
	rootCmd.AddCommand(dbCmd)

	dbInitCmd := &cobra.Command{Use: "init", Short: "Initialize sqlite schema", RunE: func(cmd *cobra.Command, args []string) error {
		ctx := context.Background()
		store, err := db.Open(ctx, dbPathGlobal)
		if err != nil {
			return err
		}
		defer store.Close()
		log.Info().Str("db", dbPathGlobal).Msg("sqlite schema ready")
		return nil
	}}
	dbCmd.AddCommand(dbInitCmd)

	dbLangCmd := &cobra.Command{Use: "languages", Short: "Aggregate language stats", RunE: func(cmd *cobra.Command, args []string) error {
		ctx := context.Background()
		store, err := db.Open(ctx, dbPathGlobal)
		if err != nil {
			return err
		}
		defer store.Close()
		rows, err := store.AggregateLanguages(ctx)
		if err != nil {
			return err
		}
		fmt.Printf("Language Aggregates\n====================\n")
		for _, r := range rows {
			fmt.Printf("%-16s PRs:%4d Files:%5d Lines:%7d\n", r.Language, r.PRs, r.Files, r.Lines)
		}
		return nil
	}}
	dbCmd.AddCommand(dbLangCmd)

	dbSysCmd := &cobra.Command{Use: "systems", Short: "Aggregate system stats", RunE: func(cmd *cobra.Command, args []string) error {
		ctx := context.Background()
		store, err := db.Open(ctx, dbPathGlobal)
		if err != nil {
			return err
		}
		defer store.Close()
		rows, err := store.AggregateSystems(ctx)
		if err != nil {
			return err
		}
		fmt.Printf("System Aggregates\n===================\n")
		for _, r := range rows {
			fmt.Printf("%-16s PRs:%4d Count:%7d\n", r.System, r.PRs, r.Count)
		}
		return nil
	}}
	dbCmd.AddCommand(dbSysCmd)

	// Filters for PR listing/summary
	dbCmd.PersistentFlags().StringVar(&filterSince, "since", "", "Filter by analyzed_at >= RFC3339")
	dbCmd.PersistentFlags().StringVar(&filterAuthor, "author", "", "Filter by substring in author/committer name or email")

	dbPRsCmd := &cobra.Command{Use: "prs", Short: "List stored analyses (per PR/commit)", RunE: func(cmd *cobra.Command, args []string) error {
		ctx := context.Background()
		store, err := db.Open(ctx, dbPathGlobal)
		if err != nil {
			return err
		}
		defer store.Close()
		var since time.Time
		if filterSince != "" {
			var parseErr error
			since, parseErr = time.Parse(time.RFC3339, filterSince)
			if parseErr != nil {
				return fmt.Errorf("invalid since (use RFC3339): %w", parseErr)
			}
		}
		rows, err := store.ListPRs(ctx, since, strings.ToLower(filterAuthor))
		if err != nil {
			return err
		}
		defer rows.Close()
		fmt.Printf("Stored Analyses\n================\n")
		for rows.Next() {
			var (
				id int64
				repo, base, pr, commit, merge string
				files, lines, commits int
				at time.Time
				an, ae string
				ad sql.NullTime
				cn, ce string
				cd sql.NullTime
				summary sql.NullString
			)
			if err := rows.Scan(&id, &repo, &base, &pr, &commit, &merge, &files, &lines, &commits, &at, &an, &ae, &ad, &cn, &ce, &cd, &summary); err != nil {
				return err
			}
			fmt.Printf("%d repo=%s commit=%s merge=%s files=%d lines=%d commits=%d at=%s author=%s <%s> summary=%s\n",
				id, repo, valueOrDash(commit), valueOrDash(merge), files, lines, commits, at.Format(time.RFC3339), an, ae, valueOrDash(summary.String))
		}
		return rows.Err()
	}}
	dbCmd.AddCommand(dbPRsCmd)

	dbSummaryCmd := &cobra.Command{Use: "summary", Short: "Summary across stored analyses", RunE: func(cmd *cobra.Command, args []string) error {
		ctx := context.Background()
		store, err := db.Open(ctx, dbPathGlobal)
		if err != nil {
			return err
		}
		defer store.Close()
		var since time.Time
		if filterSince != "" {
			var parseErr error
			since, parseErr = time.Parse(time.RFC3339, filterSince)
			if parseErr != nil {
				return fmt.Errorf("invalid since (use RFC3339): %w", parseErr)
			}
		}
		sum, err := store.Summary(ctx, since, strings.ToLower(filterAuthor))
		if err != nil {
			return err
		}
		fmt.Printf("Summary\n=======\nPRs: %d\nFiles: %d\nLines: %d\n", sum.PRs, sum.Files, sum.Lines)
		return nil
	}}
	dbCmd.AddCommand(dbSummaryCmd)
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

