package cmd

import (
	"context"
	"fmt"
	"os"

	"github.com/spf13/cobra"
	"github.com/user/git-precommit-guard/internal/utils"
	"github.com/user/git-precommit-guard/pkg/config"
	"github.com/user/git-precommit-guard/pkg/detector"
	"github.com/user/git-precommit-guard/pkg/git"
	"github.com/user/git-precommit-guard/pkg/reporter"
)

var (
	checkAllFiles bool
	failFast      bool
)

// checkCmd represents the check command
var checkCmd = &cobra.Command{
	Use:   "check",
	Short: "Check staged files for undesired content",
	Long: `Check staged files (or all files with --all) for undesired content such as
ELF binaries, large files, and files with blocked MIME types.

This command is designed to be used as a git pre-commit hook but can also be
run manually to check files.`,
	Run: runCheck,
}

func init() {
	checkCmd.Flags().BoolVarP(&checkAllFiles, "all", "a", false, "check all files instead of just staged files")
	checkCmd.Flags().BoolVar(&failFast, "fail-fast", false, "stop on first failure (overrides config)")
}

func runCheck(cmd *cobra.Command, args []string) {
	// Load configuration
	cfg, err := config.LoadConfig(getConfigFile())
	if err != nil {
		exitWithError("Failed to load configuration: %v", err)
	}

	// Override fail-fast if specified via flag
	if failFast {
		cfg.Settings.FailFast = true
	}

	// Override JSON output if specified via flag
	if jsonOutput {
		cfg.Reporting.Format = "json"
	}

	// Check if we're in a git repository (unless checking all files)
	if !checkAllFiles && !git.IsGitRepository() {
		exitWithError("Not in a git repository. Use --all to check files without git.")
	}

	// Get list of files to check
	var filesToCheck []string
	if checkAllFiles {
		// For --all flag, we would need to implement file discovery
		// For now, check files passed as arguments
		if len(args) == 0 {
			exitWithError("When using --all, specify files to check as arguments")
		}
		filesToCheck = args
	} else {
		// Get staged files
		stagedFiles, err := git.GetStagedFilePaths()
		if err != nil {
			exitWithError("Failed to get staged files: %v", err)
		}

		if len(stagedFiles) == 0 {
			if verbose {
				fmt.Println("No staged files to check")
			}
			return
		}

		filesToCheck = stagedFiles
	}

	// Create detector manager
	detectorManager := detector.NewDetectorManager(cfg)

	// Create reporter
	rep := reporter.NewReporter(cfg.Reporting, verbose)

	// Set up timeout context
	ctx, cancel := context.WithTimeout(context.Background(), cfg.Settings.Timeout)
	defer cancel()

	// Check files
	var allResults []*detector.DetectionResult
	var hasFailures bool

	for _, filePath := range filesToCheck {
		// Check if context is cancelled (timeout)
		select {
		case <-ctx.Done():
			exitWithError("Operation timed out after %v", cfg.Settings.Timeout)
		default:
		}

		// Skip if file doesn't exist (might be deleted)
		if _, err := os.Stat(filePath); os.IsNotExist(err) {
			if verbose {
				fmt.Printf("Skipping non-existent file: %s\n", filePath)
			}
			continue
		}

		// Get file information
		fileInfo, err := utils.GetFileInfo(filePath)
		if err != nil {
			if verbose {
				fmt.Printf("Warning: Failed to get info for %s: %v\n", filePath, err)
			}
			continue
		}

		// Run detectors
		results, err := detectorManager.CheckFile(fileInfo)
		if err != nil {
			exitWithError("Failed to check file %s: %v", filePath, err)
		}

		// Add results
		allResults = append(allResults, results...)

		// Check for failures
		for _, result := range results {
			if !result.Passed && (result.Severity == "error") {
				hasFailures = true
				if cfg.Settings.FailFast {
					break
				}
			}
		}

		// Exit early if fail-fast is enabled and we have failures
		if hasFailures && cfg.Settings.FailFast {
			break
		}
	}

	// Report results
	if err := rep.Report(allResults); err != nil {
		exitWithError("Failed to generate report: %v", err)
	}

	// Exit with appropriate code
	if hasFailures {
		os.Exit(1)
	}
}

