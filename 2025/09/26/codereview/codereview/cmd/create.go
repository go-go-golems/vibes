package cmd

import (
	"fmt"
	"time"

	"github.com/codereview/cli/internal/database"
	"github.com/codereview/cli/internal/git"
	"github.com/codereview/cli/internal/models"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newCreateCommand() *cobra.Command {
	var title string
	var branch string
	var commit string
	var baseCommit string
	var reviewer string

	cmd := &cobra.Command{
		Use:   "create",
		Short: "Create a new code review",
		Long:  "Create a new code review for the current or specified branch/commit",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runCreate(title, branch, commit, baseCommit, reviewer)
		},
	}

	cmd.Flags().StringVar(&title, "title", "", "Review title")
	cmd.Flags().StringVar(&branch, "branch", "", "Branch to review (default: current branch)")
	cmd.Flags().StringVar(&commit, "commit", "", "Specific commit to review (default: HEAD)")
	cmd.Flags().StringVar(&baseCommit, "base-commit", "", "Base commit for comparison (default: main)")
	cmd.Flags().StringVar(&reviewer, "reviewer", "", "Reviewer email (default: from config)")

	return cmd
}

func runCreate(title, branch, commit, baseCommit, reviewer string) error {
	// Initialize git repository
	repo, err := git.NewRepository("")
	if err != nil {
		return fmt.Errorf("not in a git repository: %w", err)
	}

	// Get database path
	dbPath := viper.GetString("db")
	if dbPath == "" {
		dbPath = database.GetDefaultDBPath()
	}

	// Open database
	db, err := database.New(dbPath)
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}
	defer db.Close()

	// Get current branch if not specified
	if branch == "" {
		branch, err = repo.GetCurrentBranch()
		if err != nil {
			return fmt.Errorf("failed to get current branch: %w", err)
		}
	}

	// Get current commit if not specified
	if commit == "" {
		commit, err = repo.GetCurrentCommit()
		if err != nil {
			return fmt.Errorf("failed to get current commit: %w", err)
		}
	}

	// Get base commit
	if baseCommit == "" {
		baseCommit = viper.GetString("git.default_base")
		if baseCommit == "" {
			baseCommit = "main"
		}
	}

	// Get reviewer
	if reviewer == "" {
		reviewer = viper.GetString("settings.default_reviewer")
		if reviewer == "" {
			return fmt.Errorf("no reviewer specified (use --reviewer or set in config)")
		}
	}

	// Generate title if not provided
	if title == "" {
		commitMsg, err := repo.GetCommitMessage(commit)
		if err == nil {
			title = fmt.Sprintf("Review: %s", commitMsg)
		} else {
			title = fmt.Sprintf("Review: %s", branch)
		}
	}

	// Generate unique review ID
	reviewID := generateReviewID()

	// Get changed files for statistics
	changedFiles, err := repo.GetChangedFiles(baseCommit, commit)
	if err != nil {
		return fmt.Errorf("failed to get changed files: %w", err)
	}

	// Create review
	review := &models.Review{
		ID:           reviewID,
		Title:        title,
		Branch:       branch,
		Commit:       commit,
		BaseCommit:   baseCommit,
		Reviewer:     reviewer,
		Status:       models.StatusPending,
		FilesChanged: len(changedFiles),
		Tags:         []string{},
	}

	// Calculate line statistics (simplified)
	// In a real implementation, you'd parse the diff to get accurate counts
	review.LinesAdded = 0
	review.LinesRemoved = 0

	if err := db.CreateReview(review); err != nil {
		return fmt.Errorf("failed to create review: %w", err)
	}

	// Output success message
	fmt.Printf("✅ Review created: %s\n", review.ID)
	fmt.Printf("   Title:       %s\n", review.Title)
	fmt.Printf("   Branch:      %s\n", review.Branch)
	fmt.Printf("   Commit:      %s\n", review.Commit[:8])
	fmt.Printf("   Base:        %s\n", review.BaseCommit)
	fmt.Printf("   Reviewer:    %s\n", review.Reviewer)
	fmt.Printf("   Status:      %s\n", review.Status)
	fmt.Printf("   Files:       %d\n", review.FilesChanged)
	fmt.Printf("   Created:     %s\n", review.Created.Format(time.RFC3339))

	return nil
}

func generateReviewID() string {
	// Generate a simple review ID based on timestamp
	now := time.Now()
	return fmt.Sprintf("rev-%d", now.Unix())
}
