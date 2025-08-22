package dbfilters

import (
	"time"

	"github.com/spf13/cobra"
)

// Filters define optional constraints for queries
// All string comparisons are case-insensitive where applicable
 type Filters struct {
	Since time.Time
	Until time.Time
	Author string
	Committer string
	RepoSubstring string
	HasMerge *bool
	Language string
	System string
	MinFiles, MaxFiles int
	MinLines, MaxLines int
	Limit, Offset int
	OrderBy string // analyzed_at|files|lines
	OrderDesc bool
}

// AddFlags registers common filter flags on a command
func AddFlags(cmd *cobra.Command) {
	cmd.Flags().String("since", "", "Filter by analyzed_at >= RFC3339")
	cmd.Flags().String("until", "", "Filter by analyzed_at <= RFC3339")
	cmd.Flags().String("author", "", "Substring in author name/email")
	cmd.Flags().String("committer", "", "Substring in committer name/email")
	cmd.Flags().String("repo-contains", "", "Substring in repository path")
	cmd.Flags().Bool("has-merge", false, "Only include analyses with merge commit")
	cmd.Flags().Bool("no-merge", false, "Only include analyses without merge commit")
	cmd.Flags().String("language", "", "Require language to appear in PR")
	cmd.Flags().String("system", "", "Require system/category to appear in PR")
	cmd.Flags().Int("min-files", 0, "Minimum files changed")
	cmd.Flags().Int("max-files", 0, "Maximum files changed")
	cmd.Flags().Int("min-lines", 0, "Minimum lines changed")
	cmd.Flags().Int("max-lines", 0, "Maximum lines changed")
	cmd.Flags().String("order-by", "", "Order by: analyzed_at|files|lines")
	cmd.Flags().Bool("desc", false, "Sort descending")
	cmd.Flags().Int("limit", 0, "Limit number of rows")
	cmd.Flags().Int("offset", 0, "Offset for pagination")
}

// FromCmdFlags parses common filter flags from a command
func FromCmdFlags(cmd *cobra.Command) (Filters, error) {
	var f Filters
	// parse times
	sinceStr, _ := cmd.Flags().GetString("since")
	untilStr, _ := cmd.Flags().GetString("until")
	if sinceStr != "" {
		if t, err := time.Parse(time.RFC3339, sinceStr); err == nil {
			f.Since = t
		} else {
			return f, err
		}
	}
	if untilStr != "" {
		if t, err := time.Parse(time.RFC3339, untilStr); err == nil {
			f.Until = t
		} else {
			return f, err
		}
	}
	// simple strings
	f.Author, _ = cmd.Flags().GetString("author")
	f.Committer, _ = cmd.Flags().GetString("committer")
	f.RepoSubstring, _ = cmd.Flags().GetString("repo-contains")
	f.Language, _ = cmd.Flags().GetString("language")
	f.System, _ = cmd.Flags().GetString("system")
	// booleans
	hasMerge, _ := cmd.Flags().GetBool("has-merge")
	noMerge, _ := cmd.Flags().GetBool("no-merge")
	if hasMerge && noMerge {
		// conflict -> ignore both
	} else if hasMerge {
		v := true
		f.HasMerge = &v
	} else if noMerge {
		v := false
		f.HasMerge = &v
	}
	// ints
	f.MinFiles, _ = cmd.Flags().GetInt("min-files")
	f.MaxFiles, _ = cmd.Flags().GetInt("max-files")
	f.MinLines, _ = cmd.Flags().GetInt("min-lines")
	f.MaxLines, _ = cmd.Flags().GetInt("max-lines")
	f.Limit, _ = cmd.Flags().GetInt("limit")
	f.Offset, _ = cmd.Flags().GetInt("offset")
	// order
	f.OrderBy, _ = cmd.Flags().GetString("order-by")
	f.OrderDesc, _ = cmd.Flags().GetBool("desc")
	return f, nil
}
