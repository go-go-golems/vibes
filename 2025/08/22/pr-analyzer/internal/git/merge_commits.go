package git

import (
	"fmt"
	"strings"
	"time"

	"github.com/go-git/go-git/v5/plumbing/object"
	"github.com/go-git/go-git/v5"
	"github.com/rs/zerolog/log"
)

// MergeCommitInfo represents information about a merge commit
type MergeCommitInfo struct {
	Hash      string    `json:"hash"`
	ShortHash string    `json:"short_hash"`
	Message   string    `json:"message"`
	Author    string    `json:"author"`
	Email     string    `json:"email"`
	Date      time.Time `json:"date"`
	Parents   []string  `json:"parents"`
}

// GetMergeCommits returns a list of merge commits in the repository
func (r *Repository) GetMergeCommits(limit int, since, author string) ([]*MergeCommitInfo, error) {
	// Get commit iterator
	commitIter, err := r.repo.Log(&git.LogOptions{})
	if err != nil {
		return nil, fmt.Errorf("failed to get commit log: %w", err)
	}
	defer commitIter.Close()

	var mergeCommits []*MergeCommitInfo
	var sinceTime time.Time

	// Parse since parameter
	if since != "" {
		sinceTime, err = parseTimeString(since)
		if err != nil {
			return nil, fmt.Errorf("failed to parse since parameter: %w", err)
		}
	}
	log.Debug().Int("limit", limit).Str("since", since).Str("author", author).Msg("listing merge commits")

	// Iterate through commits
	err = commitIter.ForEach(func(commit *object.Commit) error {
		// Stop if we've reached the limit
		if len(mergeCommits) >= limit {
			return fmt.Errorf("limit reached") // Use error to break iteration
		}

		// Check if it's a merge commit (has 2 or more parents)
		if commit.NumParents() < 2 {
			return nil
		}

		// Filter by date if specified
		if !sinceTime.IsZero() && commit.Author.When.Before(sinceTime) {
			return nil
		}

		// Filter by author if specified
		if author != "" {
			if !strings.Contains(strings.ToLower(commit.Author.Name), strings.ToLower(author)) &&
				!strings.Contains(strings.ToLower(commit.Author.Email), strings.ToLower(author)) {
				return nil
			}
		}

		// Get parent hashes
		var parents []string
		parentIter := commit.Parents()
		err := parentIter.ForEach(func(parent *object.Commit) error {
			parents = append(parents, parent.Hash.String()[:8])
			return nil
		})
		if err != nil {
			return err
		}

		// Create merge commit info
		mergeCommit := &MergeCommitInfo{
			Hash:      commit.Hash.String(),
			ShortHash: commit.Hash.String()[:8],
			Message:   strings.Split(commit.Message, "\n")[0], // First line only
			Author:    commit.Author.Name,
			Email:     commit.Author.Email,
			Date:      commit.Author.When,
			Parents:   parents,
		}

		mergeCommits = append(mergeCommits, mergeCommit)
		return nil
	})

	// Ignore "limit reached" error as it's expected
	if err != nil && !strings.Contains(err.Error(), "limit reached") {
		return nil, fmt.Errorf("failed to iterate commits: %w", err)
	}

	log.Debug().Int("merge_commits", len(mergeCommits)).Msg("listed merge commits")
	return mergeCommits, nil
}

// parseTimeString parses various time string formats
func parseTimeString(timeStr string) (time.Time, error) {
	// Try different formats
	formats := []string{
		"2006-01-02",
		"2006-01-02 15:04:05",
		time.RFC3339,
	}

	for _, format := range formats {
		if t, err := time.Parse(format, timeStr); err == nil {
			return t, nil
		}
	}

	// Handle relative time strings like "1 week ago"
	if strings.Contains(timeStr, "ago") {
		return parseRelativeTime(timeStr)
	}

	return time.Time{}, fmt.Errorf("unable to parse time string: %s", timeStr)
}

// parseRelativeTime parses relative time strings like "1 week ago"
func parseRelativeTime(timeStr string) (time.Time, error) {
	now := time.Now()
	
	// Simple parsing for common cases
	if strings.Contains(timeStr, "day") {
		if strings.Contains(timeStr, "1 day") {
			return now.AddDate(0, 0, -1), nil
		}
		// Could extend for other day counts
	}
	
	if strings.Contains(timeStr, "week") {
		if strings.Contains(timeStr, "1 week") {
			return now.AddDate(0, 0, -7), nil
		}
		// Could extend for other week counts
	}
	
	if strings.Contains(timeStr, "month") {
		if strings.Contains(timeStr, "1 month") {
			return now.AddDate(0, -1, 0), nil
		}
		// Could extend for other month counts
	}

	return time.Time{}, fmt.Errorf("unsupported relative time format: %s", timeStr)
}

