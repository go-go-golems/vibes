package github

import (
	"context"
	"fmt"
	"os"

	"github.com/google/go-github/v66/github"
	"golang.org/x/oauth2"
)

// Client wraps the GitHub API client
type Client struct {
	client *github.Client
}

// NewClient creates a new GitHub API client
func NewClient() *Client {
	var client *github.Client

	// Check for GitHub token
	token := os.Getenv("GITHUB_TOKEN")
	if token != "" {
		ts := oauth2.StaticTokenSource(
			&oauth2.Token{AccessToken: token},
		)
		tc := oauth2.NewClient(context.Background(), ts)
		client = github.NewClient(tc)
	} else {
		// Use unauthenticated client (with rate limits)
		client = github.NewClient(nil)
	}

	return &Client{client: client}
}

// GetPullRequest retrieves a pull request
func (c *Client) GetPullRequest(ctx context.Context, owner, repo string, number int) (*github.PullRequest, error) {
	pr, _, err := c.client.PullRequests.Get(ctx, owner, repo, number)
	if err != nil {
		return nil, fmt.Errorf("failed to get pull request: %w", err)
	}
	return pr, nil
}

// GetPullRequestDiff retrieves the diff for a pull request
func (c *Client) GetPullRequestDiff(ctx context.Context, owner, repo string, number int) (string, error) {
	opt := github.RawOptions{Type: github.Diff}
	diff, _, err := c.client.PullRequests.GetRaw(ctx, owner, repo, number, opt)
	if err != nil {
		return "", fmt.Errorf("failed to get pull request diff: %w", err)
	}
	return diff, nil
}

// GetPullRequestCommits retrieves commits for a pull request
func (c *Client) GetPullRequestCommits(ctx context.Context, owner, repo string, number int) ([]*github.RepositoryCommit, error) {
	opts := &github.ListOptions{PerPage: 100}
	var allCommits []*github.RepositoryCommit

	for {
		commits, resp, err := c.client.PullRequests.ListCommits(ctx, owner, repo, number, opts)
		if err != nil {
			return nil, fmt.Errorf("failed to get pull request commits: %w", err)
		}

		allCommits = append(allCommits, commits...)

		if resp.NextPage == 0 {
			break
		}
		opts.Page = resp.NextPage
	}

	return allCommits, nil
}

// GetFileCommits retrieves commit history for a specific file
func (c *Client) GetFileCommits(ctx context.Context, owner, repo, path string) ([]*github.RepositoryCommit, error) {
	opts := &github.CommitsListOptions{
		Path: path,
		ListOptions: github.ListOptions{PerPage: 100},
	}
	var allCommits []*github.RepositoryCommit

	for {
		commits, resp, err := c.client.Repositories.ListCommits(ctx, owner, repo, opts)
		if err != nil {
			return nil, fmt.Errorf("failed to get file commits: %w", err)
		}

		allCommits = append(allCommits, commits...)

		if resp.NextPage == 0 {
			break
		}
		opts.Page = resp.NextPage
	}

	return allCommits, nil
}

// GetPullRequestFiles retrieves the files changed in a pull request
func (c *Client) GetPullRequestFiles(ctx context.Context, owner, repo string, number int) ([]*github.CommitFile, error) {
	opts := &github.ListOptions{PerPage: 100}
	var allFiles []*github.CommitFile

	for {
		files, resp, err := c.client.PullRequests.ListFiles(ctx, owner, repo, number, opts)
		if err != nil {
			return nil, fmt.Errorf("failed to get pull request files: %w", err)
		}

		allFiles = append(allFiles, files...)

		if resp.NextPage == 0 {
			break
		}
		opts.Page = resp.NextPage
	}

	return allFiles, nil
}

// GetFileContent retrieves the content of a file at a specific commit
func (c *Client) GetFileContent(ctx context.Context, owner, repo, path, ref string) (string, error) {
	opts := &github.RepositoryContentGetOptions{Ref: ref}
	fileContent, _, _, err := c.client.Repositories.GetContents(ctx, owner, repo, path, opts)
	if err != nil {
		return "", fmt.Errorf("failed to get file content: %w", err)
	}

	if fileContent == nil {
		return "", fmt.Errorf("file not found")
	}

	content, err := fileContent.GetContent()
	if err != nil {
		return "", fmt.Errorf("failed to decode file content: %w", err)
	}

	return content, nil
}

