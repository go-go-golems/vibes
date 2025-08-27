package git

import (
	"os"
	"path/filepath"
	"testing"
	"time"

	git "github.com/go-git/go-git/v5"
	"github.com/go-git/go-git/v5/plumbing/object"
)

// writeFile is a tiny helper to write a file with content, creating directories as needed.
func writeFile(t *testing.T, path string, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("write file: %v", err)
	}
}

// initTempRepo creates a temp repo with an initial commit to establish HEAD.
func initTempRepo(t *testing.T) (repoDir string, repo *git.Repository, wt *git.Worktree) {
	t.Helper()
	repoDir = t.TempDir()
	repo, err := git.PlainInit(repoDir, false)
	if err != nil {
		t.Fatalf("init repo: %v", err)
	}
	wt, err = repo.Worktree()
	if err != nil {
		t.Fatalf("worktree: %v", err)
	}

	// Create initial commit so HEAD exists
	writeFile(t, filepath.Join(repoDir, "init.txt"), "init")
	if _, err := wt.Add("init.txt"); err != nil {
		t.Fatalf("add init: %v", err)
	}
	if _, err := wt.Commit("initial", &git.CommitOptions{
		Author: &object.Signature{Name: "Test", Email: "test@example.com", When: time.Now()},
	}); err != nil {
		t.Fatalf("commit init: %v", err)
	}
	return repoDir, repo, wt
}

func withChdir(t *testing.T, dir string) {
	t.Helper()
	cwd, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	if err := os.Chdir(dir); err != nil {
		t.Fatalf("chdir: %v", err)
	}
	t.Cleanup(func() {
		_ = os.Chdir(cwd)
	})
}

func TestRepositoryIntrospection(t *testing.T) {
	repoDir, _, _ := initTempRepo(t)
	withChdir(t, repoDir)

	if !IsGitRepository() {
		t.Fatalf("expected to be in a git repository")
	}
	root, err := GetRepositoryRoot()
	if err != nil {
		t.Fatalf("GetRepositoryRoot: %v", err)
	}
	if root != repoDir {
		t.Fatalf("root mismatch: got %s want %s", root, repoDir)
	}
	gitDir, err := GetGitDir()
	if err != nil {
		t.Fatalf("GetGitDir: %v", err)
	}
	if filepath.Base(gitDir) != ".git" {
		t.Fatalf("gitDir base should be .git, got %s", gitDir)
	}

	// Also test from a subdirectory
	sub := filepath.Join(repoDir, "sub", "dir")
	if err := os.MkdirAll(sub, 0o755); err != nil {
		t.Fatalf("mkdir sub: %v", err)
	}
	withChdir(t, sub)
	root2, err := GetRepositoryRoot()
	if err != nil {
		t.Fatalf("GetRepositoryRoot from sub: %v", err)
	}
	if root2 != repoDir {
		t.Fatalf("root mismatch from sub: got %s want %s", root2, repoDir)
	}
}

func TestStagedFilesLifecycle(t *testing.T) {
	repoDir, repo, wt := initTempRepo(t)
	withChdir(t, repoDir)

	// No staged changes initially
	if has, err := HasStagedChanges(); err != nil || has {
		t.Fatalf("HasStagedChanges initially: has=%v err=%v", has, err)
	}
	if paths, err := GetStagedFilePaths(); err != nil || len(paths) != 0 {
		t.Fatalf("GetStagedFilePaths initially: %v %v", paths, err)
	}

	// Stage a new file (Added)
	writeFile(t, filepath.Join(repoDir, "file1.txt"), "hello")
	if _, err := wt.Add("file1.txt"); err != nil {
		t.Fatalf("add file1: %v", err)
	}
	if has, err := HasStagedChanges(); err != nil || !has {
		t.Fatalf("HasStagedChanges after add: has=%v err=%v", has, err)
	}
	if paths, err := GetStagedFilePaths(); err != nil || len(paths) != 1 || paths[0] != "file1.txt" {
		t.Fatalf("GetStagedFilePaths after add: %v %v", paths, err)
	}
	if files, err := GetStagedFiles(); err != nil || len(files) != 1 || files[0].Path != "file1.txt" || files[0].Status != "A" {
		t.Fatalf("GetStagedFiles after add: %v %v", files, err)
	}

	// Commit the added file
	if _, err := wt.Commit("add file1", &git.CommitOptions{Author: &object.Signature{Name: "Test", Email: "t@example.com", When: time.Now()}}); err != nil {
		t.Fatalf("commit add file1: %v", err)
	}

	// Modify without staging should not appear as staged
	writeFile(t, filepath.Join(repoDir, "file1.txt"), "hello world")
	if has, err := HasStagedChanges(); err != nil || has {
		t.Fatalf("HasStagedChanges after modify but not stage: has=%v err=%v", has, err)
	}
	if paths, err := GetStagedFilePaths(); err != nil || len(paths) != 0 {
		t.Fatalf("GetStagedFilePaths after modify not staged: %v %v", paths, err)
	}

	// Stage modification (Modified)
	if _, err := wt.Add("file1.txt"); err != nil {
		t.Fatalf("add modified file1: %v", err)
	}
	if has, err := HasStagedChanges(); err != nil || !has {
		t.Fatalf("HasStagedChanges after stage modify: has=%v err=%v", has, err)
	}
	if files, err := GetStagedFiles(); err != nil || len(files) != 1 || files[0].Status != "M" {
		t.Fatalf("GetStagedFiles after modify stage: %v %v", files, err)
	}

	// Commit modification
	if _, err := wt.Commit("modify file1", &git.CommitOptions{Author: &object.Signature{Name: "Test", Email: "t@example.com", When: time.Now()}}); err != nil {
		t.Fatalf("commit modify file1: %v", err)
	}

	// Create file to later delete
	writeFile(t, filepath.Join(repoDir, "gone.txt"), "bye")
	if _, err := wt.Add("gone.txt"); err != nil {
		t.Fatalf("add gone: %v", err)
	}
	if _, err := wt.Commit("add gone", &git.CommitOptions{Author: &object.Signature{Name: "Test", Email: "t@example.com", When: time.Now()}}); err != nil {
		t.Fatalf("commit gone: %v", err)
	}

	// Delete and stage deletion
	if err := os.Remove(filepath.Join(repoDir, "gone.txt")); err != nil {
		t.Fatalf("remove gone: %v", err)
	}
	if _, err := wt.Remove("gone.txt"); err != nil {
		t.Fatalf("stage remove gone: %v", err)
	}
	if files, err := GetStagedFiles(); err != nil || len(files) != 1 || files[0].Path != "gone.txt" || files[0].Status != "D" {
		t.Fatalf("GetStagedFiles after deletion stage: %v %v", files, err)
	}
	if paths, err := GetStagedFilePaths(); err != nil || len(paths) != 0 { // deletions excluded
		t.Fatalf("GetStagedFilePaths should exclude deletions: %v %v", paths, err)
	}

	// Silence unused variable lints
	_ = repo
}

func TestWorktreePointerDetection(t *testing.T) {
	// Base repo
	repoDir, _, _ := initTempRepo(t)

	// Create a simulated worktree gitdir path under the main .git dir
	worktreesDir := filepath.Join(repoDir, ".git", "worktrees", "wt1")
	if err := os.MkdirAll(worktreesDir, 0o755); err != nil {
		t.Fatalf("mkdir worktree gitdir: %v", err)
	}

	// Create a separate working directory that will act as the linked worktree
	wtDir := filepath.Join(repoDir, "linked-wt")
	if err := os.MkdirAll(wtDir, 0o755); err != nil {
		t.Fatalf("mkdir linked-wt: %v", err)
	}

	// Write .git file pointing to the simulated worktree gitdir
	gitFile := filepath.Join(wtDir, ".git")
	content := []byte("gitdir: " + worktreesDir + "\n")
	if err := os.WriteFile(gitFile, content, 0o644); err != nil {
		t.Fatalf("write .git file: %v", err)
	}

	// Now operate from the linked worktree directory
	withChdir(t, wtDir)

	if !IsGitRepository() {
		t.Fatalf("expected linked worktree dir to be recognized as git repository")
	}
	root, err := GetRepositoryRoot()
	if err != nil {
		t.Fatalf("GetRepositoryRoot (worktree): %v", err)
	}
	if root != wtDir {
		t.Fatalf("worktree root mismatch: got %s want %s", root, wtDir)
	}
	gitDir, err := GetGitDir()
	if err != nil {
		t.Fatalf("GetGitDir (worktree): %v", err)
	}
	if gitDir != worktreesDir {
		t.Fatalf("worktree gitdir mismatch: got %s want %s", gitDir, worktreesDir)
	}
}


