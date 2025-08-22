package main

import (
	"fmt"
	"os"
	"path/filepath"
)

// HookManager manages Git hooks for automatic context attachment
type HookManager struct {
	repoPath string
	hooksDir string
}

// NewHookManager creates a new hook manager
func NewHookManager(repoPath string) *HookManager {
	hooksDir := filepath.Join(repoPath, ".git", "hooks")
	return &HookManager{
		repoPath: repoPath,
		hooksDir: hooksDir,
	}
}

// InstallHooks installs the Git hooks for automatic context attachment
func (hm *HookManager) InstallHooks() error {
	// Install post-commit hook
	if err := hm.installPostCommitHook(); err != nil {
		return fmt.Errorf("failed to install post-commit hook: %w", err)
	}
	
	// Install pre-push hook
	if err := hm.installPrePushHook(); err != nil {
		return fmt.Errorf("failed to install pre-push hook: %w", err)
	}
	
	return nil
}

// installPostCommitHook installs the post-commit hook
func (hm *HookManager) installPostCommitHook() error {
	hookPath := filepath.Join(hm.hooksDir, "post-commit")
	
	// Check if hook already exists
	if _, err := os.Stat(hookPath); err == nil {
		// Backup existing hook
		backupPath := hookPath + ".backup"
		if err := os.Rename(hookPath, backupPath); err != nil {
			return fmt.Errorf("failed to backup existing post-commit hook: %w", err)
		}
		fmt.Printf("Backed up existing post-commit hook to %s\n", backupPath)
	}
	
	// Create the hook script
	hookContent := `#!/bin/sh
# commit-context post-commit hook
# This hook automatically processes LLM context after each commit

# Get the commit hash
COMMIT_HASH=$(git rev-parse HEAD)

# Check if there's a context file to process
CONTEXT_FILE=".commit-context.json"
if [ -f "$CONTEXT_FILE" ]; then
    echo "Processing LLM context for commit $COMMIT_HASH..."
    
    # Use commit-context tool to attach the context
    commit-context attach-from-file "$COMMIT_HASH" "$CONTEXT_FILE"
    
    # Remove the context file after processing
    rm "$CONTEXT_FILE"
    
    echo "LLM context attached successfully"
fi

# Check for environment variables with context
if [ -n "$LLM_AGENT_ID" ] && [ -n "$LLM_SUMMARY" ]; then
    echo "Processing LLM context from environment for commit $COMMIT_HASH..."
    
    ARGS="--agent-id \"$LLM_AGENT_ID\" --summary \"$LLM_SUMMARY\""
    
    if [ -n "$LLM_PROMPT_ID" ]; then
        ARGS="$ARGS --prompt-id \"$LLM_PROMPT_ID\""
    fi
    
    if [ -n "$LLM_CHAT_FILE" ]; then
        ARGS="$ARGS --chat-file \"$LLM_CHAT_FILE\""
    fi
    
    if [ -n "$LLM_EXPLANATION" ]; then
        ARGS="$ARGS --explanation \"$LLM_EXPLANATION\""
    fi
    
    eval "commit-context attach \"$COMMIT_HASH\" $ARGS"
    
    echo "LLM context attached from environment"
fi
`
	
	if err := os.WriteFile(hookPath, []byte(hookContent), 0755); err != nil {
		return fmt.Errorf("failed to write post-commit hook: %w", err)
	}
	
	fmt.Printf("Installed post-commit hook at %s\n", hookPath)
	return nil
}

// installPrePushHook installs the pre-push hook
func (hm *HookManager) installPrePushHook() error {
	hookPath := filepath.Join(hm.hooksDir, "pre-push")
	
	// Check if hook already exists
	if _, err := os.Stat(hookPath); err == nil {
		// Backup existing hook
		backupPath := hookPath + ".backup"
		if err := os.Rename(hookPath, backupPath); err != nil {
			return fmt.Errorf("failed to backup existing pre-push hook: %w", err)
		}
		fmt.Printf("Backed up existing pre-push hook to %s\n", backupPath)
	}
	
	// Create the hook script
	hookContent := `#!/bin/sh
# commit-context pre-push hook
# This hook ensures notes and meta branch are pushed along with commits

remote="$1"
url="$2"

echo "Pushing LLM context notes and meta branch..."

# Push the notes ref
git push "$remote" refs/notes/llm:refs/notes/llm 2>/dev/null || echo "No LLM notes to push"

# Push the meta branch
git push "$remote" meta:meta 2>/dev/null || echo "No meta branch to push"

echo "LLM context push completed"
`
	
	if err := os.WriteFile(hookPath, []byte(hookContent), 0755); err != nil {
		return fmt.Errorf("failed to write pre-push hook: %w", err)
	}
	
	fmt.Printf("Installed pre-push hook at %s\n", hookPath)
	return nil
}

// UninstallHooks removes the Git hooks
func (hm *HookManager) UninstallHooks() error {
	hooks := []string{"post-commit", "pre-push"}
	
	for _, hook := range hooks {
		hookPath := filepath.Join(hm.hooksDir, hook)
		backupPath := hookPath + ".backup"
		
		// Remove the hook
		if err := os.Remove(hookPath); err != nil && !os.IsNotExist(err) {
			return fmt.Errorf("failed to remove %s hook: %w", hook, err)
		}
		
		// Restore backup if it exists
		if _, err := os.Stat(backupPath); err == nil {
			if err := os.Rename(backupPath, hookPath); err != nil {
				return fmt.Errorf("failed to restore %s hook backup: %w", hook, err)
			}
			fmt.Printf("Restored %s hook from backup\n", hook)
		} else {
			fmt.Printf("Removed %s hook\n", hook)
		}
	}
	
	return nil
}

// IsInstalled checks if the hooks are installed
func (hm *HookManager) IsInstalled() bool {
	postCommitPath := filepath.Join(hm.hooksDir, "post-commit")
	prePushPath := filepath.Join(hm.hooksDir, "pre-push")
	
	_, postCommitExists := os.Stat(postCommitPath)
	_, prePushExists := os.Stat(prePushPath)
	
	return postCommitExists == nil && prePushExists == nil
}

