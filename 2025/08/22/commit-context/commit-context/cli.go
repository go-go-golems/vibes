package main

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"time"

	"github.com/go-git/go-git/v5"
	"github.com/go-git/go-git/v5/plumbing"
	"github.com/go-git/go-git/v5/plumbing/object"
)

func main() {
	if len(os.Args) < 2 {
		printUsage()
		os.Exit(1)
	}

	command := os.Args[1]
	
	switch command {
	case "attach":
		handleAttach()
	case "attach-from-file":
		handleAttachFromFile()
	case "get":
		handleGet()
	case "list":
		handleList()
	case "install-hooks":
		handleInstallHooks()
	case "uninstall-hooks":
		handleUninstallHooks()
	case "test":
		handleTest()
	default:
		fmt.Printf("Unknown command: %s\n", command)
		printUsage()
		os.Exit(1)
	}
}

func printUsage() {
	fmt.Println("commit-context - Attach LLM context to Git commits")
	fmt.Println()
	fmt.Println("Usage:")
	fmt.Println("  commit-context attach <commit-hash> --agent-id <id> --summary <text> [options]")
	fmt.Println("  commit-context attach-from-file <commit-hash> <context-file>")
	fmt.Println("  commit-context get <commit-hash>")
	fmt.Println("  commit-context list")
	fmt.Println("  commit-context install-hooks")
	fmt.Println("  commit-context uninstall-hooks")
	fmt.Println("  commit-context test")
	fmt.Println()
	fmt.Println("Commands:")
	fmt.Println("  attach           Attach LLM context to a commit")
	fmt.Println("  attach-from-file Attach LLM context from a JSON file")
	fmt.Println("  get              Get LLM context for a commit")
	fmt.Println("  list             List all commits with LLM context")
	fmt.Println("  install-hooks    Install Git hooks for automatic context attachment")
	fmt.Println("  uninstall-hooks  Remove Git hooks")
	fmt.Println("  test             Run basic functionality test")
	fmt.Println()
	fmt.Println("Options for attach:")
	fmt.Println("  --agent-id <id>        Agent identifier")
	fmt.Println("  --prompt-id <id>       Prompt identifier")
	fmt.Println("  --summary <text>       Brief summary")
	fmt.Println("  --chat-file <path>     Path to chat log file")
	fmt.Println("  --explanation <text>   Explanation text")
}

func handleAttach() {
	if len(os.Args) < 3 {
		fmt.Println("Error: commit hash required")
		printUsage()
		os.Exit(1)
	}
	
	commitHashStr := os.Args[2]
	commitHash := plumbing.NewHash(commitHashStr)
	
	// Parse arguments
	var agentID, promptID, summary, chatFile, explanation string
	
	for i := 3; i < len(os.Args); i++ {
		switch os.Args[i] {
		case "--agent-id":
			if i+1 < len(os.Args) {
				agentID = os.Args[i+1]
				i++
			}
		case "--prompt-id":
			if i+1 < len(os.Args) {
				promptID = os.Args[i+1]
				i++
			}
		case "--summary":
			if i+1 < len(os.Args) {
				summary = os.Args[i+1]
				i++
			}
		case "--chat-file":
			if i+1 < len(os.Args) {
				chatFile = os.Args[i+1]
				i++
			}
		case "--explanation":
			if i+1 < len(os.Args) {
				explanation = os.Args[i+1]
				i++
			}
		}
	}
	
	if agentID == "" || summary == "" {
		fmt.Println("Error: --agent-id and --summary are required")
		os.Exit(1)
	}
	
	// Read chat file if provided
	var chatLog string
	if chatFile != "" {
		content, err := os.ReadFile(chatFile)
		if err != nil {
			fmt.Printf("Error reading chat file: %v\n", err)
			os.Exit(1)
		}
		chatLog = string(content)
	}
	
	// Get current directory as repository path
	repoPath, err := os.Getwd()
	if err != nil {
		fmt.Printf("Error getting current directory: %v\n", err)
		os.Exit(1)
	}
	
	// Find git repository root
	repoPath, err = findGitRoot(repoPath)
	if err != nil {
		fmt.Printf("Error finding git repository: %v\n", err)
		os.Exit(1)
	}
	
	// Create context manager
	cm, err := NewContextManager(repoPath)
	if err != nil {
		fmt.Printf("Error creating context manager: %v\n", err)
		os.Exit(1)
	}
	
	// Create attachment
	attachment := &ContextAttachment{
		AgentID:     agentID,
		PromptID:    promptID,
		Summary:     summary,
		ChatLog:     chatLog,
		Explanation: explanation,
		Metadata:    make(map[string]interface{}),
	}
	
	// Attach context
	if err := cm.AttachContext(commitHash, attachment); err != nil {
		fmt.Printf("Error attaching context: %v\n", err)
		os.Exit(1)
	}
	
	fmt.Printf("Successfully attached context to commit %s\n", commitHashStr)
}

func handleGet() {
	if len(os.Args) < 3 {
		fmt.Println("Error: commit hash required")
		printUsage()
		os.Exit(1)
	}
	
	commitHashStr := os.Args[2]
	commitHash := plumbing.NewHash(commitHashStr)
	
	// Get current directory as repository path
	repoPath, err := os.Getwd()
	if err != nil {
		fmt.Printf("Error getting current directory: %v\n", err)
		os.Exit(1)
	}
	
	// Find git repository root
	repoPath, err = findGitRoot(repoPath)
	if err != nil {
		fmt.Printf("Error finding git repository: %v\n", err)
		os.Exit(1)
	}
	
	// Create context manager
	cm, err := NewContextManager(repoPath)
	if err != nil {
		fmt.Printf("Error creating context manager: %v\n", err)
		os.Exit(1)
	}
	
	// Get context
	context, err := cm.GetContext(commitHash)
	if err != nil {
		fmt.Printf("Error getting context: %v\n", err)
		os.Exit(1)
	}
	
	// Print context
	fmt.Printf("Commit: %s\n", commitHashStr)
	fmt.Printf("Agent ID: %s\n", context.Manifest.AgentID)
	fmt.Printf("Prompt ID: %s\n", context.Manifest.PromptID)
	fmt.Printf("Summary: %s\n", context.Manifest.Summary)
	fmt.Printf("Timestamp: %s\n", context.Manifest.Timestamp.Format("2006-01-02 15:04:05"))
	
	if len(context.Manifest.Artifacts) > 0 {
		fmt.Println("\nArtifacts:")
		for _, artifact := range context.Manifest.Artifacts {
			fmt.Printf("  - %s: %s (%d bytes)\n", artifact.Type, artifact.Description, artifact.Size)
		}
	}
	
	if chatLog := context.GetChatLog(); chatLog != "" {
		fmt.Println("\nChat Log:")
		fmt.Println(chatLog)
	}
	
	if explanation := context.GetExplanation(); explanation != "" {
		fmt.Println("\nExplanation:")
		fmt.Println(explanation)
	}
}

func handleList() {
	// Get current directory as repository path
	repoPath, err := os.Getwd()
	if err != nil {
		fmt.Printf("Error getting current directory: %v\n", err)
		os.Exit(1)
	}
	
	// Find git repository root
	repoPath, err = findGitRoot(repoPath)
	if err != nil {
		fmt.Printf("Error finding git repository: %v\n", err)
		os.Exit(1)
	}
	
	// Create context manager
	cm, err := NewContextManager(repoPath)
	if err != nil {
		fmt.Printf("Error creating context manager: %v\n", err)
		os.Exit(1)
	}
	
	// List contexts
	contexts, err := cm.ListContexts()
	if err != nil {
		fmt.Printf("Error listing contexts: %v\n", err)
		os.Exit(1)
	}
	
	if len(contexts) == 0 {
		fmt.Println("No commits with LLM context found")
		return
	}
	
	fmt.Printf("Found %d commits with LLM context:\n\n", len(contexts))
	for _, ctx := range contexts {
		fmt.Printf("Commit: %s\n", ctx.CommitHash.String())
		fmt.Printf("  Author: %s\n", ctx.CommitInfo.Author)
		fmt.Printf("  Date: %s\n", ctx.CommitInfo.Date.Format("2006-01-02 15:04:05"))
		fmt.Printf("  Message: %s\n", ctx.CommitInfo.Message)
		fmt.Printf("  Agent: %s\n", ctx.Manifest.AgentID)
		fmt.Printf("  Summary: %s\n", ctx.Manifest.Summary)
		fmt.Printf("  Artifacts: %d\n", len(ctx.Manifest.Artifacts))
		fmt.Println()
	}
}

func handleTest() {
	fmt.Println("Running basic functionality test...")
	
	// Create a test repository
	testDir := "/tmp/test-commit-context-cli"
	if err := os.RemoveAll(testDir); err != nil {
		fmt.Printf("Warning: failed to remove test dir: %v\n", err)
	}
	
	if err := os.MkdirAll(testDir, 0755); err != nil {
		fmt.Printf("Error creating test dir: %v\n", err)
		os.Exit(1)
	}
	
	// Change to test directory
	originalDir, _ := os.Getwd()
	defer os.Chdir(originalDir)
	os.Chdir(testDir)
	
	// Run the basic test
	testBasicFunctionality()
}

func findGitRoot(startPath string) (string, error) {
	path := startPath
	for {
		gitDir := filepath.Join(path, ".git")
		if _, err := os.Stat(gitDir); err == nil {
			return path, nil
		}
		
		parent := filepath.Dir(path)
		if parent == path {
			return "", fmt.Errorf("not in a git repository")
		}
		path = parent
	}
}

func testBasicFunctionality() {
	// This runs the same test as test_basic.go but in CLI context
	main := func() {
		// Create a test repository
		testDir := "/tmp/test-commit-context-cli"
		
		// Initialize git repository
		repo, err := git.PlainInit(testDir, false)
		if err != nil {
			fmt.Printf("Failed to init repository: %v\n", err)
			os.Exit(1)
		}
		
		// Create a test file and commit
		testFile := filepath.Join(testDir, "test.txt")
		if err := os.WriteFile(testFile, []byte("Hello, World!"), 0644); err != nil {
			fmt.Printf("Failed to write test file: %v\n", err)
			os.Exit(1)
		}
		
		// Add file to staging
		worktree, err := repo.Worktree()
		if err != nil {
			fmt.Printf("Failed to get worktree: %v\n", err)
			os.Exit(1)
		}
		
		if _, err := worktree.Add("test.txt"); err != nil {
			fmt.Printf("Failed to add file: %v\n", err)
			os.Exit(1)
		}
		
		// Create commit
		commitHash, err := worktree.Commit("Initial commit", &git.CommitOptions{
			Author: &object.Signature{
				Name:  "Test User",
				Email: "test@example.com",
				When:  time.Now(),
			},
		})
		if err != nil {
			fmt.Printf("Failed to commit: %v\n", err)
			os.Exit(1)
		}
		
		fmt.Printf("Created test commit: %s\n", commitHash.String())
		
		fmt.Println("✓ Test completed successfully")
	}
	
	main()
}


func handleAttachFromFile() {
	if len(os.Args) < 4 {
		fmt.Println("Error: commit hash and context file required")
		printUsage()
		os.Exit(1)
	}
	
	commitHashStr := os.Args[2]
	contextFile := os.Args[3]
	
	commitHash := plumbing.NewHash(commitHashStr)
	
	// Read context file
	content, err := os.ReadFile(contextFile)
	if err != nil {
		fmt.Printf("Error reading context file: %v\n", err)
		os.Exit(1)
	}
	
	// Parse JSON context
	var contextData struct {
		AgentID     string                 `json:"agent_id"`
		PromptID    string                 `json:"prompt_id"`
		Summary     string                 `json:"summary"`
		ChatLog     string                 `json:"chat_log"`
		Explanation string                 `json:"explanation"`
		Metadata    map[string]interface{} `json:"metadata"`
	}
	
	if err := json.Unmarshal(content, &contextData); err != nil {
		fmt.Printf("Error parsing context file: %v\n", err)
		os.Exit(1)
	}
	
	// Get current directory as repository path
	repoPath, err := os.Getwd()
	if err != nil {
		fmt.Printf("Error getting current directory: %v\n", err)
		os.Exit(1)
	}
	
	// Find git repository root
	repoPath, err = findGitRoot(repoPath)
	if err != nil {
		fmt.Printf("Error finding git repository: %v\n", err)
		os.Exit(1)
	}
	
	// Create context manager
	cm, err := NewContextManager(repoPath)
	if err != nil {
		fmt.Printf("Error creating context manager: %v\n", err)
		os.Exit(1)
	}
	
	// Create attachment
	attachment := &ContextAttachment{
		AgentID:     contextData.AgentID,
		PromptID:    contextData.PromptID,
		Summary:     contextData.Summary,
		ChatLog:     contextData.ChatLog,
		Explanation: contextData.Explanation,
		Metadata:    contextData.Metadata,
	}
	
	// Attach context
	if err := cm.AttachContext(commitHash, attachment); err != nil {
		fmt.Printf("Error attaching context: %v\n", err)
		os.Exit(1)
	}
	
	fmt.Printf("Successfully attached context from file to commit %s\n", commitHashStr)
}

func handleInstallHooks() {
	// Get current directory as repository path
	repoPath, err := os.Getwd()
	if err != nil {
		fmt.Printf("Error getting current directory: %v\n", err)
		os.Exit(1)
	}
	
	// Find git repository root
	repoPath, err = findGitRoot(repoPath)
	if err != nil {
		fmt.Printf("Error finding git repository: %v\n", err)
		os.Exit(1)
	}
	
	// Create hook manager
	hm := NewHookManager(repoPath)
	
	// Check if already installed
	if hm.IsInstalled() {
		fmt.Println("Git hooks are already installed")
		return
	}
	
	// Install hooks
	if err := hm.InstallHooks(); err != nil {
		fmt.Printf("Error installing hooks: %v\n", err)
		os.Exit(1)
	}
	
	fmt.Println("Git hooks installed successfully!")
	fmt.Println()
	fmt.Println("Usage:")
	fmt.Println("1. Set environment variables before committing:")
	fmt.Println("   export LLM_AGENT_ID=\"your-agent-id\"")
	fmt.Println("   export LLM_SUMMARY=\"Brief description of changes\"")
	fmt.Println("   export LLM_PROMPT_ID=\"optional-prompt-id\"")
	fmt.Println("   export LLM_CHAT_FILE=\"path/to/chat.md\"")
	fmt.Println("   export LLM_EXPLANATION=\"Optional explanation\"")
	fmt.Println()
	fmt.Println("2. Or create a .commit-context.json file with context data")
	fmt.Println()
	fmt.Println("The hooks will automatically attach context to commits and push notes/meta branch")
}

func handleUninstallHooks() {
	// Get current directory as repository path
	repoPath, err := os.Getwd()
	if err != nil {
		fmt.Printf("Error getting current directory: %v\n", err)
		os.Exit(1)
	}
	
	// Find git repository root
	repoPath, err = findGitRoot(repoPath)
	if err != nil {
		fmt.Printf("Error finding git repository: %v\n", err)
		os.Exit(1)
	}
	
	// Create hook manager
	hm := NewHookManager(repoPath)
	
	// Uninstall hooks
	if err := hm.UninstallHooks(); err != nil {
		fmt.Printf("Error uninstalling hooks: %v\n", err)
		os.Exit(1)
	}
	
	fmt.Println("Git hooks uninstalled successfully!")
}

