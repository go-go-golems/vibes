package tmux

import (
	"fmt"
	"os"
	"os/exec"
	"strings"

	gotmux "github.com/jubnzv/go-tmux"
)

// TmuxManager handles tmux session management for the REPL
type TmuxManager struct {
	server       *gotmux.Server
	sessionName  string
	mainWindow   *gotmux.Window
	editorWindow *gotmux.Window
	logWindow    *gotmux.Window
}

// New creates a new TmuxManager instance
func New(sessionName string) *TmuxManager {
	return &TmuxManager{
		server:      new(gotmux.Server),
		sessionName: sessionName,
	}
}

// Initialize sets up the tmux environment
func (tm *TmuxManager) Initialize() error {
	// Check if we're already in a tmux session
	if os.Getenv("TMUX") != "" {
		return fmt.Errorf("already running inside a tmux session")
	}

	// Check if the session already exists
	exists, err := tm.server.HasSession(tm.sessionName)
	if err != nil {
		return fmt.Errorf("failed to check for existing session: %w", err)
	}

	if exists {
		// Kill the existing session if it exists
		err = tm.server.KillSession(tm.sessionName)
		if err != nil {
			return fmt.Errorf("failed to kill existing session: %w", err)
		}
	}

	// Create a new session with windows
	session := gotmux.Session{Name: tm.sessionName}
	
	// Main window for REPL
	tm.mainWindow = &gotmux.Window{Name: "repl", Id: 0}
	
	// Editor window
	tm.editorWindow = &gotmux.Window{Name: "editor", Id: 1}
	
	// Log window
	tm.logWindow = &gotmux.Window{Name: "logs", Id: 2}
	
	session.AddWindow(*tm.mainWindow)
	session.AddWindow(*tm.editorWindow)
	session.AddWindow(*tm.logWindow)
	
	tm.server.AddSession(session)
	
	// Apply the configuration
	conf := gotmux.Configuration{
		Server:        tm.server,
		Sessions:      []*gotmux.Session{&session},
		ActiveSession: &session,
	}
	
	return conf.Apply()
}

// SpawnEditor opens an editor in the editor window with the given content
func (tm *TmuxManager) SpawnEditor(filename string, content string) error {
	// Save content to file
	err := os.WriteFile(filename, []byte(content), 0644)
	if err != nil {
		return fmt.Errorf("failed to write content to file: %w", err)
	}
	
	// Get the editor from environment or default to vim
	editor := os.Getenv("EDITOR")
	if editor == "" {
		editor = "vim"
	}
	
	// Use tmux command-line to send keys to the editor window
	cmd := exec.Command("tmux", "send-keys", "-t", 
		fmt.Sprintf("%s:%d", tm.sessionName, tm.editorWindow.Id), 
		fmt.Sprintf("%s %s", editor, filename), "Enter")
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to send keys to editor window: %w", err)
	}
	
	// Switch to the editor window
	return tm.SelectWindow(tm.editorWindow.Id)
}

// ReadEditorContent reads the content from a file after editing
func (tm *TmuxManager) ReadEditorContent(filename string) (string, error) {
	content, err := os.ReadFile(filename)
	if err != nil {
		return "", fmt.Errorf("failed to read file: %w", err)
	}
	return string(content), nil
}

// SwitchToMainWindow switches back to the main REPL window
func (tm *TmuxManager) SwitchToMainWindow() error {
	return tm.SelectWindow(tm.mainWindow.Id)
}

// SwitchToLogWindow switches to the log window
func (tm *TmuxManager) SwitchToLogWindow() error {
	return tm.SelectWindow(tm.logWindow.Id)
}

// SelectWindow selects a window by ID
func (tm *TmuxManager) SelectWindow(windowId int) error {
	cmd := exec.Command("tmux", "select-window", "-t", 
		fmt.Sprintf("%s:%d", tm.sessionName, windowId))
	return cmd.Run()
}

// SendToLogWindow sends content to the log window
func (tm *TmuxManager) SendToLogWindow(content string) error {
	// Escape any quotes in the content
	escapedContent := strings.ReplaceAll(content, "\"", "\\\"")
	
	// Send the echo command to the log window
	cmd := exec.Command("tmux", "send-keys", "-t", 
		fmt.Sprintf("%s:%d", tm.sessionName, tm.logWindow.Id), 
		fmt.Sprintf("echo \"%s\"", escapedContent), "Enter")
	
	return cmd.Run()
}

// AttachToSession attaches to the tmux session
func (tm *TmuxManager) AttachToSession() error {
	cmd := exec.Command("tmux", "attach-session", "-t", tm.sessionName)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

// DetachSession detaches from the current tmux session
func (tm *TmuxManager) DetachSession() error {
	cmd := exec.Command("tmux", "send-keys", "-t", 
		fmt.Sprintf("%s:%d", tm.sessionName, tm.mainWindow.Id), 
		"C-b d")
	return cmd.Run()
}

// KillSession kills the tmux session
func (tm *TmuxManager) KillSession() error {
	return tm.server.KillSession(tm.sessionName)
}

// IsInsideTmux checks if we're currently inside a tmux session
func IsInsideTmux() bool {
	return os.Getenv("TMUX") != ""
}

// SpawnVimAndCaptureCode spawns vim in a tmux pane and returns the edited code
func (tm *TmuxManager) SpawnVimAndCaptureCode(initialContent string) (string, error) {
	// Create a temporary file
	tmpFile, err := os.CreateTemp("", "goja-repl-*.js")
	if err != nil {
		return "", fmt.Errorf("failed to create temp file: %w", err)
	}
	defer os.Remove(tmpFile.Name())
	
	// Write initial content to the file
	if initialContent != "" {
		if _, err := tmpFile.WriteString(initialContent); err != nil {
			return "", fmt.Errorf("failed to write to temp file: %w", err)
		}
	}
	tmpFile.Close()
	
	// Switch to editor window and open vim
	if err := tm.SwitchToLogWindow(); err != nil {
		return "", fmt.Errorf("failed to switch to editor window: %w", err)
	}
	
	// Open vim with the temp file
	cmd := exec.Command("tmux", "send-keys", "-t", 
		fmt.Sprintf("%s:%d", tm.sessionName, tm.logWindow.Id), 
		fmt.Sprintf("vim %s", tmpFile.Name()), "Enter")
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("failed to open vim: %w", err)
	}
	
	// Wait for user to finish editing (this will block until vim exits)
	fmt.Println("Editing in vim. Please save and quit when done (e.g., :wq)")
	
	// Read the content after editing
	content, err := os.ReadFile(tmpFile.Name())
	if err != nil {
		return "", fmt.Errorf("failed to read edited content: %w", err)
	}
	
	// Switch back to main window
	if err := tm.SwitchToMainWindow(); err != nil {
		return "", fmt.Errorf("failed to switch back to main window: %w", err)
	}
	
	return string(content), nil
}
