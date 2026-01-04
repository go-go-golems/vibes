package main

import (
	"fmt"
	"strings"

	"github.com/GianlucaP106/gotmux/gotmux"
)

// TmuxManager handles tmux session management
type TmuxManager struct {
	tmux   *gotmux.Tmux
	dryRun bool
}

// NewTmuxManager creates a new tmux manager
func NewTmuxManager(dryRun bool) (*TmuxManager, error) {
	if dryRun {
		return &TmuxManager{
			tmux:   nil,
			dryRun: true,
		}, nil
	}

	tmux, err := gotmux.DefaultTmux()
	if err != nil {
		return nil, fmt.Errorf("failed to initialize tmux client: %w", err)
	}

	return &TmuxManager{
		tmux:   tmux,
		dryRun: false,
	}, nil
}

// ApplyConfig applies the configuration to create tmux session
func (tm *TmuxManager) ApplyConfig(config *Config) error {
	if tm.dryRun {
		return tm.printDryRun(config)
	}

	// Check if session already exists and kill it
	sessions, err := tm.tmux.ListSessions()
	if err != nil {
		return fmt.Errorf("failed to list sessions: %w", err)
	}

	for _, session := range sessions {
		if session.Name == config.Session {
			// Kill existing session
			if err := session.Kill(); err != nil {
				return fmt.Errorf("failed to kill existing session: %w", err)
			}
			break
		}
	}

	// Create new session with first tab
	if len(config.Tabs) == 0 {
		return fmt.Errorf("no tabs defined")
	}

	firstTab := config.Tabs[0]
	session, err := tm.tmux.NewSession(&gotmux.SessionOptions{
		StartDirectory: "",
		Name:          config.Session,
	})
	if err != nil {
		return fmt.Errorf("failed to create session: %w", err)
	}

	// Get the first window and rename it
	windows, err := session.ListWindows()
	if err != nil {
		return fmt.Errorf("failed to list windows: %w", err)
	}

	if len(windows) > 0 {
		firstWindow := windows[0]
		if err := firstWindow.Rename(firstTab.Name); err != nil {
			return fmt.Errorf("failed to rename first window: %w", err)
		}
		if err := tm.setupWindow(firstWindow, firstTab); err != nil {
			return fmt.Errorf("failed to setup first window: %w", err)
		}
	}

	// Create additional windows
	for i := 1; i < len(config.Tabs); i++ {
		tab := config.Tabs[i]
		window, err := session.New()
		if err != nil {
			return fmt.Errorf("failed to create window '%s': %w", tab.Name, err)
		}

		if err := window.Rename(tab.Name); err != nil {
			return fmt.Errorf("failed to rename window '%s': %w", tab.Name, err)
		}

		if err := tm.setupWindow(window, tab); err != nil {
			return fmt.Errorf("failed to setup window '%s': %w", tab.Name, err)
		}
	}

	return nil
}

// setupWindow configures a window with panes and commands
func (tm *TmuxManager) setupWindow(window *gotmux.Window, tab Tab) error {
	// Get the first pane (already exists)
	panes, err := window.ListPanes()
	if err != nil {
		return fmt.Errorf("failed to list panes: %w", err)
	}

	if len(panes) == 0 {
		return fmt.Errorf("no panes found in window")
	}

	// Setup first pane
	if len(tab.Panes) > 0 {
		if err := tm.setupPane(panes[0], tab.Panes[0]); err != nil {
			return fmt.Errorf("failed to setup first pane: %w", err)
		}
	}

	// Create additional panes
	for i := 1; i < len(tab.Panes); i++ {
		err := panes[0].Split()
		if err != nil {
			return fmt.Errorf("failed to split window for pane %d: %w", i, err)
		}

		// Get updated pane list after split
		panes, err = window.ListPanes()
		if err != nil {
			return fmt.Errorf("failed to list panes after split: %w", err)
		}

		if len(panes) <= i {
			return fmt.Errorf("expected at least %d panes after split, got %d", i+1, len(panes))
		}

		if err := tm.setupPane(panes[i], tab.Panes[i]); err != nil {
			return fmt.Errorf("failed to setup pane %d: %w", i, err)
		}
	}

	// Apply layout if specified
	layout := tab.Layout
	if layout == "" {
		layout = "tiled"
	}

	// Convert layout name to gotmux layout constant
	var tmuxLayout gotmux.WindowLayout
	switch layout {
	case "tiled":
		tmuxLayout = gotmux.WindowLayoutTiled
	case "even-vertical":
		tmuxLayout = gotmux.WindowLayoutEvenVertical
	case "even-horizontal":
		tmuxLayout = gotmux.WindowLayoutEvenHorizontal
	case "main-vertical":
		tmuxLayout = gotmux.WindowLayoutMainVertical
	case "main-horizontal":
		tmuxLayout = gotmux.WindowLayoutMainVertical // Use MainVertical as fallback
	default:
		tmuxLayout = gotmux.WindowLayoutTiled
	}

	if err := window.SelectLayout(tmuxLayout); err != nil {
		return fmt.Errorf("failed to apply layout '%s': %w", layout, err)
	}

	return nil
}

// setupPane configures a pane with command and environment
func (tm *TmuxManager) setupPane(pane *gotmux.Pane, paneConfig Pane) error {
	// Set environment variables
	for key, value := range paneConfig.Env {
		envCmd := fmt.Sprintf("export %s=%s", key, value)
		if err := pane.SendKeys(envCmd); err != nil {
			return fmt.Errorf("failed to set environment variable %s: %w", key, err)
		}
		if err := pane.SendKeys("C-m"); err != nil {
			return fmt.Errorf("failed to press enter for environment variable %s: %w", key, err)
		}
	}

	// Prepare command
	var command string
	if paneConfig.Refresh > 0 {
		// Create refresh loop - escape single quotes properly
		escapedCmd := strings.ReplaceAll(paneConfig.Cmd, "'", "'\"'\"'")
		command = fmt.Sprintf("while :; do clear; date \"+%%F %%T\"; %s; sleep %d; done",
			escapedCmd, paneConfig.Refresh)
	} else {
		// Escape single quotes properly
		escapedCmd := strings.ReplaceAll(paneConfig.Cmd, "'", "'\"'\"'")
		command = escapedCmd
	}

	// Send command to pane
	if err := pane.SendKeys(command); err != nil {
		return fmt.Errorf("failed to send command: %w", err)
	}
	
	// Press Enter to execute the command
	if err := pane.SendKeys("C-m"); err != nil {
		return fmt.Errorf("failed to press enter for command: %w", err)
	}

	return nil
}

// printDryRun prints the tmux commands that would be executed
func (tm *TmuxManager) printDryRun(config *Config) error {
	fmt.Printf("# Dry run - tmux commands that would be executed:\n\n")

	// Kill existing session
	fmt.Printf("tmux kill-session -t %s 2>/dev/null || true\n", config.Session)

	if len(config.Tabs) == 0 {
		return fmt.Errorf("no tabs defined")
	}

	// Create session with first tab
	firstTab := config.Tabs[0]
	fmt.Printf("tmux new-session -d -s %s -n %s\n", config.Session, firstTab.Name)

	// Setup first window
	tm.printWindowSetup(config.Session, firstTab, 0)

	// Create additional windows
	for i := 1; i < len(config.Tabs); i++ {
		tab := config.Tabs[i]
		fmt.Printf("tmux new-window -t %s -n %s\n", config.Session, tab.Name)
		tm.printWindowSetup(config.Session, tab, i)
	}

	return nil
}

// printWindowSetup prints the setup commands for a window
func (tm *TmuxManager) printWindowSetup(sessionName string, tab Tab, windowIndex int) {
	target := fmt.Sprintf("%s:%d", sessionName, windowIndex)

	// Setup panes
	for i, pane := range tab.Panes {
		if i > 0 {
			// Split window for additional panes
			fmt.Printf("tmux split-window -t %s\n", target)
		}

		paneTarget := fmt.Sprintf("%s.%d", target, i)

		// Set environment variables
		for key, value := range pane.Env {
			envCmd := fmt.Sprintf("export %s=%s", key, value)
			fmt.Printf("tmux send-keys -t %s '%s' C-m\n", paneTarget, envCmd)
		}

		// Send command
		var command string
		if pane.Refresh > 0 {
			// Refresh loop without bash -lc
			command = fmt.Sprintf("while :; do clear; date \"+%%F %%T\"; %s; sleep %d; done",
				pane.Cmd, pane.Refresh)
		} else {
			command = pane.Cmd
		}

		// Escape single quotes in command
		command = strings.ReplaceAll(command, "'", "'\"'\"'")
		fmt.Printf("tmux send-keys -t %s '%s' C-m\n", paneTarget, command)
	}

	// Apply layout
	layout := tab.Layout
	if layout == "" {
		layout = "tiled"
	}
	fmt.Printf("tmux select-layout -t %s %s\n", target, layout)
	fmt.Println()
}

