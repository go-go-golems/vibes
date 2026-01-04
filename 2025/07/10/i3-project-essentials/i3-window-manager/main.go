package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"
	"strconv"
	"strings"

	"github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"go.i3wm.org/i3/v4"
)

// WindowInfo represents a window with its details
type WindowInfo struct {
	ID       int64
	Name     string
	Class    string
	Instance string
	Focused  bool
	Rect     i3.Rect
	Workspace string
}

// WorkspaceInfo represents a workspace with its windows
type WorkspaceInfo struct {
	Name    string
	Focused bool
	Windows []WindowInfo
}

// Model represents the application state
type Model struct {
	workspaces     []WorkspaceInfo
	selectedIndex  int
	viewMode       string // "workspaces" or "windows"
	selectedWS     int
	err            error
	width          int
	height         int
}

// Styles for the UI
var (
	titleStyle = lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("#FAFAFA")).
		Background(lipgloss.Color("#7D56F4")).
		Padding(0, 1)

	selectedStyle = lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("#FAFAFA")).
		Background(lipgloss.Color("#F25D94")).
		Padding(0, 1)

	normalStyle = lipgloss.NewStyle().
		Foreground(lipgloss.Color("#DDDDDD")).
		Padding(0, 1)

	focusedStyle = lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("#FFD700")).
		Padding(0, 1)

	helpStyle = lipgloss.NewStyle().
		Foreground(lipgloss.Color("#626262")).
		Padding(1, 0)
)

func initialModel() Model {
	return Model{
		viewMode:      "workspaces",
		selectedIndex: 0,
		selectedWS:    0,
	}
}

func (m Model) Init() tea.Cmd {
	return refreshData
}

func refreshData() tea.Msg {
	workspaces, err := getWorkspacesAndWindows()
	if err != nil {
		return errMsg{err}
	}
	return dataMsg{workspaces}
}

type dataMsg struct {
	workspaces []WorkspaceInfo
}

type errMsg struct {
	err error
}

type commandMsg struct {
	success bool
	err     error
}

func (m Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height
		return m, nil

	case dataMsg:
		m.workspaces = msg.workspaces
		// Reset selection if out of bounds
		if m.viewMode == "workspaces" && m.selectedIndex >= len(m.workspaces) {
			m.selectedIndex = 0
		}
		return m, nil

	case errMsg:
		m.err = msg.err
		return m, nil

	case commandMsg:
		if msg.err != nil {
			m.err = msg.err
		}
		return m, refreshData

	case tea.KeyMsg:
		switch msg.String() {
		case "ctrl+c", "q":
			return m, tea.Quit

		case "r":
			return m, refreshData

		case "tab":
			if m.viewMode == "workspaces" {
				m.viewMode = "windows"
				m.selectedIndex = 0
			} else {
				m.viewMode = "workspaces"
				m.selectedIndex = m.selectedWS
			}
			return m, nil

		case "up", "k":
			if m.selectedIndex > 0 {
				m.selectedIndex--
			}
			return m, nil

		case "down", "j":
			maxIndex := 0
			if m.viewMode == "workspaces" {
				maxIndex = len(m.workspaces) - 1
			} else if m.selectedWS < len(m.workspaces) {
				maxIndex = len(m.workspaces[m.selectedWS].Windows) - 1
			}
			if m.selectedIndex < maxIndex {
				m.selectedIndex++
			}
			return m, nil

		case "enter", " ":
			return m, m.focusSelected()

		case "1", "2", "3", "4", "5", "6", "7", "8", "9", "0":
			wsNum := msg.String()
			if wsNum == "0" {
				wsNum = "10"
			}
			return m, focusWorkspace(wsNum)
		}
	}

	return m, nil
}

func (m Model) focusSelected() tea.Cmd {
	if m.viewMode == "workspaces" && m.selectedIndex < len(m.workspaces) {
		ws := m.workspaces[m.selectedIndex]
		m.selectedWS = m.selectedIndex
		return focusWorkspace(ws.Name)
	} else if m.viewMode == "windows" && m.selectedWS < len(m.workspaces) {
		windows := m.workspaces[m.selectedWS].Windows
		if m.selectedIndex < len(windows) {
			window := windows[m.selectedIndex]
			return focusWindow(window.ID, window.Rect)
		}
	}
	return nil
}

func focusWorkspace(name string) tea.Cmd {
	return func() tea.Msg {
		cmd := exec.Command("i3-msg", "workspace", name)
		cmd.Env = append(os.Environ(), "DISPLAY=:99")
		err := cmd.Run()
		return commandMsg{success: err == nil, err: err}
	}
}

func focusWindow(id int64, rect i3.Rect) tea.Cmd {
	return func() tea.Msg {
		// Focus the window
		cmd := exec.Command("i3-msg", fmt.Sprintf("[id=%d] focus", id))
		cmd.Env = append(os.Environ(), "DISPLAY=:99")
		err := cmd.Run()
		if err != nil {
			return commandMsg{success: false, err: err}
		}
		
		// Try to move cursor to window center (ignore errors if xdotool not available)
		centerX := rect.X + rect.Width/2
		centerY := rect.Y + rect.Height/2
		cmd = exec.Command("xdotool", "mousemove", strconv.Itoa(int(centerX)), strconv.Itoa(int(centerY)))
		cmd.Env = append(os.Environ(), "DISPLAY=:99")
		cmd.Run() // Ignore error
		
		return commandMsg{success: true, err: nil}
	}
}

func (m Model) View() string {
	if m.err != nil {
		return fmt.Sprintf("Error: %v\n\nPress 'r' to retry or 'q' to quit.", m.err)
	}

	var s strings.Builder

	// Title
	title := "i3 Window Manager"
	if m.viewMode == "windows" && m.selectedWS < len(m.workspaces) {
		title += fmt.Sprintf(" - %s", m.workspaces[m.selectedWS].Name)
	}
	s.WriteString(titleStyle.Render(title))
	s.WriteString("\n\n")

	if m.viewMode == "workspaces" {
		s.WriteString(m.renderWorkspaces())
	} else {
		s.WriteString(m.renderWindows())
	}

	// Help text
	help := "\n\n"
	if m.viewMode == "workspaces" {
		help += "↑/↓: Navigate • Enter: Focus Workspace • Tab: View Windows • 1-9,0: Quick Switch • R: Refresh • Q: Quit"
	} else {
		help += "↑/↓: Navigate • Enter: Focus Window & Move Cursor • Tab: Back to Workspaces • R: Refresh • Q: Quit"
	}
	s.WriteString(helpStyle.Render(help))

	return s.String()
}

func (m Model) renderWorkspaces() string {
	var s strings.Builder
	s.WriteString("Workspaces:\n\n")

	for i, ws := range m.workspaces {
		style := normalStyle
		if i == m.selectedIndex {
			style = selectedStyle
		} else if ws.Focused {
			style = focusedStyle
		}

		prefix := "  "
		if i == m.selectedIndex {
			prefix = "▶ "
		} else if ws.Focused {
			prefix = "● "
		}

		windowCount := fmt.Sprintf(" (%d windows)", len(ws.Windows))
		s.WriteString(style.Render(prefix + ws.Name + windowCount))
		s.WriteString("\n")
	}

	return s.String()
}

func (m Model) renderWindows() string {
	var s strings.Builder
	
	if m.selectedWS >= len(m.workspaces) {
		s.WriteString("No workspace selected\n")
		return s.String()
	}

	ws := m.workspaces[m.selectedWS]
	s.WriteString(fmt.Sprintf("Windows in %s:\n\n", ws.Name))

	if len(ws.Windows) == 0 {
		s.WriteString("No windows in this workspace\n")
		return s.String()
	}

	for i, window := range ws.Windows {
		style := normalStyle
		if i == m.selectedIndex {
			style = selectedStyle
		} else if window.Focused {
			style = focusedStyle
		}

		prefix := "  "
		if i == m.selectedIndex {
			prefix = "▶ "
		} else if window.Focused {
			prefix = "● "
		}

		name := window.Name
		if name == "" {
			name = fmt.Sprintf("%s (%s)", window.Class, window.Instance)
		}
		if len(name) > 50 {
			name = name[:47] + "..."
		}

		info := fmt.Sprintf("%s [%dx%d at %d,%d]", name, window.Rect.Width, window.Rect.Height, window.Rect.X, window.Rect.Y)
		s.WriteString(style.Render(prefix + info))
		s.WriteString("\n")
	}

	return s.String()
}

func getWorkspacesAndWindows() ([]WorkspaceInfo, error) {
	// Get workspaces
	workspaces, err := i3.GetWorkspaces()
	if err != nil {
		return nil, fmt.Errorf("failed to get workspaces: %w", err)
	}

	// Get tree to find windows
	tree, err := i3.GetTree()
	if err != nil {
		return nil, fmt.Errorf("failed to get tree: %w", err)
	}

	var result []WorkspaceInfo

	for _, ws := range workspaces {
		wsInfo := WorkspaceInfo{
			Name:    ws.Name,
			Focused: ws.Focused,
			Windows: []WindowInfo{},
		}

		// Find workspace node in tree
		wsNode := findWorkspaceNode(tree.Root, ws.Name)
		if wsNode != nil {
			wsInfo.Windows = extractWindows(wsNode, ws.Name)
		}

		result = append(result, wsInfo)
	}

	return result, nil
}

func findWorkspaceNode(node *i3.Node, wsName string) *i3.Node {
	if node.Type == "workspace" && node.Name == wsName {
		return node
	}

	for _, child := range node.Nodes {
		if result := findWorkspaceNode(child, wsName); result != nil {
			return result
		}
	}

	for _, child := range node.FloatingNodes {
		if result := findWorkspaceNode(child, wsName); result != nil {
			return result
		}
	}

	return nil
}

func extractWindows(node *i3.Node, workspace string) []WindowInfo {
	var windows []WindowInfo

	// If this node has a window, add it
	if node.Window != 0 {
		name := node.Name
		class := node.WindowProperties.Class
		instance := node.WindowProperties.Instance
		
		if node.WindowProperties.Title != "" {
			name = node.WindowProperties.Title
		}

		if name == "" && class != "" {
			name = class
		}

		windows = append(windows, WindowInfo{
			ID:        node.Window,
			Name:      name,
			Class:     class,
			Instance:  instance,
			Focused:   node.Focused,
			Rect:      node.Rect,
			Workspace: workspace,
		})
	}

	// Recursively check child nodes
	for _, child := range node.Nodes {
		windows = append(windows, extractWindows(child, workspace)...)
	}

	for _, child := range node.FloatingNodes {
		windows = append(windows, extractWindows(child, workspace)...)
	}

	return windows
}

func main() {
	// Set DISPLAY environment variable
	os.Setenv("DISPLAY", ":99")
	
	// Check if i3 is running
	if _, err := i3.GetVersion(); err != nil {
		log.Fatalf("i3 is not running or not accessible: %v", err)
	}

	p := tea.NewProgram(initialModel(), tea.WithAltScreen())
	if _, err := p.Run(); err != nil {
		log.Fatalf("Error running program: %v", err)
	}
}

