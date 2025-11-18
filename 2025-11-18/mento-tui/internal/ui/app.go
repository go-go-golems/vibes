package ui

import (
	"mento-tui/internal/config"
	"mento-tui/internal/services"
	"time"

	tea "github.com/charmbracelet/bubbletea"
)

type ScreenType int

const (
	DashboardScreen ScreenType = iota
	LogViewerScreen
	ConfigScreen
	HelpScreen
)

type Model struct {
	currentScreen ScreenType
	dashboard     DashboardModel
	logViewer     LogViewerModel
	config        ConfigModel
	help          HelpModel
	manager       *services.Manager
	width         int
	height        int
	quitting      bool
}

type tickMsg time.Time

func NewModel(cfg *config.AppConfig) Model {
	manager := services.NewManager(cfg)

	return Model{
		currentScreen: DashboardScreen,
		dashboard:     NewDashboardModel(manager),
		logViewer:     NewLogViewerModel(manager),
		config:        NewConfigModel(manager),
		help:          NewHelpModel(manager),
		manager:       manager,
	}
}

func (m Model) Init() tea.Cmd {
	return tea.Batch(
		tickCmd(),
	)
}

func tickCmd() tea.Cmd {
	return tea.Tick(time.Second, func(t time.Time) tea.Msg {
		return tickMsg(t)
	})
}

func (m Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height

		// Update all screens
		var tmpModel tea.Model
		tmpModel, _ = m.dashboard.Update(msg)
		if d, ok := tmpModel.(DashboardModel); ok {
			m.dashboard = d
		}
		tmpModel, _ = m.logViewer.Update(msg)
		if l, ok := tmpModel.(LogViewerModel); ok {
			m.logViewer = l
		}
		tmpModel, _ = m.config.Update(msg)
		if c, ok := tmpModel.(ConfigModel); ok {
			m.config = c
		}
		tmpModel, _ = m.help.Update(msg)
		if h, ok := tmpModel.(HelpModel); ok {
			m.help = h
		}

		return m, nil

	case tickMsg:
		// Refresh UI periodically
		return m, tickCmd()

	case tea.KeyMsg:
		// Always allow quit keys regardless of screen state
		switch msg.String() {
		case "ctrl+c", "q":
			m.quitting = true
			m.manager.StopAll()
			return m, tea.Quit
		}

		// Check if log viewer is in search mode BEFORE updating
		// (so we know if it will consume the key)
		isInSearchMode := m.currentScreen == LogViewerScreen && m.logViewer.searchMode

		// Update current screen first (so it can consume keys)
		var cmd tea.Cmd
		var tmpModel tea.Model
		switch m.currentScreen {
		case DashboardScreen:
			tmpModel, cmd = m.dashboard.Update(msg)
			if d, ok := tmpModel.(DashboardModel); ok {
				m.dashboard = d
			}
		case LogViewerScreen:
			tmpModel, cmd = m.logViewer.Update(msg)
			if l, ok := tmpModel.(LogViewerModel); ok {
				m.logViewer = l
				// If log viewer was in search mode, don't process global keys
				// (it may have consumed the key to exit search mode)
				if isInSearchMode {
					return m, cmd
				}
			}
		case ConfigScreen:
			tmpModel, cmd = m.config.Update(msg)
			if c, ok := tmpModel.(ConfigModel); ok {
				m.config = c
			}
		case HelpScreen:
			tmpModel, cmd = m.help.Update(msg)
			if h, ok := tmpModel.(HelpModel); ok {
				m.help = h
			}
		}

		// Now handle global keys only if not in special mode
		switch msg.String() {
		case "h", "?":
			m.currentScreen = HelpScreen
			return m, nil

		case "esc":
			// Only handle if not in search mode (already handled by log viewer)
			if isInSearchMode {
				return m, cmd // Already handled by log viewer
			}
			if m.currentScreen != DashboardScreen {
				m.currentScreen = DashboardScreen
			}
			return m, nil

		case "t", "l":
			if m.currentScreen == DashboardScreen {
				m.currentScreen = LogViewerScreen
			}
			return m, nil

		case "c":
			if m.currentScreen == DashboardScreen {
				m.currentScreen = ConfigScreen
			}
			return m, nil

		case "e":
			if m.currentScreen == DashboardScreen {
				m.currentScreen = ConfigScreen
			}
			return m, nil

		case "enter":
			if m.currentScreen == DashboardScreen {
				svc := m.manager.GetService(m.manager.SelectedIndex)
				if svc != nil && svc.Status == 0 { // StatusStopped
					go m.manager.StartService(m.manager.SelectedIndex)
				}
			}
			return m, nil
		}

		return m, cmd
	}

	// Update current screen for non-key messages
	var cmd tea.Cmd
	var tmpModel tea.Model
	switch m.currentScreen {
	case DashboardScreen:
		tmpModel, cmd = m.dashboard.Update(msg)
		if d, ok := tmpModel.(DashboardModel); ok {
			m.dashboard = d
		}
	case LogViewerScreen:
		tmpModel, cmd = m.logViewer.Update(msg)
		if l, ok := tmpModel.(LogViewerModel); ok {
			m.logViewer = l
		}
	case ConfigScreen:
		tmpModel, cmd = m.config.Update(msg)
		if c, ok := tmpModel.(ConfigModel); ok {
			m.config = c
		}
	case HelpScreen:
		tmpModel, cmd = m.help.Update(msg)
		if h, ok := tmpModel.(HelpModel); ok {
			m.help = h
		}
	}

	return m, cmd
}

func (m Model) View() string {
	if m.quitting {
		return "Shutting down services...\n"
	}

	switch m.currentScreen {
	case DashboardScreen:
		return m.dashboard.View()
	case LogViewerScreen:
		return m.logViewer.View()
	case ConfigScreen:
		return m.config.View()
	case HelpScreen:
		return m.help.View()
	default:
		return "Unknown screen"
	}
}
