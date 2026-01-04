package model

import (
	"fmt"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/bubbles/help"
	"github.com/charmbracelet/bubbles/key"

	"dotvote-ssh/pkg/auth"
	"dotvote-ssh/pkg/session"
	"dotvote-ssh/pkg/ui/keys"
)

// Screen represents different application screens
type Screen int

const (
	ScreenLanding Screen = iota
	ScreenFacilitatorDashboard
	ScreenParticipantVoting
	ScreenResults
	ScreenWaiting
)

// MainModel is the root model that manages different screens
type MainModel struct {
	// Core dependencies
	user           *auth.UserInfo
	sessionManager *session.Manager
	authManager    *auth.AuthManager
	
	// Current state
	currentScreen  Screen
	currentSession *session.Session
	
	// UI components
	keys    keys.KeyMap
	help    help.Model
	width   int
	height  int
	
	// Screen models
	landingModel     *LandingModel
	dashboardModel   *DashboardModel
	votingModel      *VotingModel
	resultsModel     *ResultsModel
	waitingModel     *WaitingModel
	
	// Error handling
	err error
}

// NewMainModel creates a new main model
func NewMainModel(user *auth.UserInfo, sessionManager *session.Manager, authManager *auth.AuthManager) *MainModel {
	m := &MainModel{
		user:           user,
		sessionManager: sessionManager,
		authManager:    authManager,
		currentScreen:  ScreenLanding,
		keys:           keys.NewKeyMap(),
		help:           help.New(),
	}
	
	// Initialize screen models
	m.landingModel = NewLandingModel(m)
	m.dashboardModel = NewDashboardModel(m)
	m.votingModel = NewVotingModel(m)
	m.resultsModel = NewResultsModel(m)
	m.waitingModel = NewWaitingModel(m)
	
	return m
}

// Init initializes the model
func (m *MainModel) Init() tea.Cmd {
	return m.getCurrentModel().Init()
}

// Update handles messages and updates the model
func (m *MainModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd
	
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height
		m.help.Width = msg.Width
		
		// Propagate resize to all screen models
		m.landingModel.SetSize(msg.Width, msg.Height)
		m.dashboardModel.SetSize(msg.Width, msg.Height)
		m.votingModel.SetSize(msg.Width, msg.Height)
		m.resultsModel.SetSize(msg.Width, msg.Height)
		m.waitingModel.SetSize(msg.Width, msg.Height)
		
		return m, nil
		
	case tea.KeyMsg:
		// Handle global key bindings
		switch {
		case key.Matches(msg, m.keys.Quit):
			return m, tea.Quit
		case key.Matches(msg, m.keys.Help):
			m.help.ShowAll = !m.help.ShowAll
			return m, nil
		}
		
	case ScreenChangeMsg:
		m.currentScreen = msg.Screen
		if msg.Session != nil {
			m.currentSession = msg.Session
		}
		return m, m.getCurrentModel().Init()
		
	case ErrorMsg:
		m.err = msg.Err
		return m, nil
		
	case ClearErrorMsg:
		m.err = nil
		return m, nil
	}
	
	// Delegate to current screen model
	currentModel := m.getCurrentModel()
	updatedModel, cmd := currentModel.Update(msg)
	m.setCurrentModel(updatedModel)
	
	return m, cmd
}

// View renders the current screen
func (m *MainModel) View() string {
	if m.width == 0 || m.height == 0 {
		return "Loading..."
	}
	
	// Get current screen view
	content := m.getCurrentModel().View()
	
	// Add error display if present
	if m.err != nil {
		errorMsg := fmt.Sprintf("Error: %s", m.err.Error())
		content = errorMsg + "\n\n" + content
	}
	
	// Add help if needed
	if m.help.ShowAll {
		var helpView string
		if m.user.Role == auth.RoleFacilitator {
			helpView = m.help.FullHelpView(m.keys.FacilitatorHelp())
		} else {
			helpView = m.help.FullHelpView(m.keys.ParticipantHelp())
		}
		content += "\n" + helpView
	} else {
		helpView := m.help.ShortHelpView(m.keys.ShortHelp())
		content += "\n" + helpView
	}
	
	return content
}

// getCurrentModel returns the current screen model
func (m *MainModel) getCurrentModel() tea.Model {
	switch m.currentScreen {
	case ScreenLanding:
		return m.landingModel
	case ScreenFacilitatorDashboard:
		return m.dashboardModel
	case ScreenParticipantVoting:
		return m.votingModel
	case ScreenResults:
		return m.resultsModel
	case ScreenWaiting:
		return m.waitingModel
	default:
		return m.landingModel
	}
}

// setCurrentModel updates the current screen model
func (m *MainModel) setCurrentModel(model tea.Model) {
	switch m.currentScreen {
	case ScreenLanding:
		if lm, ok := model.(*LandingModel); ok {
			m.landingModel = lm
		}
	case ScreenFacilitatorDashboard:
		if dm, ok := model.(*DashboardModel); ok {
			m.dashboardModel = dm
		}
	case ScreenParticipantVoting:
		if vm, ok := model.(*VotingModel); ok {
			m.votingModel = vm
		}
	case ScreenResults:
		if rm, ok := model.(*ResultsModel); ok {
			m.resultsModel = rm
		}
	case ScreenWaiting:
		if wm, ok := model.(*WaitingModel); ok {
			m.waitingModel = wm
		}
	}
}

// Helper methods for screen models to access shared data
func (m *MainModel) GetUser() *auth.UserInfo {
	return m.user
}

func (m *MainModel) GetSessionManager() *session.Manager {
	return m.sessionManager
}

func (m *MainModel) GetAuthManager() *auth.AuthManager {
	return m.authManager
}

func (m *MainModel) GetCurrentSession() *session.Session {
	return m.currentSession
}

func (m *MainModel) GetKeys() keys.KeyMap {
	return m.keys
}

func (m *MainModel) GetSize() (int, int) {
	return m.width, m.height
}

// Custom messages
type ScreenChangeMsg struct {
	Screen  Screen
	Session *session.Session
}

type ErrorMsg struct {
	Err error
}

type ClearErrorMsg struct{}

// Helper functions to create messages
func ChangeScreen(screen Screen, session *session.Session) tea.Cmd {
	return func() tea.Msg {
		return ScreenChangeMsg{Screen: screen, Session: session}
	}
}

func ShowError(err error) tea.Cmd {
	return func() tea.Msg {
		return ErrorMsg{Err: err}
	}
}

func ClearError() tea.Cmd {
	return func() tea.Msg {
		return ClearErrorMsg{}
	}
}

