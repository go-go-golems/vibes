package model

import (
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/bubbles/textinput"
	"github.com/charmbracelet/bubbles/key"

	"dotvote-ssh/pkg/auth"
	"dotvote-ssh/pkg/session"
	"dotvote-ssh/pkg/ui/view"
)

// LandingModel handles the landing/session join screen
type LandingModel struct {
	main *MainModel
	
	// Form inputs
	sessionCodeInput textinput.Model
	nameInput        textinput.Model
	
	// State
	focusedInput int
	width        int
	height       int
	
	// For facilitators creating new sessions
	createMode bool
	titleInput textinput.Model
}

// NewLandingModel creates a new landing model
func NewLandingModel(main *MainModel) *LandingModel {
	// Session code input
	sessionCodeInput := textinput.New()
	sessionCodeInput.Placeholder = "ABCD"
	sessionCodeInput.CharLimit = 4
	sessionCodeInput.Width = 20
	sessionCodeInput.Focus()
	
	// Name input
	nameInput := textinput.New()
	nameInput.Placeholder = "Your Name"
	nameInput.CharLimit = 50
	nameInput.Width = 30
	
	// Title input (for facilitators)
	titleInput := textinput.New()
	titleInput.Placeholder = "Session Title"
	titleInput.CharLimit = 100
	titleInput.Width = 40
	
	return &LandingModel{
		main:             main,
		sessionCodeInput: sessionCodeInput,
		nameInput:        nameInput,
		titleInput:       titleInput,
		focusedInput:     0,
	}
}

// Init initializes the landing model
func (m *LandingModel) Init() tea.Cmd {
	// Set default name if available
	if m.main.user.Name != "" {
		m.nameInput.SetValue(m.main.user.Name)
	}
	
	// Facilitators can create sessions
	if m.main.user.Role == auth.RoleFacilitator {
		m.createMode = false // Start in join mode, can toggle
	}
	
	return textinput.Blink
}

// Update handles messages for the landing screen
func (m *LandingModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd
	var cmds []tea.Cmd
	
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch {
		case key.Matches(msg, m.main.keys.Tab):
			m.nextInput()
			return m, nil
			
		case key.Matches(msg, m.main.keys.Enter):
			return m.handleSubmit()
			
		case key.Matches(msg, m.main.keys.Escape):
			if m.createMode {
				m.createMode = false
				m.focusedInput = 0
				m.updateFocus()
				return m, nil
			}
			
		case msg.String() == "c" && m.main.user.Role == auth.RoleFacilitator:
			// Toggle create mode for facilitators
			m.createMode = !m.createMode
			m.focusedInput = 0
			m.updateFocus()
			return m, nil
		}
	}
	
	// Update inputs
	if m.createMode {
		switch m.focusedInput {
		case 0:
			m.titleInput, cmd = m.titleInput.Update(msg)
			cmds = append(cmds, cmd)
		case 1:
			m.nameInput, cmd = m.nameInput.Update(msg)
			cmds = append(cmds, cmd)
		}
	} else {
		switch m.focusedInput {
		case 0:
			m.sessionCodeInput, cmd = m.sessionCodeInput.Update(msg)
			cmds = append(cmds, cmd)
		case 1:
			m.nameInput, cmd = m.nameInput.Update(msg)
			cmds = append(cmds, cmd)
		}
	}
	
	return m, tea.Batch(cmds...)
}

// View renders the landing screen
func (m *LandingModel) View() string {
	if m.width == 0 {
		return "Loading..."
	}
	
	var content strings.Builder
	
	// Title
	title := "🗳️  Dot Vote"
	content.WriteString(view.RenderTitle(title, m.width))
	content.WriteString("\n\n")
	
	if m.createMode {
		// Create session mode (facilitators only)
		content.WriteString(view.Styles.Subtitle.Render("Create a new voting session"))
		content.WriteString("\n\n")
		
		// Title input
		content.WriteString("Session Title:\n")
		content.WriteString(m.titleInput.View())
		content.WriteString("\n\n")
		
		// Name input
		content.WriteString("Your Name:\n")
		content.WriteString(m.nameInput.View())
		content.WriteString("\n\n")
		
		// Create button
		createBtn := view.RenderButton("Create Session", m.focusedInput == 2)
		content.WriteString(createBtn)
		content.WriteString("\n\n")
		
		// Help text
		help := "TAB Next field  ENTER Create  ESC Cancel  'c' Join Mode"
		content.WriteString(view.RenderFooter(help, m.width))
		
	} else {
		// Join session mode
		content.WriteString(view.Styles.Subtitle.Render("Join a voting session"))
		content.WriteString("\n\n")
		
		// Session code input
		content.WriteString("Session Code:\n")
		content.WriteString(m.sessionCodeInput.View())
		content.WriteString("\n\n")
		
		// Name input
		content.WriteString("Your Name:\n")
		content.WriteString(m.nameInput.View())
		content.WriteString("\n\n")
		
		// Join button
		joinBtn := view.RenderButton("Join Session", m.focusedInput == 2)
		content.WriteString(joinBtn)
		content.WriteString("\n\n")
		
		// Help text
		var help string
		if m.main.user.Role == auth.RoleFacilitator {
			help = "TAB Next field  ENTER Join  'c' Create Mode"
		} else {
			help = "TAB Next field  ENTER Join"
		}
		content.WriteString(view.RenderFooter(help, m.width))
	}
	
	return view.Styles.Container.Width(m.width-4).Height(m.height-4).Render(content.String())
}

// SetSize updates the model size
func (m *LandingModel) SetSize(width, height int) {
	m.width = width
	m.height = height
}

// nextInput moves focus to the next input field
func (m *LandingModel) nextInput() {
	if m.createMode {
		m.focusedInput = (m.focusedInput + 1) % 3
	} else {
		m.focusedInput = (m.focusedInput + 1) % 3
	}
	m.updateFocus()
}

// updateFocus updates which input has focus
func (m *LandingModel) updateFocus() {
	if m.createMode {
		m.titleInput.Blur()
		m.nameInput.Blur()
		
		switch m.focusedInput {
		case 0:
			m.titleInput.Focus()
		case 1:
			m.nameInput.Focus()
		}
	} else {
		m.sessionCodeInput.Blur()
		m.nameInput.Blur()
		
		switch m.focusedInput {
		case 0:
			m.sessionCodeInput.Focus()
		case 1:
			m.nameInput.Focus()
		}
	}
}

// handleSubmit handles form submission
func (m *LandingModel) handleSubmit() (tea.Model, tea.Cmd) {
	if m.createMode {
		return m.handleCreateSession()
	} else {
		return m.handleJoinSession()
	}
}

// handleCreateSession creates a new session (facilitators only)
func (m *LandingModel) handleCreateSession() (tea.Model, tea.Cmd) {
	title := strings.TrimSpace(m.titleInput.Value())
	name := strings.TrimSpace(m.nameInput.Value())
	
	// Validate inputs
	if title == "" {
		return m, ShowError(session.ErrInvalidSessionCode) // Reuse error for now
	}
	
	if !session.ValidateName(name) {
		return m, ShowError(session.ErrInvalidSessionCode) // Reuse error for now
	}
	
	// Update user name if changed
	if name != m.main.user.Name {
		m.main.authManager.UpdateUserName(m.main.user.KeyFingerprint, name)
		m.main.user.Name = name
	}
	
	// Create session
	newSession, err := m.main.sessionManager.CreateSession(title, m.main.user.ID)
	if err != nil {
		return m, ShowError(err)
	}
	
	// Add facilitator as participant
	facilitator := &session.Participant{
		ID:             m.main.user.ID,
		Name:           name,
		Role:           session.RoleFacilitator,
		KeyFingerprint: m.main.user.KeyFingerprint,
	}
	newSession.AddParticipant(facilitator)
	
	// Switch to facilitator dashboard
	return m, ChangeScreen(ScreenFacilitatorDashboard, newSession)
}

// handleJoinSession joins an existing session
func (m *LandingModel) handleJoinSession() (tea.Model, tea.Cmd) {
	code := strings.ToUpper(strings.TrimSpace(m.sessionCodeInput.Value()))
	name := strings.TrimSpace(m.nameInput.Value())
	
	// Validate inputs
	if !session.ValidateSessionCode(code) {
		return m, ShowError(session.ErrInvalidSessionCode)
	}
	
	if !session.ValidateName(name) {
		return m, ShowError(session.ErrInvalidSessionCode) // Reuse error for now
	}
	
	// Update user name if changed
	if name != m.main.user.Name {
		m.main.authManager.UpdateUserName(m.main.user.KeyFingerprint, name)
		m.main.user.Name = name
	}
	
	// Join session
	joinedSession, err := m.main.sessionManager.JoinSession(code, name, m.main.user.ID, m.main.user.KeyFingerprint)
	if err != nil {
		return m, ShowError(err)
	}
	
	// Determine which screen to show based on role and session state
	var nextScreen Screen
	if m.main.user.Role == auth.RoleFacilitator {
		nextScreen = ScreenFacilitatorDashboard
	} else {
		switch joinedSession.GetState() {
		case session.StateWaitingForIdeas:
			nextScreen = ScreenWaiting
		case session.StateVoting:
			nextScreen = ScreenParticipantVoting
		case session.StateResults:
			nextScreen = ScreenResults
		default:
			nextScreen = ScreenWaiting
		}
	}
	
	return m, ChangeScreen(nextScreen, joinedSession)
}

