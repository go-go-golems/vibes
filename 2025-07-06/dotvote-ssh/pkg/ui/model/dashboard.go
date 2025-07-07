package model

import (
	"fmt"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/bubbles/textinput"
	"github.com/charmbracelet/bubbles/key"

	"dotvote-ssh/pkg/session"
	"dotvote-ssh/pkg/ui/view"
)

// DashboardModel handles the facilitator dashboard screen
type DashboardModel struct {
	main *MainModel
	
	// State
	width  int
	height int
	
	// Ideas management
	selectedIdea int
	editingIdea  bool
	editInput    textinput.Model
	newIdeaInput textinput.Model
	addingIdea   bool
	
	// Settings
	dotsPerPerson int
	allowMultiple bool
	showLiveResults bool
	
	// Focus management
	focusedSection int // 0: ideas, 1: settings, 2: controls
}

// NewDashboardModel creates a new dashboard model
func NewDashboardModel(main *MainModel) *DashboardModel {
	editInput := textinput.New()
	editInput.Width = 50
	
	newIdeaInput := textinput.New()
	newIdeaInput.Placeholder = "Enter new idea..."
	newIdeaInput.Width = 50
	
	return &DashboardModel{
		main:         main,
		editInput:    editInput,
		newIdeaInput: newIdeaInput,
		dotsPerPerson: 5,
		allowMultiple: true,
		showLiveResults: false,
	}
}

// Init initializes the dashboard model
func (m *DashboardModel) Init() tea.Cmd {
	if m.main.currentSession != nil {
		m.dotsPerPerson = m.main.currentSession.DotsPerPerson
		m.allowMultiple = m.main.currentSession.AllowMultiple
		m.showLiveResults = m.main.currentSession.ShowLiveResults
	}
	return textinput.Blink
}

// Update handles messages for the dashboard screen
func (m *DashboardModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd
	var cmds []tea.Cmd
	
	if m.main.currentSession == nil {
		return m, ChangeScreen(ScreenLanding, nil)
	}
	
	switch msg := msg.(type) {
	case tea.KeyMsg:
		// Handle editing mode
		if m.editingIdea {
			switch {
			case key.Matches(msg, m.main.keys.Enter):
				return m.saveEdit()
			case key.Matches(msg, m.main.keys.Escape):
				m.editingIdea = false
				return m, nil
			default:
				m.editInput, cmd = m.editInput.Update(msg)
				return m, cmd
			}
		}
		
		// Handle adding idea mode
		if m.addingIdea {
			switch {
			case key.Matches(msg, m.main.keys.Enter):
				return m.addIdea()
			case key.Matches(msg, m.main.keys.Escape):
				m.addingIdea = false
				m.newIdeaInput.SetValue("")
				return m, nil
			default:
				m.newIdeaInput, cmd = m.newIdeaInput.Update(msg)
				return m, cmd
			}
		}
		
		// Normal navigation
		switch {
		case key.Matches(msg, m.main.keys.Up):
			m.navigateUp()
			return m, nil
			
		case key.Matches(msg, m.main.keys.Down):
			m.navigateDown()
			return m, nil
			
		case key.Matches(msg, m.main.keys.Left):
			m.focusedSection = max(0, m.focusedSection-1)
			return m, nil
			
		case key.Matches(msg, m.main.keys.Right):
			m.focusedSection = min(2, m.focusedSection+1)
			return m, nil
			
		case key.Matches(msg, m.main.keys.Enter):
			return m.handleEnter()
			
		case key.Matches(msg, m.main.keys.Space):
			return m.handleSpace()
			
		case key.Matches(msg, m.main.keys.Add):
			m.addingIdea = true
			m.newIdeaInput.Focus()
			return m, nil
			
		case key.Matches(msg, m.main.keys.Edit):
			return m.startEdit()
			
		case key.Matches(msg, m.main.keys.Delete):
			return m.deleteIdea()
			
		case key.Matches(msg, m.main.keys.Start):
			return m.startVoting()
			
		case key.Matches(msg, m.main.keys.Stop):
			return m.closeVoting()
			
		case key.Matches(msg, m.main.keys.Results):
			return m.showResults()
			
		case key.Matches(msg, m.main.keys.Reset):
			return m.resetVoting()
			
		case key.Matches(msg, m.main.keys.New):
			return m, ChangeScreen(ScreenLanding, nil)
		}
	}
	
	return m, tea.Batch(cmds...)
}

// View renders the dashboard screen
func (m *DashboardModel) View() string {
	if m.width == 0 || m.main.currentSession == nil {
		return "Loading..."
	}
	
	var content strings.Builder
	
	// Header
	header := fmt.Sprintf("Session: %s  👥 %d participants", 
		m.main.currentSession.Code, 
		m.main.currentSession.GetParticipantCount())
	
	status := m.getStatusText()
	content.WriteString(view.RenderHeader(header, status, m.width))
	content.WriteString("\n\n")
	
	// Ideas section
	content.WriteString(m.renderIdeasSection())
	content.WriteString("\n")
	
	// Settings section
	content.WriteString(m.renderSettingsSection())
	content.WriteString("\n")
	
	// Controls section
	content.WriteString(m.renderControlsSection())
	content.WriteString("\n")
	
	// Live results preview (if enabled)
	if m.showLiveResults {
		content.WriteString(m.renderLiveResults())
		content.WriteString("\n")
	}
	
	// Help text
	help := "↑/↓ Navigate  ←/→ Sections  ENTER Edit/Confirm  'a' Add  'd' Delete  's' Start  'c' Close  'R' Results  'r' Reset  'n' New  'q' Quit"
	content.WriteString(view.RenderFooter(help, m.width))
	
	return content.String()
}

// SetSize updates the model size
func (m *DashboardModel) SetSize(width, height int) {
	m.width = width
	m.height = height
}

// renderIdeasSection renders the ideas management section
func (m *DashboardModel) renderIdeasSection() string {
	var content strings.Builder
	
	title := "Ideas"
	if m.focusedSection == 0 {
		title = view.Styles.ListSelected.Render(title)
	} else {
		title = view.Styles.ListTitle.Render(title)
	}
	content.WriteString(title)
	content.WriteString(" (TAB to edit, ENTER to add new):\n")
	
	ideas := m.getIdeasList()
	for i, idea := range ideas {
		prefix := "  "
		if m.focusedSection == 0 && i == m.selectedIdea {
			prefix = "► "
		}
		
		ideaText := idea.Text
		if m.editingIdea && i == m.selectedIdea {
			ideaText = m.editInput.View()
		}
		
		actions := "[Edit] [×]"
		line := fmt.Sprintf("%s%s %s", prefix, ideaText, actions)
		
		if m.focusedSection == 0 && i == m.selectedIdea {
			content.WriteString(view.Styles.ListSelected.Render(line))
		} else {
			content.WriteString(view.Styles.ListItem.Render(line))
		}
		content.WriteString("\n")
	}
	
	// Add new idea input
	if m.addingIdea {
		content.WriteString("  ")
		content.WriteString(m.newIdeaInput.View())
		content.WriteString("\n")
	} else {
		content.WriteString("  [+ Add new idea...]\n")
	}
	
	return content.String()
}

// renderSettingsSection renders the settings section
func (m *DashboardModel) renderSettingsSection() string {
	var content strings.Builder
	
	title := "Settings"
	if m.focusedSection == 1 {
		title = view.Styles.ListSelected.Render(title)
	} else {
		title = view.Styles.ListTitle.Render(title)
	}
	content.WriteString(title)
	content.WriteString(":\n")
	
	// Dots per person
	dotsLine := fmt.Sprintf("  Dots per person: [%d] ←→", m.dotsPerPerson)
	content.WriteString(dotsLine)
	content.WriteString("\n")
	
	// Allow multiple dots per idea
	multipleIcon := "☐"
	if m.allowMultiple {
		multipleIcon = "☑️"
	}
	multipleLine := fmt.Sprintf("  %s Allow multiple dots per idea", multipleIcon)
	content.WriteString(multipleLine)
	content.WriteString("\n")
	
	return content.String()
}

// renderControlsSection renders the controls section
func (m *DashboardModel) renderControlsSection() string {
	var content strings.Builder
	
	title := "Controls"
	if m.focusedSection == 2 {
		title = view.Styles.ListSelected.Render(title)
	} else {
		title = view.Styles.ListTitle.Render(title)
	}
	content.WriteString(title)
	content.WriteString(":\n")
	
	// Control buttons based on session state
	state := m.main.currentSession.GetState()
	switch state {
	case session.StateWaitingForIdeas:
		content.WriteString("  ")
		content.WriteString(view.RenderButton("🟢 Start Voting", false))
		content.WriteString("  ")
		content.WriteString(view.RenderButton("🔴 Close Voting", false))
		content.WriteString("\n")
		
	case session.StateVoting:
		content.WriteString("  ")
		content.WriteString(view.RenderButton("⏸️ Pause Voting", false))
		content.WriteString("  ")
		content.WriteString(view.RenderButton("🔴 Close Voting", false))
		content.WriteString("\n")
		
	case session.StateResults:
		content.WriteString("  ")
		content.WriteString(view.RenderButton("🔄 Restart Voting", false))
		content.WriteString("  ")
		content.WriteString(view.RenderButton("📊 Export Results", false))
		content.WriteString("\n")
	}
	
	// Live results toggle
	liveResultsText := "OFF"
	if m.showLiveResults {
		liveResultsText = "ON"
	}
	liveResultsLine := fmt.Sprintf("  👁️ Show Live Results: %s", liveResultsText)
	content.WriteString(liveResultsLine)
	content.WriteString("\n")
	
	return content.String()
}

// renderLiveResults renders the live results preview
func (m *DashboardModel) renderLiveResults() string {
	var content strings.Builder
	
	content.WriteString(view.Styles.ListTitle.Render("Live Results Preview"))
	content.WriteString(":\n")
	
	results := m.main.currentSession.GetResults()
	maxVotes := 0
	if len(results) > 0 {
		maxVotes = results[0].Votes
	}
	
	for _, idea := range results {
		barWidth := 20
		if maxVotes > 0 {
			barWidth = int(float64(idea.Votes) / float64(maxVotes) * 20)
		}
		
		bar := view.RenderProgressBar(idea.Votes, maxVotes, barWidth)
		line := fmt.Sprintf("  %s %s %d", idea.Text, bar, idea.Votes)
		content.WriteString(line)
		content.WriteString("\n")
	}
	
	return content.String()
}

// Helper methods

func (m *DashboardModel) getStatusText() string {
	switch m.main.currentSession.GetState() {
	case session.StateWaitingForIdeas:
		return "Status: ⏸️ Setup"
	case session.StateVoting:
		return "Status: 🟢 Voting"
	case session.StateResults:
		return "Status: 📊 Results"
	default:
		return "Status: ❓ Unknown"
	}
}

func (m *DashboardModel) getIdeasList() []*session.Idea {
	ideas := make([]*session.Idea, 0)
	for _, idea := range m.main.currentSession.Ideas {
		ideas = append(ideas, idea)
	}
	return ideas
}

func (m *DashboardModel) navigateUp() {
	if m.focusedSection == 0 {
		ideas := m.getIdeasList()
		if len(ideas) > 0 {
			m.selectedIdea = max(0, m.selectedIdea-1)
		}
	}
}

func (m *DashboardModel) navigateDown() {
	if m.focusedSection == 0 {
		ideas := m.getIdeasList()
		if len(ideas) > 0 {
			m.selectedIdea = min(len(ideas)-1, m.selectedIdea+1)
		}
	}
}

func (m *DashboardModel) handleEnter() (tea.Model, tea.Cmd) {
	switch m.focusedSection {
	case 0: // Ideas section
		return m.startEdit()
	case 2: // Controls section
		return m.startVoting()
	}
	return m, nil
}

func (m *DashboardModel) handleSpace() (tea.Model, tea.Cmd) {
	switch m.focusedSection {
	case 1: // Settings section
		m.allowMultiple = !m.allowMultiple
		m.main.currentSession.AllowMultiple = m.allowMultiple
		return m, nil
	case 2: // Controls section
		m.showLiveResults = !m.showLiveResults
		m.main.currentSession.ShowLiveResults = m.showLiveResults
		return m, nil
	}
	return m, nil
}

func (m *DashboardModel) startEdit() (tea.Model, tea.Cmd) {
	ideas := m.getIdeasList()
	if len(ideas) > 0 && m.selectedIdea < len(ideas) {
		m.editingIdea = true
		m.editInput.SetValue(ideas[m.selectedIdea].Text)
		m.editInput.Focus()
	}
	return m, nil
}

func (m *DashboardModel) saveEdit() (tea.Model, tea.Cmd) {
	ideas := m.getIdeasList()
	if len(ideas) > 0 && m.selectedIdea < len(ideas) {
		newText := strings.TrimSpace(m.editInput.Value())
		if newText != "" {
			ideas[m.selectedIdea].Text = newText
		}
	}
	m.editingIdea = false
	return m, nil
}

func (m *DashboardModel) addIdea() (tea.Model, tea.Cmd) {
	text := strings.TrimSpace(m.newIdeaInput.Value())
	if text != "" {
		m.main.currentSession.AddIdea(text, m.main.user.ID)
		m.newIdeaInput.SetValue("")
	}
	m.addingIdea = false
	return m, nil
}

func (m *DashboardModel) deleteIdea() (tea.Model, tea.Cmd) {
	ideas := m.getIdeasList()
	if len(ideas) > 0 && m.selectedIdea < len(ideas) {
		m.main.currentSession.RemoveIdea(ideas[m.selectedIdea].ID)
		if m.selectedIdea >= len(ideas)-1 {
			m.selectedIdea = max(0, len(ideas)-2)
		}
	}
	return m, nil
}

func (m *DashboardModel) startVoting() (tea.Model, tea.Cmd) {
	m.main.currentSession.SetState(session.StateVoting)
	return m, nil
}

func (m *DashboardModel) closeVoting() (tea.Model, tea.Cmd) {
	m.main.currentSession.SetState(session.StateResults)
	return m, ChangeScreen(ScreenResults, m.main.currentSession)
}

func (m *DashboardModel) showResults() (tea.Model, tea.Cmd) {
	return m, ChangeScreen(ScreenResults, m.main.currentSession)
}

func (m *DashboardModel) resetVoting() (tea.Model, tea.Cmd) {
	// Reset all votes
	m.main.currentSession.Votes = make([]session.Vote, 0)
	for _, idea := range m.main.currentSession.Ideas {
		idea.Votes = 0
		idea.VotedBy = make([]string, 0)
	}
	for _, participant := range m.main.currentSession.Participants {
		participant.DotsUsed = 0
	}
	m.main.currentSession.SetState(session.StateWaitingForIdeas)
	return m, nil
}

// Helper functions
func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

