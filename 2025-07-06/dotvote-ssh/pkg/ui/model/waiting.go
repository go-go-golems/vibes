package model

import (
	"fmt"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"

	"dotvote-ssh/pkg/session"
	"dotvote-ssh/pkg/ui/view"
)

// WaitingModel handles waiting states for participants
type WaitingModel struct {
	main *MainModel
	
	// State
	width  int
	height int
}

// NewWaitingModel creates a new waiting model
func NewWaitingModel(main *MainModel) *WaitingModel {
	return &WaitingModel{
		main: main,
	}
}

// Init initializes the waiting model
func (m *WaitingModel) Init() tea.Cmd {
	return nil
}

// Update handles messages for the waiting screen
func (m *WaitingModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	if m.main.currentSession == nil {
		return m, ChangeScreen(ScreenLanding, nil)
	}
	
	// Check if session state changed
	switch m.main.currentSession.GetState() {
	case session.StateVoting:
		return m, ChangeScreen(ScreenParticipantVoting, m.main.currentSession)
	case session.StateResults:
		return m, ChangeScreen(ScreenResults, m.main.currentSession)
	case session.StateClosed:
		return m, ChangeScreen(ScreenLanding, nil)
	}
	
	// No specific key handling needed for waiting screen
	// Just pass through to main model for global keys (quit, help, etc.)
	return m, nil
}

// View renders the waiting screen
func (m *WaitingModel) View() string {
	if m.width == 0 || m.main.currentSession == nil {
		return "Loading..."
	}
	
	var content strings.Builder
	
	// Header
	participant := m.main.currentSession.Participants[m.main.user.ID]
	dotsDisplay := ""
	if participant != nil {
		dotsDisplay = fmt.Sprintf("Dots ready: %s", view.RenderDots(0, participant.DotsTotal))
	}
	
	header := fmt.Sprintf("Session: %s", m.main.currentSession.Title)
	content.WriteString(view.RenderHeader(header, dotsDisplay, m.width))
	content.WriteString("\n\n")
	
	// Waiting message based on session state
	switch m.main.currentSession.GetState() {
	case session.StateWaitingForIdeas:
		content.WriteString(m.renderWaitingForIdeas())
	case session.StateVoting:
		// This shouldn't happen as we redirect to voting screen
		content.WriteString(m.renderVotingInProgress())
	case session.StateResults:
		// This shouldn't happen as we redirect to results screen
		content.WriteString(m.renderWaitingForResults())
	default:
		content.WriteString(m.renderGenericWaiting())
	}
	
	content.WriteString("\n")
	
	// Help text
	help := "Waiting for facilitator...                                'q' Quit"
	content.WriteString(view.RenderFooter(help, m.width))
	
	return view.Styles.Container.Width(m.width-4).Height(m.height-4).Render(content.String())
}

// SetSize updates the model size
func (m *WaitingModel) SetSize(width, height int) {
	m.width = width
	m.height = height
}

// renderWaitingForIdeas renders the waiting message when facilitator is setting up ideas
func (m *WaitingModel) renderWaitingForIdeas() string {
	var content strings.Builder
	
	// Centered waiting message
	content.WriteString("\n\n")
	content.WriteString(view.Styles.Title.Render("🕐 Waiting for ideas..."))
	content.WriteString("\n\n")
	
	content.WriteString(view.Styles.Body.Align(lipgloss.Center).Render(
		"The facilitator is setting up the\nvoting topics. Please wait."))
	content.WriteString("\n\n")
	
	// Show current ideas if any
	ideas := m.getIdeasList()
	if len(ideas) > 0 {
		content.WriteString(view.Styles.Subtitle.Render("Ideas so far:"))
		content.WriteString("\n")
		
		for _, idea := range ideas {
			content.WriteString(view.Styles.ListItem.Render("  • " + idea.Text))
			content.WriteString("\n")
		}
	}
	
	return content.String()
}

// renderVotingInProgress renders message when voting is in progress
func (m *WaitingModel) renderVotingInProgress() string {
	var content strings.Builder
	
	content.WriteString("\n\n")
	content.WriteString(view.Styles.Title.Render("🗳️ Voting in progress..."))
	content.WriteString("\n\n")
	
	content.WriteString(view.Styles.Body.Align(lipgloss.Center).Render(
		"Voting is now open!\nRedirecting to voting screen..."))
	content.WriteString("\n\n")
	
	return content.String()
}

// renderWaitingForResults renders message when waiting for results
func (m *WaitingModel) renderWaitingForResults() string {
	var content strings.Builder
	
	content.WriteString("\n\n")
	content.WriteString(view.Styles.StatusGood.Render("⏳ Voting complete!"))
	content.WriteString("\n\n")
	
	content.WriteString(view.Styles.Body.Align(lipgloss.Center).Render(
		"Waiting for facilitator to\nreveal the results..."))
	content.WriteString("\n\n")
	
	// Show user's votes
	content.WriteString(m.renderMyVotes())
	
	return content.String()
}

// renderGenericWaiting renders a generic waiting message
func (m *WaitingModel) renderGenericWaiting() string {
	var content strings.Builder
	
	content.WriteString("\n\n")
	content.WriteString(view.Styles.Title.Render("⏳ Please wait..."))
	content.WriteString("\n\n")
	
	content.WriteString(view.Styles.Body.Align(lipgloss.Center).Render(
		"Waiting for session updates..."))
	content.WriteString("\n\n")
	
	return content.String()
}

// renderMyVotes renders the participant's votes
func (m *WaitingModel) renderMyVotes() string {
	var content strings.Builder
	
	// Get participant's votes
	myVotes := make(map[string]int)
	for _, vote := range m.main.currentSession.Votes {
		if vote.ParticipantID == m.main.user.ID {
			myVotes[vote.IdeaID]++
		}
	}
	
	if len(myVotes) == 0 {
		return content.String()
	}
	
	content.WriteString(view.Styles.Subtitle.Render("Your votes:"))
	content.WriteString("\n")
	
	ideas := m.getIdeasList()
	for _, idea := range ideas {
		if votes, exists := myVotes[idea.ID]; exists && votes > 0 {
			dots := strings.Repeat("💙", votes)
			line := fmt.Sprintf("  %s %s", dots, idea.Text)
			content.WriteString(view.Styles.ListItem.Render(line))
			content.WriteString("\n")
		}
	}
	
	return content.String()
}

// Helper methods

func (m *WaitingModel) getIdeasList() []*session.Idea {
	ideas := make([]*session.Idea, 0)
	for _, idea := range m.main.currentSession.Ideas {
		ideas = append(ideas, idea)
	}
	return ideas
}

