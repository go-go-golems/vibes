package model

import (
	"fmt"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/bubbles/key"

	"dotvote-ssh/pkg/auth"
	"dotvote-ssh/pkg/session"
	"dotvote-ssh/pkg/ui/view"
)

// ResultsModel handles the results screen
type ResultsModel struct {
	main *MainModel
	
	// State
	width  int
	height int
	
	// View options
	showParticipants bool
}

// NewResultsModel creates a new results model
func NewResultsModel(main *MainModel) *ResultsModel {
	return &ResultsModel{
		main: main,
	}
}

// Init initializes the results model
func (m *ResultsModel) Init() tea.Cmd {
	return nil
}

// Update handles messages for the results screen
func (m *ResultsModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	if m.main.currentSession == nil {
		return m, ChangeScreen(ScreenLanding, nil)
	}
	
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch {
		case key.Matches(msg, m.main.keys.Escape):
			// Go back to appropriate screen based on role
			if m.main.user.Role == auth.RoleFacilitator {
				return m, ChangeScreen(ScreenFacilitatorDashboard, m.main.currentSession)
			} else {
				return m, ChangeScreen(ScreenParticipantVoting, m.main.currentSession)
			}
			
		case msg.String() == "v":
			m.showParticipants = !m.showParticipants
			return m, nil
			
		case key.Matches(msg, m.main.keys.Export) && m.main.user.Role == auth.RoleFacilitator:
			return m.exportResults()
			
		case key.Matches(msg, m.main.keys.New) && m.main.user.Role == auth.RoleFacilitator:
			return m, ChangeScreen(ScreenLanding, nil)
			
		case msg.String() == "r" && m.main.user.Role == auth.RoleFacilitator:
			// Restart voting
			m.main.currentSession.SetState(session.StateWaitingForIdeas)
			return m, ChangeScreen(ScreenFacilitatorDashboard, m.main.currentSession)
		}
	}
	
	return m, nil
}

// View renders the results screen
func (m *ResultsModel) View() string {
	if m.width == 0 || m.main.currentSession == nil {
		return "Loading..."
	}
	
	var content strings.Builder
	
	// Header
	header := fmt.Sprintf("Session: %s", m.main.currentSession.Title)
	status := "📊 Final Results"
	content.WriteString(view.RenderHeader(header, status, m.width))
	content.WriteString("\n\n")
	
	// Results title
	content.WriteString(view.Styles.Title.Render("Final Rankings"))
	content.WriteString("\n\n")
	
	// Results list
	content.WriteString(m.renderResults())
	content.WriteString("\n")
	
	// Participants (if enabled)
	if m.showParticipants {
		content.WriteString(m.renderParticipants())
		content.WriteString("\n")
	}
	
	// Facilitator actions (if facilitator)
	if m.main.user.Role == auth.RoleFacilitator {
		content.WriteString(m.renderFacilitatorActions())
		content.WriteString("\n")
	}
	
	// Help text
	var help string
	if m.main.user.Role == auth.RoleFacilitator {
		help = "'v' Toggle participants  'e' Export  'n' New session  'r' Restart  ESC Back  'q' Quit"
	} else {
		help = "'v' Toggle participants  ESC Back  'q' Quit"
	}
	content.WriteString(view.RenderFooter(help, m.width))
	
	return view.Styles.Container.Width(m.width-4).Height(m.height-4).Render(content.String())
}

// SetSize updates the model size
func (m *ResultsModel) SetSize(width, height int) {
	m.width = width
	m.height = height
}

// renderResults renders the voting results
func (m *ResultsModel) renderResults() string {
	var content strings.Builder
	
	results := m.main.currentSession.GetResults()
	if len(results) == 0 {
		content.WriteString(view.Styles.Muted.Render("No ideas were voted on."))
		content.WriteString("\n")
		return content.String()
	}
	
	// Find max votes for progress bar scaling
	maxVotes := 0
	if len(results) > 0 {
		maxVotes = results[0].Votes
	}
	
	for i, idea := range results {
		rank := i + 1
		
		// Progress bar
		bar := view.RenderProgressBar(idea.Votes, maxVotes, 20)
		
		// Vote count
		voteText := "vote"
		if idea.Votes != 1 {
			voteText = "votes"
		}
		
		line := fmt.Sprintf("%d. %-40s %s %d %s", 
			rank, idea.Text, bar, idea.Votes, voteText)
		
		content.WriteString(view.Styles.ListItem.Render(line))
		content.WriteString("\n")
	}
	
	return content.String()
}

// renderParticipants renders the list of participants
func (m *ResultsModel) renderParticipants() string {
	var content strings.Builder
	
	content.WriteString(view.Styles.ListTitle.Render("Participants:"))
	content.WriteString("\n")
	
	participants := make([]*session.Participant, 0)
	for _, participant := range m.main.currentSession.Participants {
		if participant.Connected {
			participants = append(participants, participant)
		}
	}
	
	if len(participants) == 0 {
		content.WriteString(view.Styles.Muted.Render("  No participants"))
		content.WriteString("\n")
		return content.String()
	}
	
	participantNames := make([]string, 0)
	for _, participant := range participants {
		name := participant.Name
		if participant.Role == session.RoleFacilitator {
			name += " (Facilitator)"
		}
		participantNames = append(participantNames, name)
	}
	
	content.WriteString(view.Styles.Body.Render("  " + strings.Join(participantNames, ", ")))
	content.WriteString("\n")
	
	return content.String()
}

// renderFacilitatorActions renders actions available to facilitators
func (m *ResultsModel) renderFacilitatorActions() string {
	var content strings.Builder
	
	content.WriteString(view.Styles.ListTitle.Render("Facilitator Actions:"))
	content.WriteString("\n")
	
	actions := []string{
		view.RenderButton("📥 Export CSV", false),
		view.RenderButton("🔄 New Session", false),
		view.RenderButton("↩️ Restart Voting", false),
	}
	
	content.WriteString("  ")
	content.WriteString(strings.Join(actions, "  "))
	content.WriteString("\n")
	
	return content.String()
}

// exportResults exports the results (placeholder implementation)
func (m *ResultsModel) exportResults() (tea.Model, tea.Cmd) {
	// In a real implementation, this would export to CSV or other format
	// For now, just show a success message
	
	results := m.main.currentSession.GetResults()
	
	// Create CSV content
	csvContent := "Rank,Idea,Votes\n"
	for i, idea := range results {
		csvContent += fmt.Sprintf("%d,\"%s\",%d\n", i+1, idea.Text, idea.Votes)
	}
	
	// In a real implementation, you might save this to a file or provide download
	// For now, we'll just show a message
	
	return m, ShowError(fmt.Errorf("Export functionality not implemented in demo"))
}

