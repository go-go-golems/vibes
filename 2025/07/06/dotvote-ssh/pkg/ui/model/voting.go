package model

import (
	"fmt"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/bubbles/key"

	"dotvote-ssh/pkg/session"
	"dotvote-ssh/pkg/ui/view"
)

// VotingModel handles the participant voting screen
type VotingModel struct {
	main *MainModel
	
	// State
	width  int
	height int
	
	// Navigation
	selectedIdea int
	
	// Voting state
	myVotes map[string]int // idea ID -> vote count
}

// NewVotingModel creates a new voting model
func NewVotingModel(main *MainModel) *VotingModel {
	return &VotingModel{
		main:    main,
		myVotes: make(map[string]int),
	}
}

// Init initializes the voting model
func (m *VotingModel) Init() tea.Cmd {
	// Load existing votes for this participant
	if m.main.currentSession != nil {
		m.loadMyVotes()
	}
	return nil
}

// Update handles messages for the voting screen
func (m *VotingModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	if m.main.currentSession == nil {
		return m, ChangeScreen(ScreenLanding, nil)
	}
	
	// Check if session state changed
	switch m.main.currentSession.GetState() {
	case session.StateWaitingForIdeas:
		return m, ChangeScreen(ScreenWaiting, m.main.currentSession)
	case session.StateResults:
		return m, ChangeScreen(ScreenResults, m.main.currentSession)
	case session.StateClosed:
		return m, ChangeScreen(ScreenLanding, nil)
	}
	
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch {
		case key.Matches(msg, m.main.keys.Up):
			m.navigateUp()
			return m, nil
			
		case key.Matches(msg, m.main.keys.Down):
			m.navigateDown()
			return m, nil
			
		case key.Matches(msg, m.main.keys.Vote), key.Matches(msg, m.main.keys.Space):
			return m.castVote()
			
		case key.Matches(msg, m.main.keys.Remove):
			return m.removeVote()
		}
	}
	
	return m, nil
}

// View renders the voting screen
func (m *VotingModel) View() string {
	if m.width == 0 || m.main.currentSession == nil {
		return "Loading..."
	}
	
	var content strings.Builder
	
	// Header
	participant := m.main.currentSession.Participants[m.main.user.ID]
	dotsUsed := 0
	dotsTotal := 5
	if participant != nil {
		dotsUsed = participant.DotsUsed
		dotsTotal = participant.DotsTotal
	}
	
	header := fmt.Sprintf("Session: %s", m.main.currentSession.Title)
	dotsDisplay := fmt.Sprintf("Dots: %s", view.RenderDots(dotsUsed, dotsTotal))
	content.WriteString(view.RenderHeader(header, dotsDisplay, m.width))
	content.WriteString("\n\n")
	
	// Voting instructions
	if dotsUsed == 0 {
		content.WriteString(view.Styles.Subtitle.Render("🗳️ Cast your votes!"))
		content.WriteString("\n")
		content.WriteString(view.Styles.Body.Render("Select ideas and press SPACE or 'v' to vote"))
		content.WriteString("\n\n")
	} else if dotsUsed == dotsTotal {
		content.WriteString(view.Styles.StatusGood.Render("✅ All votes cast!"))
		content.WriteString("\n")
		content.WriteString(view.Styles.Body.Render("Waiting for other participants..."))
		content.WriteString("\n\n")
	} else {
		remaining := dotsTotal - dotsUsed
		content.WriteString(view.Styles.Subtitle.Render(fmt.Sprintf("🗳️ %d votes remaining", remaining)))
		content.WriteString("\n\n")
	}
	
	// Ideas list
	content.WriteString(m.renderIdeasList())
	content.WriteString("\n")
	
	// My votes summary
	if dotsUsed > 0 {
		content.WriteString(m.renderMyVotes())
		content.WriteString("\n")
	}
	
	// Help text
	help := "↑/↓ Navigate  SPACE/v Vote  x Remove vote  'q' Quit"
	content.WriteString(view.RenderFooter(help, m.width))
	
	return view.Styles.Container.Width(m.width-4).Height(m.height-4).Render(content.String())
}

// SetSize updates the model size
func (m *VotingModel) SetSize(width, height int) {
	m.width = width
	m.height = height
}

// renderIdeasList renders the list of ideas with voting interface
func (m *VotingModel) renderIdeasList() string {
	var content strings.Builder
	
	content.WriteString(view.Styles.ListTitle.Render("Ideas:"))
	content.WriteString("\n")
	
	ideas := m.getIdeasList()
	if len(ideas) == 0 {
		content.WriteString(view.Styles.Muted.Render("  No ideas available yet..."))
		content.WriteString("\n")
		return content.String()
	}
	
	for i, idea := range ideas {
		prefix := "  "
		if i == m.selectedIdea {
			prefix = "► "
		}
		
		// Show votes on this idea by current user
		myVotesOnIdea := m.myVotes[idea.ID]
		voteIndicator := ""
		if myVotesOnIdea > 0 {
			voteIndicator = fmt.Sprintf(" %s", strings.Repeat("💙", myVotesOnIdea))
		}
		
		// Show total votes if live results are enabled
		totalVotes := ""
		if m.main.currentSession.ShowLiveResults {
			totalVotes = fmt.Sprintf(" (%d total)", idea.Votes)
		}
		
		line := fmt.Sprintf("%s%s%s%s", prefix, idea.Text, voteIndicator, totalVotes)
		
		if i == m.selectedIdea {
			content.WriteString(view.Styles.ListSelected.Render(line))
		} else {
			content.WriteString(view.Styles.ListItem.Render(line))
		}
		content.WriteString("\n")
	}
	
	return content.String()
}

// renderMyVotes renders a summary of the user's votes
func (m *VotingModel) renderMyVotes() string {
	var content strings.Builder
	
	content.WriteString(view.Styles.ListTitle.Render("Your votes:"))
	content.WriteString("\n")
	
	ideas := m.getIdeasList()
	for _, idea := range ideas {
		if votes, exists := m.myVotes[idea.ID]; exists && votes > 0 {
			dots := strings.Repeat("💙", votes)
			line := fmt.Sprintf("  %s %s", dots, idea.Text)
			content.WriteString(view.Styles.ListItem.Render(line))
			content.WriteString("\n")
		}
	}
	
	return content.String()
}

// Helper methods

func (m *VotingModel) getIdeasList() []*session.Idea {
	ideas := make([]*session.Idea, 0)
	for _, idea := range m.main.currentSession.Ideas {
		ideas = append(ideas, idea)
	}
	return ideas
}

func (m *VotingModel) navigateUp() {
	ideas := m.getIdeasList()
	if len(ideas) > 0 {
		m.selectedIdea = max(0, m.selectedIdea-1)
	}
}

func (m *VotingModel) navigateDown() {
	ideas := m.getIdeasList()
	if len(ideas) > 0 {
		m.selectedIdea = min(len(ideas)-1, m.selectedIdea+1)
	}
}

func (m *VotingModel) castVote() (tea.Model, tea.Cmd) {
	ideas := m.getIdeasList()
	if len(ideas) == 0 || m.selectedIdea >= len(ideas) {
		return m, nil
	}
	
	selectedIdea := ideas[m.selectedIdea]
	
	// Check if user has dots available
	participant := m.main.currentSession.Participants[m.main.user.ID]
	if participant == nil || participant.DotsUsed >= participant.DotsTotal {
		return m, ShowError(session.ErrNoDotsAvailable)
	}
	
	// Cast the vote
	err := m.main.currentSession.CastVote(m.main.user.ID, selectedIdea.ID)
	if err != nil {
		return m, ShowError(err)
	}
	
	// Update local vote tracking
	m.myVotes[selectedIdea.ID]++
	
	return m, ClearError()
}

func (m *VotingModel) removeVote() (tea.Model, tea.Cmd) {
	ideas := m.getIdeasList()
	if len(ideas) == 0 || m.selectedIdea >= len(ideas) {
		return m, nil
	}
	
	selectedIdea := ideas[m.selectedIdea]
	
	// Check if user has votes on this idea
	if m.myVotes[selectedIdea.ID] == 0 {
		return m, nil
	}
	
	// Remove a vote (this is simplified - in a real implementation,
	// you'd need to track individual votes to remove them properly)
	m.removeVoteFromSession(selectedIdea.ID)
	m.myVotes[selectedIdea.ID]--
	if m.myVotes[selectedIdea.ID] == 0 {
		delete(m.myVotes, selectedIdea.ID)
	}
	
	return m, nil
}

func (m *VotingModel) removeVoteFromSession(ideaID string) {
	// Find and remove one vote for this user on this idea
	for i, vote := range m.main.currentSession.Votes {
		if vote.ParticipantID == m.main.user.ID && vote.IdeaID == ideaID {
			// Remove this vote
			m.main.currentSession.Votes = append(
				m.main.currentSession.Votes[:i],
				m.main.currentSession.Votes[i+1:]...,
			)
			
			// Update participant dots used
			if participant := m.main.currentSession.Participants[m.main.user.ID]; participant != nil {
				participant.DotsUsed--
			}
			
			// Update idea vote count
			if idea := m.main.currentSession.Ideas[ideaID]; idea != nil {
				idea.Votes--
			}
			
			break
		}
	}
}

func (m *VotingModel) loadMyVotes() {
	m.myVotes = make(map[string]int)
	
	// Count votes by this participant
	for _, vote := range m.main.currentSession.Votes {
		if vote.ParticipantID == m.main.user.ID {
			m.myVotes[vote.IdeaID]++
		}
	}
}

