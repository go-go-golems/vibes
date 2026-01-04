package main

import (
	"fmt"
	"strings"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

// AppState represents the current state of the application
type AppState int

const (
	StateWelcome AppState = iota
	StateQuestion
	StateShowingAnswer
	StateSummary
	StateError
)

// Model represents the main application model
type Model struct {
	state       AppState
	session     *SessionState
	textInput   textinput.Model
	selectedOption int
	errorMessage string
	width       int
	height      int
}

// NewModel creates a new application model
func NewModel(pretest *Pretest) Model {
	ti := textinput.New()
	ti.Placeholder = "Type your answer here..."
	ti.Focus()
	ti.CharLimit = 500
	ti.Width = 50

	return Model{
		state:       StateWelcome,
		session:     NewSession(pretest),
		textInput:   ti,
		selectedOption: 0,
		width:       80,
		height:      24,
	}
}

// Init initializes the model
func (m Model) Init() tea.Cmd {
	return textinput.Blink
}

// Update handles messages and updates the model
func (m Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height
		return m, nil

	case tea.KeyMsg:
		switch m.state {
		case StateWelcome:
			return m.handleWelcomeKeys(msg)
		case StateQuestion:
			return m.handleQuestionKeys(msg)
		case StateShowingAnswer:
			return m.handleAnswerKeys(msg)
		case StateSummary:
			return m.handleSummaryKeys(msg)
		case StateError:
			return m.handleErrorKeys(msg)
		}
	}

	// Update text input for short answer questions
	if m.state == StateQuestion {
		currentQ := m.session.GetCurrentQuestion()
		if currentQ != nil && currentQ.Type == "short" {
			m.textInput, cmd = m.textInput.Update(msg)
		}
	}

	return m, cmd
}

// handleWelcomeKeys handles key presses in the welcome state
func (m Model) handleWelcomeKeys(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch msg.String() {
	case "q", "ctrl+c":
		return m, tea.Quit
	case "enter", " ":
		m.state = StateQuestion
		return m, nil
	}
	return m, nil
}

// handleQuestionKeys handles key presses in the question state
func (m Model) handleQuestionKeys(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	currentQ := m.session.GetCurrentQuestion()
	if currentQ == nil {
		m.state = StateSummary
		return m, nil
	}

	switch msg.String() {
	case "q", "ctrl+c":
		return m, tea.Quit
	case "h":
		m.session.ShowNextHint()
		return m, nil
	case "r":
		m.session.ToggleReferences()
		return m, nil
	}

	if currentQ.Type == "mcq" {
		return m.handleMCQKeys(msg)
	} else {
		return m.handleShortAnswerKeys(msg)
	}
}

// handleMCQKeys handles key presses for multiple choice questions
func (m Model) handleMCQKeys(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	currentQ := m.session.GetCurrentQuestion()
	if currentQ == nil {
		return m, nil
	}

	switch msg.String() {
	case "up", "k":
		if m.selectedOption > 0 {
			m.selectedOption--
		}
	case "down", "j":
		if m.selectedOption < len(currentQ.Options)-1 {
			m.selectedOption++
		}
	case "enter", " ":
		selectedOptionID := currentQ.Options[m.selectedOption].ID
		err := m.session.SubmitAnswer(selectedOptionID)
		if err != nil {
			m.errorMessage = err.Error()
			m.state = StateError
		} else {
			m.state = StateShowingAnswer
		}
		return m, nil
	}

	return m, nil
}

// handleShortAnswerKeys handles key presses for short answer questions
func (m Model) handleShortAnswerKeys(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch msg.String() {
	case "ctrl+enter":
		answer := strings.TrimSpace(m.textInput.Value())
		if answer == "" {
			return m, nil
		}
		
		err := m.session.SubmitAnswer(answer)
		if err != nil {
			m.errorMessage = err.Error()
			m.state = StateError
		} else {
			m.state = StateShowingAnswer
			m.textInput.SetValue("") // Clear input for next question
		}
		return m, nil
	}

	return m, nil
}

// handleAnswerKeys handles key presses in the answer showing state
func (m Model) handleAnswerKeys(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch msg.String() {
	case "q", "ctrl+c":
		return m, tea.Quit
	case "enter", " ", "n":
		m.session.NextQuestion()
		m.selectedOption = 0
		if m.session.Completed {
			m.state = StateSummary
		} else {
			m.state = StateQuestion
			// Focus text input for next question if it's short answer
			nextQ := m.session.GetCurrentQuestion()
			if nextQ != nil && nextQ.Type == "short" {
				m.textInput.Focus()
			}
		}
		return m, nil
	case "r":
		m.session.ToggleReferences()
		return m, nil
	}
	return m, nil
}

// handleSummaryKeys handles key presses in the summary state
func (m Model) handleSummaryKeys(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch msg.String() {
	case "q", "ctrl+c", "enter", " ":
		return m, tea.Quit
	}
	return m, nil
}

// handleErrorKeys handles key presses in the error state
func (m Model) handleErrorKeys(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch msg.String() {
	case "q", "ctrl+c", "enter", " ":
		return m, tea.Quit
	}
	return m, nil
}

// View renders the current view
func (m Model) View() string {
	switch m.state {
	case StateWelcome:
		return m.renderWelcome()
	case StateQuestion:
		return m.renderQuestion()
	case StateShowingAnswer:
		return m.renderAnswer()
	case StateSummary:
		return m.renderSummary()
	case StateError:
		return m.renderError()
	}
	return ""
}

// renderWelcome renders the welcome screen
func (m Model) renderWelcome() string {
	title := titleStyle.Width(m.width).Render(m.session.Pretest.Title)
	subtitle := subtitleStyle.Width(m.width).Render("Press Enter to start the pretest")
	
	help := helpStyle.Width(m.width).Render("Press 'q' to quit")
	
	content := lipgloss.JoinVertical(
		lipgloss.Center,
		title,
		subtitle,
		help,
	)
	
	return lipgloss.Place(m.width, m.height, lipgloss.Center, lipgloss.Center, content)
}

// renderQuestion renders the current question
func (m Model) renderQuestion() string {
	currentQ := m.session.GetCurrentQuestion()
	if currentQ == nil {
		return "No question available"
	}

	var content strings.Builder
	
	// Progress bar
	progress := m.session.GetProgress()
	progressBar := renderProgressBar(progress, m.width-10)
	content.WriteString(progressBar + "\n\n")
	
	// Question number and prompt
	questionNum := fmt.Sprintf("Question %d of %d", m.session.CurrentQuestion+1, len(m.session.Pretest.Questions))
	content.WriteString(questionStyle.Render(questionNum) + "\n")
	content.WriteString(promptStyle.Render(currentQ.Prompt) + "\n")
	
	// Render options or input based on question type
	if currentQ.Type == "mcq" {
		content.WriteString(m.renderMCQOptions(currentQ))
	} else {
		content.WriteString(m.renderShortAnswerInput())
	}
	
	// Show hints if available
	if m.session.ShowingHints && len(currentQ.Hints) > 0 {
		content.WriteString(m.renderHints(currentQ))
	}
	
	// Show references if toggled
	if m.session.ShowingReferences && len(currentQ.References) > 0 {
		content.WriteString(m.renderReferences(currentQ))
	}
	
	// Help text
	help := m.renderQuestionHelp(currentQ)
	content.WriteString(help)
	
	return content.String()
}

// renderMCQOptions renders multiple choice options
func (m Model) renderMCQOptions(question *Question) string {
	var options strings.Builder
	
	for i, option := range question.Options {
		var style lipgloss.Style
		if i == m.selectedOption {
			style = selectedOptionStyle
		} else {
			style = optionStyle
		}
		
		optionText := fmt.Sprintf("%s) %s", option.ID, option.Text)
		options.WriteString(style.Render(optionText) + "\n")
	}
	
	return options.String()
}

// renderShortAnswerInput renders the text input for short answer questions
func (m Model) renderShortAnswerInput() string {
	return inputStyle.Render(m.textInput.View()) + "\n"
}

// renderHints renders the current hints
func (m Model) renderHints(question *Question) string {
	if len(question.Hints) == 0 {
		return ""
	}
	
	var hints strings.Builder
	hints.WriteString("💡 Hints:\n")
	
	for i := 0; i <= m.session.CurrentHintIndex && i < len(question.Hints); i++ {
		hints.WriteString(fmt.Sprintf("  %d. %s\n", i+1, question.Hints[i]))
	}
	
	return hintBoxStyle.Render(hints.String()) + "\n"
}

// renderReferences renders the references
func (m Model) renderReferences(question *Question) string {
	if len(question.References) == 0 {
		return ""
	}
	
	var refs strings.Builder
	refs.WriteString("📚 References:\n")
	
	for i, ref := range question.References {
		refs.WriteString(fmt.Sprintf("  %d. %s\n", i+1, ref))
	}
	
	return referenceBoxStyle.Render(refs.String()) + "\n"
}

// renderQuestionHelp renders help text for the current question
func (m Model) renderQuestionHelp(question *Question) string {
	var helpText []string
	
	if question.Type == "mcq" {
		helpText = append(helpText, "↑/↓ navigate • Enter select")
	} else {
		helpText = append(helpText, "Ctrl+Enter submit answer")
	}
	
	if len(question.Hints) > 0 {
		helpText = append(helpText, "h show hint")
	}
	
	if len(question.References) > 0 {
		helpText = append(helpText, "r toggle references")
	}
	
	helpText = append(helpText, "q quit")
	
	return helpStyle.Width(m.width).Render(strings.Join(helpText, " • "))
}


// renderAnswer renders the answer feedback screen
func (m Model) renderAnswer() string {
	currentQ := m.session.GetCurrentQuestion()
	if currentQ == nil {
		return "No question available"
	}

	var content strings.Builder
	
	// Progress bar
	progress := m.session.GetProgress()
	progressBar := renderProgressBar(progress, m.width-10)
	content.WriteString(progressBar + "\n\n")
	
	// Question number
	questionNum := fmt.Sprintf("Question %d of %d", m.session.CurrentQuestion+1, len(m.session.Pretest.Questions))
	content.WriteString(questionStyle.Render(questionNum) + "\n")
	content.WriteString(promptStyle.Render(currentQ.Prompt) + "\n")
	
	// Show user's answer and feedback for MCQ
	if currentQ.Type == "mcq" {
		lastAnswer := m.session.UserAnswers[len(m.session.UserAnswers)-1]
		
		for _, option := range currentQ.Options {
			var style lipgloss.Style
			var prefix string
			
			if option.ID == currentQ.Answer {
				// Correct answer
				style = correctOptionStyle
				prefix = "✓ "
			} else if option.ID == lastAnswer.Answer {
				// User's incorrect answer
				style = incorrectOptionStyle
				prefix = "✗ "
			} else {
				// Other options
				style = optionStyle
				prefix = "  "
			}
			
			optionText := fmt.Sprintf("%s%s) %s", prefix, option.ID, option.Text)
			content.WriteString(style.Render(optionText) + "\n")
		}
		
		// Show rationales
		content.WriteString("\n📝 Explanations:\n")
		for _, option := range currentQ.Options {
			if option.Rationale != "" {
				var style lipgloss.Style
				if option.ID == currentQ.Answer {
					style = lipgloss.NewStyle().Foreground(secondaryColor)
				} else {
					style = lipgloss.NewStyle().Foreground(mutedColor)
				}
				
				explanation := fmt.Sprintf("%s) %s", option.ID, option.Rationale)
				content.WriteString(style.Render(explanation) + "\n")
			}
		}
		
		// Show if answer was correct
		if lastAnswer.IsCorrect {
			content.WriteString(scoreStyle.Render("\n✓ Correct!") + "\n")
		} else {
			content.WriteString(lipgloss.NewStyle().Foreground(errorColor).Render("\n✗ Incorrect") + "\n")
		}
	} else {
		// For short answer, just show what they wrote
		lastAnswer := m.session.UserAnswers[len(m.session.UserAnswers)-1]
		content.WriteString("Your answer:\n")
		content.WriteString(inputStyle.Render(lastAnswer.Answer) + "\n")
	}
	
	// Show references if toggled
	if m.session.ShowingReferences && len(currentQ.References) > 0 {
		content.WriteString(m.renderReferences(currentQ))
	}
	
	// Help text
	var helpText []string
	helpText = append(helpText, "Enter continue")
	if len(currentQ.References) > 0 {
		helpText = append(helpText, "r toggle references")
	}
	helpText = append(helpText, "q quit")
	
	help := helpStyle.Width(m.width).Render(strings.Join(helpText, " • "))
	content.WriteString(help)
	
	return content.String()
}

// renderSummary renders the final summary screen
func (m Model) renderSummary() string {
	summary := m.session.GetSummary()
	
	title := titleStyle.Width(m.width).Render("Pretest Complete!")
	
	// Add detailed statistics
	var stats strings.Builder
	stats.WriteString(summary)
	
	if len(m.session.UserAnswers) > 0 {
		stats.WriteString("\nDetailed Results:\n")
		for i, answer := range m.session.UserAnswers {
			question := m.session.Pretest.Questions[i]
			
			var status string
			if question.Type == "mcq" {
				if answer.IsCorrect {
					status = "✓ Correct"
				} else {
					status = "✗ Incorrect"
				}
			} else {
				status = "Answered"
			}
			
			timeStr := answer.TimeSpent.Round(1000000).String() // Round to microseconds for readability
			stats.WriteString(fmt.Sprintf("  %d. %s (%s, %s)\n", 
				i+1, question.ID, status, timeStr))
		}
	}
	
	summaryBox := summaryStyle.Width(m.width-4).Render(stats.String())
	
	help := helpStyle.Width(m.width).Render("Press any key to exit")
	
	content := lipgloss.JoinVertical(
		lipgloss.Center,
		title,
		summaryBox,
		help,
	)
	
	return lipgloss.Place(m.width, m.height, lipgloss.Center, lipgloss.Center, content)
}

// renderError renders the error screen
func (m Model) renderError() string {
	title := lipgloss.NewStyle().
		Foreground(errorColor).
		Bold(true).
		Align(lipgloss.Center).
		Width(m.width).
		Render("Error")
	
	errorBox := lipgloss.NewStyle().
		Foreground(errorColor).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(errorColor).
		Padding(1, 2).
		Width(m.width-4).
		Render(m.errorMessage)
	
	help := helpStyle.Width(m.width).Render("Press any key to exit")
	
	content := lipgloss.JoinVertical(
		lipgloss.Center,
		title,
		errorBox,
		help,
	)
	
	return lipgloss.Place(m.width, m.height, lipgloss.Center, lipgloss.Center, content)
}

