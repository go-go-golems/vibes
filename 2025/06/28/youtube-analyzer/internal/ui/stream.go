package ui

import (
	"context"
	"fmt"
	"strings"
	"time"

	"github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/user/youtube-analyzer-go/pkg/models"
)

// StreamModel represents the state of the streaming UI
type StreamModel struct {
	videoURL      string
	sessionID     string
	mode          string
	width         int
	height        int
	
	// Analysis state
	isAnalyzing   bool
	isComplete    bool
	hasError      bool
	errorMsg      string
	
	// Streaming content
	chunks        []string
	currentChunk  string
	totalChunks   int
	
	// Progress tracking
	startTime     time.Time
	elapsed       time.Duration
	
	// Analysis result
	analysis      *models.TechnicalAnalysis
	
	// UI state
	showFullContent bool
	scrollOffset    int
	maxScroll       int
}

// StreamChunk represents a chunk of streaming content
type StreamChunk struct {
	Content string
}

// AnalysisComplete represents completion of the analysis
type AnalysisComplete struct {
	Analysis *models.TechnicalAnalysis
	Error    error
}

// TickMsg represents a timer tick
type TickMsg time.Time

// Styles for the UI
var (
	headerStyle = lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("#00D4AA")).
		BorderStyle(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("#00D4AA")).
		Padding(0, 1)
	
	statusStyle = lipgloss.NewStyle().
		Foreground(lipgloss.Color("#7C7C7C"))
	
	contentStyle = lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("#444444")).
		Padding(1).
		Height(15)
	
	chunkStyle = lipgloss.NewStyle().
		Foreground(lipgloss.Color("#FFFF00"))
	
	completeStyle = lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("#00FF00"))
	
	errorStyle = lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("#FF0000"))
	
	helpStyle = lipgloss.NewStyle().
		Foreground(lipgloss.Color("#626262"))
)

// NewStreamModel creates a new streaming model
func NewStreamModel(videoURL, sessionID, mode string) StreamModel {
	return StreamModel{
		videoURL:     videoURL,
		sessionID:    sessionID,
		mode:         mode,
		chunks:       make([]string, 0),
		startTime:    time.Now(),
		isAnalyzing:  true,
	}
}

// Init initializes the model
func (m StreamModel) Init() tea.Cmd {
	return tea.Batch(
		tea.Tick(time.Millisecond*100, func(t time.Time) tea.Msg {
			return TickMsg(t)
		}),
	)
}

// Update handles messages
func (m StreamModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height
		return m, nil
		
	case tea.KeyMsg:
		switch msg.String() {
		case "ctrl+c", "q":
			return m, tea.Quit
		case "f":
			m.showFullContent = !m.showFullContent
			return m, nil
		case "up":
			if m.scrollOffset > 0 {
				m.scrollOffset--
			}
			return m, nil
		case "down":
			if m.scrollOffset < m.maxScroll {
				m.scrollOffset++
			}
			return m, nil
		case "home":
			m.scrollOffset = 0
			return m, nil
		case "end":
			m.scrollOffset = m.maxScroll
			return m, nil
		}
		
	case StreamChunk:
		m.chunks = append(m.chunks, msg.Content)
		m.currentChunk = msg.Content
		m.totalChunks++
		return m, nil
		
	case AnalysisComplete:
		m.isAnalyzing = false
		m.isComplete = true
		if msg.Error != nil {
			m.hasError = true
			m.errorMsg = msg.Error.Error()
		} else {
			m.analysis = msg.Analysis
		}
		return m, nil
		
	case TickMsg:
		m.elapsed = time.Since(m.startTime)
		if m.isAnalyzing {
			return m, tea.Tick(time.Millisecond*100, func(t time.Time) tea.Msg {
				return TickMsg(t)
			})
		}
		return m, nil
	}
	
	return m, nil
}

// View renders the UI
func (m StreamModel) View() string {
	if m.width == 0 {
		return "Loading..."
	}
	
	var sections []string
	
	// Header
	header := headerStyle.Render("🎬 YouTube Analyzer - Streaming Mode")
	sections = append(sections, header)
	
	// Video info
	videoInfo := fmt.Sprintf("📺 %s\n🆔 %s\n⚙️  %s", 
		truncateURL(m.videoURL, 60), 
		m.sessionID, 
		m.mode)
	sections = append(sections, videoInfo)
	
	// Status
	status := m.renderStatus()
	sections = append(sections, status)
	
	// Content area
	content := m.renderContent()
	sections = append(sections, content)
	
	// Analysis summary (if complete)
	if m.isComplete && m.analysis != nil {
		summary := m.renderSummary()
		sections = append(sections, summary)
	}
	
	// Help
	help := m.renderHelp()
	sections = append(sections, help)
	
	return strings.Join(sections, "\n\n")
}

// renderStatus renders the current status
func (m StreamModel) renderStatus() string {
	if m.hasError {
		return errorStyle.Render(fmt.Sprintf("❌ Error: %s", m.errorMsg))
	}
	
	if m.isComplete {
		return completeStyle.Render(fmt.Sprintf("✅ Analysis Complete! ⏱️  %s", m.elapsed.Round(time.Second)))
	}
	
	if m.isAnalyzing {
		dots := strings.Repeat(".", int(m.elapsed.Seconds())%4)
		return statusStyle.Render(fmt.Sprintf("🔄 Analyzing%s ⏱️  %s | 📦 %d chunks received", 
			dots, 
			m.elapsed.Round(time.Second),
			m.totalChunks))
	}
	
	return statusStyle.Render("⏸️  Waiting...")
}

// renderContent renders the streaming content
func (m StreamModel) renderContent() string {
	if len(m.chunks) == 0 {
		return contentStyle.Render("Waiting for analysis to begin...")
	}
	
	content := strings.Join(m.chunks, "")
	
	if m.showFullContent {
		// Show scrollable full content
		lines := strings.Split(content, "\n")
		availableHeight := 20
		
		start := m.scrollOffset
		end := start + availableHeight
		if end > len(lines) {
			end = len(lines)
		}
		
		m.maxScroll = len(lines) - availableHeight
		if m.maxScroll < 0 {
			m.maxScroll = 0
		}
		
		displayLines := lines[start:end]
		scrollInfo := fmt.Sprintf("(Scroll: %d/%d)", start, len(lines))
		
		return contentStyle.
			Width(m.width - 4).
			Height(availableHeight + 2).
			Render(strings.Join(displayLines, "\n") + "\n\n" + scrollInfo)
	} else {
		// Show just the latest chunks with highlighting
		recentContent := content
		if len(content) > 800 {
			recentContent = "..." + content[len(content)-800:]
		}
		
		// Highlight the most recent chunk
		if m.currentChunk != "" {
			recentContent = strings.ReplaceAll(recentContent, m.currentChunk, chunkStyle.Render(m.currentChunk))
		}
		
		return contentStyle.
			Width(m.width - 4).
			Render(recentContent)
	}
}

// renderSummary renders the final analysis summary
func (m StreamModel) renderSummary() string {
	if m.analysis == nil {
		return ""
	}
	
	summary := []string{
		completeStyle.Render("📊 Analysis Summary:"),
		"─────────────────────",
	}
	
	if m.analysis.Summary != "" {
		truncated := m.analysis.Summary
		if len(truncated) > 200 {
			truncated = truncated[:200] + "..."
		}
		summary = append(summary, fmt.Sprintf("📝 %s", truncated))
	}
	
	if m.analysis.TechnicalScore > 0 {
		summary = append(summary, fmt.Sprintf("🎯 Technical Score: %.1f/10", m.analysis.TechnicalScore))
	}
	
	if m.analysis.ViralPotential > 0 {
		summary = append(summary, fmt.Sprintf("🚀 Viral Potential: %.1f/10", m.analysis.ViralPotential))
	}
	
	if m.analysis.TargetAudience != "" {
		summary = append(summary, fmt.Sprintf("🎯 Target: %s", m.analysis.TargetAudience))
	}
	
	if len(m.analysis.Technologies) > 0 {
		techs := strings.Join(m.analysis.Technologies, ", ")
		if len(techs) > 50 {
			techs = techs[:50] + "..."
		}
		summary = append(summary, fmt.Sprintf("🔧 Tech: %s", techs))
	}
	
	return strings.Join(summary, "\n")
}

// renderHelp renders help information
func (m StreamModel) renderHelp() string {
	if m.isComplete {
		return helpStyle.Render("Press 'f' to toggle full content view | ↑↓ to scroll | 'q' or Ctrl+C to exit")
	}
	return helpStyle.Render("Press 'q' or Ctrl+C to cancel analysis")
}

// truncateURL truncates a URL for display
func truncateURL(url string, maxLen int) string {
	if len(url) <= maxLen {
		return url
	}
	return url[:maxLen-3] + "..."
}

// RunStreamingUI runs the streaming UI
func RunStreamingUI(ctx context.Context, videoURL, sessionID, mode string, onReady func(func(string)) (*models.TechnicalAnalysis, error)) (*models.TechnicalAnalysis, error) {
	model := NewStreamModel(videoURL, sessionID, mode)
	
	p := tea.NewProgram(model, tea.WithAltScreen())
	
	// Channel to communicate with the tea program
	resultChan := make(chan *models.TechnicalAnalysis)
	errorChan := make(chan error)
	
	// Start the analysis in a goroutine
	go func() {
		defer close(resultChan)
		defer close(errorChan)
		
		// Create streaming callback that sends chunks to UI
		streamCallback := func(chunk string) {
			p.Send(StreamChunk{Content: chunk})
		}
		
		// Start analysis with streaming callback
		analysis, err := onReady(streamCallback)
		if err != nil {
			errorChan <- err
			return
		}
		
		resultChan <- analysis
	}()
	
	// Handle streaming results
	go func() {
		for {
			select {
			case analysis, ok := <-resultChan:
				if !ok {
					return
				}
				p.Send(AnalysisComplete{Analysis: analysis, Error: nil})
				return
				
			case err, ok := <-errorChan:
				if !ok {
					return
				}
				p.Send(AnalysisComplete{Analysis: nil, Error: err})
				return
				
			case <-ctx.Done():
				p.Send(AnalysisComplete{Analysis: nil, Error: ctx.Err()})
				return
			}
		}
	}()
	
	// Run the program
	finalModel, err := p.Run()
	if err != nil {
		return nil, err
	}
	
	// Extract the final result
	if streamModel, ok := finalModel.(StreamModel); ok {
		if streamModel.hasError {
			return nil, fmt.Errorf(streamModel.errorMsg)
		}
		return streamModel.analysis, nil
	}
	
	return nil, fmt.Errorf("unexpected model type")
}
