package model

import (
	"context"
	"fmt"
	"time"

	"github.com/charmbracelet/bubbles/spinner"
	tea "github.com/charmbracelet/bubbletea"

	"github.com/user/youtube-analyzer-go/pkg/ui/keys"
	"github.com/user/youtube-analyzer-go/pkg/ui/view"
)

// LoadingModel handles the loading screen during analysis
type LoadingModel struct {
	Common   CommonState
	VideoURL string
	spinner  spinner.Model
	keyMap   keys.KeyMap
	progress string
	stage    string
	elapsed  time.Duration
	started  time.Time
}

// NewLoadingModel creates a new loading model
func NewLoadingModel(common CommonState) LoadingModel {
	s := spinner.New()
	s.Spinner = spinner.Dot
	s.Style = view.Styles.Spinner

	return LoadingModel{
		Common:  common,
		spinner: s,
		keyMap:  keys.NewKeyMap(),
		stage:   "Initializing",
	}
}

// Init initializes the loading model
func (m LoadingModel) Init() tea.Cmd {
	m.started = time.Now()
	return tea.Batch(
		m.spinner.Tick,
		m.startAnalysis(),
		m.tickCmd(),
	)
}

// Update handles messages for the loading model
func (m LoadingModel) Update(msg tea.Msg) (LoadingModel, tea.Cmd) {
	var cmd tea.Cmd

	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch {
		case msg.String() == "ctrl+c":
			return m, tea.Quit
		case msg.String() == "esc":
			return m, func() tea.Msg {
				return ScreenChangeMsg{Screen: ScreenInput}
			}
		}

	case spinner.TickMsg:
		m.spinner, cmd = m.spinner.Update(msg)
		return m, cmd

	case ProgressMsg:
		m.stage = msg.Stage
		m.progress = msg.Message
		return m, nil

	case TickMsg:
		m.elapsed = time.Since(m.started)
		return m, m.tickCmd()

	case AnalysisCompleteMsg:
		if msg.Error != nil {
			return m, func() tea.Msg {
				return ScreenChangeMsg{
					Screen: ScreenError,
					Error:  msg.Error,
				}
			}
		}
		return m, func() tea.Msg {
			return ScreenChangeMsg{
				Screen:   ScreenResults,
				Analysis: msg.Analysis,
			}
		}
	}

	return m, cmd
}

// View renders the loading screen
func (m LoadingModel) View() string {
	width := view.AdaptWidth(m.Common.Width)
	height := view.AdaptHeight(m.Common.Height)

	// Header
	header := view.RenderHeader("🎬 YouTube Analyzer - Processing", width)

	// Spinner and status
	spinnerView := fmt.Sprintf("%s %s", m.spinner.View(), m.stage)

	// Progress information
	var progressInfo []string
	progressInfo = append(progressInfo, view.RenderKeyValue("URL", m.VideoURL))
	progressInfo = append(progressInfo, view.RenderKeyValue("Model", m.Common.Config.GetModelName()))
	progressInfo = append(progressInfo, view.RenderKeyValue("Mode", m.Common.Config.Mode))
	progressInfo = append(progressInfo, view.RenderKeyValue("Elapsed", m.formatDuration(m.elapsed)))

	if m.progress != "" {
		progressInfo = append(progressInfo, "")
		progressInfo = append(progressInfo, view.Styles.Value.Render(m.progress))
	}

	// Status stages
	stages := []string{
		"🔍 Analyzing video content",
		"🤖 Processing with AI",
		"📊 Extracting insights",
		"✨ Generating recommendations",
	}

	var stageViews []string
	for _, stage := range stages {
		if stage == "🔍 Analyzing video content" && m.stage != "Initializing" {
			stageViews = append(stageViews, view.Styles.Success.Render("✅ "+stage))
		} else if stage == "🤖 Processing with AI" && (m.stage == "Extracting insights" || m.stage == "Generating recommendations" || m.stage == "Complete") {
			stageViews = append(stageViews, view.Styles.Success.Render("✅ "+stage))
		} else if stage == "📊 Extracting insights" && (m.stage == "Generating recommendations" || m.stage == "Complete") {
			stageViews = append(stageViews, view.Styles.Success.Render("✅ "+stage))
		} else if stage == "✨ Generating recommendations" && m.stage == "Complete" {
			stageViews = append(stageViews, view.Styles.Success.Render("✅ "+stage))
		} else {
			stageViews = append(stageViews, view.Styles.Value.Render(stage))
		}
	}

	// Help
	helpView := m.Common.Help.View(m.keyMap)

	// Instructions
	instructions := view.Styles.Help.Render(
		"Please wait while we analyze your video • Esc to cancel • Ctrl+C to quit",
	)

	// Footer
	footer := view.RenderFooter("Analyzing with "+m.Common.Config.GetModelName(), width)

	// Content
	content := view.Styles.Content.Render(
		view.RenderSection("Analysis in Progress",
			fmt.Sprintf("%s\n\n%s\n\n%s\n\n%s\n\n%s",
				spinnerView,
				fmt.Sprintf("Progress Information:\n%s", view.Styles.Value.Render(fmt.Sprintf("%s", progressInfo))),
				fmt.Sprintf("Stages:\n%s", view.Styles.Value.Render(fmt.Sprintf("%s", stageViews))),
				instructions,
				helpView,
			),
			width,
		),
	)

	// Center content vertically
	usedHeight := len(header) + len(content) + len(footer) + 3
	remainingHeight := height - usedHeight
	if remainingHeight > 0 {
		padding := fmt.Sprintf("%*s", remainingHeight/2, "")
		content = padding + content
	}

	return header + "\n" + content + "\n" + footer
}

// startAnalysis starts the video analysis
func (m LoadingModel) startAnalysis() tea.Cmd {
	return func() tea.Msg {
		ctx := context.Background()

		// Start analysis
		analysis, err := m.Common.GeminiClient.AnalyzeVideo(ctx, m.VideoURL)

		return AnalysisCompleteMsg{
			Analysis: analysis,
			Error:    err,
		}
	}
}

// tickCmd returns a command to update the elapsed time
func (m LoadingModel) tickCmd() tea.Cmd {
	return tea.Tick(time.Second, func(t time.Time) tea.Msg {
		return TickMsg(t)
	})
}

// formatDuration formats the duration for display
func (m LoadingModel) formatDuration(d time.Duration) string {
	if d < time.Minute {
		return fmt.Sprintf("%.0fs", d.Seconds())
	}
	return fmt.Sprintf("%.0fm %.0fs", d.Minutes(), d.Seconds()-60*d.Minutes())
}

// ProgressMsg represents a progress update
type ProgressMsg struct {
	Stage   string
	Message string
}

// TickMsg represents a timer tick
type TickMsg time.Time
