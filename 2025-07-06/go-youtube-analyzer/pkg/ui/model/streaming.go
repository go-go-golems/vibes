package model

import (
	"context"
	"fmt"
	"time"

	"github.com/charmbracelet/bubbles/help"
	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/progress"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/glamour"
	"github.com/charmbracelet/lipgloss"

	"github.com/user/youtube-analyzer-go/internal/logger"
	"github.com/user/youtube-analyzer-go/pkg/models"
)

// StreamingModel represents the streaming analysis screen
type StreamingModel struct {
	Common         CommonState
	VideoURL       string
	Prompt         string
	viewport       viewport.Model
	progress       progress.Model
	help           help.Model
	keyMap         StreamingKeyMap
	renderer       *glamour.TermRenderer
	content        string
	isStreaming    bool
	isPaused       bool
	streamContext  context.Context
	streamCancel   context.CancelFunc
	analysis       *models.TechnicalAnalysis
	err            error
	statusMessage  string
	lastUpdateTime time.Time
	logger         *logger.Logger
}

// StreamingKeyMap defines key bindings for streaming screen
type StreamingKeyMap struct {
	Pause      key.Binding
	Resume     key.Binding
	Stop       key.Binding
	ScrollUp   key.Binding
	ScrollDown key.Binding
	ViewResult key.Binding
	Back       key.Binding
	Help       key.Binding
	Quit       key.Binding
}

// ShortHelp returns keybindings to be shown in the mini help view
func (k StreamingKeyMap) ShortHelp() []key.Binding {
	return []key.Binding{k.Pause, k.Stop, k.Back, k.Help, k.Quit}
}

// FullHelp returns keybindings for the expanded help view
func (k StreamingKeyMap) FullHelp() [][]key.Binding {
	return [][]key.Binding{
		{k.Pause, k.Resume, k.Stop, k.ViewResult},
		{k.ScrollUp, k.ScrollDown, k.Back, k.Help, k.Quit},
	}
}

// DefaultStreamingKeyMap returns the default key bindings for streaming
func DefaultStreamingKeyMap() StreamingKeyMap {
	return StreamingKeyMap{
		Pause: key.NewBinding(
			key.WithKeys("p", "space"),
			key.WithHelp("p/space", "pause/resume"),
		),
		Resume: key.NewBinding(
			key.WithKeys("r"),
			key.WithHelp("r", "resume"),
		),
		Stop: key.NewBinding(
			key.WithKeys("s"),
			key.WithHelp("s", "stop streaming"),
		),
		ScrollUp: key.NewBinding(
			key.WithKeys("up", "k"),
			key.WithHelp("↑/k", "scroll up"),
		),
		ScrollDown: key.NewBinding(
			key.WithKeys("down", "j"),
			key.WithHelp("↓/j", "scroll down"),
		),
		ViewResult: key.NewBinding(
			key.WithKeys("enter"),
			key.WithHelp("enter", "view results"),
		),
		Back: key.NewBinding(
			key.WithKeys("esc", "b"),
			key.WithHelp("esc/b", "back to input"),
		),
		Help: key.NewBinding(
			key.WithKeys("?"),
			key.WithHelp("?", "toggle help"),
		),
		Quit: key.NewBinding(
			key.WithKeys("ctrl+c"),
			key.WithHelp("ctrl+c", "quit"),
		),
	}
}

// NewStreamingModel creates a new streaming model
func NewStreamingModel(common CommonState) StreamingModel {
	vp := viewport.New(common.Width-4, common.Height-8)
	vp.Style = lipgloss.NewStyle().
		BorderStyle(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("62")).
		PaddingLeft(2).
		PaddingRight(2)

	prog := progress.New(progress.WithDefaultGradient())
	prog.Width = common.Width - 4

	renderer, _ := glamour.NewTermRenderer(
		glamour.WithAutoStyle(),
		glamour.WithWordWrap(common.Width-8),
	)

	return StreamingModel{
		Common:         common,
		viewport:       vp,
		progress:       prog,
		help:           help.New(),
		keyMap:         DefaultStreamingKeyMap(),
		renderer:       renderer,
		content:        "",
		isStreaming:    false,
		isPaused:       false,
		statusMessage:  "Initializing...",
		lastUpdateTime: time.Now(),
		logger:         nil, // Will be set by the main model
	}
}

// Init initializes the streaming model
func (m StreamingModel) Init() tea.Cmd {
	return tea.Batch(
		m.startStreaming(),
		tea.Tick(time.Millisecond*100, func(time.Time) tea.Msg {
			return StreamingTickMsg{}
		}),
	)
}

// Update handles messages for the streaming model
func (m StreamingModel) Update(msg tea.Msg) (StreamingModel, tea.Cmd) {
	var cmd tea.Cmd
	var cmds []tea.Cmd

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.Common.Width = msg.Width
		m.Common.Height = msg.Height
		m.viewport.Width = msg.Width - 4
		m.viewport.Height = msg.Height - 8
		m.progress.Width = msg.Width - 4

	case tea.KeyMsg:
		switch {
		case key.Matches(msg, m.keyMap.Pause):
			if m.isStreaming {
				if m.isPaused {
					m.isPaused = false
					m.statusMessage = "Streaming resumed..."
				} else {
					m.isPaused = true
					m.statusMessage = "Streaming paused"
				}
			}

		case key.Matches(msg, m.keyMap.Stop):
			if m.streamCancel != nil {
				m.streamCancel()
			}
			m.isStreaming = false
			m.statusMessage = "Streaming stopped"

		case key.Matches(msg, m.keyMap.ViewResult):
			if m.analysis != nil {
				return m, func() tea.Msg {
					return ScreenChangeMsg{
						Screen:   ScreenResults,
						Analysis: m.analysis,
					}
				}
			}

		case key.Matches(msg, m.keyMap.Back):
			if m.streamCancel != nil {
				m.streamCancel()
			}
			return m, func() tea.Msg {
				return ScreenChangeMsg{Screen: ScreenInput}
			}

		case key.Matches(msg, m.keyMap.Help):
			m.help.ShowAll = !m.help.ShowAll

		case key.Matches(msg, m.keyMap.Quit):
			if m.streamCancel != nil {
				m.streamCancel()
			}
			return m, tea.Quit
		}

	case StreamingTickMsg:
		if m.isStreaming {
			cmds = append(cmds, tea.Tick(time.Millisecond*100, func(time.Time) tea.Msg {
				return StreamingTickMsg{}
			}))
		}

	case StreamingContentMsg:
		if m.logger != nil {
			m.logger.Debug().
				Str("component", "streaming").
				Int("contentLength", len(msg.Content)).
				Bool("isPaused", m.isPaused).
				Msg("Received streaming content message")
		}

		if !m.isPaused {
			m.content += msg.Content
			m.updateViewport()
			m.statusMessage = fmt.Sprintf("Streaming... (%.1f KB received)", float64(len(m.content))/1024)
			m.lastUpdateTime = time.Now()

			if m.logger != nil {
				m.logger.Debug().
					Str("component", "streaming").
					Int("totalContentLength", len(m.content)).
					Float64("contentSizeKB", float64(len(m.content))/1024).
					Msg("Updated streaming content")
			}
		}

	case StreamingCompleteMsg:
		if m.logger != nil {
			m.logger.Info().
				Str("component", "streaming").
				Bool("hasAnalysis", msg.Analysis != nil).
				Bool("hasError", msg.Error != nil).
				Msg("Received streaming complete message")
		}

		m.isStreaming = false
		m.analysis = msg.Analysis
		m.statusMessage = "Analysis complete! Press Enter to view results."
		if msg.Error != nil {
			m.err = msg.Error
			m.statusMessage = fmt.Sprintf("Error: %v", msg.Error)
			if m.logger != nil {
				m.logger.Error().
					Err(msg.Error).
					Str("component", "streaming").
					Msg("Streaming completed with error")
			}
		}

	case StreamingErrorMsg:
		if m.logger != nil {
			m.logger.Error().
				Err(msg.Error).
				Str("component", "streaming").
				Msg("Received streaming error message")
		}

		m.isStreaming = false
		m.err = msg.Error
		m.statusMessage = fmt.Sprintf("Streaming error: %v", msg.Error)
	}

	// Update viewport
	m.viewport, cmd = m.viewport.Update(msg)
	cmds = append(cmds, cmd)

	return m, tea.Batch(cmds...)
}

// View renders the streaming screen
func (m StreamingModel) View() string {
	var sections []string

	// Header
	title := lipgloss.NewStyle().
		Foreground(lipgloss.Color("205")).
		Background(lipgloss.Color("235")).
		Padding(0, 1).
		Render("🎬 YouTube Video Analysis - Streaming")

	sections = append(sections, title)

	// Video URL
	if m.VideoURL != "" {
		urlStyle := lipgloss.NewStyle().
			Foreground(lipgloss.Color("39")).
			Italic(true)
		sections = append(sections, fmt.Sprintf("📺 Video: %s", urlStyle.Render(m.VideoURL)))
	}

	// Status and progress
	statusStyle := lipgloss.NewStyle().
		Foreground(lipgloss.Color("214")).
		Bold(true)
	sections = append(sections, fmt.Sprintf("📊 Status: %s", statusStyle.Render(m.statusMessage)))

	if m.isStreaming {
		// Animated progress bar
		progressPercent := float64(time.Since(m.lastUpdateTime).Seconds()) / 10.0
		if progressPercent > 1.0 {
			progressPercent = 1.0
		}
		sections = append(sections, m.progress.ViewAs(progressPercent))
	}

	// Content viewport
	m.viewport.SetContent(m.getRenderedContent())
	sections = append(sections, m.viewport.View())

	// Help
	helpView := m.help.View(m.keyMap)
	sections = append(sections, helpView)

	return lipgloss.JoinVertical(lipgloss.Left, sections...)
}

// startStreaming starts the streaming analysis
func (m StreamingModel) startStreaming() tea.Cmd {
	return func() tea.Msg {
		if m.logger != nil {
			m.logger.Debug().
				Str("component", "streaming").
				Str("function", "startStreaming").
				Str("videoURL", m.VideoURL).
				Str("prompt", m.Prompt).
				Msg("Starting streaming analysis")
		}

		if m.VideoURL == "" && m.Prompt == "" {
			err := fmt.Errorf("no video URL or prompt provided")
			if m.logger != nil {
				m.logger.Error().
					Err(err).
					Str("component", "streaming").
					Msg("No input provided for streaming")
			}
			return StreamingErrorMsg{Error: err}
		}

		// Create context for streaming
		ctx, cancel := context.WithCancel(context.Background())
		m.streamContext = ctx
		m.streamCancel = cancel
		m.isStreaming = true

		if m.logger != nil {
			m.logger.Info().
				Str("component", "streaming").
				Bool("isStreaming", m.isStreaming).
				Bool("hasVideoURL", m.VideoURL != "").
				Bool("hasPrompt", m.Prompt != "").
				Msg("Streaming context created")
		}

		// Start streaming analysis in a goroutine
		go func() {
			if m.logger != nil {
				m.logger.Debug().
					Str("component", "streaming").
					Msg("Starting performStreamingAnalysis goroutine")
			}
			m.performStreamingAnalysis(ctx)
		}()

		if m.VideoURL != "" {
			if m.logger != nil {
				m.logger.Info().
					Str("component", "streaming").
					Str("type", "video_analysis").
					Str("videoURL", m.VideoURL).
					Msg("Starting video analysis streaming")
			}
			return StreamingContentMsg{Content: "🚀 Starting video analysis...\n\n"}
		} else {
			if m.logger != nil {
				m.logger.Info().
					Str("component", "streaming").
					Str("type", "text_generation").
					Str("prompt", m.Prompt).
					Msg("Starting text generation streaming")
			}
			return StreamingContentMsg{Content: "🚀 Starting text generation...\n\n"}
		}
	}
}

// performStreamingAnalysis performs the actual streaming analysis using Gemini client
func (m StreamingModel) performStreamingAnalysis(ctx context.Context) {
	if m.logger != nil {
		m.logger.Debug().
			Str("component", "streaming").
			Str("function", "performStreamingAnalysis").
			Str("videoURL", m.VideoURL).
			Str("prompt", m.Prompt).
			Msg("Starting streaming analysis goroutine")
	}

	var simulatedContent []string

	if m.VideoURL != "" {
		if m.logger != nil {
			m.logger.Info().
				Str("component", "streaming").
				Str("analysisType", "video").
				Str("videoURL", m.VideoURL).
				Msg("Preparing video analysis simulation")
		}

		// Simulate video analysis streaming content
		simulatedContent = []string{
			"## 🎬 Video Analysis Started\n\n",
			"**Analyzing video content...**\n\n",
			"### 📊 Initial Assessment\n",
			"- Video URL validated ✅\n",
			"- Connecting to AI service ✅\n",
			"- Processing video frames...\n\n",
			"### 🔍 Content Analysis\n",
			"- Extracting key topics...\n",
			"- Analyzing engagement factors...\n",
			"- Evaluating technical content...\n\n",
			"### 🎯 Target Audience Detection\n",
			"- Identifying primary demographics...\n",
			"- Analyzing content complexity...\n",
			"- Mapping to interest categories...\n\n",
			"### 📈 Engagement Metrics\n",
			"- Calculating viral potential...\n",
			"- Assessing social media readiness...\n",
			"- Generating recommendations...\n\n",
			"### ✅ Analysis Complete\n",
			"Finalizing results and structured data...\n\n",
		}
	} else if m.Prompt != "" {
		if m.logger != nil {
			m.logger.Info().
				Str("component", "streaming").
				Str("analysisType", "text").
				Str("prompt", m.Prompt).
				Msg("Preparing text generation simulation")
		}

		// Simulate text generation streaming content
		simulatedContent = []string{
			"## 🤖 AI Text Generation Started\n\n",
			fmt.Sprintf("**Processing prompt:** %s\n\n", m.Prompt),
			"### 🧠 Thinking Process\n",
			"- Understanding prompt context ✅\n",
			"- Generating creative response...\n",
			"- Applying language model knowledge...\n\n",
			"### ✍️ Content Generation\n",
			"- Crafting introduction...\n",
			"- Developing main content...\n",
			"- Adding creative elements...\n\n",
			"### 🎨 Styling & Formatting\n",
			"- Applying markdown formatting...\n",
			"- Enhancing readability...\n",
			"- Adding final touches...\n\n",
			"### ✅ Generation Complete\n",
			"Your creative content is ready!\n\n",
		}
	}

	if m.logger != nil {
		m.logger.Debug().
			Str("component", "streaming").
			Int("contentChunks", len(simulatedContent)).
			Msg("Starting content simulation loop")
	}

	for i, contentPart := range simulatedContent {
		select {
		case <-ctx.Done():
			if m.logger != nil {
				m.logger.Warn().
					Str("component", "streaming").
					Int("chunkIndex", i).
					Int("totalChunks", len(simulatedContent)).
					Msg("Streaming cancelled by context")
			}
			return
		default:
			if m.logger != nil {
				m.logger.Debug().
					Str("component", "streaming").
					Int("chunkIndex", i).
					Int("totalChunks", len(simulatedContent)).
					Int("contentLength", len(contentPart)).
					Msg("Processing content chunk")
			}

			// Send content update
			time.Sleep(time.Millisecond * 500)
			// In real implementation, this would be sent via tea.Cmd
			// For now, we'll simulate the final result
			_ = contentPart // Use the content part
			if i == len(simulatedContent)-1 {
				if m.logger != nil {
					m.logger.Info().
						Str("component", "streaming").
						Msg("Simulation complete - all content chunks processed")
				}
				// Complete the analysis
				break
			}
		}
	}

	if m.logger != nil {
		m.logger.Debug().
			Str("component", "streaming").
			Str("function", "performStreamingAnalysis").
			Msg("Streaming analysis goroutine completed")
	}
}

// updateViewport updates the viewport content and scrolls to bottom
func (m StreamingModel) updateViewport() {
	content := m.getRenderedContent()
	m.viewport.SetContent(content)
	m.viewport.GotoBottom()
}

// getRenderedContent returns the rendered markdown content
func (m StreamingModel) getRenderedContent() string {
	if m.renderer == nil {
		return m.content
	}

	rendered, err := m.renderer.Render(m.content)
	if err != nil {
		// Fallback to raw content if rendering fails
		return m.content
	}

	return rendered
}

// StreamingTickMsg is sent periodically during streaming
type StreamingTickMsg struct{}

// StreamingContentMsg contains streaming content updates
type StreamingContentMsg struct {
	Content string
}

// StreamingCompleteMsg is sent when streaming analysis is complete
type StreamingCompleteMsg struct {
	Analysis *models.TechnicalAnalysis
	Error    error
}

// StreamingErrorMsg is sent when streaming encounters an error
type StreamingErrorMsg struct {
	Error error
}
