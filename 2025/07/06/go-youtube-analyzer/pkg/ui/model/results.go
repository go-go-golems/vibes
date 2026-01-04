package model

import (
	"fmt"
	"strings"

	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"

	"github.com/user/youtube-analyzer-go/pkg/models"
	"github.com/user/youtube-analyzer-go/pkg/ui/keys"
	"github.com/user/youtube-analyzer-go/pkg/ui/view"
)

// ResultsModel handles the results display screen
type ResultsModel struct {
	Common   CommonState
	Analysis *models.TechnicalAnalysis
	viewport viewport.Model
	keyMap   keys.ResultsKeyMap
	content  string
	ready    bool
}

// NewResultsModel creates a new results model
func NewResultsModel(common CommonState) ResultsModel {
	return ResultsModel{
		Common: common,
		keyMap: keys.NewResultsKeyMap(),
	}
}

// Init initializes the results model
func (m ResultsModel) Init() tea.Cmd {
	return nil
}

// Update handles messages for the results model
func (m ResultsModel) Update(msg tea.Msg) (ResultsModel, tea.Cmd) {
	var cmd tea.Cmd

	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch {
		case msg.String() == "ctrl+c":
			return m, tea.Quit
		case msg.String() == "n":
			return m, func() tea.Msg {
				return ScreenChangeMsg{Screen: ScreenInput}
			}
		case msg.String() == "esc":
			return m, func() tea.Msg {
				return ScreenChangeMsg{Screen: ScreenInput}
			}
		}

	case tea.WindowSizeMsg:
		headerHeight := 3
		footerHeight := 3
		verticalMargins := headerHeight + footerHeight

		if !m.ready {
			m.viewport = viewport.New(msg.Width, msg.Height-verticalMargins)
			m.viewport.YPosition = headerHeight
			m.viewport.HighPerformanceRendering = false
			m.content = m.renderContent()
			m.viewport.SetContent(m.content)
			m.ready = true
		} else {
			m.viewport.Width = msg.Width
			m.viewport.Height = msg.Height - verticalMargins
		}
	}

	// Update viewport
	m.viewport, cmd = m.viewport.Update(msg)
	return m, cmd
}

// View renders the results screen
func (m ResultsModel) View() string {
	if !m.ready {
		return "\n  Initializing..."
	}

	width := view.AdaptWidth(m.Common.Width)

	// Header
	header := view.RenderHeader("🎬 YouTube Analyzer - Results", width)

	// Viewport content
	viewportContent := m.viewport.View()

	// Footer with help
	helpView := m.Common.Help.View(m.keyMap)
	instructions := view.Styles.Help.Render(
		"↑/↓ scroll • u/d page up/down • n new analysis • esc back • q quit",
	)

	footer := view.RenderFooter(fmt.Sprintf("%s | %s", instructions, helpView), width)

	return header + "\n" + viewportContent + "\n" + footer
}

// renderContent renders the analysis results content
func (m ResultsModel) renderContent() string {
	if m.Analysis == nil {
		return view.RenderError(fmt.Errorf("no analysis data available"))
	}

	var sections []string

	// Summary section
	sections = append(sections, m.renderSummarySection())

	// Scores section
	sections = append(sections, m.renderScoresSection())

	// Technologies section
	sections = append(sections, m.renderTechnologiesSection())

	// Timestamps section
	sections = append(sections, m.renderTimestampsSection())

	// Assessments section
	sections = append(sections, m.renderAssessmentsSection())

	// Social Media section
	sections = append(sections, m.renderSocialMediaSection())

	// Platform Recommendations section
	sections = append(sections, m.renderPlatformRecommendationsSection())

	// Raw response section (if available)
	if m.Analysis.RawResponse != "" {
		sections = append(sections, m.renderRawResponseSection())
	}

	return strings.Join(sections, "\n\n")
}

// renderSummarySection renders the summary section
func (m ResultsModel) renderSummarySection() string {
	if m.Analysis.Summary == "" {
		return ""
	}

	return view.RenderSection("📝 Summary", m.Analysis.Summary, m.Common.Width-4)
}

// renderScoresSection renders the scores section
func (m ResultsModel) renderScoresSection() string {
	var scores []string

	if m.Analysis.TechnicalScore > 0 {
		scores = append(scores, view.RenderKeyValue("Technical Score", fmt.Sprintf("%.1f/10", m.Analysis.TechnicalScore)))
	}
	if m.Analysis.ViralPotential > 0 {
		scores = append(scores, view.RenderKeyValue("Viral Potential", fmt.Sprintf("%.1f/10", m.Analysis.ViralPotential)))
	}
	if m.Analysis.TargetAudience != "" {
		scores = append(scores, view.RenderKeyValue("Target Audience", m.Analysis.TargetAudience))
	}

	if len(scores) == 0 {
		return ""
	}

	return view.RenderSection("📊 Scores & Metrics", strings.Join(scores, "\n"), m.Common.Width-4)
}

// renderTechnologiesSection renders the technologies section
func (m ResultsModel) renderTechnologiesSection() string {
	if len(m.Analysis.Technologies) == 0 {
		return ""
	}

	var techItems []string
	for _, tech := range m.Analysis.Technologies {
		techItems = append(techItems, "• "+tech)
	}

	return view.RenderSection("🔧 Technologies/Topics", strings.Join(techItems, "\n"), m.Common.Width-4)
}

// renderTimestampsSection renders the key timestamps section
func (m ResultsModel) renderTimestampsSection() string {
	if len(m.Analysis.KeyTimestamps) == 0 {
		return ""
	}

	var timestamps []string
	for _, ts := range m.Analysis.KeyTimestamps {
		importance := strings.ToUpper(ts.Importance)
		tsType := strings.ToUpper(ts.Type)

		timestamp := fmt.Sprintf("[%s] %s (%s, %s)",
			ts.Time,
			ts.Description,
			importance,
			tsType,
		)
		timestamps = append(timestamps, timestamp)
	}

	return view.RenderSection("⏰ Key Timestamps", strings.Join(timestamps, "\n\n"), m.Common.Width-4)
}

// renderAssessmentsSection renders the technical assessments section
func (m ResultsModel) renderAssessmentsSection() string {
	var assessments []string

	if m.Analysis.TechnicalAccuracy != "" {
		assessments = append(assessments, view.RenderKeyValue("Technical Accuracy", m.Analysis.TechnicalAccuracy))
	}
	if m.Analysis.EducationalValue != "" {
		assessments = append(assessments, view.RenderKeyValue("Educational Value", m.Analysis.EducationalValue))
	}
	if m.Analysis.CodeQuality != "" {
		assessments = append(assessments, view.RenderKeyValue("Code Quality", m.Analysis.CodeQuality))
	}
	if m.Analysis.DeveloperRelevance != "" {
		assessments = append(assessments, view.RenderKeyValue("Developer Relevance", m.Analysis.DeveloperRelevance))
	}

	if len(assessments) == 0 {
		return ""
	}

	return view.RenderSection("🔍 Technical Assessment", strings.Join(assessments, "\n\n"), m.Common.Width-4)
}

// renderSocialMediaSection renders the social media tips section
func (m ResultsModel) renderSocialMediaSection() string {
	if len(m.Analysis.SocialMediaTips) == 0 {
		return ""
	}

	var tips []string
	for _, tip := range m.Analysis.SocialMediaTips {
		tips = append(tips, "• "+tip)
	}

	return view.RenderSection("📱 Social Media Tips", strings.Join(tips, "\n"), m.Common.Width-4)
}

// renderPlatformRecommendationsSection renders the platform recommendations section
func (m ResultsModel) renderPlatformRecommendationsSection() string {
	if len(m.Analysis.PlatformRecommendations) == 0 {
		return ""
	}

	var recommendations []string
	platforms := []string{"twitter", "linkedin", "youtube", "tiktok", "reddit", "instagram"}

	for _, platform := range platforms {
		if rec, exists := m.Analysis.PlatformRecommendations[platform]; exists && rec != "" {
			recommendations = append(recommendations,
				view.RenderKeyValue(strings.ToUpper(platform), rec))
		}
	}

	if len(recommendations) == 0 {
		return ""
	}

	return view.RenderSection("🌐 Platform Recommendations", strings.Join(recommendations, "\n\n"), m.Common.Width-4)
}

// renderRawResponseSection renders the raw AI response section
func (m ResultsModel) renderRawResponseSection() string {
	if m.Analysis.RawResponse == "" {
		return ""
	}

	// Truncate if too long
	rawResponse := m.Analysis.RawResponse
	if len(rawResponse) > 1000 {
		rawResponse = rawResponse[:1000] + "\n\n... [truncated - showing first 1000 characters]"
	}

	return view.RenderSection("🤖 Raw AI Response", rawResponse, m.Common.Width-4)
}
