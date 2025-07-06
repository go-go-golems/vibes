package model

import (
	"github.com/charmbracelet/bubbles/help"
	tea "github.com/charmbracelet/bubbletea"

	"github.com/user/youtube-analyzer-go/internal/config"
	"github.com/user/youtube-analyzer-go/internal/logger"
	"github.com/user/youtube-analyzer-go/pkg/gemini"
	"github.com/user/youtube-analyzer-go/pkg/models"
)

// Screen represents the different screens in the application
type Screen int

const (
	ScreenInput Screen = iota
	ScreenLoading
	ScreenResults
	ScreenError
)

// CommonState holds shared state across all screens
type CommonState struct {
	Width        int
	Height       int
	GeminiClient *gemini.Client
	Config       *config.Config
	Logger       *logger.Logger
	Help         help.Model
}

// MainModel is the root model that manages screen transitions
type MainModel struct {
	Common        CommonState
	CurrentScreen Screen
	InputModel    InputModel
	LoadingModel  LoadingModel
	ResultsModel  ResultsModel
	ErrorModel    ErrorModel
}

// NewMainModel creates a new main model
func NewMainModel(geminiClient *gemini.Client, cfg *config.Config, log *logger.Logger) MainModel {
	common := CommonState{
		Width:        80,
		Height:       24,
		GeminiClient: geminiClient,
		Config:       cfg,
		Logger:       log,
		Help:         help.New(),
	}

	return MainModel{
		Common:        common,
		CurrentScreen: ScreenInput,
		InputModel:    NewInputModel(common),
		LoadingModel:  NewLoadingModel(common),
		ResultsModel:  NewResultsModel(common),
		ErrorModel:    NewErrorModel(common),
	}
}

// Init initializes the main model
func (m MainModel) Init() tea.Cmd {
	return m.InputModel.Init()
}

// Update handles all messages for the main model
func (m MainModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd

	// Handle window size changes
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.Common.Width = msg.Width
		m.Common.Height = msg.Height
		m.InputModel.Common = m.Common
		m.LoadingModel.Common = m.Common
		m.ResultsModel.Common = m.Common
		m.ErrorModel.Common = m.Common

	case ScreenChangeMsg:
		m.CurrentScreen = msg.Screen
		switch msg.Screen {
		case ScreenLoading:
			m.LoadingModel.VideoURL = msg.VideoURL
			return m, m.LoadingModel.Init()
		case ScreenResults:
			m.ResultsModel.Analysis = msg.Analysis
			return m, m.ResultsModel.Init()
		case ScreenError:
			m.ErrorModel.Err = msg.Error
			return m, m.ErrorModel.Init()
		case ScreenInput:
			return m, m.InputModel.Init()
		}

	case tea.KeyMsg:
		switch msg.String() {
		case "ctrl+c":
			return m, tea.Quit
		}
	}

	// Route to appropriate screen
	switch m.CurrentScreen {
	case ScreenInput:
		m.InputModel, cmd = m.InputModel.Update(msg)
	case ScreenLoading:
		m.LoadingModel, cmd = m.LoadingModel.Update(msg)
	case ScreenResults:
		m.ResultsModel, cmd = m.ResultsModel.Update(msg)
	case ScreenError:
		m.ErrorModel, cmd = m.ErrorModel.Update(msg)
	}

	return m, cmd
}

// View renders the current screen
func (m MainModel) View() string {
	switch m.CurrentScreen {
	case ScreenInput:
		return m.InputModel.View()
	case ScreenLoading:
		return m.LoadingModel.View()
	case ScreenResults:
		return m.ResultsModel.View()
	case ScreenError:
		return m.ErrorModel.View()
	default:
		return "Unknown screen"
	}
}

// ScreenChangeMsg is sent when changing screens
type ScreenChangeMsg struct {
	Screen   Screen
	VideoURL string
	Analysis *models.TechnicalAnalysis
	Error    error
}

// AnalysisCompleteMsg is sent when analysis is complete
type AnalysisCompleteMsg struct {
	Analysis *models.TechnicalAnalysis
	Error    error
}

// BackToInputMsg is sent when user wants to go back to input
type BackToInputMsg struct{}
