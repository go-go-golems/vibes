package models

import (
	"cyberpunk-tui/animations"
	tea "github.com/charmbracelet/bubbletea"
)

// Resizable interface for components that handle resize events
type Resizable interface {
	Resize(width, height int)
}

// Animatable interface for components with animations
type Animatable interface {
	UpdateAnimation(clock *animations.AnimationClock)
}

// Component interface combines all component capabilities
type Component interface {
	tea.Model
	Resizable
	Animatable
}

// BaseModel provides common functionality for all components
type BaseModel struct {
	Width  int
	Height int
}

// NewBaseModel creates a new base model
func NewBaseModel(width, height int) BaseModel {
	return BaseModel{
		Width:  width,
		Height: height,
	}
}

// Resize updates the model dimensions
func (m *BaseModel) Resize(width, height int) {
	m.Width = width
	m.Height = height
}

// TickMsg is sent periodically to update animations
type TickMsg struct{}

// GlitchMsg is sent to trigger glitch effects
type GlitchMsg struct{}

// ResizeMsg is sent when terminal is resized
type ResizeMsg struct {
	Width  int
	Height int
}

// KeyMsg wraps tea.KeyMsg for consistency
type KeyMsg tea.KeyMsg

// MouseMsg wraps tea.MouseMsg for consistency  
type MouseMsg tea.MouseMsg

