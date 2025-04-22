package models

import "time"

// Blackboard represents a level in the blackboard system
type Blackboard struct {
	ID        int64     `json:"id"`
	Level     string    `json:"level"`
	CreatedAt time.Time `json:"created_at"`
}

// Hypothesis represents a hypothesis in the blackboard system
type Hypothesis struct {
	ID           string    `json:"id"`
	BlackboardID int64     `json:"blackboard_id"`
	Content      string    `json:"content"`
	Confidence   float64   `json:"confidence"`
	TimeRange    string    `json:"time_range"`
	Owner        string    `json:"owner,omitempty"`
	Locked       bool      `json:"locked"`
	CreatedAt    time.Time `json:"created_at"`
}

// KnowledgeSource represents a knowledge source (agent) in the system
type KnowledgeSource struct {
	ID                string    `json:"id"`
	Name              string    `json:"name"`
	Status            string    `json:"status"`
	LastAction        string    `json:"last_action,omitempty"`
	BidValue          float64   `json:"bid_value"`
	ActivationPattern string    `json:"activation_pattern"`
	CreatedAt         time.Time `json:"created_at"`
}

// ActivityLog represents an entry in the activity log
type ActivityLog struct {
	ID                int64     `json:"id"`
	Time              string    `json:"time"`
	KnowledgeSourceID string    `json:"knowledge_source_id,omitempty"`
	Action            string    `json:"action"`
	Level             string    `json:"level"`
	CreatedAt         time.Time `json:"created_at"`
}

// FocusControl represents the current focus of control
type FocusControl struct {
	ID           int64     `json:"id"`
	CurrentFocus string    `json:"current_focus"`
	CreatedAt    time.Time `json:"created_at"`
}

// FocusPriority represents the priority of a level in the focus control
type FocusPriority struct {
	ID             int64     `json:"id"`
	Level          string    `json:"level"`
	Priority       float64   `json:"priority"`
	FocusControlID int64     `json:"focus_control_id"`
	CreatedAt      time.Time `json:"created_at"`
}

// BidQueue represents a bid from a knowledge source
type BidQueue struct {
	ID                int64     `json:"id"`
	KnowledgeSourceID string    `json:"knowledge_source_id"`
	BidValue          float64   `json:"bid_value"`
	Target            string    `json:"target,omitempty"`
	Action            string    `json:"action"`
	CreatedAt         time.Time `json:"created_at"`
}

// BlackboardState represents the complete state of the blackboard system
type BlackboardState struct {
	Phrase    []Hypothesis     `json:"phrase"`
	Word      []Hypothesis     `json:"word"`
	Syllable  []Hypothesis     `json:"syllable"`
	Segment   []Hypothesis     `json:"segment"`
	Parameter []Hypothesis     `json:"parameter"`
	KSources  []KnowledgeSource `json:"knowledge_sources"`
	Focus     struct {
		CurrentFocus    string             `json:"current_focus"`
		FocusPriorities map[string]float64 `json:"focus_priorities"`
		BidQueue        []BidQueue         `json:"bid_queue"`
	} `json:"focus"`
	ActivityLog []ActivityLog `json:"activity_log"`
}
