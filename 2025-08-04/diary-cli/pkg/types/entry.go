package types

import (
	"time"
)

// EntryType represents the type of diary entry
type EntryType string

const (
	EntryTypeTIL      EntryType = "til"
	EntryTypeThought  EntryType = "thought"
	EntryTypeDid      EntryType = "did"
	EntryTypeLink     EntryType = "link"
	EntryTypeTodo     EntryType = "todo"
)

// Format represents the output format for entries
type Format string

const (
	FormatDefault  Format = "default"
	FormatMarkdown Format = "markdown"
	FormatTask     Format = "task"
)

// Priority represents todo priority levels
type Priority string

const (
	PriorityHigh   Priority = "high"
	PriorityMedium Priority = "medium"
	PriorityLow    Priority = "low"
)

// DiaryEntry represents a diary entry with all possible fields
type DiaryEntry struct {
	Type         EntryType  `json:"type"`
	Title        string     `json:"title,omitempty"`
	Content      string     `json:"content"`
	Subtitle     string     `json:"subtitle,omitempty"`
	Date         time.Time  `json:"date"`
	Tags         []string   `json:"tags"`
	File         string     `json:"file"`
	LineNum      int        `json:"line_number"`
	SubtitleSlug string     `json:"subtitle_slug,omitempty"`
	Format       Format     `json:"format"`
	Priority     Priority   `json:"priority,omitempty"`
	DueDate      *time.Time `json:"due_date,omitempty"`
	Completed    bool       `json:"completed"`
	TaskID       string     `json:"task_id,omitempty"`
	URL          string     `json:"url,omitempty"` // for links
}

// InteractiveForm represents the form data for interactive entry creation
type InteractiveForm struct {
	EntryType    string
	Format       string
	Title        string
	Content      string
	Date         string
	UseEditor    bool
	SubtitleSlug string
}

// TodoForm represents the form data for interactive todo creation
type TodoForm struct {
	Description string
	Priority    string
	DueDate     string
	Tags        string
	UseEditor   bool
}

// AppendForm represents the form data for appending to entries
type AppendForm struct {
	SelectedEntry string
	SubtitleSlug  string
	Content       string
	UseEditor     bool
}

// IsValid checks if the entry type is valid
func (et EntryType) IsValid() bool {
	switch et {
	case EntryTypeTIL, EntryTypeThought, EntryTypeDid, EntryTypeLink, EntryTypeTodo:
		return true
	default:
		return false
	}
}

// IsValid checks if the format is valid
func (f Format) IsValid() bool {
	switch f {
	case FormatDefault, FormatMarkdown, FormatTask:
		return true
	default:
		return false
	}
}

// IsValid checks if the priority is valid
func (p Priority) IsValid() bool {
	switch p {
	case PriorityHigh, PriorityMedium, PriorityLow:
		return true
	default:
		return false
	}
}

// String returns the string representation of EntryType
func (et EntryType) String() string {
	return string(et)
}

// String returns the string representation of Format
func (f Format) String() string {
	return string(f)
}

// String returns the string representation of Priority
func (p Priority) String() string {
	return string(p)
}

