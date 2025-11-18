package models

import (
	"os/exec"
	"sync"
	"time"
)

type ServiceStatus int

const (
	StatusStopped ServiceStatus = iota
	StatusStarting
	StatusRunning
	StatusFailed
)

func (s ServiceStatus) String() string {
	switch s {
	case StatusStopped:
		return "STOPPED"
	case StatusStarting:
		return "STARTING"
	case StatusRunning:
		return "RUNNING"
	case StatusFailed:
		return "FAILED"
	default:
		return "UNKNOWN"
	}
}

func (s ServiceStatus) Icon() string {
	switch s {
	case StatusStopped:
		return "⭕"
	case StatusStarting:
		return "⏳"
	case StatusRunning:
		return "✅"
	case StatusFailed:
		return "❌"
	default:
		return "❓"
	}
}

type Service struct {
	Name       string
	Port       int
	Status     ServiceStatus
	PID        int
	CPUPercent float64
	MemoryMB   int
	StartTime  time.Time
	Cmd        *exec.Cmd
	LogBuffer  *LogBuffer
	BinaryPath string
	EnvVars    []string
}

type LogLine struct {
	Timestamp time.Time
	Service   string
	Message   string
}

type LogBuffer struct {
	Lines    []LogLine
	MaxLines int
	mu       sync.RWMutex
}

func NewLogBuffer(maxLines int) *LogBuffer {
	return &LogBuffer{
		Lines:    make([]LogLine, 0),
		MaxLines: maxLines,
	}
}

func (lb *LogBuffer) Add(service, message string) {
	lb.mu.Lock()
	defer lb.mu.Unlock()

	line := LogLine{
		Timestamp: time.Now(),
		Service:   service,
		Message:   message,
	}

	lb.Lines = append(lb.Lines, line)
	if len(lb.Lines) > lb.MaxLines {
		lb.Lines = lb.Lines[1:]
	}
}

func (lb *LogBuffer) GetLines() []LogLine {
	lb.mu.RLock()
	defer lb.mu.RUnlock()

	result := make([]LogLine, len(lb.Lines))
	copy(result, lb.Lines)
	return result
}

func (lb *LogBuffer) GetFilteredLines(service string) []LogLine {
	lb.mu.RLock()
	defer lb.mu.RUnlock()

	if service == "All" {
		result := make([]LogLine, len(lb.Lines))
		copy(result, lb.Lines)
		return result
	}

	result := make([]LogLine, 0)
	for _, line := range lb.Lines {
		if line.Service == service {
			result = append(result, line)
		}
	}
	return result
}

type Config struct {
	EnvSources    []EnvSource
	Database      map[string]string
	OAuth         map[string]string
	ServiceConfig map[string]string
}

type EnvSource struct {
	Path   string
	Loaded bool
}

type Warning struct {
	Timestamp time.Time
	Service   string
	Message   string
	Impact    string
	Action    string
}

type Error struct {
	Timestamp  time.Time
	Service    string
	Message    string
	StackTrace []string
	Impact     string
	Action     string
}
