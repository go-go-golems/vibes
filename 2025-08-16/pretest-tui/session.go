package main

import (
	"fmt"
	"time"
)

// UserAnswer represents a user's answer to a question
type UserAnswer struct {
	QuestionID    string    `json:"question_id"`
	Answer        string    `json:"answer"`        // Option ID for MCQ, text for short answer
	IsCorrect     bool      `json:"is_correct"`    // Only meaningful for MCQ
	TimeSpent     time.Duration `json:"time_spent"`
	HintsUsed     int       `json:"hints_used"`
	AnsweredAt    time.Time `json:"answered_at"`
}

// SessionState tracks the user's progress through the pretest
type SessionState struct {
	Pretest         *Pretest
	CurrentQuestion int
	UserAnswers     []UserAnswer
	StartTime       time.Time
	QuestionStartTime time.Time
	ShowingHints    bool
	CurrentHintIndex int
	ShowingRationale bool
	ShowingReferences bool
	Completed       bool
	Score           int  // For MCQ questions only
	TotalMCQ        int  // Total number of MCQ questions
}

// NewSession creates a new session for the given pretest
func NewSession(pretest *Pretest) *SessionState {
	totalMCQ := 0
	for _, q := range pretest.Questions {
		if q.Type == "mcq" {
			totalMCQ++
		}
	}

	return &SessionState{
		Pretest:           pretest,
		CurrentQuestion:   0,
		UserAnswers:       make([]UserAnswer, 0),
		StartTime:         time.Now(),
		QuestionStartTime: time.Now(),
		ShowingHints:      false,
		CurrentHintIndex:  0,
		ShowingRationale:  false,
		ShowingReferences: false,
		Completed:         false,
		Score:             0,
		TotalMCQ:          totalMCQ,
	}
}

// GetCurrentQuestion returns the current question or nil if completed
func (s *SessionState) GetCurrentQuestion() *Question {
	if s.CurrentQuestion >= len(s.Pretest.Questions) {
		return nil
	}
	return &s.Pretest.Questions[s.CurrentQuestion]
}

// SubmitAnswer submits an answer for the current question
func (s *SessionState) SubmitAnswer(answer string) error {
	if s.Completed {
		return fmt.Errorf("session already completed")
	}

	currentQ := s.GetCurrentQuestion()
	if currentQ == nil {
		return fmt.Errorf("no current question")
	}

	timeSpent := time.Since(s.QuestionStartTime)
	isCorrect := false

	// Check if answer is correct for MCQ
	if currentQ.Type == "mcq" {
		isCorrect = (answer == currentQ.Answer)
		if isCorrect {
			s.Score++
		}
	}

	userAnswer := UserAnswer{
		QuestionID: currentQ.ID,
		Answer:     answer,
		IsCorrect:  isCorrect,
		TimeSpent:  timeSpent,
		HintsUsed:  s.CurrentHintIndex,
		AnsweredAt: time.Now(),
	}

	s.UserAnswers = append(s.UserAnswers, userAnswer)
	s.ShowingRationale = (currentQ.Type == "mcq") // Show rationale for MCQ
	s.ShowingReferences = false
	s.ShowingHints = false
	s.CurrentHintIndex = 0

	return nil
}

// NextQuestion moves to the next question
func (s *SessionState) NextQuestion() {
	s.CurrentQuestion++
	s.QuestionStartTime = time.Now()
	s.ShowingHints = false
	s.CurrentHintIndex = 0
	s.ShowingRationale = false
	s.ShowingReferences = false

	if s.CurrentQuestion >= len(s.Pretest.Questions) {
		s.Completed = true
	}
}

// ShowNextHint shows the next available hint for the current question
func (s *SessionState) ShowNextHint() bool {
	currentQ := s.GetCurrentQuestion()
	if currentQ == nil || len(currentQ.Hints) == 0 {
		return false
	}

	if !s.ShowingHints {
		s.ShowingHints = true
		s.CurrentHintIndex = 0
		return true
	}

	if s.CurrentHintIndex < len(currentQ.Hints)-1 {
		s.CurrentHintIndex++
		return true
	}

	return false
}

// ToggleReferences toggles the display of references for the current question
func (s *SessionState) ToggleReferences() {
	s.ShowingReferences = !s.ShowingReferences
}

// GetProgress returns the current progress as a percentage
func (s *SessionState) GetProgress() float64 {
	if len(s.Pretest.Questions) == 0 {
		return 100.0
	}
	return float64(s.CurrentQuestion) / float64(len(s.Pretest.Questions)) * 100.0
}

// GetScorePercentage returns the score as a percentage (for MCQ questions only)
func (s *SessionState) GetScorePercentage() float64 {
	if s.TotalMCQ == 0 {
		return 0.0
	}
	return float64(s.Score) / float64(s.TotalMCQ) * 100.0
}

// GetTotalTime returns the total time spent in the session
func (s *SessionState) GetTotalTime() time.Duration {
	return time.Since(s.StartTime)
}

// GetSummary returns a summary of the session results
func (s *SessionState) GetSummary() string {
	if !s.Completed {
		return "Session not completed"
	}

	totalTime := s.GetTotalTime()
	avgTimePerQuestion := totalTime / time.Duration(len(s.Pretest.Questions))

	summary := fmt.Sprintf("Pretest Completed: %s\n", s.Pretest.Title)
	summary += fmt.Sprintf("Total Questions: %d\n", len(s.Pretest.Questions))
	
	if s.TotalMCQ > 0 {
		summary += fmt.Sprintf("MCQ Score: %d/%d (%.1f%%)\n", s.Score, s.TotalMCQ, s.GetScorePercentage())
	}
	
	summary += fmt.Sprintf("Total Time: %v\n", totalTime.Round(time.Second))
	summary += fmt.Sprintf("Average Time per Question: %v\n", avgTimePerQuestion.Round(time.Second))

	return summary
}

