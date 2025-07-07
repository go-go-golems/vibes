package session

import (
	"sync"
	"time"
)

// Role represents the role of a participant
type Role string

const (
	RoleFacilitator Role = "facilitator"
	RoleParticipant Role = "participant"
)

// SessionState represents the current state of a voting session
type SessionState string

const (
	StateWaitingForIdeas SessionState = "waiting_for_ideas"
	StateVoting         SessionState = "voting"
	StateResults        SessionState = "results"
	StateClosed         SessionState = "closed"
)

// Participant represents a user in a session
type Participant struct {
	ID           string    `json:"id"`
	Name         string    `json:"name"`
	Role         Role      `json:"role"`
	KeyFingerprint string  `json:"key_fingerprint"`
	DotsUsed     int       `json:"dots_used"`
	DotsTotal    int       `json:"dots_total"`
	JoinedAt     time.Time `json:"joined_at"`
	Connected    bool      `json:"connected"`
}

// Idea represents a voting item
type Idea struct {
	ID          string    `json:"id"`
	Text        string    `json:"text"`
	Votes       int       `json:"votes"`
	VotedBy     []string  `json:"voted_by"` // participant IDs
	CreatedAt   time.Time `json:"created_at"`
	CreatedBy   string    `json:"created_by"` // participant ID
}

// Vote represents a single vote cast by a participant
type Vote struct {
	ParticipantID string    `json:"participant_id"`
	IdeaID        string    `json:"idea_id"`
	Timestamp     time.Time `json:"timestamp"`
}

// Session represents a dot voting session
type Session struct {
	ID            string                 `json:"id"`
	Code          string                 `json:"code"`
	Title         string                 `json:"title"`
	State         SessionState           `json:"state"`
	Participants  map[string]*Participant `json:"participants"`
	Ideas         map[string]*Idea       `json:"ideas"`
	Votes         []Vote                 `json:"votes"`
	DotsPerPerson int                    `json:"dots_per_person"`
	AllowMultiple bool                   `json:"allow_multiple"` // allow multiple dots per idea
	CreatedAt     time.Time              `json:"created_at"`
	CreatedBy     string                 `json:"created_by"` // facilitator ID
	ShowLiveResults bool                 `json:"show_live_results"`
	
	// Thread safety
	mu sync.RWMutex
}

// NewSession creates a new voting session
func NewSession(code, title, facilitatorID string) *Session {
	return &Session{
		ID:            generateID(),
		Code:          code,
		Title:         title,
		State:         StateWaitingForIdeas,
		Participants:  make(map[string]*Participant),
		Ideas:         make(map[string]*Idea),
		Votes:         make([]Vote, 0),
		DotsPerPerson: 5,
		AllowMultiple: true,
		CreatedAt:     time.Now(),
		CreatedBy:     facilitatorID,
		ShowLiveResults: false,
	}
}

// AddParticipant adds a participant to the session
func (s *Session) AddParticipant(participant *Participant) {
	s.mu.Lock()
	defer s.mu.Unlock()
	
	participant.DotsTotal = s.DotsPerPerson
	participant.DotsUsed = 0
	participant.JoinedAt = time.Now()
	participant.Connected = true
	
	s.Participants[participant.ID] = participant
}

// RemoveParticipant removes a participant from the session
func (s *Session) RemoveParticipant(participantID string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	
	if participant, exists := s.Participants[participantID]; exists {
		participant.Connected = false
	}
}

// AddIdea adds a new idea to the session
func (s *Session) AddIdea(text, createdBy string) *Idea {
	s.mu.Lock()
	defer s.mu.Unlock()
	
	idea := &Idea{
		ID:        generateID(),
		Text:      text,
		Votes:     0,
		VotedBy:   make([]string, 0),
		CreatedAt: time.Now(),
		CreatedBy: createdBy,
	}
	
	s.Ideas[idea.ID] = idea
	return idea
}

// RemoveIdea removes an idea from the session
func (s *Session) RemoveIdea(ideaID string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	
	// Remove votes for this idea
	newVotes := make([]Vote, 0)
	for _, vote := range s.Votes {
		if vote.IdeaID != ideaID {
			newVotes = append(newVotes, vote)
		} else {
			// Return dots to participant
			if participant, exists := s.Participants[vote.ParticipantID]; exists {
				participant.DotsUsed--
			}
		}
	}
	s.Votes = newVotes
	
	delete(s.Ideas, ideaID)
}

// CastVote casts a vote for an idea
func (s *Session) CastVote(participantID, ideaID string) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	
	participant, exists := s.Participants[participantID]
	if !exists {
		return ErrParticipantNotFound
	}
	
	idea, exists := s.Ideas[ideaID]
	if !exists {
		return ErrIdeaNotFound
	}
	
	// Check if participant has dots available
	if participant.DotsUsed >= participant.DotsTotal {
		return ErrNoDotsAvailable
	}
	
	// Check if multiple votes per idea are allowed
	if !s.AllowMultiple {
		for _, vote := range s.Votes {
			if vote.ParticipantID == participantID && vote.IdeaID == ideaID {
				return ErrAlreadyVoted
			}
		}
	}
	
	// Cast the vote
	vote := Vote{
		ParticipantID: participantID,
		IdeaID:        ideaID,
		Timestamp:     time.Now(),
	}
	
	s.Votes = append(s.Votes, vote)
	participant.DotsUsed++
	idea.Votes++
	
	// Add to voted by list if not already there
	found := false
	for _, voterID := range idea.VotedBy {
		if voterID == participantID {
			found = true
			break
		}
	}
	if !found {
		idea.VotedBy = append(idea.VotedBy, participantID)
	}
	
	return nil
}

// GetResults returns the voting results sorted by vote count
func (s *Session) GetResults() []*Idea {
	s.mu.RLock()
	defer s.mu.RUnlock()
	
	ideas := make([]*Idea, 0, len(s.Ideas))
	for _, idea := range s.Ideas {
		ideas = append(ideas, idea)
	}
	
	// Sort by votes (descending)
	for i := 0; i < len(ideas)-1; i++ {
		for j := i + 1; j < len(ideas); j++ {
			if ideas[i].Votes < ideas[j].Votes {
				ideas[i], ideas[j] = ideas[j], ideas[i]
			}
		}
	}
	
	return ideas
}

// GetParticipantVotes returns all votes cast by a participant
func (s *Session) GetParticipantVotes(participantID string) []Vote {
	s.mu.RLock()
	defer s.mu.RUnlock()
	
	votes := make([]Vote, 0)
	for _, vote := range s.Votes {
		if vote.ParticipantID == participantID {
			votes = append(votes, vote)
		}
	}
	
	return votes
}

// SetState changes the session state
func (s *Session) SetState(state SessionState) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.State = state
}

// GetState returns the current session state
func (s *Session) GetState() SessionState {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return s.State
}

// GetParticipantCount returns the number of connected participants
func (s *Session) GetParticipantCount() int {
	s.mu.RLock()
	defer s.mu.RUnlock()
	
	count := 0
	for _, participant := range s.Participants {
		if participant.Connected {
			count++
		}
	}
	return count
}

