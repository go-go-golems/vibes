package session

import (
	"errors"
	"sync"
	"time"
)

// Manager manages multiple voting sessions
type Manager struct {
	sessions map[string]*Session // code -> session
	mu       sync.RWMutex
}

// NewManager creates a new session manager
func NewManager() *Manager {
	return &Manager{
		sessions: make(map[string]*Session),
	}
}

// CreateSession creates a new voting session
func (m *Manager) CreateSession(title, facilitatorID string) (*Session, error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	
	// Generate unique session code
	var code string
	for {
		code = GenerateSessionCode()
		if _, exists := m.sessions[code]; !exists {
			break
		}
	}
	
	session := NewSession(code, title, facilitatorID)
	m.sessions[code] = session
	
	return session, nil
}

// GetSession retrieves a session by code
func (m *Manager) GetSession(code string) (*Session, error) {
	m.mu.RLock()
	defer m.mu.RUnlock()
	
	session, exists := m.sessions[code]
	if !exists {
		return nil, ErrSessionNotFound
	}
	
	return session, nil
}

// JoinSession adds a participant to a session
func (m *Manager) JoinSession(code, participantName, participantID, keyFingerprint string) (*Session, error) {
	session, err := m.GetSession(code)
	if err != nil {
		return nil, err
	}
	
	if session.GetState() == StateClosed {
		return nil, ErrSessionClosed
	}
	
	if !ValidateName(participantName) {
		return nil, errors.New("invalid participant name")
	}
	
	participant := &Participant{
		ID:             participantID,
		Name:           participantName,
		Role:           RoleParticipant,
		KeyFingerprint: keyFingerprint,
	}
	
	session.AddParticipant(participant)
	
	return session, nil
}

// CloseSession closes a session
func (m *Manager) CloseSession(code string) error {
	m.mu.Lock()
	defer m.mu.Unlock()
	
	session, exists := m.sessions[code]
	if !exists {
		return ErrSessionNotFound
	}
	
	session.SetState(StateClosed)
	
	// Clean up after some time
	go func() {
		time.Sleep(1 * time.Hour)
		m.mu.Lock()
		delete(m.sessions, code)
		m.mu.Unlock()
	}()
	
	return nil
}

// GetActiveSessions returns all active sessions
func (m *Manager) GetActiveSessions() []*Session {
	m.mu.RLock()
	defer m.mu.RUnlock()
	
	sessions := make([]*Session, 0, len(m.sessions))
	for _, session := range m.sessions {
		if session.GetState() != StateClosed {
			sessions = append(sessions, session)
		}
	}
	
	return sessions
}

// CleanupInactiveSessions removes sessions that have been inactive for too long
func (m *Manager) CleanupInactiveSessions() {
	m.mu.Lock()
	defer m.mu.Unlock()
	
	cutoff := time.Now().Add(-24 * time.Hour)
	
	for code, session := range m.sessions {
		if session.CreatedAt.Before(cutoff) && session.GetParticipantCount() == 0 {
			delete(m.sessions, code)
		}
	}
}

