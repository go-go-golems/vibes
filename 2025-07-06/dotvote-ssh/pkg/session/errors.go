package session

import "errors"

var (
	ErrSessionNotFound      = errors.New("session not found")
	ErrParticipantNotFound  = errors.New("participant not found")
	ErrIdeaNotFound         = errors.New("idea not found")
	ErrNoDotsAvailable      = errors.New("no dots available")
	ErrAlreadyVoted         = errors.New("already voted for this idea")
	ErrInvalidSessionCode   = errors.New("invalid session code")
	ErrSessionClosed        = errors.New("session is closed")
	ErrUnauthorized         = errors.New("unauthorized action")
	ErrInvalidState         = errors.New("invalid session state for this action")
)

