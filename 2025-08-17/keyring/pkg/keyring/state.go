package keyring

import "time"

// KeyStatus represents the state of a key
type KeyStatus int

const (
	StatusActive KeyStatus = iota
	StatusDeprecated
	StatusInvalidated
)

// String returns the string representation of the key status
func (s KeyStatus) String() string {
	switch s {
	case StatusActive:
		return "active"
	case StatusDeprecated:
		return "deprecated"
	case StatusInvalidated:
		return "invalidated"
	default:
		return "unknown"
	}
}

// KeyState represents the state information for a key
type KeyState struct {
	Status         KeyStatus
	Since          time.Time         // when this state started (server-time)
	Message        string            // deprecation/invalidation note
	ReplaceWith    *Path             // optional path to use instead
	Reason         string            // for invalidation
	DeleteAtSource bool              // if true, we attempted/should attempt deleting from writer
	Extra          map[string]string // free-form
}

// StateStore interface for managing key states
type StateStore interface {
	Get(profile string, path Path) (KeyState, error)     // ErrNotFound => Active implied
	Put(profile string, path Path, ks KeyState) error
	Delete(profile string, path Path) error // remove state row entirely
}

