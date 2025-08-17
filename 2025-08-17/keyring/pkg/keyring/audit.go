package keyring

import (
	"context"
	"time"
)

// EventType represents the type of audit event
type EventType string

const (
	EventAcquire           EventType = "acquire"             // secret looked up and returned
	EventAcquireDeprecated EventType = "acquire_deprecated" // acquire succeeded but key is deprecated
	EventAcquireDenied     EventType = "acquire_denied"     // denied due to invalidated
	EventUse               EventType = "use"                // caller used the secret
	EventPut               EventType = "put"
	EventDeleteSource      EventType = "delete_source"
	EventInvalidate        EventType = "invalidate"
	EventDeprecate         EventType = "deprecate"
	EventReinstate         EventType = "reinstate"
)

// AuditEvent represents a single audit event
type AuditEvent struct {
	At      time.Time
	Type    EventType
	Profile string
	Path    string
	Backend string // source of the value (for acquires), or writer name for writes
	Actor   string // optional: service/user identifying string configured on Ring
	Success bool
	Err     string
	Meta    map[string]string // anything else (version, lease_id, etc.)
}

// AuditSink interface for recording audit events
type AuditSink interface {
	Record(ctx context.Context, evt AuditEvent) error
}

// noopAudit is a no-op implementation of AuditSink
type noopAudit struct{}

func (noopAudit) Record(context.Context, AuditEvent) error { return nil }

// NewNoopAudit returns a no-op audit sink
func NewNoopAudit() AuditSink {
	return noopAudit{}
}

