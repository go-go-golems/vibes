package keyring

import (
	"context"
	"errors"
	"time"
)

// DeprecateOpts contains options for deprecating a key
type DeprecateOpts struct {
	Message     string
	ReplaceWith *Path
	Since       time.Time // if zero, now
	Extra       map[string]string
}

// Deprecate marks a key as deprecated
func (r *Ring) Deprecate(ctx context.Context, path Path, opts DeprecateOpts) error {
	if r.state == nil {
		return errors.New("no StateStore configured")
	}
	since := opts.Since
	if since.IsZero() {
		since = time.Now()
	}
	ks := KeyState{
		Status:      StatusDeprecated,
		Since:       since,
		Message:     opts.Message,
		ReplaceWith: opts.ReplaceWith,
		Extra:       opts.Extra,
	}
	if err := r.state.Put(r.profiles[0], path, ks); err != nil {
		return err
	}
	_ = r.audit.Record(ctx, AuditEvent{
		At: time.Now(), Type: EventDeprecate, Profile: r.profiles[0],
		Path: path.String(), Actor: r.actor, Success: true,
	})
	return nil
}

// InvalidateOpts contains options for invalidating a key
type InvalidateOpts struct {
	Reason         string
	DeleteAtSource bool // attempt to delete via writer.Delete
}

// Invalidate marks a key as invalidated
func (r *Ring) Invalidate(ctx context.Context, path Path, opts InvalidateOpts) error {
	if r.state == nil {
		return errors.New("no StateStore configured")
	}
	if opts.DeleteAtSource && r.writer != nil {
		err := r.writer.Delete(ctx, r.profiles[0], path)
		_ = r.audit.Record(ctx, AuditEvent{
			At: time.Now(), Type: EventDeleteSource, Profile: r.profiles[0],
			Path: path.String(), Backend: r.writer.Name(), Actor: r.actor, Success: err == nil,
			Err: errString(err),
		})
		// Proceed regardless: an on-disk/env remnant must still fail via policy.
	}
	ks := KeyState{
		Status:         StatusInvalidated,
		Since:          time.Now(),
		Reason:         opts.Reason,
		DeleteAtSource: opts.DeleteAtSource,
	}
	if err := r.state.Put(r.profiles[0], path, ks); err != nil {
		return err
	}
	_ = r.audit.Record(ctx, AuditEvent{
		At: time.Now(), Type: EventInvalidate, Profile: r.profiles[0],
		Path: path.String(), Actor: r.actor, Success: true,
	})
	return nil
}

// Reinstate restores a key to active state
func (r *Ring) Reinstate(ctx context.Context, path Path) error {
	if r.state == nil {
		return errors.New("no StateStore configured")
	}
	if err := r.state.Delete(r.profiles[0], path); err != nil {
		return err
	}
	_ = r.audit.Record(ctx, AuditEvent{
		At: time.Now(), Type: EventReinstate, Profile: r.profiles[0],
		Path: path.String(), Actor: r.actor, Success: true,
	})
	return nil
}

// GetKeyState returns the state of a key
func (r *Ring) GetKeyState(profile string, path Path) (KeyState, error) {
	if r.state == nil {
		return KeyState{Status: StatusActive}, nil
	}
	ks, err := r.state.Get(profile, path)
	if IsNotFound(err) {
		return KeyState{Status: StatusActive}, nil
	}
	return ks, err
}

