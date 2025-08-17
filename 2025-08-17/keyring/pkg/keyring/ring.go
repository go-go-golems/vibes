package keyring

import (
	"context"
	"errors"
	"time"
)

var ErrInvalidated = errors.New("key is invalidated")

// Warning represents a deprecation warning
type Warning struct {
	Profile     string
	Path        Path
	Kind        string // "deprecated"
	Message     string
	Since       time.Time
	ReplaceWith *Path
}

// Ring is the main keyring aggregator
type Ring struct {
	profiles []string   // search order: e.g., ["work", "default"]
	readers  []Backend  // ordered
	writer   Backend    // single designated write target (optional)
	state    StateStore // state store for key lifecycle management
	audit    AuditSink  // audit sink for logging events
	warn     func(Warning)
	actor    string
}

// Option is a functional option for configuring the Ring
type Option func(*Ring)

// WithProfiles sets the profile search order
func WithProfiles(profiles ...string) Option {
	return func(r *Ring) { r.profiles = append([]string(nil), profiles...) }
}

// WithReaders sets the reader backends
func WithReaders(backends ...Backend) Option {
	return func(r *Ring) { r.readers = append([]Backend(nil), backends...) }
}

// WithWriter sets the writer backend
func WithWriter(backend Backend) Option {
	return func(r *Ring) { r.writer = backend }
}

// WithStateStore sets the state store
func WithStateStore(ss StateStore) Option {
	return func(r *Ring) { r.state = ss }
}

// WithAuditSink sets the audit sink
func WithAuditSink(as AuditSink) Option {
	return func(r *Ring) {
		if as == nil {
			r.audit = NewNoopAudit()
		} else {
			r.audit = as
		}
	}
}

// WithWarningCallback sets the warning callback function
func WithWarningCallback(fn func(Warning)) Option {
	return func(r *Ring) { r.warn = fn }
}

// WithActor sets the actor name for audit logging
func WithActor(actor string) Option {
	return func(r *Ring) { r.actor = actor }
}

// New creates a new Ring with the given options
func New(opts ...Option) *Ring {
	r := &Ring{
		audit: NewNoopAudit(),
	}
	for _, opt := range opts {
		opt(r)
	}
	if len(r.profiles) == 0 {
		r.profiles = []string{"default"}
	}
	if r.warn == nil {
		r.warn = func(Warning) {}
	}
	return r
}

// Handle wraps a successful acquisition; caller may log "uses" via h.Use(...)
type Handle struct {
	Secret  Secret
	Profile string
	Backend string
	Path    Path
	ring    *Ring
}

// Value returns the secret value
func (h Handle) Value() string { return h.Secret.Value }

// Use records a "use" event; will error if key is now invalidated
func (h Handle) Use(ctx context.Context, purpose string, attrs map[string]string) error {
	// Re-check invalidation at use-time
	if r := h.ring; r != nil && r.state != nil {
		ks, err := r.state.Get(h.Profile, h.Path)
		if err == nil && ks.Status == StatusInvalidated {
			meta := map[string]string{"purpose": purpose}
			for k, v := range attrs {
				meta[k] = v
			}
			_ = r.audit.Record(ctx, AuditEvent{
				At: time.Now(), Type: EventUse, Profile: h.Profile, Path: h.Path.String(),
				Backend: h.Backend, Actor: r.actor, Success: false, Err: "invalidated",
				Meta: meta,
			})
			return ErrInvalidated
		}
	}
	meta := map[string]string{"purpose": purpose}
	for k, v := range attrs {
		meta[k] = v
	}
	_ = h.ring.audit.Record(ctx, AuditEvent{
		At: time.Now(), Type: EventUse, Profile: h.Profile, Path: h.Path.String(),
		Backend: h.Backend, Actor: h.ring.actor, Success: true,
		Meta: meta,
	})
	return nil
}

// Acquire returns a Handle with source info + deprecation warnings/errors
func (r *Ring) Acquire(ctx context.Context, path Path) (Handle, error) {
	for _, prof := range r.profiles {
		for _, b := range r.readers {
			sec, err := b.Get(ctx, prof, path)
			if err == nil {
				// state gate
				if r.state != nil {
					ks, err2 := r.state.Get(prof, path)
					if err2 == nil {
						switch ks.Status {
						case StatusInvalidated:
							_ = r.audit.Record(ctx, AuditEvent{
								At: time.Now(), Type: EventAcquireDenied, Profile: prof, Path: path.String(),
								Backend: b.Name(), Actor: r.actor, Success: false, Err: "invalidated",
							})
							return Handle{}, ErrInvalidated
						case StatusDeprecated:
							r.warn(Warning{
								Profile: prof, Path: path, Kind: "deprecated",
								Message: ks.Message, Since: ks.Since, ReplaceWith: ks.ReplaceWith,
							})
							sec.Metadata = cloneAndSet(sec.Metadata,
								"deprecated", "true",
								"deprecated_since", ks.Since.Format(time.RFC3339),
							)
							_ = r.audit.Record(ctx, AuditEvent{
								At: time.Now(), Type: EventAcquireDeprecated, Profile: prof,
								Path: path.String(), Backend: b.Name(), Actor: r.actor, Success: true,
							})
						default:
							_ = r.audit.Record(ctx, AuditEvent{
								At: time.Now(), Type: EventAcquire, Profile: prof,
								Path: path.String(), Backend: b.Name(), Actor: r.actor, Success: true,
							})
						}
					} else {
						// no row => treated as Active
						_ = r.audit.Record(ctx, AuditEvent{
							At: time.Now(), Type: EventAcquire, Profile: prof,
							Path: path.String(), Backend: b.Name(), Actor: r.actor, Success: true,
						})
					}
				} else {
					_ = r.audit.Record(ctx, AuditEvent{
						At: time.Now(), Type: EventAcquire, Profile: prof,
						Path: path.String(), Backend: b.Name(), Actor: r.actor, Success: true,
					})
				}
				return Handle{Secret: sec, Profile: prof, Backend: b.Name(), Path: path, ring: r}, nil
			}
			if !IsNotFound(err) {
				return Handle{}, err
			}
		}
	}
	return Handle{}, ErrNotFound
}

// Get returns a secret (back-compat helper)
func (r *Ring) Get(ctx context.Context, path Path) (Secret, error) {
	h, err := r.Acquire(ctx, path)
	if err != nil {
		return Secret{}, err
	}
	return h.Secret, nil
}

// GetString returns a secret value as string
func (r *Ring) GetString(ctx context.Context, path Path) (string, error) {
	sec, err := r.Get(ctx, path)
	if err != nil {
		return "", err
	}
	return sec.Value, nil
}

// Put stores a secret using the writer backend
func (r *Ring) Put(ctx context.Context, path Path, s Secret) error {
	if r.writer == nil {
		return ErrReadOnly
	}
	err := r.writer.Put(ctx, r.profiles[0], path, s)
	_ = r.audit.Record(ctx, AuditEvent{
		At: time.Now(), Type: EventPut, Profile: r.profiles[0],
		Path: path.String(), Backend: r.writer.Name(), Actor: r.actor,
		Success: err == nil, Err: errString(err),
	})
	return err
}

// Delete removes a secret using the writer backend
func (r *Ring) Delete(ctx context.Context, path Path) error {
	if r.writer == nil {
		return ErrReadOnly
	}
	err := r.writer.Delete(ctx, r.profiles[0], path)
	_ = r.audit.Record(ctx, AuditEvent{
		At: time.Now(), Type: EventDeleteSource, Profile: r.profiles[0],
		Path: path.String(), Backend: r.writer.Name(), Actor: r.actor,
		Success: err == nil, Err: errString(err),
	})
	return err
}

// List returns children of the given prefix
func (r *Ring) List(ctx context.Context, prefix Path) ([]Path, error) {
	seen := map[string]struct{}{}
	var out []Path

	for _, prof := range r.profiles {
		var foundAny bool
		for _, b := range r.readers {
			children, err := b.List(ctx, prof, prefix)
			if err != nil {
				if IsNotFound(err) {
					continue
				}
				return nil, err
			}
			if len(children) > 0 {
				foundAny = true
			}
			for _, c := range children {
				k := c.String()
				if _, ok := seen[k]; ok {
					continue
				}
				seen[k] = struct{}{}
				out = append(out, c)
			}
		}
		if foundAny {
			return out, nil
		}
	}
	return nil, ErrNotFound
}

func cloneAndSet(m map[string]string, kv ...string) map[string]string {
	cp := map[string]string{}
	for k, v := range m {
		cp[k] = v
	}
	for i := 0; i+1 < len(kv); i += 2 {
		cp[kv[i]] = kv[i+1]
	}
	return cp
}

func errString(err error) string {
	if err == nil {
		return ""
	}
	return err.Error()
}



// GetReaders returns the reader backends
func (r *Ring) GetReaders() []Backend {
	return r.readers
}

// GetWriter returns the writer backend
func (r *Ring) GetWriter() Backend {
	return r.writer
}

// GetStateStore returns the state store
func (r *Ring) GetStateStore() StateStore {
	return r.state
}

// GetAuditSink returns the audit sink
func (r *Ring) GetAuditSink() AuditSink {
	return r.audit
}

// GetActor returns the actor name
func (r *Ring) GetActor() string {
	return r.actor
}

// GetProfiles returns the profile search order
func (r *Ring) GetProfiles() []string {
	return r.profiles
}

