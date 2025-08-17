package keyring

import "time"

// Secret represents a stored secret with metadata and expiration
type Secret struct {
	// Value is the raw secret text (API tokens, passwords, etc.)
	Value string

	// Metadata is optional, used for hints like "source=vault", "version=42", etc.
	Metadata map[string]string

	// ExpiresAt supports dynamic/leased secrets (e.g., Vault, STS). Zero means "no expiry"
	ExpiresAt time.Time
}

// ExpiredAt returns true if the secret has expired at the given time
func (s Secret) ExpiredAt(t time.Time) bool {
	return !s.ExpiresAt.IsZero() && !t.Before(s.ExpiresAt)
}

// IsExpired returns true if the secret has expired now
func (s Secret) IsExpired() bool {
	return s.ExpiredAt(time.Now())
}

// Clone creates a deep copy of the secret
func (s Secret) Clone() Secret {
	metadata := make(map[string]string)
	for k, v := range s.Metadata {
		metadata[k] = v
	}
	return Secret{
		Value:     s.Value,
		Metadata:  metadata,
		ExpiresAt: s.ExpiresAt,
	}
}

