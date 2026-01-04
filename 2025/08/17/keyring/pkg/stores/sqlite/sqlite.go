package sqlite

import (
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"strings"
	"time"

	"keyring/pkg/keyring"

	_ "github.com/mattn/go-sqlite3"
)

// Store implements Backend, StateStore, and AuditSink using SQLite
type Store struct {
	DB   *sql.DB
	name string
}

// New creates a new SQLite store
func New(db *sql.DB) *Store {
	return &Store{DB: db, name: "sqlite"}
}

// NewFromPath creates a new SQLite store from a database path
func NewFromPath(path string) (*Store, error) {
	db, err := sql.Open("sqlite3", path+"?_foreign_keys=on")
	if err != nil {
		return nil, err
	}
	return New(db), nil
}

// EnsureSchema creates tables if they don't exist
func (s *Store) EnsureSchema(ctx context.Context) error {
	ddl := `
CREATE TABLE IF NOT EXISTS secrets (
  profile TEXT NOT NULL,
  path    TEXT NOT NULL,
  value   TEXT NOT NULL,
  metadata TEXT,           -- JSON
  expires_at TEXT,         -- RFC3339 format, empty if no expiry
  created_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  PRIMARY KEY (profile, path)
);

CREATE TABLE IF NOT EXISTS key_states (
  profile TEXT NOT NULL,
  path    TEXT NOT NULL,
  status  INTEGER NOT NULL, -- 0 active, 1 deprecated, 2 invalidated
  since   TEXT NOT NULL,
  message TEXT,
  replace_with TEXT,
  reason  TEXT,
  delete_at_source INTEGER NOT NULL DEFAULT 0,
  extra   TEXT,             -- JSON
  updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  PRIMARY KEY (profile, path)
);

CREATE TABLE IF NOT EXISTS audit_events (
  id      INTEGER PRIMARY KEY AUTOINCREMENT,
  at      TEXT NOT NULL,
  type    TEXT NOT NULL,
  profile TEXT,
  path    TEXT,
  backend TEXT,
  actor   TEXT,
  success INTEGER NOT NULL,
  err     TEXT,
  meta    TEXT         -- JSON
);

CREATE INDEX IF NOT EXISTS idx_audit_profile_path_at ON audit_events(profile, path, at);
CREATE INDEX IF NOT EXISTS idx_audit_type_at ON audit_events(type, at);
`
	_, err := s.DB.ExecContext(ctx, ddl)
	return err
}

// Close closes the database connection
func (s *Store) Close() error {
	return s.DB.Close()
}

// ---- Backend implementation ----

// Name returns the backend name
func (s *Store) Name() string {
	return s.name
}

// Get retrieves a secret
func (s *Store) Get(ctx context.Context, profile string, path keyring.Path) (keyring.Secret, error) {
	row := s.DB.QueryRowContext(ctx, `
		SELECT value, metadata, expires_at 
		FROM secrets 
		WHERE profile=? AND path=?`,
		profile, path.String())

	var value, metadataStr, expiresAtStr string
	if err := row.Scan(&value, &metadataStr, &expiresAtStr); err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return keyring.Secret{}, keyring.ErrNotFound
		}
		return keyring.Secret{}, err
	}

	var metadata map[string]string
	if metadataStr != "" {
		_ = json.Unmarshal([]byte(metadataStr), &metadata)
	}

	var expiresAt time.Time
	if expiresAtStr != "" {
		expiresAt, _ = time.Parse(time.RFC3339, expiresAtStr)
	}

	return keyring.Secret{
		Value:     value,
		Metadata:  metadata,
		ExpiresAt: expiresAt,
	}, nil
}

// Put stores a secret
func (s *Store) Put(ctx context.Context, profile string, path keyring.Path, secret keyring.Secret) error {
	metadataBytes, _ := json.Marshal(secret.Metadata)
	expiresAtStr := ""
	if !secret.ExpiresAt.IsZero() {
		expiresAtStr = secret.ExpiresAt.Format(time.RFC3339)
	}

	_, err := s.DB.ExecContext(ctx, `
		INSERT INTO secrets(profile, path, value, metadata, expires_at, updated_at)
		VALUES(?,?,?,?,?,strftime('%Y-%m-%dT%H:%M:%fZ','now'))
		ON CONFLICT(profile, path) DO UPDATE SET
		  value=excluded.value, metadata=excluded.metadata, expires_at=excluded.expires_at,
		  updated_at=excluded.updated_at`,
		profile, path.String(), secret.Value, string(metadataBytes), expiresAtStr,
	)
	return err
}

// Delete removes a secret
func (s *Store) Delete(ctx context.Context, profile string, path keyring.Path) error {
	result, err := s.DB.ExecContext(ctx, `
		DELETE FROM secrets WHERE profile=? AND path=?`,
		profile, path.String())
	if err != nil {
		return err
	}
	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return err
	}
	if rowsAffected == 0 {
		return keyring.ErrNotFound
	}
	return nil
}

// List returns immediate children of prefix
func (s *Store) List(ctx context.Context, profile string, prefix keyring.Path) ([]keyring.Path, error) {
	prefixStr := prefix.String()
	if prefixStr != "" {
		prefixStr += "/"
	}

	rows, err := s.DB.QueryContext(ctx, `
		SELECT DISTINCT path FROM secrets 
		WHERE profile=? AND path LIKE ?`,
		profile, prefixStr+"%")
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	seen := make(map[string]struct{})
	var paths []keyring.Path

	for rows.Next() {
		var fullPath string
		if err := rows.Scan(&fullPath); err != nil {
			return nil, err
		}

		// Extract the next segment after the prefix
		if prefixStr != "" {
			if !strings.HasPrefix(fullPath, prefixStr) {
				continue
			}
			remainder := strings.TrimPrefix(fullPath, prefixStr)
			segments := strings.Split(remainder, "/")
			if len(segments) > 0 {
				nextSegment := segments[0]
				if _, exists := seen[nextSegment]; !exists {
					seen[nextSegment] = struct{}{}
					paths = append(paths, keyring.P(nextSegment))
				}
			}
		} else {
			segments := strings.Split(fullPath, "/")
			if len(segments) > 0 {
				nextSegment := segments[0]
				if _, exists := seen[nextSegment]; !exists {
					seen[nextSegment] = struct{}{}
					paths = append(paths, keyring.P(nextSegment))
				}
			}
		}
	}

	if len(paths) == 0 {
		return nil, keyring.ErrNotFound
	}
	return paths, nil
}

// ---- StateStore implementation ----

// GetKeyState retrieves key state
func (s *Store) GetKeyState(profile string, path keyring.Path) (keyring.KeyState, error) {
	row := s.DB.QueryRow(`SELECT status, since, message, replace_with, reason, delete_at_source, extra
	                       FROM key_states WHERE profile=? AND path=?`, profile, path.String())
	var (
		status                                int
		sinceStr, msg, repl, reason           string
		delSrc                                int
		extraStr                              sql.NullString
	)
	if err := row.Scan(&status, &sinceStr, &msg, &repl, &reason, &delSrc, &extraStr); err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return keyring.KeyState{}, keyring.ErrNotFound
		}
		return keyring.KeyState{}, err
	}
	since, _ := time.Parse(time.RFC3339Nano, sinceStr)
	var replPtr *keyring.Path
	if repl != "" {
		p := keyring.P(repl)
		replPtr = &p
	}
	var extra map[string]string
	if extraStr.Valid && extraStr.String != "" {
		_ = json.Unmarshal([]byte(extraStr.String), &extra)
	}
	return keyring.KeyState{
		Status:         keyring.KeyStatus(status),
		Since:          since,
		Message:        msg,
		ReplaceWith:    replPtr,
		Reason:         reason,
		DeleteAtSource: delSrc == 1,
		Extra:          extra,
	}, nil
}

// PutKeyState stores key state
func (s *Store) PutKeyState(profile string, path keyring.Path, ks keyring.KeyState) error {
	repl := ""
	if ks.ReplaceWith != nil {
		repl = ks.ReplaceWith.String()
	}
	extraBytes, _ := json.Marshal(ks.Extra)
	_, err := s.DB.Exec(`INSERT INTO key_states(profile, path, status, since, message, replace_with, reason, delete_at_source, extra, updated_at)
	                     VALUES(?,?,?,?,?,?,?,?,?,strftime('%Y-%m-%dT%H:%M:%fZ','now'))
	                     ON CONFLICT(profile, path) DO UPDATE SET
	                       status=excluded.status, since=excluded.since, message=excluded.message,
	                       replace_with=excluded.replace_with, reason=excluded.reason,
	                       delete_at_source=excluded.delete_at_source, extra=excluded.extra,
	                       updated_at=excluded.updated_at`,
		profile, path.String(), int(ks.Status),
		ks.Since.Format(time.RFC3339Nano), ks.Message, repl, ks.Reason, boolToInt(ks.DeleteAtSource), string(extraBytes),
	)
	return err
}

// DeleteKeyState removes key state
func (s *Store) DeleteKeyState(profile string, path keyring.Path) error {
	_, err := s.DB.Exec(`DELETE FROM key_states WHERE profile=? AND path=?`, profile, path.String())
	return err
}

// ---- AuditSink implementation ----

// Record stores an audit event
func (s *Store) Record(ctx context.Context, evt keyring.AuditEvent) error {
	metaBytes, _ := json.Marshal(evt.Meta)
	_, err := s.DB.ExecContext(ctx, `INSERT INTO audit_events(at,type,profile,path,backend,actor,success,err,meta)
	                                 VALUES(?,?,?,?,?,?,?,?,?)`,
		evt.At.Format(time.RFC3339Nano), string(evt.Type), evt.Profile, evt.Path, evt.Backend, evt.Actor,
		boolToInt(evt.Success), evt.Err, string(metaBytes),
	)
	return err
}

// GetAuditEvents retrieves audit events with optional filtering
func (s *Store) GetAuditEvents(ctx context.Context, profile, path string, eventType keyring.EventType, since, until time.Time, limit int) ([]keyring.AuditEvent, error) {
	query := `SELECT at, type, profile, path, backend, actor, success, err, meta FROM audit_events WHERE 1=1`
	args := []interface{}{}

	if profile != "" {
		query += ` AND profile = ?`
		args = append(args, profile)
	}
	if path != "" {
		query += ` AND path = ?`
		args = append(args, path)
	}
	if eventType != "" {
		query += ` AND type = ?`
		args = append(args, string(eventType))
	}
	if !since.IsZero() {
		query += ` AND at >= ?`
		args = append(args, since.Format(time.RFC3339Nano))
	}
	if !until.IsZero() {
		query += ` AND at <= ?`
		args = append(args, until.Format(time.RFC3339Nano))
	}

	query += ` ORDER BY at DESC`
	if limit > 0 {
		query += ` LIMIT ?`
		args = append(args, limit)
	}

	rows, err := s.DB.QueryContext(ctx, query, args...)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var events []keyring.AuditEvent
	for rows.Next() {
		var evt keyring.AuditEvent
		var atStr, typeStr, metaStr string
		var successInt int

		if err := rows.Scan(&atStr, &typeStr, &evt.Profile, &evt.Path, &evt.Backend, &evt.Actor, &successInt, &evt.Err, &metaStr); err != nil {
			return nil, err
		}

		evt.At, _ = time.Parse(time.RFC3339Nano, atStr)
		evt.Type = keyring.EventType(typeStr)
		evt.Success = successInt == 1

		if metaStr != "" {
			_ = json.Unmarshal([]byte(metaStr), &evt.Meta)
		}

		events = append(events, evt)
	}

	return events, nil
}

func boolToInt(b bool) int {
	if b {
		return 1
	}
	return 0
}

