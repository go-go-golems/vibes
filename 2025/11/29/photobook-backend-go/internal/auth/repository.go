package auth

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"photobook-backend-go/pkg/types"
)

// UserRepository handles user data access
type UserRepository interface {
	Create(ctx context.Context, user *types.User, passwordHash *string) (int64, error)
	GetByOpenID(ctx context.Context, openID string) (*types.User, error)
	GetByEmail(ctx context.Context, email string) (*types.User, error)
	GetByID(ctx context.Context, id int64) (*types.User, error)
	GetPasswordHash(ctx context.Context, email string) (*string, error)
	Update(ctx context.Context, user *types.User) error
	UpdateLastSignedIn(ctx context.Context, openID string) error
}

// SQLiteUserRepository implements UserRepository for SQLite
type SQLiteUserRepository struct {
	db *sql.DB
}

// NewSQLiteUserRepository creates a new SQLite user repository
func NewSQLiteUserRepository(db *sql.DB) *SQLiteUserRepository {
	return &SQLiteUserRepository{db: db}
}

// Create creates a new user
func (r *SQLiteUserRepository) Create(ctx context.Context, user *types.User, passwordHash *string) (int64, error) {
	query := `
		INSERT INTO users (open_id, name, email, login_method, role, password_hash, created_at, updated_at, last_signed_in)
		VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
	`
	
	now := time.Now()
	result, err := r.db.ExecContext(ctx, query,
		user.OpenID, user.Name, user.Email, user.LoginMethod, user.Role, passwordHash,
		now, now, now,
	)
	if err != nil {
		return 0, fmt.Errorf("failed to create user: %w", err)
	}
	
	id, err := result.LastInsertId()
	if err != nil {
		return 0, fmt.Errorf("failed to get last insert id: %w", err)
	}
	
	return id, nil
}

// GetPasswordHash retrieves the password hash for a user
func (r *SQLiteUserRepository) GetPasswordHash(ctx context.Context, email string) (*string, error) {
	query := `SELECT password_hash FROM users WHERE email = ?`
	
	var hash sql.NullString
	err := r.db.QueryRowContext(ctx, query, email).Scan(&hash)
	if err == sql.ErrNoRows {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("failed to get password hash: %w", err)
	}
	
	if !hash.Valid {
		return nil, nil
	}
	
	result := hash.String
	return &result, nil
}

// GetByOpenID retrieves a user by open ID
func (r *SQLiteUserRepository) GetByOpenID(ctx context.Context, openID string) (*types.User, error) {
	query := `
		SELECT id, open_id, name, email, login_method, role, created_at, updated_at, last_signed_in
		FROM users
		WHERE open_id = ?
	`
	
	user := &types.User{}
	err := r.db.QueryRowContext(ctx, query, openID).Scan(
		&user.ID, &user.OpenID, &user.Name, &user.Email, &user.LoginMethod,
		&user.Role, &user.CreatedAt, &user.UpdatedAt, &user.LastSignedIn,
	)
	if err == sql.ErrNoRows {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("failed to get user by open_id: %w", err)
	}
	
	return user, nil
}

// GetByEmail retrieves a user by email
func (r *SQLiteUserRepository) GetByEmail(ctx context.Context, email string) (*types.User, error) {
	query := `
		SELECT id, open_id, name, email, login_method, role, created_at, updated_at, last_signed_in
		FROM users
		WHERE email = ?
	`
	
	user := &types.User{}
	err := r.db.QueryRowContext(ctx, query, email).Scan(
		&user.ID, &user.OpenID, &user.Name, &user.Email, &user.LoginMethod,
		&user.Role, &user.CreatedAt, &user.UpdatedAt, &user.LastSignedIn,
	)
	if err == sql.ErrNoRows {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("failed to get user by email: %w", err)
	}
	
	return user, nil
}

// GetByID retrieves a user by ID
func (r *SQLiteUserRepository) GetByID(ctx context.Context, id int64) (*types.User, error) {
	query := `
		SELECT id, open_id, name, email, login_method, role, created_at, updated_at, last_signed_in
		FROM users
		WHERE id = ?
	`
	
	user := &types.User{}
	err := r.db.QueryRowContext(ctx, query, id).Scan(
		&user.ID, &user.OpenID, &user.Name, &user.Email, &user.LoginMethod,
		&user.Role, &user.CreatedAt, &user.UpdatedAt, &user.LastSignedIn,
	)
	if err == sql.ErrNoRows {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("failed to get user by id: %w", err)
	}
	
	return user, nil
}

// Update updates a user
func (r *SQLiteUserRepository) Update(ctx context.Context, user *types.User) error {
	query := `
		UPDATE users
		SET name = ?, email = ?, login_method = ?, role = ?, updated_at = ?
		WHERE id = ?
	`
	
	_, err := r.db.ExecContext(ctx, query,
		user.Name, user.Email, user.LoginMethod, user.Role, time.Now(), user.ID,
	)
	if err != nil {
		return fmt.Errorf("failed to update user: %w", err)
	}
	
	return nil
}

// UpdateLastSignedIn updates the last signed in timestamp
func (r *SQLiteUserRepository) UpdateLastSignedIn(ctx context.Context, openID string) error {
	query := `UPDATE users SET last_signed_in = ? WHERE open_id = ?`
	_, err := r.db.ExecContext(ctx, query, time.Now(), openID)
	if err != nil {
		return fmt.Errorf("failed to update last_signed_in: %w", err)
	}
	return nil
}

