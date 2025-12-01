package photos

import (
	"context"
	"database/sql"
	"fmt"

	"photobook-backend-go/pkg/types"
)

// Repository handles photo data access
type Repository interface {
	GetByUserID(ctx context.Context, userID int64) ([]*types.Photo, error)
	GetByIDs(ctx context.Context, userID int64, ids []int64) ([]*types.Photo, error)
}

// SQLiteRepository implements Repository for SQLite
type SQLiteRepository struct {
	db *sql.DB
}

// NewSQLiteRepository creates a new SQLite photo repository
func NewSQLiteRepository(db *sql.DB) *SQLiteRepository {
	return &SQLiteRepository{db: db}
}

// GetByUserID retrieves all photos for a user
func (r *SQLiteRepository) GetByUserID(ctx context.Context, userID int64) ([]*types.Photo, error) {
	query := `
		SELECT id, user_id, file_key, url, filename, position, created_at, updated_at
		FROM photos
		WHERE user_id = ?
		ORDER BY position ASC
	`

	rows, err := r.db.QueryContext(ctx, query, userID)
	if err != nil {
		return nil, fmt.Errorf("failed to query photos: %w", err)
	}
	defer rows.Close()

	var photos []*types.Photo
	for rows.Next() {
		var photo types.Photo
		if err := rows.Scan(
			&photo.ID, &photo.UserID, &photo.FileKey, &photo.URL,
			&photo.Filename, &photo.Position, &photo.CreatedAt, &photo.UpdatedAt,
		); err != nil {
			return nil, fmt.Errorf("failed to scan photo: %w", err)
		}
		photos = append(photos, &photo)
	}

	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("error iterating photos: %w", err)
	}

	return photos, nil
}

// GetByIDs retrieves photos by their IDs for a specific user, maintaining the order of IDs
func (r *SQLiteRepository) GetByIDs(ctx context.Context, userID int64, ids []int64) ([]*types.Photo, error) {
	if len(ids) == 0 {
		return []*types.Photo{}, nil
	}

	// Build placeholders for IN clause
	placeholders := ""
	args := []interface{}{userID}
	for i, id := range ids {
		if i > 0 {
			placeholders += ","
		}
		placeholders += "?"
		args = append(args, id)
	}

	query := fmt.Sprintf(`
		SELECT id, user_id, file_key, url, filename, position, created_at, updated_at
		FROM photos
		WHERE user_id = ? AND id IN (%s)
	`, placeholders)

	rows, err := r.db.QueryContext(ctx, query, args...)
	if err != nil {
		return nil, fmt.Errorf("failed to query photos: %w", err)
	}
	defer rows.Close()

	// Create a map for quick lookup
	photoMap := make(map[int64]*types.Photo)
	for rows.Next() {
		var photo types.Photo
		if err := rows.Scan(
			&photo.ID, &photo.UserID, &photo.FileKey, &photo.URL,
			&photo.Filename, &photo.Position, &photo.CreatedAt, &photo.UpdatedAt,
		); err != nil {
			return nil, fmt.Errorf("failed to scan photo: %w", err)
		}
		photoMap[photo.ID] = &photo
	}

	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("error iterating photos: %w", err)
	}

	// Return photos in the order of requested IDs
	var photos []*types.Photo
	for _, id := range ids {
		if photo, ok := photoMap[id]; ok {
			photos = append(photos, photo)
		}
	}

	return photos, nil
}


