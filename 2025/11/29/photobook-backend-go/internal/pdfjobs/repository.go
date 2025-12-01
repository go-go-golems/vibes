package pdfjobs

import (
	"context"
	"database/sql"
	"encoding/json"
	"fmt"
	"time"

	"photobook-backend-go/pkg/types"
)

// Repository handles PDF job data access
type Repository interface {
	Create(ctx context.Context, userID int64, photoIDs []int64) (int64, error)
	GetByID(ctx context.Context, id int64) (*types.PdfJob, error)
	GetByUserID(ctx context.Context, userID int64) ([]*types.PdfJob, error)
	ClaimPendingJobs(ctx context.Context, limit int) ([]*types.PdfJob, error)
	MarkCompleted(ctx context.Context, id int64, fileKey, url string) error
	MarkFailed(ctx context.Context, id int64, errorMsg string) error
}

// SQLiteRepository implements Repository for SQLite
type SQLiteRepository struct {
	db *sql.DB
}

// NewSQLiteRepository creates a new SQLite PDF job repository
func NewSQLiteRepository(db *sql.DB) *SQLiteRepository {
	return &SQLiteRepository{db: db}
}

// Create creates a new PDF job
func (r *SQLiteRepository) Create(ctx context.Context, userID int64, photoIDs []int64) (int64, error) {
	photoIDsJSON, err := json.Marshal(photoIDs)
	if err != nil {
		return 0, fmt.Errorf("failed to marshal photo IDs: %w", err)
	}

	query := `
		INSERT INTO pdf_jobs (user_id, status, photo_ids, created_at, updated_at)
		VALUES (?, ?, ?, ?, ?)
	`

	now := time.Now()
	result, err := r.db.ExecContext(ctx, query,
		userID, "pending", string(photoIDsJSON), now, now,
	)
	if err != nil {
		return 0, fmt.Errorf("failed to create PDF job: %w", err)
	}

	id, err := result.LastInsertId()
	if err != nil {
		return 0, fmt.Errorf("failed to get last insert id: %w", err)
	}

	return id, nil
}

// GetByID retrieves a PDF job by ID
func (r *SQLiteRepository) GetByID(ctx context.Context, id int64) (*types.PdfJob, error) {
	query := `
		SELECT id, user_id, status, photo_ids, file_key, url, error, created_at, updated_at
		FROM pdf_jobs
		WHERE id = ?
	`

	var job types.PdfJob
	var photoIDsJSON sql.NullString
	var fileKey sql.NullString
	var url sql.NullString
	var errorMsg sql.NullString

	err := r.db.QueryRowContext(ctx, query, id).Scan(
		&job.ID, &job.UserID, &job.Status, &photoIDsJSON,
		&fileKey, &url, &errorMsg, &job.CreatedAt, &job.UpdatedAt,
	)
	if err == sql.ErrNoRows {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("failed to get PDF job by id: %w", err)
	}

	// Parse photo IDs
	if photoIDsJSON.Valid {
		if err := json.Unmarshal([]byte(photoIDsJSON.String), &job.PhotoIDs); err != nil {
			return nil, fmt.Errorf("failed to unmarshal photo IDs: %w", err)
		}
	}

	if fileKey.Valid {
		job.FileKey = fileKey.String
	}
	if url.Valid {
		job.URL = url.String
	}
	if errorMsg.Valid {
		job.Error = errorMsg.String
	}

	return &job, nil
}

// GetByUserID retrieves all PDF jobs for a user
func (r *SQLiteRepository) GetByUserID(ctx context.Context, userID int64) ([]*types.PdfJob, error) {
	query := `
		SELECT id, user_id, status, photo_ids, file_key, url, error, created_at, updated_at
		FROM pdf_jobs
		WHERE user_id = ?
		ORDER BY created_at DESC
	`

	rows, err := r.db.QueryContext(ctx, query, userID)
	if err != nil {
		return nil, fmt.Errorf("failed to query PDF jobs: %w", err)
	}
	defer rows.Close()

	var jobs []*types.PdfJob
	for rows.Next() {
		var job types.PdfJob
		var photoIDsJSON sql.NullString
		var fileKey sql.NullString
		var url sql.NullString
		var errorMsg sql.NullString

		if err := rows.Scan(
			&job.ID, &job.UserID, &job.Status, &photoIDsJSON,
			&fileKey, &url, &errorMsg, &job.CreatedAt, &job.UpdatedAt,
		); err != nil {
			return nil, fmt.Errorf("failed to scan PDF job: %w", err)
		}

		// Parse photo IDs
		if photoIDsJSON.Valid {
			if err := json.Unmarshal([]byte(photoIDsJSON.String), &job.PhotoIDs); err != nil {
				return nil, fmt.Errorf("failed to unmarshal photo IDs: %w", err)
			}
		}

		if fileKey.Valid {
			job.FileKey = fileKey.String
		}
		if url.Valid {
			job.URL = url.String
		}
		if errorMsg.Valid {
			job.Error = errorMsg.String
		}

		jobs = append(jobs, &job)
	}

	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("error iterating PDF jobs: %w", err)
	}

	return jobs, nil
}

// ClaimPendingJobs atomically claims up to limit pending jobs
// This uses an atomic UPDATE to prevent race conditions
func (r *SQLiteRepository) ClaimPendingJobs(ctx context.Context, limit int) ([]*types.PdfJob, error) {
	// SQLite doesn't support UPDATE ... RETURNING, so we need to:
	// 1. Get IDs of pending jobs (with limit)
	// 2. Update those jobs atomically
	// 3. Return the updated jobs

	tx, err := r.db.BeginTx(ctx, nil)
	if err != nil {
		return nil, fmt.Errorf("failed to begin transaction: %w", err)
	}
	defer tx.Rollback()

	// Step 1: Get IDs of pending jobs
	selectQuery := `
		SELECT id FROM pdf_jobs
		WHERE status = 'pending'
		ORDER BY created_at ASC
		LIMIT ?
	`
	rows, err := tx.QueryContext(ctx, selectQuery, limit)
	if err != nil {
		return nil, fmt.Errorf("failed to select pending jobs: %w", err)
	}

	var ids []int64
	for rows.Next() {
		var id int64
		if err := rows.Scan(&id); err != nil {
			rows.Close()
			return nil, fmt.Errorf("failed to scan job id: %w", err)
		}
		ids = append(ids, id)
	}
	rows.Close()

	if len(ids) == 0 {
		return []*types.PdfJob{}, nil
	}

	// Step 2: Update jobs atomically
	// Build placeholders for IN clause
	placeholders := ""
	args := []interface{}{time.Now()}
	for i, id := range ids {
		if i > 0 {
			placeholders += ","
		}
		placeholders += "?"
		args = append(args, id)
	}

	updateQuery := fmt.Sprintf(`
		UPDATE pdf_jobs
		SET status = 'processing', updated_at = ?
		WHERE id IN (%s) AND status = 'pending'
	`, placeholders)

	result, err := tx.ExecContext(ctx, updateQuery, args...)
	if err != nil {
		return nil, fmt.Errorf("failed to update jobs: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return nil, fmt.Errorf("failed to get rows affected: %w", err)
	}

	// Only return jobs that were actually updated (to handle race conditions)
	if rowsAffected == 0 {
		return []*types.PdfJob{}, nil
	}

	// Step 3: Fetch the updated jobs
	claimedIDs := ids[:rowsAffected]
	claimedPlaceholders := ""
	claimedArgs := []interface{}{}
	for i, id := range claimedIDs {
		if i > 0 {
			claimedPlaceholders += ","
		}
		claimedPlaceholders += "?"
		claimedArgs = append(claimedArgs, id)
	}

	fetchQuery := fmt.Sprintf(`
		SELECT id, user_id, status, photo_ids, file_key, url, error, created_at, updated_at
		FROM pdf_jobs
		WHERE id IN (%s)
	`, claimedPlaceholders)

	rows, err = tx.QueryContext(ctx, fetchQuery, claimedArgs...)
	if err != nil {
		return nil, fmt.Errorf("failed to fetch claimed jobs: %w", err)
	}
	defer rows.Close()

	var jobs []*types.PdfJob
	for rows.Next() {
		var job types.PdfJob
		var photoIDsJSON sql.NullString
		var fileKey sql.NullString
		var url sql.NullString
		var errorMsg sql.NullString

		if err := rows.Scan(
			&job.ID, &job.UserID, &job.Status, &photoIDsJSON,
			&fileKey, &url, &errorMsg, &job.CreatedAt, &job.UpdatedAt,
		); err != nil {
			return nil, fmt.Errorf("failed to scan PDF job: %w", err)
		}

		// Parse photo IDs
		if photoIDsJSON.Valid {
			if err := json.Unmarshal([]byte(photoIDsJSON.String), &job.PhotoIDs); err != nil {
				return nil, fmt.Errorf("failed to unmarshal photo IDs: %w", err)
			}
		}

		if fileKey.Valid {
			job.FileKey = fileKey.String
		}
		if url.Valid {
			job.URL = url.String
		}
		if errorMsg.Valid {
			job.Error = errorMsg.String
		}

		jobs = append(jobs, &job)
	}

	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("error iterating claimed jobs: %w", err)
	}

	if err := tx.Commit(); err != nil {
		return nil, fmt.Errorf("failed to commit transaction: %w", err)
	}

	return jobs, nil
}

// MarkCompleted marks a job as completed with the result file key and URL
func (r *SQLiteRepository) MarkCompleted(ctx context.Context, id int64, fileKey, url string) error {
	query := `
		UPDATE pdf_jobs
		SET status = 'completed', file_key = ?, url = ?, updated_at = ?
		WHERE id = ?
	`

	_, err := r.db.ExecContext(ctx, query, fileKey, url, time.Now(), id)
	if err != nil {
		return fmt.Errorf("failed to mark job as completed: %w", err)
	}

	return nil
}

// MarkFailed marks a job as failed with an error message
func (r *SQLiteRepository) MarkFailed(ctx context.Context, id int64, errorMsg string) error {
	query := `
		UPDATE pdf_jobs
		SET status = 'failed', error = ?, updated_at = ?
		WHERE id = ?
	`

	_, err := r.db.ExecContext(ctx, query, errorMsg, time.Now(), id)
	if err != nil {
		return fmt.Errorf("failed to mark job as failed: %w", err)
	}

	return nil
}


