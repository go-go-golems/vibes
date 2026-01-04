package database

import (
	"database/sql"
	"fmt"
	"time"

	"github.com/codereview/cli/internal/models"
)

// CreateAnnotation creates a new annotation in the database
func (db *DB) CreateAnnotation(annotation *models.Annotation) error {
	query := `
		INSERT INTO annotations (review_id, file, line, line_start, line_end, type, severity, message, suggestion, status, created, updated)
		VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
	`

	now := time.Now()
	annotation.Created = now
	annotation.Updated = now

	result, err := db.Exec(query,
		annotation.ReviewID, annotation.File, annotation.Line, annotation.LineStart, annotation.LineEnd,
		annotation.Type, annotation.Severity, annotation.Message, annotation.Suggestion,
		annotation.Status, annotation.Created, annotation.Updated,
	)

	if err != nil {
		return fmt.Errorf("failed to create annotation: %w", err)
	}

	id, err := result.LastInsertId()
	if err != nil {
		return fmt.Errorf("failed to get annotation ID: %w", err)
	}

	annotation.ID = int(id)
	return nil
}

// GetAnnotation retrieves an annotation by ID
func (db *DB) GetAnnotation(id int) (*models.Annotation, error) {
	query := `
		SELECT id, review_id, file, line, line_start, line_end, type, severity, message, suggestion, status, created, updated
		FROM annotations
		WHERE id = ?
	`

	var annotation models.Annotation
	err := db.QueryRow(query, id).Scan(
		&annotation.ID, &annotation.ReviewID, &annotation.File, &annotation.Line,
		&annotation.LineStart, &annotation.LineEnd, &annotation.Type, &annotation.Severity,
		&annotation.Message, &annotation.Suggestion, &annotation.Status,
		&annotation.Created, &annotation.Updated,
	)

	if err != nil {
		if err == sql.ErrNoRows {
			return nil, fmt.Errorf("annotation not found: %d", id)
		}
		return nil, fmt.Errorf("failed to get annotation: %w", err)
	}

	// Load threads
	threads, err := db.GetThreadsForAnnotation(annotation.ID)
	if err != nil {
		return nil, fmt.Errorf("failed to load threads: %w", err)
	}
	annotation.Threads = threads

	return &annotation, nil
}

// GetAnnotationsForReview retrieves all annotations for a review
func (db *DB) GetAnnotationsForReview(reviewID string) ([]*models.Annotation, error) {
	query := `
		SELECT id, review_id, file, line, line_start, line_end, type, severity, message, suggestion, status, created, updated
		FROM annotations
		WHERE review_id = ?
		ORDER BY file, COALESCE(line, line_start, 0)
	`

	rows, err := db.Query(query, reviewID)
	if err != nil {
		return nil, fmt.Errorf("failed to get annotations: %w", err)
	}
	defer rows.Close()

	var annotations []*models.Annotation
	for rows.Next() {
		var annotation models.Annotation
		err := rows.Scan(
			&annotation.ID, &annotation.ReviewID, &annotation.File, &annotation.Line,
			&annotation.LineStart, &annotation.LineEnd, &annotation.Type, &annotation.Severity,
			&annotation.Message, &annotation.Suggestion, &annotation.Status,
			&annotation.Created, &annotation.Updated,
		)
		if err != nil {
			return nil, fmt.Errorf("failed to scan annotation: %w", err)
		}

		// Load threads for each annotation
		threads, err := db.GetThreadsForAnnotation(annotation.ID)
		if err != nil {
			return nil, fmt.Errorf("failed to load threads for annotation %d: %w", annotation.ID, err)
		}
		annotation.Threads = threads

		annotations = append(annotations, &annotation)
	}

	return annotations, nil
}

// UpdateAnnotation updates an existing annotation
func (db *DB) UpdateAnnotation(annotation *models.Annotation) error {
	query := `
		UPDATE annotations
		SET file = ?, line = ?, line_start = ?, line_end = ?, type = ?, severity = ?, message = ?, suggestion = ?, status = ?, updated = ?
		WHERE id = ?
	`

	annotation.Updated = time.Now()

	result, err := db.Exec(query,
		annotation.File, annotation.Line, annotation.LineStart, annotation.LineEnd,
		annotation.Type, annotation.Severity, annotation.Message, annotation.Suggestion,
		annotation.Status, annotation.Updated, annotation.ID,
	)

	if err != nil {
		return fmt.Errorf("failed to update annotation: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("failed to get rows affected: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("annotation not found: %d", annotation.ID)
	}

	return nil
}

// DeleteAnnotation deletes an annotation and all associated threads
func (db *DB) DeleteAnnotation(id int) error {
	query := "DELETE FROM annotations WHERE id = ?"

	result, err := db.Exec(query, id)
	if err != nil {
		return fmt.Errorf("failed to delete annotation: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("failed to get rows affected: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("annotation not found: %d", id)
	}

	return nil
}

// GetThreadsForAnnotation retrieves all threads for an annotation
func (db *DB) GetThreadsForAnnotation(annotationID int) ([]models.Thread, error) {
	query := `
		SELECT id, annotation_id, author, message, timestamp
		FROM threads
		WHERE annotation_id = ?
		ORDER BY timestamp
	`

	rows, err := db.Query(query, annotationID)
	if err != nil {
		return nil, fmt.Errorf("failed to get threads: %w", err)
	}
	defer rows.Close()

	var threads []models.Thread
	for rows.Next() {
		var thread models.Thread
		err := rows.Scan(
			&thread.ID, &thread.AnnotationID, &thread.Author,
			&thread.Message, &thread.Timestamp,
		)
		if err != nil {
			return nil, fmt.Errorf("failed to scan thread: %w", err)
		}

		threads = append(threads, thread)
	}

	return threads, nil
}

// CreateThread creates a new thread message for an annotation
func (db *DB) CreateThread(thread *models.Thread) error {
	query := `
		INSERT INTO threads (annotation_id, author, message, timestamp)
		VALUES (?, ?, ?, ?)
	`

	thread.Timestamp = time.Now()

	result, err := db.Exec(query,
		thread.AnnotationID, thread.Author, thread.Message, thread.Timestamp,
	)

	if err != nil {
		return fmt.Errorf("failed to create thread: %w", err)
	}

	id, err := result.LastInsertId()
	if err != nil {
		return fmt.Errorf("failed to get thread ID: %w", err)
	}

	thread.ID = int(id)
	return nil
}
