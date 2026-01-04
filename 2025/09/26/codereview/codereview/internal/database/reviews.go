package database

import (
	"database/sql"
	"fmt"
	"time"

	"github.com/codereview/cli/internal/models"
)

// CreateReview creates a new review in the database
func (db *DB) CreateReview(review *models.Review) error {
	if err := review.BeforeSave(); err != nil {
		return fmt.Errorf("failed to prepare review for save: %w", err)
	}

	query := `
		INSERT INTO reviews (id, title, branch, "commit", base_commit, reviewer, created, updated, status, files_changed, lines_added, lines_removed, tags)
		VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
	`

	now := time.Now()
	review.Created = now
	review.Updated = now

	_, err := db.Exec(query,
		review.ID, review.Title, review.Branch, review.Commit, review.BaseCommit,
		review.Reviewer, review.Created, review.Updated, review.Status,
		review.FilesChanged, review.LinesAdded, review.LinesRemoved, review.TagsJSON,
	)

	if err != nil {
		return fmt.Errorf("failed to create review: %w", err)
	}

	return nil
}

// GetReview retrieves a review by ID
func (db *DB) GetReview(id string) (*models.Review, error) {
	query := `
		SELECT id, title, branch, "commit", base_commit, reviewer, created, updated, status, files_changed, lines_added, lines_removed, tags
		FROM reviews
		WHERE id = ?
	`

	var review models.Review
	err := db.QueryRow(query, id).Scan(
		&review.ID, &review.Title, &review.Branch, &review.Commit, &review.BaseCommit,
		&review.Reviewer, &review.Created, &review.Updated, &review.Status,
		&review.FilesChanged, &review.LinesAdded, &review.LinesRemoved, &review.TagsJSON,
	)

	if err != nil {
		if err == sql.ErrNoRows {
			return nil, fmt.Errorf("review not found: %s", id)
		}
		return nil, fmt.Errorf("failed to get review: %w", err)
	}

	if err := review.AfterLoad(); err != nil {
		return nil, fmt.Errorf("failed to process review after load: %w", err)
	}

	return &review, nil
}

// ListReviews retrieves all reviews with optional filtering
func (db *DB) ListReviews(status string) ([]*models.Review, error) {
	query := `
		SELECT id, title, branch, "commit", base_commit, reviewer, created, updated, status, files_changed, lines_added, lines_removed, tags
		FROM reviews
	`
	args := []interface{}{}

	if status != "" {
		query += " WHERE status = ?"
		args = append(args, status)
	}

	query += " ORDER BY created DESC"

	rows, err := db.Query(query, args...)
	if err != nil {
		return nil, fmt.Errorf("failed to list reviews: %w", err)
	}
	defer rows.Close()

	var reviews []*models.Review
	for rows.Next() {
		var review models.Review
		err := rows.Scan(
			&review.ID, &review.Title, &review.Branch, &review.Commit, &review.BaseCommit,
			&review.Reviewer, &review.Created, &review.Updated, &review.Status,
			&review.FilesChanged, &review.LinesAdded, &review.LinesRemoved, &review.TagsJSON,
		)
		if err != nil {
			return nil, fmt.Errorf("failed to scan review: %w", err)
		}

		if err := review.AfterLoad(); err != nil {
			return nil, fmt.Errorf("failed to process review after load: %w", err)
		}

		reviews = append(reviews, &review)
	}

	return reviews, nil
}

// UpdateReview updates an existing review
func (db *DB) UpdateReview(review *models.Review) error {
	if err := review.BeforeSave(); err != nil {
		return fmt.Errorf("failed to prepare review for save: %w", err)
	}

	query := `
		UPDATE reviews
		SET title = ?, branch = ?, "commit" = ?, base_commit = ?, reviewer = ?, updated = ?, status = ?, files_changed = ?, lines_added = ?, lines_removed = ?, tags = ?
		WHERE id = ?
	`

	review.Updated = time.Now()

	result, err := db.Exec(query,
		review.Title, review.Branch, review.Commit, review.BaseCommit,
		review.Reviewer, review.Updated, review.Status,
		review.FilesChanged, review.LinesAdded, review.LinesRemoved, review.TagsJSON,
		review.ID,
	)

	if err != nil {
		return fmt.Errorf("failed to update review: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("failed to get rows affected: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("review not found: %s", review.ID)
	}

	return nil
}

// DeleteReview deletes a review and all associated annotations
func (db *DB) DeleteReview(id string) error {
	query := "DELETE FROM reviews WHERE id = ?"

	result, err := db.Exec(query, id)
	if err != nil {
		return fmt.Errorf("failed to delete review: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("failed to get rows affected: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("review not found: %s", id)
	}

	return nil
}

// GetReviewSummary gets summary statistics for a review
func (db *DB) GetReviewSummary(reviewID string) (*models.ReviewSummary, error) {
	query := `
		SELECT 
			COUNT(*) as total_annotations,
			COALESCE(SUM(CASE WHEN type = 'issue' THEN 1 ELSE 0 END), 0) as issues_count,
			COALESCE(SUM(CASE WHEN type = 'suggestion' THEN 1 ELSE 0 END), 0) as suggestions_count,
			COALESCE(SUM(CASE WHEN type = 'praise' THEN 1 ELSE 0 END), 0) as praise_count,
			COALESCE(SUM(CASE WHEN type = 'question' THEN 1 ELSE 0 END), 0) as questions_count,
			COALESCE(SUM(CASE WHEN severity = 'critical' THEN 1 ELSE 0 END), 0) as critical_count,
			COALESCE(SUM(CASE WHEN severity = 'major' THEN 1 ELSE 0 END), 0) as major_count,
			COALESCE(SUM(CASE WHEN severity = 'minor' THEN 1 ELSE 0 END), 0) as minor_count,
			COALESCE(SUM(CASE WHEN status = 'open' THEN 1 ELSE 0 END), 0) as open_count,
			COALESCE(SUM(CASE WHEN status = 'resolved' THEN 1 ELSE 0 END), 0) as resolved_count
		FROM annotations
		WHERE review_id = ?
	`

	summary := &models.ReviewSummary{ReviewID: reviewID}
	err := db.QueryRow(query, reviewID).Scan(
		&summary.TotalAnnotations, &summary.IssuesCount, &summary.SuggestionsCount,
		&summary.PraiseCount, &summary.QuestionsCount, &summary.CriticalCount,
		&summary.MajorCount, &summary.MinorCount, &summary.OpenCount, &summary.ResolvedCount,
	)

	if err != nil {
		return nil, fmt.Errorf("failed to get review summary: %w", err)
	}

	return summary, nil
}
