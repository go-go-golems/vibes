package db

import (
	"context"
	"database/sql"
	"fmt"
	"strings"
	"time"

	"github.com/pkg/errors"
	_ "modernc.org/sqlite"
	"pr-analyzer/internal/analysis"
	appfilters "pr-analyzer/pkg/dbfilters"
)

// Store wraps a sql.DB with helpers for schema management and inserts
 type Store struct {
	db *sql.DB
}

// Open opens or creates a sqlite database at path and ensures schema
func Open(ctx context.Context, path string) (*Store, error) {
	dsn := fmt.Sprintf("file:%s?_pragma=busy_timeout(5000)&_pragma=foreign_keys(ON)", path)
	db, err := sql.Open("sqlite", dsn)
	if err != nil {
		return nil, fmt.Errorf("open sqlite: %w", err)
	}
	if err := db.PingContext(ctx); err != nil {
		return nil, fmt.Errorf("ping sqlite: %w", err)
	}
	s := &Store{db: db}
	if err := s.ensureSchema(ctx); err != nil {
		_ = db.Close()
		return nil, err
	}
	return s, nil
}

func (s *Store) Close() error { return s.db.Close() }

func (s *Store) ensureSchema(ctx context.Context) error {
	stmts := []string{
		`CREATE TABLE IF NOT EXISTS prs (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			repo_path TEXT NOT NULL,
			base_branch TEXT,
			pr_branch TEXT,
			commit_hash TEXT,
			merge_commit TEXT,
			total_files INTEGER NOT NULL,
			total_lines INTEGER NOT NULL,
			total_commits INTEGER NOT NULL,
			analyzed_at TIMESTAMP NOT NULL,
			merge_author_name TEXT,
			merge_author_email TEXT,
			merge_author_date TIMESTAMP,
			merge_committer_name TEXT,
			merge_committer_email TEXT,
			merge_committer_date TIMESTAMP,
			merge_summary TEXT
		);`,
		`CREATE UNIQUE INDEX IF NOT EXISTS idx_prs_unique_merge ON prs(repo_path, merge_commit) WHERE merge_commit IS NOT NULL;`,
		`CREATE UNIQUE INDEX IF NOT EXISTS idx_prs_unique_commit ON prs(repo_path, commit_hash) WHERE commit_hash IS NOT NULL;`,
		`CREATE TABLE IF NOT EXISTS languages (
			pr_id INTEGER NOT NULL,
			language TEXT NOT NULL,
			files_changed INTEGER NOT NULL,
			lines_added INTEGER NOT NULL,
			lines_deleted INTEGER NOT NULL,
			lines_modified INTEGER NOT NULL,
			percentage REAL NOT NULL,
			FOREIGN KEY(pr_id) REFERENCES prs(id) ON DELETE CASCADE
		);`,
		`CREATE INDEX IF NOT EXISTS idx_languages_pr ON languages(pr_id);`,
		`CREATE TABLE IF NOT EXISTS system_touch (
			pr_id INTEGER NOT NULL,
			system TEXT NOT NULL,
			count INTEGER NOT NULL,
			FOREIGN KEY(pr_id) REFERENCES prs(id) ON DELETE CASCADE
		);`,
		`CREATE INDEX IF NOT EXISTS idx_system_touch_pr ON system_touch(pr_id);`,
		`CREATE TABLE IF NOT EXISTS system_matrix (
			pr_id INTEGER NOT NULL,
			system1 TEXT NOT NULL,
			system2 TEXT NOT NULL,
			count INTEGER NOT NULL,
			FOREIGN KEY(pr_id) REFERENCES prs(id) ON DELETE CASCADE
		);`,
		`CREATE INDEX IF NOT EXISTS idx_system_matrix_pr ON system_matrix(pr_id);`,
	}
	for _, stmt := range stmts {
		if _, err := s.db.ExecContext(ctx, stmt); err != nil {
			return fmt.Errorf("ensure schema: %w", err)
		}
	}
	return nil
}

// ErrAnalysisExists is returned when an analysis for the same commit already exists and overwrite is disabled.
var ErrAnalysisExists = errors.New("analysis already exists")

// InsertAnalysis persists a PRAnalysisResult in a transaction. If overwriteIfExists is true,
// an existing analysis for the same repo+commit/merge will be deleted before inserting.
func (s *Store) InsertAnalysis(ctx context.Context, result *analysis.PRAnalysisResult, overwriteIfExists bool) (int64, error) {
	tx, err := s.db.BeginTx(ctx, nil)
	if err != nil {
		return 0, err
	}
	defer func() {
		if err != nil {
			_ = tx.Rollback()
		}
	}()

	// Handle existing analysis per unique keys
	var existingID sql.NullInt64
	var row *sql.Row
	if result.PRInfo.MergeCommit != "" {
		row = tx.QueryRowContext(ctx, "SELECT id FROM prs WHERE repo_path = ? AND merge_commit = ?", result.PRInfo.RepoPath, result.PRInfo.MergeCommit)
	} else if result.PRInfo.Commit != "" {
		row = tx.QueryRowContext(ctx, "SELECT id FROM prs WHERE repo_path = ? AND commit_hash = ?", result.PRInfo.RepoPath, result.PRInfo.Commit)
	}
	if row != nil {
		scanErr := row.Scan(&existingID)
		if scanErr == nil && existingID.Valid {
			if !overwriteIfExists {
				return 0, ErrAnalysisExists
			}
			if _, err = tx.ExecContext(ctx, "DELETE FROM prs WHERE id = ?", existingID.Int64); err != nil {
				return 0, errors.Wrap(err, "delete existing analysis")
			}
		} else if scanErr != nil && scanErr != sql.ErrNoRows {
			return 0, errors.Wrap(scanErr, "lookup existing analysis")
		}
	}

	// Insert PR row
	res, err := tx.ExecContext(ctx, `
		INSERT INTO prs (
			repo_path, base_branch, pr_branch, commit_hash, merge_commit,
			total_files, total_lines, total_commits, analyzed_at,
			merge_author_name, merge_author_email, merge_author_date,
			merge_committer_name, merge_committer_email, merge_committer_date,
			merge_summary
		) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
		result.PRInfo.RepoPath,
		result.PRInfo.BaseBranch,
		result.PRInfo.PRBranch,
		nullableString(result.PRInfo.Commit),
		nullableString(result.PRInfo.MergeCommit),
		result.PRInfo.TotalFiles,
		result.PRInfo.TotalLines,
		result.PRInfo.TotalCommits,
		time.Now().UTC(),
		nullableString(result.PRInfo.MergeAuthorName),
		nullableString(result.PRInfo.MergeAuthorEmail),
		nullableTime(result.PRInfo.MergeAuthorDate),
		nullableString(result.PRInfo.MergeCommitterName),
		nullableString(result.PRInfo.MergeCommitterEmail),
		nullableTime(result.PRInfo.MergeCommitterDate),
		nullableString(result.PRInfo.MergeSummary),
	)
	if err != nil {
		return 0, fmt.Errorf("insert prs: %w", err)
	}
	prID, err := res.LastInsertId()
	if err != nil {
		return 0, fmt.Errorf("last insert id: %w", err)
	}

	// Languages
	for _, ls := range result.LanguageStats {
		_, err = tx.ExecContext(ctx, `
			INSERT INTO languages(pr_id, language, files_changed, lines_added, lines_deleted, lines_modified, percentage)
			VALUES (?, ?, ?, ?, ?, ?, ?)`,
			prID, ls.Language, ls.FilesChanged, ls.LinesAdded, ls.LinesDeleted, ls.LinesModified, ls.Percentage,
		)
		if err != nil {
			return 0, fmt.Errorf("insert language: %w", err)
		}
	}

	// System touch counts
	for _, st := range result.CrossSystemStats.MostTouchedSystems {
		_, err = tx.ExecContext(ctx, `
			INSERT INTO system_touch(pr_id, system, count)
			VALUES (?, ?, ?)`, prID, st.System, st.Count)
		if err != nil {
			return 0, fmt.Errorf("insert system_touch: %w", err)
		}
	}

	// System matrix
	for s1, inner := range result.CrossSystemStats.SystemTouchMatrix {
		for s2, c := range inner {
			_, err = tx.ExecContext(ctx, `
				INSERT INTO system_matrix(pr_id, system1, system2, count)
				VALUES (?, ?, ?, ?)`, prID, s1, s2, c)
			if err != nil {
				return 0, fmt.Errorf("insert system_matrix: %w", err)
			}
		}
	}

	if err = tx.Commit(); err != nil {
		return 0, fmt.Errorf("commit: %w", err)
	}
	return prID, nil
}

// AggregateLanguages totals lines_modified and counts PRs per language
func (s *Store) AggregateLanguages(ctx context.Context) ([]struct{
	Language string
	PRs int
	Files int
	Lines int
}, error) {
	rows, err := s.db.QueryContext(ctx, `
		SELECT language, COUNT(DISTINCT pr_id) AS prs, SUM(files_changed) AS files, SUM(lines_modified) AS lines
		FROM languages
		GROUP BY language
		ORDER BY lines DESC`)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var out []struct{ Language string; PRs, Files, Lines int }
	for rows.Next() {
		var r struct{ Language string; PRs, Files, Lines int }
		if err := rows.Scan(&r.Language, &r.PRs, &r.Files, &r.Lines); err != nil {
			return nil, err
		}
		out = append(out, r)
	}
	return out, rows.Err()
}

// AggregateSystems totals counts from system_touch
func (s *Store) AggregateSystems(ctx context.Context) ([]struct{
	System string
	PRs int
	Count int
}, error) {
	rows, err := s.db.QueryContext(ctx, `
		SELECT system, COUNT(DISTINCT pr_id) AS prs, SUM(count) AS count
		FROM system_touch
		GROUP BY system
		ORDER BY count DESC`)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var out []struct{ System string; PRs, Count int }
	for rows.Next() {
		var r struct{ System string; PRs, Count int }
		if err := rows.Scan(&r.System, &r.PRs, &r.Count); err != nil {
			return nil, err
		}
		out = append(out, r)
	}
	return out, rows.Err()
}

// Filters define optional constraints for queries (type alias to pkg/dbfilters)
type Filters = appfilters.Filters

// ListPRs returns stored PR rows with filters
func (s *Store) ListPRs(ctx context.Context, f Filters) (*sql.Rows, error) {
	q := `SELECT id, repo_path, base_branch, pr_branch, commit_hash, merge_commit, total_files, total_lines, total_commits, analyzed_at,
		merge_author_name, merge_author_email, merge_author_date,
		merge_committer_name, merge_committer_email, merge_committer_date,
		merge_summary
		FROM prs WHERE 1=1`
	var args []any
	if !f.Since.IsZero() {
		q += " AND analyzed_at >= ?"
		args = append(args, f.Since)
	}
	if !f.Until.IsZero() {
		q += " AND analyzed_at <= ?"
		args = append(args, f.Until)
	}
	if f.Author != "" {
		q += " AND (lower(merge_author_name) LIKE ? OR lower(merge_author_email) LIKE ?)"
		like := "%" + f.Author + "%"
		args = append(args, like, like)
	}
	if f.Committer != "" {
		q += " AND (lower(merge_committer_name) LIKE ? OR lower(merge_committer_email) LIKE ?)"
		like := "%" + f.Committer + "%"
		args = append(args, like, like)
	}
	if f.RepoSubstring != "" {
		q += " AND lower(repo_path) LIKE ?"
		args = append(args, "%"+f.RepoSubstring+"%")
	}
	if f.HasMerge != nil {
		if *f.HasMerge {
			q += " AND merge_commit IS NOT NULL AND merge_commit <> ''"
		} else {
			q += " AND (merge_commit IS NULL OR merge_commit = '')"
		}
	}
	if f.MinFiles > 0 { q += " AND total_files >= ?"; args = append(args, f.MinFiles) }
	if f.MaxFiles > 0 { q += " AND total_files <= ?"; args = append(args, f.MaxFiles) }
	if f.MinLines > 0 { q += " AND total_lines >= ?"; args = append(args, f.MinLines) }
	if f.MaxLines > 0 { q += " AND total_lines <= ?"; args = append(args, f.MaxLines) }
	if f.Language != "" {
		q += " AND EXISTS (SELECT 1 FROM languages l WHERE l.pr_id = prs.id AND lower(l.language) = ?)"
		args = append(args, strings.ToLower(f.Language))
	}
	if f.System != "" {
		q += " AND EXISTS (SELECT 1 FROM system_touch st WHERE st.pr_id = prs.id AND lower(st.system) = ?)"
		args = append(args, strings.ToLower(f.System))
	}
	order := "analyzed_at"
	if f.OrderBy == "files" { order = "total_files" }
	if f.OrderBy == "lines" { order = "total_lines" }
	dir := "ASC"
	if f.OrderDesc { dir = "DESC" }
	q += " ORDER BY " + order + " " + dir
	if f.Limit > 0 { q += " LIMIT ?"; args = append(args, f.Limit) }
	if f.Offset > 0 { q += " OFFSET ?"; args = append(args, f.Offset) }
	return s.db.QueryContext(ctx, q, args...)
}

// Summary aggregates across PRs with optional filters
func (s *Store) Summary(ctx context.Context, f Filters) (struct{
	PRs int
	Files int
	Lines int
}, error) {
	q := `SELECT COUNT(*) AS prs, COALESCE(SUM(total_files),0) AS files, COALESCE(SUM(total_lines),0) AS lines FROM prs WHERE 1=1`
	var args []any
	if !f.Since.IsZero() { q += " AND analyzed_at >= ?"; args = append(args, f.Since) }
	if !f.Until.IsZero() { q += " AND analyzed_at <= ?"; args = append(args, f.Until) }
	if f.Author != "" {
		q += " AND (lower(merge_author_name) LIKE ? OR lower(merge_author_email) LIKE ?)"
		like := "%" + f.Author + "%"
		args = append(args, like, like)
	}
	if f.Committer != "" {
		q += " AND (lower(merge_committer_name) LIKE ? OR lower(merge_committer_email) LIKE ?)"
		like := "%" + f.Committer + "%"
		args = append(args, like, like)
	}
	if f.RepoSubstring != "" { q += " AND lower(repo_path) LIKE ?"; args = append(args, "%"+f.RepoSubstring+"%") }
	if f.HasMerge != nil {
		if *f.HasMerge { q += " AND merge_commit IS NOT NULL AND merge_commit <> ''" } else { q += " AND (merge_commit IS NULL OR merge_commit = '')" }
	}
	if f.MinFiles > 0 { q += " AND total_files >= ?"; args = append(args, f.MinFiles) }
	if f.MaxFiles > 0 { q += " AND total_files <= ?"; args = append(args, f.MaxFiles) }
	if f.MinLines > 0 { q += " AND total_lines >= ?"; args = append(args, f.MinLines) }
	if f.MaxLines > 0 { q += " AND total_lines <= ?"; args = append(args, f.MaxLines) }
	if f.Language != "" {
		q += " AND EXISTS (SELECT 1 FROM languages l WHERE l.pr_id = prs.id AND lower(l.language) = ?)"
		args = append(args, strings.ToLower(f.Language))
	}
	if f.System != "" {
		q += " AND EXISTS (SELECT 1 FROM system_touch st WHERE st.pr_id = prs.id AND lower(st.system) = ?)"
		args = append(args, strings.ToLower(f.System))
	}
	row := s.db.QueryRowContext(ctx, q, args...)
	var r struct{ PRs, Files, Lines int }
	if err := row.Scan(&r.PRs, &r.Files, &r.Lines); err != nil {
		return r, err
	}
	return r, nil
}

func nullableString(s string) any {
	if s == "" {
		return nil
	}
	return s
}

func nullableTime(t time.Time) any {
	if t.IsZero() {
		return nil
	}
	return t
}
