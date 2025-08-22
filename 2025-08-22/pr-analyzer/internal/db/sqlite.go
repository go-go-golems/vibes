package db

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	_ "modernc.org/sqlite"
	"pr-analyzer/internal/analysis"
)

// Store wraps a sql.DB with helpers for schema management and inserts
 type Store struct {
	db *sql.DB
}

// Open opens or creates a sqlite database at path and ensures schema
func Open(ctx context.Context, path string) (*Store, error) {
	dsn := fmt.Sprintf("file:%s?_pragma=busy_timeout(5000)", path)
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

// InsertAnalysis persists a PRAnalysisResult in a transaction
func (s *Store) InsertAnalysis(ctx context.Context, result *analysis.PRAnalysisResult) (int64, error) {
	tx, err := s.db.BeginTx(ctx, nil)
	if err != nil {
		return 0, err
	}
	defer func() {
		if err != nil {
			_ = tx.Rollback()
		}
	}()

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

// ListPRs returns stored PR rows with filters
func (s *Store) ListPRs(ctx context.Context, since time.Time, authorOrEmail string) (*sql.Rows, error) {
	q := `SELECT id, repo_path, base_branch, pr_branch, commit_hash, merge_commit, total_files, total_lines, total_commits, analyzed_at,
		merge_author_name, merge_author_email, merge_author_date,
		merge_committer_name, merge_committer_email, merge_committer_date,
		merge_summary
		FROM prs WHERE 1=1`
	var args []any
	if !since.IsZero() {
		q += " AND analyzed_at >= ?"
		args = append(args, since)
	}
	if authorOrEmail != "" {
		q += " AND (lower(merge_author_name) LIKE ? OR lower(merge_author_email) LIKE ? OR lower(merge_committer_name) LIKE ? OR lower(merge_committer_email) LIKE ?)"
		like := "%" + authorOrEmail + "%"
		args = append(args, like, like, like, like)
	}
	q += " ORDER BY analyzed_at DESC"
	return s.db.QueryContext(ctx, q, args...)
}

// Summary aggregates across PRs with optional filters
func (s *Store) Summary(ctx context.Context, since time.Time, authorOrEmail string) (struct{
	PRs int
	Files int
	Lines int
}, error) {
	q := `SELECT COUNT(*) AS prs, COALESCE(SUM(total_files),0) AS files, COALESCE(SUM(total_lines),0) AS lines FROM prs WHERE 1=1`
	var args []any
	if !since.IsZero() {
		q += " AND analyzed_at >= ?"
		args = append(args, since)
	}
	if authorOrEmail != "" {
		q += " AND (lower(merge_author_name) LIKE ? OR lower(merge_author_email) LIKE ? OR lower(merge_committer_name) LIKE ? OR lower(merge_committer_email) LIKE ?)"
		like := "%" + authorOrEmail + "%"
		args = append(args, like, like, like, like)
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
