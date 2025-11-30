package storage

import (
	"database/sql"
	"encoding/json"
	"fmt"

	"github.com/fact-extraction/go-extractor/pkg/types"
	_ "github.com/mattn/go-sqlite3"
)

// SQLiteWriter writes extraction results to SQLite database
type SQLiteWriter struct {
	db *sql.DB
}

// NewSQLiteWriter creates a new SQLite writer
func NewSQLiteWriter(dbPath string) (*SQLiteWriter, error) {
	db, err := sql.Open("sqlite3", dbPath)
	if err != nil {
		return nil, fmt.Errorf("failed to open database: %w", err)
	}

	writer := &SQLiteWriter{db: db}
	if err := writer.initSchema(); err != nil {
		db.Close()
		return nil, err
	}

	return writer, nil
}

// Close closes the database connection
func (sw *SQLiteWriter) Close() error {
	return sw.db.Close()
}

// initSchema creates the database schema
func (sw *SQLiteWriter) initSchema() error {
	schema := `
	CREATE TABLE IF NOT EXISTS documents (
		doc_id TEXT PRIMARY KEY,
		processed_at TIMESTAMP,
		tokens_in INTEGER,
		tokens_out INTEGER,
		cost_usd REAL
	);

	CREATE TABLE IF NOT EXISTS rdf_triples (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		doc_id TEXT,
		actor TEXT,
		action TEXT,
		target TEXT,
		explicit_topic TEXT,
		implicit_topic TEXT,
		tags TEXT,
		timestamp TEXT,
		location TEXT,
		actor_likely_type TEXT,
		FOREIGN KEY (doc_id) REFERENCES documents(doc_id)
	);

	CREATE TABLE IF NOT EXISTS processing_log (
		doc_id TEXT PRIMARY KEY,
		status TEXT,
		timestamp TIMESTAMP,
		error_message TEXT
	);

	CREATE INDEX IF NOT EXISTS idx_actor ON rdf_triples(actor);
	CREATE INDEX IF NOT EXISTS idx_action ON rdf_triples(action);
	CREATE INDEX IF NOT EXISTS idx_target ON rdf_triples(target);
	`

	_, err := sw.db.Exec(schema)
	return err
}

// SaveResult saves an extraction result to the database
func (sw *SQLiteWriter) SaveResult(result *types.ExtractionResult) error {
	tx, err := sw.db.Begin()
	if err != nil {
		return fmt.Errorf("failed to begin transaction: %w", err)
	}
	defer tx.Rollback()

	// Insert document
	_, err = tx.Exec(`
		INSERT OR REPLACE INTO documents (doc_id, processed_at, tokens_in, tokens_out, cost_usd)
		VALUES (?, ?, ?, ?, ?)
	`, result.DocumentID, result.ProcessedAt, result.TokensIn, result.TokensOut, result.CostUSD)
	if err != nil {
		return fmt.Errorf("failed to insert document: %w", err)
	}

	// Insert triples
	for _, triple := range result.Triples {
		tagsJSON, _ := json.Marshal(triple.Tags)

		_, err = tx.Exec(`
			INSERT INTO rdf_triples (
				doc_id, actor, action, target, explicit_topic, implicit_topic,
				tags, timestamp, location, actor_likely_type
			) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
		`,
			result.DocumentID,
			triple.Actor,
			triple.Action,
			triple.Target,
			triple.ExplicitTopic,
			triple.ImplicitTopic,
			string(tagsJSON),
			ptrToString(triple.Timestamp),
			ptrToString(triple.Location),
			ptrToString(triple.ActorLikelyType),
		)
		if err != nil {
			return fmt.Errorf("failed to insert triple: %w", err)
		}
	}

	// Log success
	_, err = tx.Exec(`
		INSERT OR REPLACE INTO processing_log (doc_id, status, timestamp)
		VALUES (?, 'success', CURRENT_TIMESTAMP)
	`, result.DocumentID)
	if err != nil {
		return fmt.Errorf("failed to log success: %w", err)
	}

	return tx.Commit()
}

// GetStats returns statistics about the extraction
func (sw *SQLiteWriter) GetStats() (map[string]interface{}, error) {
	stats := make(map[string]interface{})

	// Count documents
	var docCount int
	err := sw.db.QueryRow("SELECT COUNT(*) FROM documents").Scan(&docCount)
	if err != nil {
		return nil, err
	}
	stats["documents"] = docCount

	// Count triples
	var tripleCount int
	err = sw.db.QueryRow("SELECT COUNT(*) FROM rdf_triples").Scan(&tripleCount)
	if err != nil {
		return nil, err
	}
	stats["triples"] = tripleCount

	// Total cost
	var totalCost float64
	err = sw.db.QueryRow("SELECT COALESCE(SUM(cost_usd), 0) FROM documents").Scan(&totalCost)
	if err != nil {
		return nil, err
	}
	stats["total_cost"] = totalCost

	// Average triples per document
	if docCount > 0 {
		stats["avg_triples_per_doc"] = float64(tripleCount) / float64(docCount)
	}

	return stats, nil
}

// ptrToString converts a string pointer to a string (empty if nil)
func ptrToString(ptr *string) string {
	if ptr == nil {
		return ""
	}
	return *ptr
}
