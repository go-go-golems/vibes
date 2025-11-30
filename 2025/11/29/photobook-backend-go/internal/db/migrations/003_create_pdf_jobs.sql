-- +goose Up
CREATE TABLE IF NOT EXISTS pdf_jobs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER NOT NULL,
    status TEXT NOT NULL CHECK(status IN ('pending', 'processing', 'completed', 'failed')) DEFAULT 'pending',
    file_key TEXT,
    url TEXT,
    error TEXT,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_pdf_jobs_user_id ON pdf_jobs(user_id);
CREATE INDEX IF NOT EXISTS idx_pdf_jobs_status ON pdf_jobs(status);

-- +goose Down
DROP TABLE IF EXISTS pdf_jobs;

