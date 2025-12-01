-- +goose Up
ALTER TABLE users ADD COLUMN password_hash TEXT;

-- +goose Down
-- SQLite doesn't support DROP COLUMN, so we'll recreate the table
-- This is a simplified version - in production you'd want a more careful migration
CREATE TABLE IF NOT EXISTS users_new (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    open_id TEXT UNIQUE NOT NULL,
    name TEXT,
    email TEXT,
    login_method TEXT,
    role TEXT CHECK(role IN ('user', 'admin')) DEFAULT 'user',
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    last_signed_in DATETIME DEFAULT CURRENT_TIMESTAMP
);
INSERT INTO users_new SELECT id, open_id, name, email, login_method, role, created_at, updated_at, last_signed_in FROM users;
DROP TABLE users;
ALTER TABLE users_new RENAME TO users;
CREATE INDEX IF NOT EXISTS idx_users_open_id ON users(open_id);

