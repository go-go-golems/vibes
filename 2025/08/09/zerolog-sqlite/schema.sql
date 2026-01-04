-- SQLite schema for zerolog backend
-- This schema uses key-value storage for structured log data

-- Main logs table
CREATE TABLE IF NOT EXISTS logs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp DATETIME NOT NULL,
    level TEXT NOT NULL,
    message TEXT,
    caller TEXT,
    stack TEXT,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for efficient querying
CREATE INDEX IF NOT EXISTS idx_logs_timestamp ON logs(timestamp);
CREATE INDEX IF NOT EXISTS idx_logs_level ON logs(level);
CREATE INDEX IF NOT EXISTS idx_logs_created_at ON logs(created_at);
CREATE INDEX IF NOT EXISTS idx_logs_level_timestamp ON logs(level, timestamp);

-- Key-value table for all structured fields
CREATE TABLE IF NOT EXISTS log_fields (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    log_id INTEGER NOT NULL,
    field_name TEXT NOT NULL,
    field_value TEXT NOT NULL,
    field_type TEXT NOT NULL, -- 'string', 'number', 'boolean', 'object'
    FOREIGN KEY (log_id) REFERENCES logs(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_log_fields_log_id ON log_fields(log_id);
CREATE INDEX IF NOT EXISTS idx_log_fields_name ON log_fields(field_name);
CREATE INDEX IF NOT EXISTS idx_log_fields_name_value ON log_fields(field_name, field_value);
CREATE INDEX IF NOT EXISTS idx_log_fields_name_type ON log_fields(field_name, field_type);

-- View for easy querying with aggregated fields
CREATE VIEW IF NOT EXISTS logs_with_fields AS
SELECT 
    l.id,
    l.timestamp,
    l.level,
    l.message,
    l.caller,
    l.stack,
    l.created_at,
    GROUP_CONCAT(
        lf.field_name || '=' || lf.field_value, 
        '; '
    ) as fields_summary
FROM logs l
LEFT JOIN log_fields lf ON l.id = lf.log_id
GROUP BY l.id, l.timestamp, l.level, l.message, l.caller, l.stack, l.created_at;

