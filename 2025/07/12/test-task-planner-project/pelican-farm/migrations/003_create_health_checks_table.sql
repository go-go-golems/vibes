-- Create health_checks table
CREATE TABLE IF NOT EXISTS health_checks (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    pelican_id INTEGER NOT NULL,
    check_date DATE NOT NULL,
    weight REAL,
    temperature REAL,
    notes TEXT,
    veterinarian TEXT,
    created DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (pelican_id) REFERENCES pelicans(id) ON DELETE CASCADE
);

-- Create indexes for faster lookups
CREATE INDEX IF NOT EXISTS idx_health_checks_pelican_id ON health_checks(pelican_id);
CREATE INDEX IF NOT EXISTS idx_health_checks_check_date ON health_checks(check_date);
CREATE INDEX IF NOT EXISTS idx_health_checks_veterinarian ON health_checks(veterinarian);
