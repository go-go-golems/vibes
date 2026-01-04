-- Create pelicans table
CREATE TABLE IF NOT EXISTS pelicans (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    species TEXT NOT NULL,
    age INTEGER NOT NULL,
    weight REAL,
    health_status TEXT NOT NULL DEFAULT 'healthy',
    arrival_date DATE NOT NULL,
    notes TEXT,
    created DATETIME DEFAULT CURRENT_TIMESTAMP,
    modified DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Create indexes for faster lookups
CREATE INDEX IF NOT EXISTS idx_pelicans_name ON pelicans(name);
CREATE INDEX IF NOT EXISTS idx_pelicans_species ON pelicans(species);
CREATE INDEX IF NOT EXISTS idx_pelicans_health_status ON pelicans(health_status);
