-- Initial schema for Pelican Farm Management System
-- This migration creates the base tables for pelicans and farms

-- Create pelicans table
CREATE TABLE IF NOT EXISTS pelicans (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    species TEXT NOT NULL,
    age INTEGER NOT NULL,
    weight REAL NOT NULL,
    health TEXT NOT NULL CHECK (health IN ('healthy', 'sick', 'injured', 'recovering', 'critical')),
    location TEXT NOT NULL,
    gender TEXT NOT NULL CHECK (gender IN ('male', 'female', 'unknown')),
    color TEXT NOT NULL,
    farm_id INTEGER,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (farm_id) REFERENCES farms(id) ON DELETE SET NULL
);

-- Create farms table
CREATE TABLE IF NOT EXISTS farms (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL UNIQUE,
    location TEXT NOT NULL,
    capacity INTEGER NOT NULL DEFAULT 100,
    description TEXT,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Create indexes for performance
CREATE INDEX IF NOT EXISTS idx_pelicans_farm_id ON pelicans(farm_id);
CREATE INDEX IF NOT EXISTS idx_pelicans_health ON pelicans(health);
CREATE INDEX IF NOT EXISTS idx_pelicans_species ON pelicans(species);
CREATE INDEX IF NOT EXISTS idx_farms_name ON farms(name);

-- Insert default farm if none exists
INSERT OR IGNORE INTO farms (name, location, capacity, description) 
VALUES ('Default Farm', 'Main Location', 200, 'Primary pelican sanctuary and breeding facility');

-- Create triggers for updated_at timestamps
CREATE TRIGGER IF NOT EXISTS pelicans_updated_at 
    AFTER UPDATE ON pelicans
BEGIN
    UPDATE pelicans SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
END;

CREATE TRIGGER IF NOT EXISTS farms_updated_at 
    AFTER UPDATE ON farms
BEGIN
    UPDATE farms SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
END;
