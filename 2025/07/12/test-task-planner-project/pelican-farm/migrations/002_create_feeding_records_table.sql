-- Create feeding_records table
CREATE TABLE IF NOT EXISTS feeding_records (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    pelican_id INTEGER NOT NULL,
    food_type TEXT NOT NULL,
    amount_kg REAL NOT NULL,
    feeding_time DATETIME NOT NULL,
    notes TEXT,
    created DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (pelican_id) REFERENCES pelicans(id) ON DELETE CASCADE
);

-- Create indexes for faster lookups
CREATE INDEX IF NOT EXISTS idx_feeding_records_pelican_id ON feeding_records(pelican_id);
CREATE INDEX IF NOT EXISTS idx_feeding_records_feeding_time ON feeding_records(feeding_time);
CREATE INDEX IF NOT EXISTS idx_feeding_records_food_type ON feeding_records(food_type);
