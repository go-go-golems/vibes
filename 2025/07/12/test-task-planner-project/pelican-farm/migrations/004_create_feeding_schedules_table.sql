-- Create feeding_schedules table
CREATE TABLE IF NOT EXISTS feeding_schedules (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    pelican_id INTEGER NOT NULL,
    scheduled_time DATETIME NOT NULL,
    food_type TEXT NOT NULL,
    amount_kg REAL NOT NULL,
    completed BOOLEAN NOT NULL DEFAULT FALSE,
    created DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (pelican_id) REFERENCES pelicans(id) ON DELETE CASCADE
);

-- Create indexes for faster lookups
CREATE INDEX IF NOT EXISTS idx_feeding_schedules_pelican_id ON feeding_schedules(pelican_id);
CREATE INDEX IF NOT EXISTS idx_feeding_schedules_scheduled_time ON feeding_schedules(scheduled_time);
CREATE INDEX IF NOT EXISTS idx_feeding_schedules_completed ON feeding_schedules(completed);
