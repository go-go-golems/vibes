// SQLite database schema for tweet manager
const sqlite3 = require('better-sqlite3');
const path = require('path');

// Create a database connection
const dbPath = path.join(__dirname, 'tweets.db');
const db = new sqlite3(dbPath);

// Initialize database schema
function initializeDatabase() {
  // Create tweets table
  db.exec(`
    CREATE TABLE IF NOT EXISTS tweets (
      id TEXT PRIMARY KEY,
      author_id TEXT NOT NULL,
      content TEXT NOT NULL,
      timestamp TEXT NOT NULL,
      likes INTEGER DEFAULT 0,
      retweets INTEGER DEFAULT 0,
      quotes INTEGER DEFAULT 0,
      replies INTEGER DEFAULT 0,
      is_flagged INTEGER DEFAULT 0,
      flag_type TEXT,
      is_bookmarked INTEGER DEFAULT 0,
      bookmark_note TEXT,
      FOREIGN KEY (author_id) REFERENCES authors(id)
    )
  `);

  // Create authors table
  db.exec(`
    CREATE TABLE IF NOT EXISTS authors (
      id TEXT PRIMARY KEY,
      username TEXT NOT NULL,
      display_name TEXT,
      profile_image TEXT,
      is_muted INTEGER DEFAULT 0,
      is_blocked INTEGER DEFAULT 0
    )
  `);

  // Create tags table
  db.exec(`
    CREATE TABLE IF NOT EXISTS tags (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT NOT NULL UNIQUE,
      type TEXT NOT NULL,
      is_predefined INTEGER DEFAULT 0
    )
  `);

  // Create tweet_tags junction table
  db.exec(`
    CREATE TABLE IF NOT EXISTS tweet_tags (
      tweet_id TEXT NOT NULL,
      tag_id INTEGER NOT NULL,
      PRIMARY KEY (tweet_id, tag_id),
      FOREIGN KEY (tweet_id) REFERENCES tweets(id),
      FOREIGN KEY (tag_id) REFERENCES tags(id)
    )
  `);

  // Create author_tags junction table
  db.exec(`
    CREATE TABLE IF NOT EXISTS author_tags (
      author_id TEXT NOT NULL,
      tag_id INTEGER NOT NULL,
      PRIMARY KEY (author_id, tag_id),
      FOREIGN KEY (author_id) REFERENCES authors(id),
      FOREIGN KEY (tag_id) REFERENCES tags(id)
    )
  `);

  // Create predefined tags
  const predefinedTags = [
    { name: 'Important', type: 'tweet', is_predefined: 1 },
    { name: 'Funny', type: 'tweet', is_predefined: 1 },
    { name: 'Insightful', type: 'tweet', is_predefined: 1 },
    { name: 'News', type: 'tweet', is_predefined: 1 },
    { name: 'Personal', type: 'tweet', is_predefined: 1 },
    { name: 'Verified', type: 'author', is_predefined: 1 },
    { name: 'Celebrity', type: 'author', is_predefined: 1 },
    { name: 'Friend', type: 'author', is_predefined: 1 },
    { name: 'Organization', type: 'author', is_predefined: 1 },
    { name: 'Media', type: 'author', is_predefined: 1 }
  ];

  // Insert predefined tags if they don't exist
  const insertTag = db.prepare('INSERT OR IGNORE INTO tags (name, type, is_predefined) VALUES (?, ?, ?)');
  predefinedTags.forEach(tag => {
    insertTag.run(tag.name, tag.type, tag.is_predefined);
  });

  console.log('Database schema initialized successfully');
  return db;
}

// Export the database and initialization function
module.exports = {
  db,
  initializeDatabase
};
