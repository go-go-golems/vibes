// Database utility functions for tweet manager
const { db, initializeDatabase } = require('./schema');

// Initialize the database
const database = initializeDatabase();

// Tweet related functions
const tweetFunctions = {
  // Get all tweets (with optional filtering)
  getAllTweets: (filters = {}) => {
    let query = `
      SELECT t.*, a.username, a.display_name, a.is_blocked, a.is_muted
      FROM tweets t
      JOIN authors a ON t.author_id = a.id
      WHERE 1=1
    `;
    
    const params = [];
    
    // Apply filters
    if (filters.authorId) {
      query += ` AND t.author_id = ?`;
      params.push(filters.authorId);
    }
    
    if (filters.searchText) {
      query += ` AND (t.content LIKE ? OR a.username LIKE ? OR a.display_name LIKE ?)`;
      const searchParam = `%${filters.searchText}%`;
      params.push(searchParam, searchParam, searchParam);
    }
    
    if (filters.isBookmarked) {
      query += ` AND t.is_bookmarked = 1`;
    }
    
    if (filters.isFlagged) {
      query += ` AND t.is_flagged = 1`;
    }
    
    if (filters.hideBlocked) {
      query += ` AND a.is_blocked = 0`;
    }
    
    if (filters.hideMuted) {
      query += ` AND a.is_muted = 0`;
    }
    
    if (filters.tagId) {
      query += ` AND t.id IN (SELECT tweet_id FROM tweet_tags WHERE tag_id = ?)`;
      params.push(filters.tagId);
    }
    
    query += ` ORDER BY t.timestamp DESC`;
    
    return database.prepare(query).all(...params);
  },
  
  // Get a single tweet by ID
  getTweetById: (id) => {
    return database.prepare(`
      SELECT t.*, a.username, a.display_name
      FROM tweets t
      JOIN authors a ON t.author_id = a.id
      WHERE t.id = ?
    `).get(id);
  },
  
  // Add a new tweet
  addTweet: (tweet) => {
    const stmt = database.prepare(`
      INSERT INTO tweets (
        id, author_id, content, timestamp, 
        likes, retweets, quotes, replies,
        is_flagged, flag_type, is_bookmarked, bookmark_note
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    `);
    
    return stmt.run(
      tweet.id,
      tweet.author_id,
      tweet.content,
      tweet.timestamp,
      tweet.likes || 0,
      tweet.retweets || 0,
      tweet.quotes || 0,
      tweet.replies || 0,
      tweet.is_flagged || 0,
      tweet.flag_type || null,
      tweet.is_bookmarked || 0,
      tweet.bookmark_note || null
    );
  },
  
  // Update tweet flags and bookmarks
  updateTweetStatus: (id, updates) => {
    let query = 'UPDATE tweets SET ';
    const params = [];
    const updateFields = [];
    
    if (updates.hasOwnProperty('is_flagged')) {
      updateFields.push('is_flagged = ?');
      params.push(updates.is_flagged ? 1 : 0);
    }
    
    if (updates.hasOwnProperty('flag_type')) {
      updateFields.push('flag_type = ?');
      params.push(updates.flag_type);
    }
    
    if (updates.hasOwnProperty('is_bookmarked')) {
      updateFields.push('is_bookmarked = ?');
      params.push(updates.is_bookmarked ? 1 : 0);
    }
    
    if (updates.hasOwnProperty('bookmark_note')) {
      updateFields.push('bookmark_note = ?');
      params.push(updates.bookmark_note);
    }
    
    if (updateFields.length === 0) {
      return { changes: 0 };
    }
    
    query += updateFields.join(', ') + ' WHERE id = ?';
    params.push(id);
    
    return database.prepare(query).run(...params);
  },
  
  // Delete a tweet
  deleteTweet: (id) => {
    // First delete related tags
    database.prepare('DELETE FROM tweet_tags WHERE tweet_id = ?').run(id);
    // Then delete the tweet
    return database.prepare('DELETE FROM tweets WHERE id = ?').run(id);
  }
};

// Author related functions
const authorFunctions = {
  // Get all authors
  getAllAuthors: (filters = {}) => {
    let query = 'SELECT * FROM authors WHERE 1=1';
    const params = [];
    
    if (filters.searchText) {
      query += ` AND (username LIKE ? OR display_name LIKE ?)`;
      const searchParam = `%${filters.searchText}%`;
      params.push(searchParam, searchParam);
    }
    
    if (filters.isBlocked) {
      query += ` AND is_blocked = 1`;
    }
    
    if (filters.isMuted) {
      query += ` AND is_muted = 1`;
    }
    
    if (filters.tagId) {
      query += ` AND id IN (SELECT author_id FROM author_tags WHERE tag_id = ?)`;
      params.push(filters.tagId);
    }
    
    return database.prepare(query).all(...params);
  },
  
  // Get a single author by ID
  getAuthorById: (id) => {
    return database.prepare('SELECT * FROM authors WHERE id = ?').get(id);
  },
  
  // Add a new author
  addAuthor: (author) => {
    const stmt = database.prepare(`
      INSERT OR IGNORE INTO authors (
        id, username, display_name, profile_image, is_muted, is_blocked
      ) VALUES (?, ?, ?, ?, ?, ?)
    `);
    
    return stmt.run(
      author.id,
      author.username,
      author.display_name || null,
      author.profile_image || null,
      author.is_muted || 0,
      author.is_blocked || 0
    );
  },
  
  // Update author status (muted/blocked)
  updateAuthorStatus: (id, updates) => {
    let query = 'UPDATE authors SET ';
    const params = [];
    const updateFields = [];
    
    if (updates.hasOwnProperty('is_muted')) {
      updateFields.push('is_muted = ?');
      params.push(updates.is_muted ? 1 : 0);
    }
    
    if (updates.hasOwnProperty('is_blocked')) {
      updateFields.push('is_blocked = ?');
      params.push(updates.is_blocked ? 1 : 0);
    }
    
    if (updateFields.length === 0) {
      return { changes: 0 };
    }
    
    query += updateFields.join(', ') + ' WHERE id = ?';
    params.push(id);
    
    return database.prepare(query).run(...params);
  }
};

// Tag related functions
const tagFunctions = {
  // Get all tags
  getAllTags: (type = null) => {
    let query = 'SELECT * FROM tags';
    const params = [];
    
    if (type) {
      query += ' WHERE type = ?';
      params.push(type);
    }
    
    query += ' ORDER BY is_predefined DESC, name ASC';
    
    return database.prepare(query).all(...params);
  },
  
  // Add a new tag
  addTag: (name, type, isPredefined = 0) => {
    return database.prepare(`
      INSERT OR IGNORE INTO tags (name, type, is_predefined)
      VALUES (?, ?, ?)
    `).run(name, type, isPredefined);
  },
  
  // Get tags for a tweet
  getTweetTags: (tweetId) => {
    return database.prepare(`
      SELECT t.* FROM tags t
      JOIN tweet_tags tt ON t.id = tt.tag_id
      WHERE tt.tweet_id = ?
    `).all(tweetId);
  },
  
  // Get tags for an author
  getAuthorTags: (authorId) => {
    return database.prepare(`
      SELECT t.* FROM tags t
      JOIN author_tags at ON t.id = at.tag_id
      WHERE at.author_id = ?
    `).all(authorId);
  },
  
  // Add tag to tweet
  addTagToTweet: (tweetId, tagId) => {
    return database.prepare(`
      INSERT OR IGNORE INTO tweet_tags (tweet_id, tag_id)
      VALUES (?, ?)
    `).run(tweetId, tagId);
  },
  
  // Add tag to author
  addTagToAuthor: (authorId, tagId) => {
    return database.prepare(`
      INSERT OR IGNORE INTO author_tags (author_id, tag_id)
      VALUES (?, ?)
    `).run(authorId, tagId);
  },
  
  // Remove tag from tweet
  removeTagFromTweet: (tweetId, tagId) => {
    return database.prepare(`
      DELETE FROM tweet_tags
      WHERE tweet_id = ? AND tag_id = ?
    `).run(tweetId, tagId);
  },
  
  // Remove tag from author
  removeTagFromAuthor: (authorId, tagId) => {
    return database.prepare(`
      DELETE FROM author_tags
      WHERE author_id = ? AND tag_id = ?
    `).run(authorId, tagId);
  }
};

// Export all database functions
module.exports = {
  db: database,
  tweets: tweetFunctions,
  authors: authorFunctions,
  tags: tagFunctions
};
