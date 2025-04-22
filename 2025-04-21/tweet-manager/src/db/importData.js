// Data import functionality for tweet manager
const fs = require('fs');
const path = require('path');
const Papa = require('papaparse');
const db = require('./database');

// Import data from JSON file
const importFromJSON = (filePath) => {
  try {
    // Read and parse the JSON file
    const data = JSON.parse(fs.readFileSync(filePath, 'utf8'));
    
    // Begin transaction
    db.db.prepare('BEGIN').run();
    
    let importStats = {
      authors: 0,
      tweets: 0,
      errors: []
    };
    
    try {
      // Process authors first
      if (Array.isArray(data.authors)) {
        data.authors.forEach(author => {
          try {
            const result = db.authors.addAuthor({
              id: author.id,
              username: author.username,
              display_name: author.display_name || author.username,
              profile_image: author.profile_image || null,
              is_muted: author.is_muted || 0,
              is_blocked: author.is_blocked || 0
            });
            
            if (result.changes > 0) {
              importStats.authors++;
            }
          } catch (err) {
            importStats.errors.push(`Error importing author ${author.username}: ${err.message}`);
          }
        });
      }
      
      // Process tweets
      if (Array.isArray(data.tweets)) {
        data.tweets.forEach(tweet => {
          try {
            const result = db.tweets.addTweet({
              id: tweet.id,
              author_id: tweet.author_id,
              content: tweet.content,
              timestamp: tweet.timestamp,
              likes: tweet.likes || 0,
              retweets: tweet.retweets || 0,
              quotes: tweet.quotes || 0,
              replies: tweet.replies || 0,
              is_flagged: tweet.is_flagged || 0,
              flag_type: tweet.flag_type || null,
              is_bookmarked: tweet.is_bookmarked || 0,
              bookmark_note: tweet.bookmark_note || null
            });
            
            if (result.changes > 0) {
              importStats.tweets++;
            }
            
            // Import tweet tags if available
            if (Array.isArray(tweet.tags)) {
              tweet.tags.forEach(tagName => {
                // First ensure the tag exists
                const tagResult = db.tags.addTag(tagName, 'tweet', 0);
                const tagId = tagResult.lastInsertRowid || 
                              db.db.prepare('SELECT id FROM tags WHERE name = ? AND type = ?')
                                .get(tagName, 'tweet').id;
                
                // Then add the tag to the tweet
                db.tags.addTagToTweet(tweet.id, tagId);
              });
            }
          } catch (err) {
            importStats.errors.push(`Error importing tweet ${tweet.id}: ${err.message}`);
          }
        });
      }
      
      // Commit transaction
      db.db.prepare('COMMIT').run();
      
      return {
        success: true,
        stats: importStats
      };
    } catch (err) {
      // Rollback on error
      db.db.prepare('ROLLBACK').run();
      throw err;
    }
  } catch (err) {
    return {
      success: false,
      error: `Failed to import JSON: ${err.message}`
    };
  }
};

// Import data from CSV file
const importFromCSV = (filePath, fileType) => {
  try {
    // Read the CSV file
    const fileContent = fs.readFileSync(filePath, 'utf8');
    
    // Parse CSV
    const parseResult = Papa.parse(fileContent, {
      header: true,
      skipEmptyLines: true
    });
    
    if (parseResult.errors.length > 0) {
      return {
        success: false,
        error: `CSV parsing errors: ${parseResult.errors.map(e => e.message).join(', ')}`
      };
    }
    
    // Begin transaction
    db.db.prepare('BEGIN').run();
    
    let importStats = {
      authors: 0,
      tweets: 0,
      errors: []
    };
    
    try {
      // Process based on file type
      if (fileType === 'authors') {
        parseResult.data.forEach(row => {
          try {
            const result = db.authors.addAuthor({
              id: row.id,
              username: row.username,
              display_name: row.display_name || row.username,
              profile_image: row.profile_image || null,
              is_muted: parseInt(row.is_muted || '0'),
              is_blocked: parseInt(row.is_blocked || '0')
            });
            
            if (result.changes > 0) {
              importStats.authors++;
            }
          } catch (err) {
            importStats.errors.push(`Error importing author ${row.username}: ${err.message}`);
          }
        });
      } else if (fileType === 'tweets') {
        parseResult.data.forEach(row => {
          try {
            const result = db.tweets.addTweet({
              id: row.id,
              author_id: row.author_id,
              content: row.content,
              timestamp: row.timestamp,
              likes: parseInt(row.likes || '0'),
              retweets: parseInt(row.retweets || '0'),
              quotes: parseInt(row.quotes || '0'),
              replies: parseInt(row.replies || '0'),
              is_flagged: parseInt(row.is_flagged || '0'),
              flag_type: row.flag_type || null,
              is_bookmarked: parseInt(row.is_bookmarked || '0'),
              bookmark_note: row.bookmark_note || null
            });
            
            if (result.changes > 0) {
              importStats.tweets++;
            }
            
            // Import tweet tags if available
            if (row.tags) {
              const tags = row.tags.split(',').map(tag => tag.trim()).filter(tag => tag);
              tags.forEach(tagName => {
                // First ensure the tag exists
                const tagResult = db.tags.addTag(tagName, 'tweet', 0);
                const tagId = tagResult.lastInsertRowid || 
                              db.db.prepare('SELECT id FROM tags WHERE name = ? AND type = ?')
                                .get(tagName, 'tweet').id;
                
                // Then add the tag to the tweet
                db.tags.addTagToTweet(row.id, tagId);
              });
            }
          } catch (err) {
            importStats.errors.push(`Error importing tweet ${row.id}: ${err.message}`);
          }
        });
      }
      
      // Commit transaction
      db.db.prepare('COMMIT').run();
      
      return {
        success: true,
        stats: importStats
      };
    } catch (err) {
      // Rollback on error
      db.db.prepare('ROLLBACK').run();
      throw err;
    }
  } catch (err) {
    return {
      success: false,
      error: `Failed to import CSV: ${err.message}`
    };
  }
};

module.exports = {
  importFromJSON,
  importFromCSV
};
