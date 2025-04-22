import React, { useState, useEffect } from 'react';
import { Container } from 'react-bootstrap';
import Navbar from './components/Navbar';
import TweetList from './components/TweetList';
import './styles/RetroTheme.css';
import mockData from './mock-data.json';

function App() {
  // State for tweets and authors
  const [tweets, setTweets] = useState([]);
  const [authors, setAuthors] = useState([]);
  const [tweetTags, setTweetTags] = useState([]);
  const [authorTags, setAuthorTags] = useState([]);

  // Load mock data on initial render
  useEffect(() => {
    // In a real app, this would fetch from the database
    const loadMockData = () => {
      try {
        const data = mockData[0]; // Get the first item from the array
        
        // Process authors
        if (data.authors) {
          setAuthors(data.authors.map(author => ({
            ...author,
            tags: [] // Initialize tags array
          })));
        }
        
        // Process tweets
        if (data.tweets) {
          setTweets(data.tweets);
        }
        
        // Extract unique tags
        const tweetTagSet = new Set();
        const authorTagSet = new Set();
        
        // Add predefined tags
        const predefinedTweetTags = ['Important', 'Funny', 'Insightful', 'News', 'Personal'];
        const predefinedAuthorTags = ['Verified', 'Celebrity', 'Friend', 'Organization', 'Media'];
        
        predefinedTweetTags.forEach((tag, index) => {
          tweetTagSet.add({ id: index + 1, name: tag, type: 'tweet', is_predefined: 1 });
        });
        
        predefinedAuthorTags.forEach((tag, index) => {
          authorTagSet.add({ id: index + 100, name: tag, type: 'author', is_predefined: 1 });
        });
        
        // Add tags from tweets
        data.tweets.forEach(tweet => {
          if (tweet.tags) {
            tweet.tags.forEach(tagName => {
              if (!predefinedTweetTags.includes(tagName)) {
                const tagId = tweetTagSet.size + 1 + authorTagSet.size;
                tweetTagSet.add({ id: tagId, name: tagName, type: 'tweet', is_predefined: 0 });
              }
            });
          }
        });
        
        setTweetTags(Array.from(tweetTagSet));
        setAuthorTags(Array.from(authorTagSet));
        
        console.log('Mock data loaded successfully');
      } catch (error) {
        console.error('Error loading mock data:', error);
      }
    };
    
    loadMockData();
  }, []);

  // Handler for importing data
  const handleImportData = (data, importType) => {
    console.log(`Importing ${importType} data:`, data);
    // In a real app, this would call the database import functions
    
    if (importType === 'json') {
      // Process the imported JSON data
      if (data.authors) {
        setAuthors(prevAuthors => {
          const newAuthors = [...prevAuthors];
          data.authors.forEach(author => {
            const existingIndex = newAuthors.findIndex(a => a.id === author.id);
            if (existingIndex >= 0) {
              newAuthors[existingIndex] = { ...author, tags: newAuthors[existingIndex].tags || [] };
            } else {
              newAuthors.push({ ...author, tags: [] });
            }
          });
          return newAuthors;
        });
      }
      
      if (data.tweets) {
        setTweets(prevTweets => {
          const newTweets = [...prevTweets];
          data.tweets.forEach(tweet => {
            const existingIndex = newTweets.findIndex(t => t.id === tweet.id);
            if (existingIndex >= 0) {
              newTweets[existingIndex] = tweet;
            } else {
              newTweets.push(tweet);
            }
          });
          return newTweets;
        });
      }
    } else if (importType === 'csv') {
      // Process CSV data (simplified for demo)
      alert('CSV import functionality would be implemented in the full version');
    }
  };

  // Tag management handlers
  const handleAddTag = (name, type) => {
    const newTag = {
      id: type === 'tweet' ? tweetTags.length + 1 : authorTags.length + 1000,
      name,
      type,
      is_predefined: 0
    };
    
    if (type === 'tweet') {
      setTweetTags(prev => [...prev, newTag]);
    } else {
      setAuthorTags(prev => [...prev, newTag]);
    }
  };
  
  const handleDeleteTag = (id, type) => {
    if (type === 'tweet') {
      setTweetTags(prev => prev.filter(tag => tag.id !== id));
      
      // Remove this tag from all tweets
      setTweets(prev => prev.map(tweet => ({
        ...tweet,
        tags: tweet.tags ? tweet.tags.filter(tagName => 
          !tweetTags.find(t => t.id === id)?.name === tagName
        ) : []
      })));
    } else {
      setAuthorTags(prev => prev.filter(tag => tag.id !== id));
      
      // Remove this tag from all authors
      setAuthors(prev => prev.map(author => ({
        ...author,
        tags: author.tags ? author.tags.filter(tagName => 
          !authorTags.find(t => t.id === id)?.name === tagName
        ) : []
      })));
    }
  };

  // Tweet action handlers
  const handleTagTweet = (tweetId, tagName) => {
    // Find or create the tag
    let tagObj = tweetTags.find(t => t.name === tagName);
    if (!tagObj) {
      tagObj = {
        id: tweetTags.length + 1,
        name: tagName,
        type: 'tweet',
        is_predefined: 0
      };
      setTweetTags(prev => [...prev, tagObj]);
    }
    
    // Add tag to the tweet
    setTweets(prev => prev.map(tweet => {
      if (tweet.id === tweetId) {
        const currentTags = tweet.tags || [];
        if (!currentTags.includes(tagName)) {
          return { ...tweet, tags: [...currentTags, tagName] };
        }
      }
      return tweet;
    }));
  };
  
  const handleFlagTweet = (tweetId, flagType) => {
    setTweets(prev => prev.map(tweet => {
      if (tweet.id === tweetId) {
        // Toggle flag if it's already the same type
        if (tweet.flag_type === flagType) {
          return { ...tweet, is_flagged: 0, flag_type: null };
        } else {
          return { ...tweet, is_flagged: 1, flag_type: flagType };
        }
      }
      return tweet;
    }));
  };
  
  const handleBookmarkTweet = (tweetId, note) => {
    setTweets(prev => prev.map(tweet => {
      if (tweet.id === tweetId) {
        if (note === null && tweet.is_bookmarked) {
          // Remove bookmark
          return { ...tweet, is_bookmarked: 0, bookmark_note: null };
        } else {
          // Add or update bookmark
          return { ...tweet, is_bookmarked: 1, bookmark_note: note };
        }
      }
      return tweet;
    }));
  };

  // Author action handlers
  const handleMuteAuthor = (authorId) => {
    setAuthors(prev => prev.map(author => {
      if (author.id === authorId) {
        return { ...author, is_muted: author.is_muted ? 0 : 1 };
      }
      return author;
    }));
  };
  
  const handleBlockAuthor = (authorId) => {
    setAuthors(prev => prev.map(author => {
      if (author.id === authorId) {
        return { ...author, is_blocked: author.is_blocked ? 0 : 1 };
      }
      return author;
    }));
  };
  
  const handleTagAuthor = (authorId, tagName) => {
    // Find or create the tag
    let tagObj = authorTags.find(t => t.name === tagName);
    if (!tagObj) {
      tagObj = {
        id: authorTags.length + 1000,
        name: tagName,
        type: 'author',
        is_predefined: 0
      };
      setAuthorTags(prev => [...prev, tagObj]);
    }
    
    // Add tag to the author
    setAuthors(prev => prev.map(author => {
      if (author.id === authorId) {
        const currentTags = author.tags || [];
        if (!currentTags.includes(tagName)) {
          return { ...author, tags: [...currentTags, tagName] };
        }
      }
      return author;
    }));
  };

  return (
    <div className="App terminal-effect">
      <Navbar 
        onImportData={handleImportData}
        tweetTags={tweetTags}
        authorTags={authorTags}
        onAddTag={handleAddTag}
        onDeleteTag={handleDeleteTag}
        authors={authors}
        onMuteAuthor={handleMuteAuthor}
        onBlockAuthor={handleBlockAuthor}
        onTagAuthor={handleTagAuthor}
        tweets={tweets}
        onRemoveBookmark={(id) => handleBookmarkTweet(id, null)}
        onUpdateBookmarkNote={handleBookmarkTweet}
      />
      
      <Container>
        <TweetList 
          tweets={tweets}
          authors={authors}
          tags={tweetTags}
          onTagTweet={handleTagTweet}
          onFlagTweet={handleFlagTweet}
          onBookmarkTweet={handleBookmarkTweet}
          onMuteAuthor={handleMuteAuthor}
          onBlockAuthor={handleBlockAuthor}
        />
      </Container>
    </div>
  );
}

export default App;
