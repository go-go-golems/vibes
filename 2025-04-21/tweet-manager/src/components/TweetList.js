import React, { useState, useEffect } from 'react';
import { Container, Row, Col, Form, InputGroup, Button, Dropdown } from 'react-bootstrap';
import { FaSearch, FaFilter } from 'react-icons/fa';
import Tweet from './Tweet';

const TweetList = ({ tweets, authors, tags, onTagTweet, onFlagTweet, onBookmarkTweet, onMuteAuthor, onBlockAuthor }) => {
  const [filteredTweets, setFilteredTweets] = useState([]);
  const [searchText, setSearchText] = useState('');
  const [selectedTag, setSelectedTag] = useState(null);
  const [hideBlocked, setHideBlocked] = useState(true);
  const [hideMuted, setHideMuted] = useState(true);
  const [showBookmarked, setShowBookmarked] = useState(false);
  const [showFlagged, setShowFlagged] = useState(false);

  // Apply filters whenever filter criteria change
  useEffect(() => {
    let filtered = [...tweets];
    
    // Filter by search text
    if (searchText) {
      const searchLower = searchText.toLowerCase();
      filtered = filtered.filter(tweet => 
        tweet.content.toLowerCase().includes(searchLower) || 
        authors.find(a => a.id === tweet.author_id)?.username.toLowerCase().includes(searchLower) ||
        authors.find(a => a.id === tweet.author_id)?.display_name.toLowerCase().includes(searchLower)
      );
    }
    
    // Filter by tag
    if (selectedTag) {
      filtered = filtered.filter(tweet => 
        tweet.tags && tweet.tags.includes(selectedTag)
      );
    }
    
    // Filter by author status
    if (hideBlocked) {
      filtered = filtered.filter(tweet => 
        !authors.find(a => a.id === tweet.author_id)?.is_blocked
      );
    }
    
    if (hideMuted) {
      filtered = filtered.filter(tweet => 
        !authors.find(a => a.id === tweet.author_id)?.is_muted
      );
    }
    
    // Filter by tweet status
    if (showBookmarked) {
      filtered = filtered.filter(tweet => tweet.is_bookmarked);
    }
    
    if (showFlagged) {
      filtered = filtered.filter(tweet => tweet.is_flagged);
    }
    
    setFilteredTweets(filtered);
  }, [tweets, authors, searchText, selectedTag, hideBlocked, hideMuted, showBookmarked, showFlagged]);

  // Get all unique tags from tweets
  const getAllTags = () => {
    const tagSet = new Set();
    tweets.forEach(tweet => {
      if (tweet.tags) {
        tweet.tags.forEach(tag => tagSet.add(tag));
      }
    });
    return Array.from(tagSet);
  };

  return (
    <Container>
      <Row className="mb-4">
        <Col>
          <InputGroup>
            <InputGroup.Text>
              <FaSearch />
            </InputGroup.Text>
            <Form.Control
              placeholder="Search tweets or authors..."
              value={searchText}
              onChange={(e) => setSearchText(e.target.value)}
            />
            <Dropdown>
              <Dropdown.Toggle variant="outline-secondary">
                <FaFilter /> Filters
              </Dropdown.Toggle>
              <Dropdown.Menu>
                <Dropdown.Header>Tags</Dropdown.Header>
                <Dropdown.Item 
                  active={selectedTag === null}
                  onClick={() => setSelectedTag(null)}
                >
                  All Tags
                </Dropdown.Item>
                {getAllTags().map((tag, index) => (
                  <Dropdown.Item 
                    key={index} 
                    active={selectedTag === tag}
                    onClick={() => setSelectedTag(tag)}
                  >
                    {tag}
                  </Dropdown.Item>
                ))}
                <Dropdown.Divider />
                <Dropdown.Header>Author Status</Dropdown.Header>
                <Dropdown.Item onClick={() => setHideBlocked(!hideBlocked)}>
                  {hideBlocked ? '✓ Hide Blocked' : '○ Show Blocked'}
                </Dropdown.Item>
                <Dropdown.Item onClick={() => setHideMuted(!hideMuted)}>
                  {hideMuted ? '✓ Hide Muted' : '○ Show Muted'}
                </Dropdown.Item>
                <Dropdown.Divider />
                <Dropdown.Header>Tweet Status</Dropdown.Header>
                <Dropdown.Item onClick={() => setShowBookmarked(!showBookmarked)}>
                  {showBookmarked ? '✓ Only Bookmarked' : '○ All Tweets'}
                </Dropdown.Item>
                <Dropdown.Item onClick={() => setShowFlagged(!showFlagged)}>
                  {showFlagged ? '✓ Only Flagged' : '○ All Tweets'}
                </Dropdown.Item>
              </Dropdown.Menu>
            </Dropdown>
          </InputGroup>
        </Col>
      </Row>
      
      <Row>
        <Col>
          {filteredTweets.length === 0 ? (
            <div className="text-center my-5">
              <h4>No tweets match your filters</h4>
              <Button variant="link" onClick={() => {
                setSearchText('');
                setSelectedTag(null);
                setHideBlocked(true);
                setHideMuted(true);
                setShowBookmarked(false);
                setShowFlagged(false);
              }}>
                Clear all filters
              </Button>
            </div>
          ) : (
            filteredTweets.map(tweet => (
              <Tweet
                key={tweet.id}
                tweet={tweet}
                author={authors.find(a => a.id === tweet.author_id)}
                onTagTweet={onTagTweet}
                onFlagTweet={onFlagTweet}
                onBookmarkTweet={onBookmarkTweet}
                onMuteAuthor={onMuteAuthor}
                onBlockAuthor={onBlockAuthor}
              />
            ))
          )}
        </Col>
      </Row>
    </Container>
  );
};

export default TweetList;
