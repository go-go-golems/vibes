import React, { useState, useEffect } from 'react';
import { Container, Row, Col, Card, Badge, Button, Form, InputGroup } from 'react-bootstrap';
import { FaBookmark, FaRetweet, FaReply, FaTag, FaVolumeMute, FaBan, FaSearch } from 'react-icons/fa';

// Tweet component to display individual tweets
const Tweet = ({ tweet, author, onTagTweet, onFlagTweet, onBookmarkTweet, onMuteAuthor, onBlockAuthor }) => {
  const [showTagInput, setShowTagInput] = useState(false);
  const [newTag, setNewTag] = useState('');
  const [bookmarkNote, setBookmarkNote] = useState(tweet.bookmark_note || '');
  const [showBookmarkInput, setShowBookmarkInput] = useState(false);

  // Format date for display
  const formatDate = (dateString) => {
    const date = new Date(dateString);
    return date.toLocaleString();
  };

  // Handle adding a new tag
  const handleAddTag = () => {
    if (newTag.trim()) {
      onTagTweet(tweet.id, newTag.trim());
      setNewTag('');
      setShowTagInput(false);
    }
  };

  // Handle bookmarking with note
  const handleBookmark = () => {
    onBookmarkTweet(tweet.id, bookmarkNote);
    setShowBookmarkInput(false);
  };

  return (
    <Card className="mb-3 tweet-card">
      <Card.Header className="d-flex justify-content-between align-items-center">
        <div className="d-flex align-items-center">
          <img 
            src={author.profile_image} 
            alt={author.display_name} 
            className="rounded-circle me-2" 
            width="40" 
            height="40" 
          />
          <div>
            <div className="fw-bold">{author.display_name}</div>
            <div className="text-muted">@{author.username}</div>
          </div>
        </div>
        <div>
          <Button 
            variant="outline-secondary" 
            size="sm" 
            className="me-1" 
            onClick={() => onMuteAuthor(author.id)}
            title={author.is_muted ? "Unmute author" : "Mute author"}
          >
            <FaVolumeMute className={author.is_muted ? "text-danger" : ""} />
          </Button>
          <Button 
            variant="outline-secondary" 
            size="sm" 
            onClick={() => onBlockAuthor(author.id)}
            title={author.is_blocked ? "Unblock author" : "Block author"}
          >
            <FaBan className={author.is_blocked ? "text-danger" : ""} />
          </Button>
        </div>
      </Card.Header>
      <Card.Body>
        <Card.Text>{tweet.content}</Card.Text>
        <div className="d-flex flex-wrap mb-2">
          {tweet.tags && tweet.tags.map((tag, index) => (
            <Badge bg="secondary" className="me-1 mb-1" key={index}>{tag}</Badge>
          ))}
        </div>
        {showTagInput ? (
          <InputGroup className="mb-3">
            <Form.Control
              placeholder="Add tag"
              value={newTag}
              onChange={(e) => setNewTag(e.target.value)}
              onKeyPress={(e) => e.key === 'Enter' && handleAddTag()}
            />
            <Button variant="outline-secondary" onClick={handleAddTag}>Add</Button>
            <Button variant="outline-secondary" onClick={() => setShowTagInput(false)}>Cancel</Button>
          </InputGroup>
        ) : null}
        {showBookmarkInput ? (
          <InputGroup className="mb-3">
            <Form.Control
              placeholder="Bookmark note"
              value={bookmarkNote}
              onChange={(e) => setBookmarkNote(e.target.value)}
              onKeyPress={(e) => e.key === 'Enter' && handleBookmark()}
            />
            <Button variant="outline-secondary" onClick={handleBookmark}>Save</Button>
            <Button variant="outline-secondary" onClick={() => setShowBookmarkInput(false)}>Cancel</Button>
          </InputGroup>
        ) : null}
      </Card.Body>
      <Card.Footer className="text-muted">
        <div className="d-flex justify-content-between align-items-center">
          <small>{formatDate(tweet.timestamp)}</small>
          <div>
            <span className="me-2" title="Likes">❤️ {tweet.likes}</span>
            <span className="me-2" title="Retweets">🔄 {tweet.retweets}</span>
            <span className="me-2" title="Quotes">💬 {tweet.quotes}</span>
            <span title="Replies">↩️ {tweet.replies}</span>
          </div>
        </div>
        <div className="d-flex justify-content-between mt-2">
          <div>
            <Button 
              variant="outline-secondary" 
              size="sm" 
              className="me-1"
              onClick={() => setShowTagInput(!showTagInput)}
              title="Add tag"
            >
              <FaTag />
            </Button>
            <Button 
              variant="outline-secondary" 
              size="sm" 
              className="me-1"
              onClick={() => onFlagTweet(tweet.id, 'reply')}
              title="Flag for reply"
              className={tweet.flag_type === 'reply' ? "active" : ""}
            >
              <FaReply />
            </Button>
            <Button 
              variant="outline-secondary" 
              size="sm" 
              className="me-1"
              onClick={() => onFlagTweet(tweet.id, 'quote')}
              title="Flag for quote tweet"
              className={tweet.flag_type === 'quote' ? "active" : ""}
            >
              <FaRetweet />
            </Button>
          </div>
          <Button 
            variant="outline-secondary" 
            size="sm"
            onClick={() => {
              if (tweet.is_bookmarked) {
                onBookmarkTweet(tweet.id, null);
              } else {
                setShowBookmarkInput(!showBookmarkInput);
              }
            }}
            title={tweet.is_bookmarked ? "Remove bookmark" : "Bookmark tweet"}
            className={tweet.is_bookmarked ? "active" : ""}
          >
            <FaBookmark className={tweet.is_bookmarked ? "text-warning" : ""} />
          </Button>
        </div>
      </Card.Footer>
    </Card>
  );
};

export default Tweet;
