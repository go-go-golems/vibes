import React, { useState } from 'react';
import { Modal, Button, Form, ListGroup, Badge } from 'react-bootstrap';
import { FaBookmark, FaRetweet, FaReply } from 'react-icons/fa';

const BookmarksModal = ({ show, onHide, tweets, authors, onRemoveBookmark, onUpdateBookmarkNote }) => {
  const [searchText, setSearchText] = useState('');
  const [selectedTweet, setSelectedTweet] = useState(null);
  const [bookmarkNote, setBookmarkNote] = useState('');
  
  // Filter bookmarked tweets
  const bookmarkedTweets = tweets.filter(tweet => 
    tweet.is_bookmarked && 
    (!searchText || 
      tweet.content.toLowerCase().includes(searchText.toLowerCase()) ||
      authors.find(a => a.id === tweet.author_id)?.username.toLowerCase().includes(searchText.toLowerCase()) ||
      authors.find(a => a.id === tweet.author_id)?.display_name.toLowerCase().includes(searchText.toLowerCase()) ||
      tweet.bookmark_note?.toLowerCase().includes(searchText.toLowerCase())
    )
  );
  
  const handleSelectTweet = (tweet) => {
    setSelectedTweet(tweet);
    setBookmarkNote(tweet.bookmark_note || '');
  };
  
  const handleUpdateNote = () => {
    if (selectedTweet) {
      onUpdateBookmarkNote(selectedTweet.id, bookmarkNote);
      // Update the local state to reflect the change
      setSelectedTweet({
        ...selectedTweet,
        bookmark_note: bookmarkNote
      });
    }
  };
  
  // Format date for display
  const formatDate = (dateString) => {
    const date = new Date(dateString);
    return date.toLocaleString();
  };
  
  return (
    <Modal show={show} onHide={onHide} centered size="lg">
      <Modal.Header closeButton>
        <Modal.Title>Bookmarked Tweets</Modal.Title>
      </Modal.Header>
      <Modal.Body>
        <Form.Group className="mb-3">
          <Form.Control
            type="text"
            placeholder="Search bookmarks..."
            value={searchText}
            onChange={(e) => setSearchText(e.target.value)}
          />
        </Form.Group>
        
        <div className="row">
          <div className="col-md-6">
            <h5>Bookmarks</h5>
            <ListGroup className="bookmark-list">
              {bookmarkedTweets.length > 0 ? (
                bookmarkedTweets.map(tweet => (
                  <ListGroup.Item 
                    key={tweet.id}
                    action
                    active={selectedTweet && selectedTweet.id === tweet.id}
                    onClick={() => handleSelectTweet(tweet)}
                  >
                    <div className="d-flex justify-content-between">
                      <small className="text-muted">
                        @{authors.find(a => a.id === tweet.author_id)?.username}
                      </small>
                      <small className="text-muted">
                        {formatDate(tweet.timestamp)}
                      </small>
                    </div>
                    <p className="mb-1 tweet-content-preview">
                      {tweet.content.length > 100 
                        ? tweet.content.substring(0, 100) + '...' 
                        : tweet.content}
                    </p>
                    {tweet.bookmark_note && (
                      <div className="bookmark-note-preview">
                        <small className="text-warning">
                          <FaBookmark className="me-1" />
                          {tweet.bookmark_note.length > 50 
                            ? tweet.bookmark_note.substring(0, 50) + '...' 
                            : tweet.bookmark_note}
                        </small>
                      </div>
                    )}
                  </ListGroup.Item>
                ))
              ) : (
                <ListGroup.Item className="text-center text-muted">
                  No bookmarked tweets found
                </ListGroup.Item>
              )}
            </ListGroup>
          </div>
          
          <div className="col-md-6">
            {selectedTweet ? (
              <div>
                <h5>Tweet Details</h5>
                <div className="card mb-3">
                  <div className="card-body">
                    <div className="d-flex justify-content-between mb-2">
                      <h6 className="card-subtitle text-muted">
                        @{authors.find(a => a.id === selectedTweet.author_id)?.username}
                      </h6>
                      <small className="text-muted">
                        {formatDate(selectedTweet.timestamp)}
                      </small>
                    </div>
                    
                    <p className="card-text">{selectedTweet.content}</p>
                    
                    {selectedTweet.tags && selectedTweet.tags.length > 0 && (
                      <div className="d-flex flex-wrap mb-2">
                        {selectedTweet.tags.map((tag, index) => (
                          <Badge bg="secondary" className="me-1 mb-1" key={index}>{tag}</Badge>
                        ))}
                      </div>
                    )}
                    
                    <div className="d-flex justify-content-between text-muted mb-3">
                      <span>❤️ {selectedTweet.likes}</span>
                      <span>🔄 {selectedTweet.retweets}</span>
                      <span>💬 {selectedTweet.quotes}</span>
                      <span>↩️ {selectedTweet.replies}</span>
                    </div>
                    
                    <Form.Group className="mb-3">
                      <Form.Label>Bookmark Note</Form.Label>
                      <Form.Control
                        as="textarea"
                        rows={3}
                        placeholder="Why is this tweet important?"
                        value={bookmarkNote}
                        onChange={(e) => setBookmarkNote(e.target.value)}
                      />
                    </Form.Group>
                    
                    <div className="d-flex justify-content-between">
                      <Button 
                        variant="outline-warning" 
                        onClick={handleUpdateNote}
                      >
                        Update Note
                      </Button>
                      <Button 
                        variant="outline-danger" 
                        onClick={() => {
                          onRemoveBookmark(selectedTweet.id);
                          setSelectedTweet(null);
                        }}
                      >
                        Remove Bookmark
                      </Button>
                    </div>
                    
                    {selectedTweet.flag_type && (
                      <div className="mt-3">
                        <Badge bg="info" className="me-1">
                          {selectedTweet.flag_type === 'reply' ? (
                            <><FaReply className="me-1" /> Flagged for Reply</>
                          ) : (
                            <><FaRetweet className="me-1" /> Flagged for Quote</>
                          )}
                        </Badge>
                      </div>
                    )}
                  </div>
                </div>
              </div>
            ) : (
              <div className="text-center text-muted mt-5">
                <p>Select a bookmarked tweet to view details</p>
              </div>
            )}
          </div>
        </div>
      </Modal.Body>
      <Modal.Footer>
        <Button variant="secondary" onClick={onHide}>
          Close
        </Button>
      </Modal.Footer>
    </Modal>
  );
};

export default BookmarksModal;
