import React, { useState } from 'react';
import { Modal, Button, Form, ListGroup } from 'react-bootstrap';
import { FaUser, FaVolumeMute, FaBan, FaTag } from 'react-icons/fa';

const AuthorManagementModal = ({ show, onHide, authors, onMuteAuthor, onBlockAuthor, onTagAuthor }) => {
  const [searchText, setSearchText] = useState('');
  const [showMuted, setShowMuted] = useState(false);
  const [showBlocked, setShowBlocked] = useState(false);
  const [selectedAuthor, setSelectedAuthor] = useState(null);
  const [newTag, setNewTag] = useState('');
  
  // Filter authors based on search and filters
  const filteredAuthors = authors.filter(author => {
    const matchesSearch = !searchText || 
      author.username.toLowerCase().includes(searchText.toLowerCase()) ||
      author.display_name.toLowerCase().includes(searchText.toLowerCase());
    
    const matchesFilter = 
      (!showMuted && !showBlocked) || 
      (showMuted && author.is_muted) || 
      (showBlocked && author.is_blocked);
    
    return matchesSearch && matchesFilter;
  });
  
  const handleAddTag = () => {
    if (selectedAuthor && newTag.trim()) {
      onTagAuthor(selectedAuthor.id, newTag.trim());
      setNewTag('');
    }
  };
  
  return (
    <Modal show={show} onHide={onHide} centered size="lg">
      <Modal.Header closeButton>
        <Modal.Title>Manage Authors</Modal.Title>
      </Modal.Header>
      <Modal.Body>
        <Form.Group className="mb-3">
          <Form.Control
            type="text"
            placeholder="Search authors..."
            value={searchText}
            onChange={(e) => setSearchText(e.target.value)}
          />
        </Form.Group>
        
        <div className="d-flex mb-3">
          <Form.Check 
            type="checkbox"
            id="show-muted"
            label="Show Muted"
            checked={showMuted}
            onChange={() => setShowMuted(!showMuted)}
            className="me-3"
          />
          <Form.Check 
            type="checkbox"
            id="show-blocked"
            label="Show Blocked"
            checked={showBlocked}
            onChange={() => setShowBlocked(!showBlocked)}
          />
        </div>
        
        <div className="row">
          <div className="col-md-6">
            <h5>Authors</h5>
            <ListGroup className="author-list">
              {filteredAuthors.length > 0 ? (
                filteredAuthors.map(author => (
                  <ListGroup.Item 
                    key={author.id}
                    action
                    active={selectedAuthor && selectedAuthor.id === author.id}
                    onClick={() => setSelectedAuthor(author)}
                    className="d-flex justify-content-between align-items-center"
                  >
                    <div>
                      <FaUser className="me-2" />
                      <strong>{author.display_name}</strong>
                      <small className="ms-2 text-muted">@{author.username}</small>
                    </div>
                    <div>
                      {author.is_muted && <FaVolumeMute className="text-danger me-2" title="Muted" />}
                      {author.is_blocked && <FaBan className="text-danger" title="Blocked" />}
                    </div>
                  </ListGroup.Item>
                ))
              ) : (
                <ListGroup.Item className="text-center text-muted">
                  No authors found
                </ListGroup.Item>
              )}
            </ListGroup>
          </div>
          
          <div className="col-md-6">
            {selectedAuthor ? (
              <div>
                <h5>Author Details</h5>
                <div className="card mb-3">
                  <div className="card-body">
                    <h5 className="card-title">{selectedAuthor.display_name}</h5>
                    <h6 className="card-subtitle mb-2 text-muted">@{selectedAuthor.username}</h6>
                    
                    <div className="d-flex mb-3 mt-3">
                      <Button 
                        variant={selectedAuthor.is_muted ? "danger" : "outline-danger"}
                        size="sm"
                        className="me-2"
                        onClick={() => onMuteAuthor(selectedAuthor.id)}
                      >
                        <FaVolumeMute className="me-1" />
                        {selectedAuthor.is_muted ? "Unmute" : "Mute"}
                      </Button>
                      <Button 
                        variant={selectedAuthor.is_blocked ? "danger" : "outline-danger"}
                        size="sm"
                        onClick={() => onBlockAuthor(selectedAuthor.id)}
                      >
                        <FaBan className="me-1" />
                        {selectedAuthor.is_blocked ? "Unblock" : "Block"}
                      </Button>
                    </div>
                    
                    <h6>Tags</h6>
                    {selectedAuthor.tags && selectedAuthor.tags.length > 0 ? (
                      <div className="d-flex flex-wrap mb-2">
                        {selectedAuthor.tags.map((tag, index) => (
                          <span className="badge bg-secondary me-1 mb-1" key={index}>
                            {tag}
                          </span>
                        ))}
                      </div>
                    ) : (
                      <p className="text-muted">No tags</p>
                    )}
                    
                    <div className="input-group mt-3">
                      <input
                        type="text"
                        className="form-control"
                        placeholder="Add tag"
                        value={newTag}
                        onChange={(e) => setNewTag(e.target.value)}
                        onKeyPress={(e) => e.key === 'Enter' && handleAddTag()}
                      />
                      <button 
                        className="btn btn-outline-secondary" 
                        type="button"
                        onClick={handleAddTag}
                        disabled={!newTag.trim()}
                      >
                        <FaTag />
                      </button>
                    </div>
                  </div>
                </div>
              </div>
            ) : (
              <div className="text-center text-muted mt-5">
                <p>Select an author to view details</p>
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

export default AuthorManagementModal;
