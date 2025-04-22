import React, { useState } from 'react';
import { Modal, Button, Form, ListGroup, Badge } from 'react-bootstrap';
import { FaTag, FaPlus } from 'react-icons/fa';

const TagManagementModal = ({ show, onHide, tweetTags, authorTags, onAddTag, onDeleteTag }) => {
  const [newTagName, setNewTagName] = useState('');
  const [newTagType, setNewTagType] = useState('tweet');
  const [activeTab, setActiveTab] = useState('tweet');
  
  const handleAddTag = () => {
    if (newTagName.trim()) {
      onAddTag(newTagName.trim(), newTagType);
      setNewTagName('');
    }
  };
  
  return (
    <Modal show={show} onHide={onHide} centered>
      <Modal.Header closeButton>
        <Modal.Title>Manage Tags</Modal.Title>
      </Modal.Header>
      <Modal.Body>
        <Form.Group className="mb-3">
          <Form.Label>Add New Tag</Form.Label>
          <div className="d-flex">
            <Form.Control
              type="text"
              placeholder="Enter tag name"
              value={newTagName}
              onChange={(e) => setNewTagName(e.target.value)}
              onKeyPress={(e) => e.key === 'Enter' && handleAddTag()}
            />
            <Form.Select 
              className="ms-2 w-50"
              value={newTagType}
              onChange={(e) => setNewTagType(e.target.value)}
            >
              <option value="tweet">Tweet Tag</option>
              <option value="author">Author Tag</option>
            </Form.Select>
            <Button 
              variant="outline-primary" 
              className="ms-2"
              onClick={handleAddTag}
              disabled={!newTagName.trim()}
            >
              <FaPlus />
            </Button>
          </div>
        </Form.Group>
        
        <div className="d-flex mb-3">
          <Button 
            variant={activeTab === 'tweet' ? 'primary' : 'outline-primary'} 
            className="me-2 w-50"
            onClick={() => setActiveTab('tweet')}
          >
            Tweet Tags
          </Button>
          <Button 
            variant={activeTab === 'author' ? 'primary' : 'outline-primary'} 
            className="w-50"
            onClick={() => setActiveTab('author')}
          >
            Author Tags
          </Button>
        </div>
        
        <ListGroup>
          {activeTab === 'tweet' ? (
            tweetTags.length > 0 ? (
              tweetTags.map((tag, index) => (
                <ListGroup.Item 
                  key={index}
                  className="d-flex justify-content-between align-items-center"
                >
                  <div>
                    <FaTag className="me-2" />
                    {tag.name}
                    {tag.is_predefined ? (
                      <Badge bg="secondary" className="ms-2">Predefined</Badge>
                    ) : null}
                  </div>
                  {!tag.is_predefined && (
                    <Button 
                      variant="outline-danger" 
                      size="sm"
                      onClick={() => onDeleteTag(tag.id, 'tweet')}
                    >
                      Delete
                    </Button>
                  )}
                </ListGroup.Item>
              ))
            ) : (
              <ListGroup.Item className="text-center text-muted">
                No tweet tags found
              </ListGroup.Item>
            )
          ) : (
            authorTags.length > 0 ? (
              authorTags.map((tag, index) => (
                <ListGroup.Item 
                  key={index}
                  className="d-flex justify-content-between align-items-center"
                >
                  <div>
                    <FaTag className="me-2" />
                    {tag.name}
                    {tag.is_predefined ? (
                      <Badge bg="secondary" className="ms-2">Predefined</Badge>
                    ) : null}
                  </div>
                  {!tag.is_predefined && (
                    <Button 
                      variant="outline-danger" 
                      size="sm"
                      onClick={() => onDeleteTag(tag.id, 'author')}
                    >
                      Delete
                    </Button>
                  )}
                </ListGroup.Item>
              ))
            ) : (
              <ListGroup.Item className="text-center text-muted">
                No author tags found
              </ListGroup.Item>
            )
          )}
        </ListGroup>
      </Modal.Body>
      <Modal.Footer>
        <Button variant="secondary" onClick={onHide}>
          Close
        </Button>
      </Modal.Footer>
    </Modal>
  );
};

export default TagManagementModal;
