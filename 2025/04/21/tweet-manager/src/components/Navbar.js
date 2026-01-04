import React, { useState } from 'react';
import { Navbar, Nav, Container, Button } from 'react-bootstrap';
import { FaTwitter, FaUpload, FaTag, FaUsers, FaBookmark } from 'react-icons/fa';
import ImportModal from './ImportModal';
import TagManagementModal from './TagManagementModal';
import AuthorManagementModal from './AuthorManagementModal';
import BookmarksModal from './BookmarksModal';

const AppNavbar = ({ 
  onImportData, 
  tweetTags, 
  authorTags, 
  onAddTag, 
  onDeleteTag,
  authors,
  onMuteAuthor,
  onBlockAuthor,
  onTagAuthor,
  tweets,
  onRemoveBookmark,
  onUpdateBookmarkNote
}) => {
  const [showImportModal, setShowImportModal] = useState(false);
  const [showTagModal, setShowTagModal] = useState(false);
  const [showAuthorModal, setShowAuthorModal] = useState(false);
  const [showBookmarksModal, setShowBookmarksModal] = useState(false);

  return (
    <>
      <Navbar expand="lg" className="mb-4">
        <Container>
          <Navbar.Brand href="#home" className="cursor-blink">
            <FaTwitter className="me-2" />
            PDP11 Tweet Manager
          </Navbar.Brand>
          <Navbar.Toggle aria-controls="basic-navbar-nav" />
          <Navbar.Collapse id="basic-navbar-nav">
            <Nav className="ms-auto">
              <Button 
                variant="outline-light" 
                className="me-2"
                onClick={() => setShowImportModal(true)}
              >
                <FaUpload className="me-1" /> Import
              </Button>
              <Button 
                variant="outline-light" 
                className="me-2"
                onClick={() => setShowTagModal(true)}
              >
                <FaTag className="me-1" /> Tags
              </Button>
              <Button 
                variant="outline-light" 
                className="me-2"
                onClick={() => setShowAuthorModal(true)}
              >
                <FaUsers className="me-1" /> Authors
              </Button>
              <Button 
                variant="outline-light"
                onClick={() => setShowBookmarksModal(true)}
              >
                <FaBookmark className="me-1" /> Bookmarks
              </Button>
            </Nav>
          </Navbar.Collapse>
        </Container>
      </Navbar>

      {/* Import Modal */}
      <ImportModal 
        show={showImportModal} 
        onHide={() => setShowImportModal(false)} 
        onImport={onImportData}
      />

      {/* Tag Management Modal */}
      <TagManagementModal 
        show={showTagModal} 
        onHide={() => setShowTagModal(false)} 
        tweetTags={tweetTags}
        authorTags={authorTags}
        onAddTag={onAddTag}
        onDeleteTag={onDeleteTag}
      />

      {/* Author Management Modal */}
      <AuthorManagementModal 
        show={showAuthorModal} 
        onHide={() => setShowAuthorModal(false)} 
        authors={authors}
        onMuteAuthor={onMuteAuthor}
        onBlockAuthor={onBlockAuthor}
        onTagAuthor={onTagAuthor}
      />

      {/* Bookmarks Modal */}
      <BookmarksModal 
        show={showBookmarksModal} 
        onHide={() => setShowBookmarksModal(false)} 
        tweets={tweets}
        authors={authors}
        onRemoveBookmark={onRemoveBookmark}
        onUpdateBookmarkNote={onUpdateBookmarkNote}
      />
    </>
  );
};

export default AppNavbar;
