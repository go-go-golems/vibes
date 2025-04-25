package model

import (
	"fmt"
	"path/filepath"
	"sync"

	"github.com/user/ttmp-go/pkg/errors"
)

// TTMPCollection represents a collection of TTMP documents
type TTMPCollection struct {
	Documents      []*TTMPDocument
	DocumentMap    map[string]*TTMPDocument
	mu             sync.RWMutex
}

// NewCollection creates a new TTMPCollection
func NewCollection() *TTMPCollection {
	return &TTMPCollection{
		Documents:   make([]*TTMPDocument, 0),
		DocumentMap: make(map[string]*TTMPDocument),
	}
}

// AddDocument adds a document to the collection
func (c *TTMPCollection) AddDocument(doc *TTMPDocument) error {
	c.mu.Lock()
	defer c.mu.Unlock()

	// Check if a document with the same ID already exists
	if _, exists := c.DocumentMap[doc.ID]; exists {
		return errors.NewValidationError(
			fmt.Sprintf("Document with ID '%s' already exists in the collection", doc.ID),
		)
	}

	// Add the document
	c.Documents = append(c.Documents, doc)
	c.DocumentMap[doc.ID] = doc

	return nil
}

// GetDocumentByID retrieves a document by ID
func (c *TTMPCollection) GetDocumentByID(id string) (*TTMPDocument, error) {
	c.mu.RLock()
	defer c.mu.RUnlock()

	doc, exists := c.DocumentMap[id]
	if !exists {
		return nil, errors.NewQueryError(
			fmt.Sprintf("Document with ID '%s' not found", id),
			nil,
		)
	}

	return doc, nil
}

// GetDocumentsByType retrieves documents by type
func (c *TTMPCollection) GetDocumentsByType(docType string) []*TTMPDocument {
	c.mu.RLock()
	defer c.mu.RUnlock()

	results := make([]*TTMPDocument, 0)
	for _, doc := range c.Documents {
		if doc.Type == docType {
			results = append(results, doc)
		}
	}

	return results
}

// GetDocumentsByTag retrieves documents with a specific tag
func (c *TTMPCollection) GetDocumentsByTag(tag string) []*TTMPDocument {
	c.mu.RLock()
	defer c.mu.RUnlock()

	results := make([]*TTMPDocument, 0)
	for _, doc := range c.Documents {
		for _, docTag := range doc.Tags {
			if docTag == tag {
				results = append(results, doc)
				break
			}
		}
	}

	return results
}

// GetAllDocuments returns all documents in the collection
func (c *TTMPCollection) GetAllDocuments() []*TTMPDocument {
	c.mu.RLock()
	defer c.mu.RUnlock()

	// Return a copy of the documents slice
	docs := make([]*TTMPDocument, len(c.Documents))
	copy(docs, c.Documents)

	return docs
}

// RemoveDocument removes a document from the collection
func (c *TTMPCollection) RemoveDocument(id string) error {
	c.mu.Lock()
	defer c.mu.Unlock()

	// Check if the document exists
	if _, exists := c.DocumentMap[id]; !exists {
		return errors.NewQueryError(
			fmt.Sprintf("Document with ID '%s' not found", id),
			nil,
		)
	}

	// Remove from DocumentMap
	delete(c.DocumentMap, id)

	// Remove from Documents slice
	for i, doc := range c.Documents {
		if doc.ID == id {
			c.Documents = append(c.Documents[:i], c.Documents[i+1:]...)
			break
		}
	}

	return nil
}

// Size returns the number of documents in the collection
func (c *TTMPCollection) Size() int {
	c.mu.RLock()
	defer c.mu.RUnlock()

	return len(c.Documents)
}

// Clear removes all documents from the collection
func (c *TTMPCollection) Clear() {
	c.mu.Lock()
	defer c.mu.Unlock()

	c.Documents = make([]*TTMPDocument, 0)
	c.DocumentMap = make(map[string]*TTMPDocument)
}

// FindLinkedDocuments finds all documents linked to the specified document
func (c *TTMPCollection) FindLinkedDocuments(doc *TTMPDocument) []*TTMPDocument {
	c.mu.RLock()
	defer c.mu.RUnlock()

	linkedDocs := make([]*TTMPDocument, 0)

	// Check if any documents link to this document
	for _, otherDoc := range c.Documents {
		if otherDoc.ID == doc.ID {
			continue // Skip the document itself
		}

		for _, link := range otherDoc.Links {
			// Check if the link URL is a relative path to the document
			if isDocumentLink(link.URL, doc.Filename) {
				linkedDocs = append(linkedDocs, otherDoc)
				break
			}
		}
	}

	return linkedDocs
}

// isDocumentLink checks if a link URL points to a document
func isDocumentLink(linkURL, docPath string) bool {
	// Check if the link is a relative path
	if filepath.IsAbs(linkURL) {
		return false
	}

	// Get the filename from the document path
	docFilename := filepath.Base(docPath)

	// Check if the link points to the document
	linkFilename := filepath.Base(linkURL)
	return linkFilename == docFilename
}