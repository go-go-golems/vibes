package model

import (
	"fmt"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

// TTMPCollection represents a collection of TTMP documents
type TTMPCollection struct {
	Documents    []*TTMPDocument
	BaseDir      string
	IndexByID    map[string]*TTMPDocument
	IndexByPath  map[string]*TTMPDocument
	IndexByTag   map[string][]*TTMPDocument
	IndexByType  map[string][]*TTMPDocument
	IndexByConcept map[string][]*TTMPDocument
	IndexBySourceFile map[string][]*TTMPDocument
}

// NewTTMPCollection creates a new TTMP collection
func NewTTMPCollection(baseDir string) *TTMPCollection {
	return &TTMPCollection{
		Documents:    []*TTMPDocument{},
		BaseDir:      baseDir,
		IndexByID:    make(map[string]*TTMPDocument),
		IndexByPath:  make(map[string]*TTMPDocument),
		IndexByTag:   make(map[string][]*TTMPDocument),
		IndexByType:  make(map[string][]*TTMPDocument),
		IndexByConcept: make(map[string][]*TTMPDocument),
		IndexBySourceFile: make(map[string][]*TTMPDocument),
	}
}

// AddDocument adds a document to the collection and updates indices
func (c *TTMPCollection) AddDocument(doc *TTMPDocument) {
	// Set relative file path
	doc.SetRelativeFilePath(c.BaseDir)
	
	// Add to main collection
	c.Documents = append(c.Documents, doc)
	
	// Index by ID
	if doc.ID != "" {
		c.IndexByID[doc.ID] = doc
	}
	
	// Index by file path
	if doc.FilePath != "" {
		c.IndexByPath[doc.FilePath] = doc
		if doc.RelativeFilePath != "" {
			c.IndexByPath[doc.RelativeFilePath] = doc
		}
	}
	
	// Index by tags
	for _, tag := range doc.Tags {
		c.IndexByTag[tag] = append(c.IndexByTag[tag], doc)
	}
	
	// Index by document type
	if doc.DocumentType != "" {
		c.IndexByType[doc.DocumentType] = append(c.IndexByType[doc.DocumentType], doc)
	}
	
	// Index by concept
	for _, concept := range doc.Concepts {
		c.IndexByConcept[concept] = append(c.IndexByConcept[concept], doc)
	}
	
	// Index by source file
	for _, sourceFile := range doc.SourceFiles {
		c.IndexBySourceFile[sourceFile] = append(c.IndexBySourceFile[sourceFile], doc)
	}
}

// GetDocumentByID retrieves a document by its ID
func (c *TTMPCollection) GetDocumentByID(id string) *TTMPDocument {
	return c.IndexByID[id]
}

// GetDocumentByPath retrieves a document by its file path
func (c *TTMPCollection) GetDocumentByPath(path string) *TTMPDocument {
	return c.IndexByPath[path]
}

// GetDocumentsByTag retrieves documents by tag
func (c *TTMPCollection) GetDocumentsByTag(tag string) []*TTMPDocument {
	return c.IndexByTag[tag]
}

// GetDocumentsByType retrieves documents by document type
func (c *TTMPCollection) GetDocumentsByType(docType string) []*TTMPDocument {
	return c.IndexByType[docType]
}

// GetDocumentsByConcept retrieves documents by concept
func (c *TTMPCollection) GetDocumentsByConcept(concept string) []*TTMPDocument {
	return c.IndexByConcept[concept]
}

// GetDocumentsBySourceFile retrieves documents by source file reference
func (c *TTMPCollection) GetDocumentsBySourceFile(sourceFile string) []*TTMPDocument {
	return c.IndexBySourceFile[sourceFile]
}

// GetDocumentsCreatedAfter filters documents created after a specific date
func (c *TTMPCollection) GetDocumentsCreatedAfter(date time.Time) []*TTMPDocument {
	var results []*TTMPDocument
	for _, doc := range c.Documents {
		if doc.Created != nil && doc.Created.After(date) {
			results = append(results, doc)
		}
	}
	return results
}

// GetDocumentsUpdatedAfter filters documents updated after a specific date
func (c *TTMPCollection) GetDocumentsUpdatedAfter(date time.Time) []*TTMPDocument {
	var results []*TTMPDocument
	for _, doc := range c.Documents {
		if doc.Updated != nil && doc.Updated.After(date) {
			results = append(results, doc)
		}
	}
	return results
}

// GetAllTags returns all unique tags in the collection
func (c *TTMPCollection) GetAllTags() []string {
	tagMap := make(map[string]bool)
	for tag := range c.IndexByTag {
		tagMap[tag] = true
	}
	
	tags := make([]string, 0, len(tagMap))
	for tag := range tagMap {
		tags = append(tags, tag)
	}
	
	sort.Strings(tags)
	return tags
}

// GetAllDocumentTypes returns all unique document types in the collection
func (c *TTMPCollection) GetAllDocumentTypes() []string {
	typeMap := make(map[string]bool)
	for docType := range c.IndexByType {
		typeMap[docType] = true
	}
	
	types := make([]string, 0, len(typeMap))
	for docType := range typeMap {
		types = append(types, docType)
	}
	
	sort.Strings(types)
	return types
}

// GetAllConcepts returns all unique concepts in the collection
func (c *TTMPCollection) GetAllConcepts() []string {
	conceptMap := make(map[string]bool)
	for concept := range c.IndexByConcept {
		conceptMap[concept] = true
	}
	
	concepts := make([]string, 0, len(conceptMap))
	for concept := range conceptMap {
		concepts = append(concepts, concept)
	}
	
	sort.Strings(concepts)
	return concepts
}

// GetAllSourceFiles returns all unique source files in the collection
func (c *TTMPCollection) GetAllSourceFiles() []string {
	sourceFileMap := make(map[string]bool)
	for sourceFile := range c.IndexBySourceFile {
		sourceFileMap[sourceFile] = true
	}
	
	sourceFiles := make([]string, 0, len(sourceFileMap))
	for sourceFile := range sourceFileMap {
		sourceFiles = append(sourceFiles, sourceFile)
	}
	
	sort.Strings(sourceFiles)
	return sourceFiles
}

// DirectoryNode represents a node in a file directory tree
type DirectoryNode struct {
	Name      string          `json:"name"`
	Path      string          `json:"path"`
	IsDir     bool            `json:"is_dir"`
	Children  []*DirectoryNode `json:"children,omitempty"`
	Document  *Summary        `json:"document,omitempty"`
}

// BuildDirectoryTree builds a directory tree from the documents
func (c *TTMPCollection) BuildDirectoryTree() *DirectoryNode {
	root := &DirectoryNode{
		Name:     filepath.Base(c.BaseDir),
		Path:     "",
		IsDir:    true,
		Children: []*DirectoryNode{},
	}
	
	// Create a map to track directories
	dirMap := make(map[string]*DirectoryNode)
	dirMap[""] = root
	
	// Add document paths to the tree
	for _, doc := range c.Documents {
		if doc.RelativeFilePath == "" {
			continue
		}
		
		dir := filepath.Dir(doc.RelativeFilePath)
		if dir == "." {
			dir = ""
		}
		
		// Ensure all parent directories exist
		parent := root
		if dir != "" {
			parts := strings.Split(dir, string(filepath.Separator))
			currentPath := ""
			
			for _, part := range parts {
				currentPath = filepath.Join(currentPath, part)
				
				if _, exists := dirMap[currentPath]; !exists {
					newDir := &DirectoryNode{
						Name:     part,
						Path:     currentPath,
						IsDir:    true,
						Children: []*DirectoryNode{},
					}
					parent.Children = append(parent.Children, newDir)
					dirMap[currentPath] = newDir
				}
				
				parent = dirMap[currentPath]
			}
		}
		
		// Add document to the parent directory
		fileName := filepath.Base(doc.RelativeFilePath)
		fileNode := &DirectoryNode{
			Name:     fileName,
			Path:     doc.RelativeFilePath,
			IsDir:    false,
			Document: &Summary{},
		}
		
		// Add document metadata
		summary := doc.ToSummary()
		fileNode.Document = &summary
		
		parent.Children = append(parent.Children, fileNode)
	}
	
	// Sort each directory's children
	sortDirectoryTree(root)
	
	return root
}

// sortDirectoryTree sorts the children of a directory node
func sortDirectoryTree(node *DirectoryNode) {
	// Sort directories first, then files
	sort.Slice(node.Children, func(i, j int) bool {
		// If both are dirs or both are files, sort by name
		if node.Children[i].IsDir == node.Children[j].IsDir {
			return node.Children[i].Name < node.Children[j].Name
		}
		// Otherwise, directories come first
		return node.Children[i].IsDir
	})
	
	// Recursively sort children
	for _, child := range node.Children {
		if child.IsDir {
			sortDirectoryTree(child)
		}
	}
}

// String returns a string representation of the collection
func (c *TTMPCollection) String() string {
	return fmt.Sprintf("TTMPCollection{BaseDir: %s, Documents: %d}", c.BaseDir, len(c.Documents))
}