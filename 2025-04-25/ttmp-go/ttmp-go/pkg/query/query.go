package query

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"time"

	"github.com/sirupsen/logrus"

	ttmperrors "github.com/scrapybara/ttmp/pkg/errors"
	"github.com/scrapybara/ttmp/pkg/model"
	"github.com/scrapybara/ttmp/pkg/parser"
)

// QueryEngine handles TTMP document queries
type QueryEngine struct {
	logger *logrus.Logger
	parser *parser.TTMPParser
}

// NewQueryEngine creates a new query engine
func NewQueryEngine(logger *logrus.Logger, parser *parser.TTMPParser) *QueryEngine {
	return &QueryEngine{
		logger: logger,
		parser: parser,
	}
}

// QueryOptions contains options for querying TTMP documents
type QueryOptions struct {
	ID             string
	Tags           []string
	Category       string
	Status         string
	Owner          string
	ModifiedAfter  *time.Time
	ModifiedBefore *time.Time
	SourceFile     string
	TextSearch     string
	Longevity      string
	DocumentType   string
	Concepts       []string
	TrackedFunction string
	SortBy         string
	SortDescending bool
	Limit          int
}

// FindDocuments searches for TTMP documents in a directory that match the query options
func (qe *QueryEngine) FindDocuments(rootDir string, options QueryOptions) ([]*model.TTMPDocument, error) {
	// Check if directory exists
	if _, err := os.Stat(rootDir); os.IsNotExist(err) {
		return nil, ttmperrors.NewError(ttmperrors.ErrFileNotFound, "Directory not found").WithFile(rootDir)
	}
	
	var results []*model.TTMPDocument

	err := filepath.Walk(rootDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			// Skip errors for individual files
			qe.logger.Warnf("Error accessing path %s: %v", path, err)
			return nil
		}

		// Skip directories
		if info.IsDir() {
			return nil
		}

		// Skip non-markdown files
		if !strings.HasSuffix(strings.ToLower(info.Name()), ".md") {
			return nil
		}

		// Parse the document
		doc, err := qe.parser.ParseFile(path)
		if err != nil {
			qe.logger.Warnf("Error parsing %s: %v", path, err)
			return nil
		}

		// Check if the document matches the query options
		if qe.matchesQuery(doc, options) {
			results = append(results, doc)
		}

		return nil
	})

	if err != nil {
		return nil, ttmperrors.NewError(ttmperrors.ErrQuery, "Error querying documents").WithCause(err)
	}
	
	// Sort the results
	qe.sortResults(results, options)
	
	// Apply limit if specified
	if options.Limit > 0 && len(results) > options.Limit {
		results = results[:options.Limit]
	}
	
	// Check if results were found
	if len(results) == 0 {
		return nil, ttmperrors.NewError(ttmperrors.ErrQueryNoResults, "No documents found matching the query criteria")
	}

	return results, nil
}

// FindDocumentsInCollection searches for TTMP documents in a collection that match the query options
func (qe *QueryEngine) FindDocumentsInCollection(collection *model.TTMPCollection, options QueryOptions) ([]*model.TTMPDocument, error) {
	var results []*model.TTMPDocument

	for _, doc := range collection.Documents {
		if qe.matchesQuery(doc, options) {
			results = append(results, doc)
		}
	}
	
	// Sort the results
	qe.sortResults(results, options)
	
	// Apply limit if specified
	if options.Limit > 0 && len(results) > options.Limit {
		results = results[:options.Limit]
	}
	
	// Check if results were found
	if len(results) == 0 {
		return nil, ttmperrors.NewError(ttmperrors.ErrQueryNoResults, "No documents found matching the query criteria")
	}

	return results, nil
}

// sortResults sorts the documents based on the sort options
func (qe *QueryEngine) sortResults(docs []*model.TTMPDocument, options QueryOptions) {
	if options.SortBy == "" {
		options.SortBy = "updated" // Default sort
	}
	
	sort.Slice(docs, func(i, j int) bool {
		var result bool
		
		switch options.SortBy {
		case "id":
			result = docs[i].ID < docs[j].ID
		case "title":
			result = docs[i].GetTitle() < docs[j].GetTitle()
		case "created":
			if docs[i].Created == nil {
				return !options.SortDescending // Nil values at end
			}
			if docs[j].Created == nil {
				return options.SortDescending // Nil values at end
			}
			result = docs[i].Created.Before(*docs[j].Created)
		case "updated":
			if docs[i].Updated == nil {
				return !options.SortDescending // Nil values at end
			}
			if docs[j].Updated == nil {
				return options.SortDescending // Nil values at end
			}
			result = docs[i].Updated.Before(*docs[j].Updated)
		case "type":
			result = docs[i].DocumentType < docs[j].DocumentType
		case "status":
			result = docs[i].Status < docs[j].Status
		case "wordcount":
			result = docs[i].WordCount < docs[j].WordCount
		default:
			result = docs[i].ID < docs[j].ID
		}
		
		if options.SortDescending {
			return !result
		}
		return result
	})
}

// matchesQuery checks if a document matches the query options
func (qe *QueryEngine) matchesQuery(doc *model.TTMPDocument, options QueryOptions) bool {
	// Match by ID
	if options.ID != "" && doc.ID != options.ID {
		return false
	}

	// Match by tags
	if len(options.Tags) > 0 {
		hasTag := false
		for _, tag := range options.Tags {
			for _, docTag := range doc.Tags {
				if strings.EqualFold(tag, docTag) {
					hasTag = true
					break
				}
			}
			if hasTag {
				break
			}
		}
		if !hasTag {
			return false
		}
	}

	// Match by category
	if options.Category != "" && !strings.EqualFold(doc.Category, options.Category) {
		return false
	}

	// Match by status
	if options.Status != "" && !strings.EqualFold(doc.Status, options.Status) {
		return false
	}

	// Match by owner
	if options.Owner != "" && !strings.EqualFold(doc.Owner, options.Owner) {
		return false
	}

	// Match by longevity
	if options.Longevity != "" && !strings.EqualFold(doc.Longevity, options.Longevity) {
		return false
	}

	// Match by document type
	if options.DocumentType != "" && !strings.EqualFold(doc.DocumentType, options.DocumentType) {
		return false
	}

	// Match by modified after
	if options.ModifiedAfter != nil && doc.Updated != nil && doc.Updated.Before(*options.ModifiedAfter) {
		return false
	}

	// Match by modified before
	if options.ModifiedBefore != nil && doc.Updated != nil && doc.Updated.After(*options.ModifiedBefore) {
		return false
	}

	// Match by source file
	if options.SourceFile != "" {
		hasSourceFile := false
		for _, sourceFile := range doc.SourceFiles {
			if strings.Contains(sourceFile, options.SourceFile) {
				hasSourceFile = true
				break
			}
		}
		if !hasSourceFile {
			return false
		}
	}
	
	// Match by tracked function
	if options.TrackedFunction != "" {
		hasTrackedFunction := false
		for _, fn := range doc.TrackedFunctions {
			if strings.Contains(fn, options.TrackedFunction) {
				hasTrackedFunction = true
				break
			}
		}
		if !hasTrackedFunction {
			return false
		}
	}
	
	// Match by concepts
	if len(options.Concepts) > 0 {
		hasConcept := false
		for _, concept := range options.Concepts {
			for _, docConcept := range doc.Concepts {
				if strings.EqualFold(concept, docConcept) {
					hasConcept = true
					break
				}
			}
			if hasConcept {
				break
			}
		}
		if !hasConcept {
			return false
		}
	}

	// Match by text search in content or title
	if options.TextSearch != "" {
		if !strings.Contains(strings.ToLower(doc.Content), strings.ToLower(options.TextSearch)) &&
		   !strings.Contains(strings.ToLower(doc.Title), strings.ToLower(options.TextSearch)) &&
		   !strings.Contains(strings.ToLower(doc.Abstract), strings.ToLower(options.TextSearch)) {
			return false
		}
	}

	return true
}

// GetDocumentByID finds a document by ID
func (qe *QueryEngine) GetDocumentByID(rootDir string, id string) (*model.TTMPDocument, error) {
	docs, err := qe.FindDocuments(rootDir, QueryOptions{ID: id})
	if err != nil {
		if ttmperrors.IsFileNotFound(err) {
			return nil, err
		}
		return nil, ttmperrors.NewError(ttmperrors.ErrQuery, "Error querying documents").WithCause(err)
	}

	if len(docs) == 0 {
		return nil, ttmperrors.NewError(ttmperrors.ErrQueryNoResults, fmt.Sprintf("Document with ID '%s' not found", id))
	}

	if len(docs) > 1 {
		qe.logger.Warnf("Multiple documents found with ID %s, returning the first one", id)
	}

	return docs[0], nil
}

// SearchByContent searches for documents containing specific text
func (qe *QueryEngine) SearchByContent(rootDir string, searchTerm string) ([]*model.TTMPDocument, error) {
	return qe.FindDocuments(rootDir, QueryOptions{TextSearch: searchTerm})
}

// FindRelatedDocuments finds documents related to a given document
func (qe *QueryEngine) FindRelatedDocuments(rootDir string, doc *model.TTMPDocument) ([]*model.TTMPDocument, error) {
	var relatedDocs []*model.TTMPDocument
	
	// Find documents with shared tags
	if len(doc.Tags) > 0 {
		for _, tag := range doc.Tags {
			tagDocs, err := qe.FindDocuments(rootDir, QueryOptions{Tags: []string{tag}})
			if err != nil {
				// Continue even if no documents are found with this tag
				if IsQueryNoResults(err) {
					continue
				}
				return nil, err
			}
			
			for _, tagDoc := range tagDocs {
				if tagDoc.ID != doc.ID {
					relatedDocs = append(relatedDocs, tagDoc)
				}
			}
		}
	}
	
	// Find documents with shared source files
	if len(doc.SourceFiles) > 0 {
		for _, sourceFile := range doc.SourceFiles {
			sourceDocs, err := qe.FindDocuments(rootDir, QueryOptions{SourceFile: sourceFile})
			if err != nil {
				// Continue even if no documents are found with this source file
				if IsQueryNoResults(err) {
					continue
				}
				return nil, err
			}
			
			for _, sourceDoc := range sourceDocs {
				if sourceDoc.ID != doc.ID {
					relatedDocs = append(relatedDocs, sourceDoc)
				}
			}
		}
	}
	
	// Find documents with shared concepts
	if len(doc.Concepts) > 0 {
		for _, concept := range doc.Concepts {
			conceptDocs, err := qe.FindDocuments(rootDir, QueryOptions{Concepts: []string{concept}})
			if err != nil {
				// Continue even if no documents are found with this concept
				if IsQueryNoResults(err) {
					continue
				}
				return nil, err
			}
			
			for _, conceptDoc := range conceptDocs {
				if conceptDoc.ID != doc.ID {
					relatedDocs = append(relatedDocs, conceptDoc)
				}
			}
		}
	}
	
	// Find documents referenced in see_also
	for _, ref := range doc.SeeAlso {
		// Try as file path
		if strings.HasSuffix(ref, ".md") {
			absolutePath := ref
			if !filepath.IsAbs(ref) {
				// Try relative to doc's location
				docDir := filepath.Dir(doc.FilePath)
				absolutePath = filepath.Join(docDir, ref)
			}
			
			refDoc, err := qe.parser.ParseFile(absolutePath)
			if err == nil {
				relatedDocs = append(relatedDocs, refDoc)
			}
		} else {
			// Try as ID
			refDoc, err := qe.GetDocumentByID(rootDir, ref)
			if err == nil {
				relatedDocs = append(relatedDocs, refDoc)
			}
		}
	}
	
	// Remove duplicates
	dedupMap := make(map[string]*model.TTMPDocument)
	for _, relDoc := range relatedDocs {
		dedupMap[relDoc.ID] = relDoc
	}
	
	result := make([]*model.TTMPDocument, 0, len(dedupMap))
	for _, relDoc := range dedupMap {
		result = append(result, relDoc)
	}
	
	if len(result) == 0 {
		return nil, ttmperrors.NewError(ttmperrors.ErrQueryNoResults, "No related documents found")
	}
	
	return result, nil
}

// ExtractCodeReferences finds all code references in a document
func (qe *QueryEngine) ExtractCodeReferences(doc *model.TTMPDocument) []string {
	var refs []string
	
	// Add source files
	refs = append(refs, doc.SourceFiles...)
	
	// Add tracked functions
	refs = append(refs, doc.TrackedFunctions...)
	
	// Find code references in content
	codePattern := regexp.MustCompile("(?:\\`{1,3}|`)(.*?\\.(py|go|js|ts|jsx|tsx|java|rb|c|cpp|h|hpp|cs))(?:\\`{1,3}|`)")
	matches := codePattern.FindAllStringSubmatch(doc.Content, -1)
	
	for _, match := range matches {
		if len(match) > 1 {
			refs = append(refs, match[1])
		}
	}
	
	// Remove duplicates
	uniqueRefs := make(map[string]bool)
	for _, ref := range refs {
		uniqueRefs[ref] = true
	}
	
	result := make([]string, 0, len(uniqueRefs))
	for ref := range uniqueRefs {
		result = append(result, ref)
	}
	
	return result
}

// IsQueryNoResults checks if the error is a query no results error
func IsQueryNoResults(err error) bool {
	var e *ttmperrors.TTMPError
	if ok := ttmperrors.AsError(err, &e); ok {
		return e.Type == ttmperrors.ErrQueryNoResults
	}
	return false
}