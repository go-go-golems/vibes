package parser

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v3"

	ttmperrors "github.com/scrapybara/ttmp/pkg/errors"
	"github.com/scrapybara/ttmp/pkg/model"
)

// TTMPParser parses TTMP documents from files or strings
type TTMPParser struct {
	logger *logrus.Logger
	schema *model.TTMPSchema
}

// NewTTMPParser creates a new TTMPParser instance
func NewTTMPParser(logger *logrus.Logger) *TTMPParser {
	return &TTMPParser{
		logger: logger,
		schema: model.NewDefaultSchema(),
	}
}

// ParseFile parses a TTMP document from a file
func (p *TTMPParser) ParseFile(filePath string) (*model.TTMPDocument, error) {
	// Check if file exists
	if _, err := os.Stat(filePath); os.IsNotExist(err) {
		return nil, ttmperrors.NewError(ttmperrors.ErrFileNotFound, "File not found").WithFile(filePath)
	}
	
	file, err := os.Open(filePath)
	if err != nil {
		if os.IsPermission(err) {
			return nil, ttmperrors.NewError(ttmperrors.ErrFilePermission, "Permission denied").WithFile(filePath)
		}
		return nil, ttmperrors.NewError(ttmperrors.ErrFile, "Error opening file").WithFile(filePath).WithCause(err)
	}
	defer file.Close()

	doc, err := p.Parse(file)
	if err != nil {
		return nil, ttmperrors.NewError(ttmperrors.ErrParse, "Error parsing document").WithFile(filePath).WithCause(err)
	}

	// Set file metadata
	doc.FilePath = filePath
	fileInfo, err := os.Stat(filePath)
	if err == nil {
		doc.LastModified = fileInfo.ModTime()
		
		// If no updated time set, use file modified time
		if doc.Updated == nil {
			doc.Updated = &doc.LastModified
		}
	}
	
	// Generate ID from filename if not set
	doc.GenerateID()
	
	// Calculate word count if not set
	if doc.WordCount == 0 {
		doc.UpdateWordCount()
	}

	return doc, nil
}

// ParseString parses a TTMP document from a string
func (p *TTMPParser) ParseString(content string) (*model.TTMPDocument, error) {
	return p.Parse(strings.NewReader(content))
}

// Parse reads a TTMP document from any io.Reader
func (p *TTMPParser) Parse(reader io.Reader) (*model.TTMPDocument, error) {
	scanner := bufio.NewScanner(reader)
	scanner.Buffer(make([]byte, 1024*1024), 1024*1024) // Increase scanner buffer size for large files

	// Check if the file starts with frontmatter delimiter
	if !scanner.Scan() {
		// Empty document
		if scanner.Err() != nil {
			return nil, ttmperrors.NewError(ttmperrors.ErrParse, "Error scanning document").WithCause(scanner.Err())
		}
		return nil, ttmperrors.NewError(ttmperrors.ErrParse, "Empty document")
	}

	firstLine := scanner.Text()
	
	// Determine the frontmatter style (--- or ```)
	var frontmatterDelimiter string
	if strings.TrimSpace(firstLine) == "---" {
		frontmatterDelimiter = "---"
	} else {
		// If no frontmatter delimiter is found, treat the entire document as content
		var fullContent strings.Builder
		fullContent.WriteString(firstLine)
		fullContent.WriteString("\n")
		
		for scanner.Scan() {
			fullContent.WriteString(scanner.Text())
			fullContent.WriteString("\n")
		}
		
		if err := scanner.Err(); err != nil {
			return nil, ttmperrors.NewError(ttmperrors.ErrParse, "Error scanning document content").WithCause(err)
		}
		
		doc := model.NewTTMPDocument()
		doc.Content = fullContent.String()
		return doc, nil
	}

	// Extract frontmatter
	var frontmatter strings.Builder
	lineNum := 1
	endFrontmatterFound := false
	
	for scanner.Scan() {
		lineNum++
		line := scanner.Text()
		if strings.TrimSpace(line) == frontmatterDelimiter {
			// End of frontmatter
			endFrontmatterFound = true
			break
		}
		frontmatter.WriteString(line)
		frontmatter.WriteString("\n")
	}
	
	if !endFrontmatterFound {
		return nil, ttmperrors.NewError(ttmperrors.ErrParseYAML, "Unclosed YAML frontmatter (missing closing delimiter)")
	}

	// Extract content (everything after the frontmatter)
	var content strings.Builder
	for scanner.Scan() {
		content.WriteString(scanner.Text())
		content.WriteString("\n")
	}

	if err := scanner.Err(); err != nil {
		return nil, ttmperrors.NewError(ttmperrors.ErrParseContent, "Error reading document content").WithCause(err)
	}

	// Parse YAML frontmatter into TTMPDocument
	doc := model.NewTTMPDocument()
	yamlContent := frontmatter.String()
	err := yaml.Unmarshal([]byte(yamlContent), doc)
	if err != nil {
		// Try to determine the line number of the error
		yamlErrorRegex := regexp.MustCompile(`line (\d+)`)
		matches := yamlErrorRegex.FindStringSubmatch(err.Error())
		if len(matches) > 1 {
			errorLine := 0
			fmt.Sscanf(matches[1], "%d", &errorLine)
			return nil, ttmperrors.NewError(ttmperrors.ErrParseYAML, "Invalid YAML frontmatter").WithLine(errorLine).WithCause(err)
		}
		return nil, ttmperrors.NewError(ttmperrors.ErrParseYAML, "Invalid YAML frontmatter").WithCause(err)
	}

	doc.Content = content.String()
	
	// Process the document
	p.processDocument(doc)
	
	return doc, nil
}

// processDocument performs additional processing on a parsed document
func (p *TTMPParser) processDocument(doc *model.TTMPDocument) {
	// Calculate word count if not set
	if doc.WordCount == 0 {
		doc.UpdateWordCount()
	}
	
	// Set created/updated time if not set
	now := time.Now()
	if doc.Created == nil {
		doc.Created = &now
	}
	if doc.Updated == nil {
		doc.Updated = &now
	}
	
	// Generate ID if not set
	doc.GenerateID()
}

// WriteTTMP writes a TTMPDocument to a file
func (p *TTMPParser) WriteTTMP(doc *model.TTMPDocument, filePath string) error {
	// Create directory if not exists
	dir := filepath.Dir(filePath)
	if _, err := os.Stat(dir); os.IsNotExist(err) {
		if err := os.MkdirAll(dir, 0755); err != nil {
			return ttmperrors.NewError(ttmperrors.ErrFile, "Error creating directory").WithFile(dir).WithCause(err)
		}
	}
	
	file, err := os.Create(filePath)
	if err != nil {
		if os.IsPermission(err) {
			return ttmperrors.NewError(ttmperrors.ErrFilePermission, "Permission denied").WithFile(filePath)
		}
		return ttmperrors.NewError(ttmperrors.ErrFile, "Error creating file").WithFile(filePath).WithCause(err)
	}
	defer file.Close()

	return p.WriteToWriter(doc, file)
}

// WriteToWriter writes a TTMPDocument to any io.Writer
func (p *TTMPParser) WriteToWriter(doc *model.TTMPDocument, writer io.Writer) error {
	// Update modified time
	now := time.Now()
	doc.Updated = &now
	
	// Update word count
	doc.UpdateWordCount()
	
	// Marshal the document to YAML
	var yamlBuf bytes.Buffer
	encoder := yaml.NewEncoder(&yamlBuf)
	encoder.SetIndent(2)
	
	if err := encoder.Encode(doc); err != nil {
		return ttmperrors.NewError(ttmperrors.ErrParseYAML, "Error encoding document to YAML").WithCause(err)
	}
	
	// Write the frontmatter delimiter
	if _, err := writer.Write([]byte("---\n")); err != nil {
		return ttmperrors.NewError(ttmperrors.ErrFile, "Error writing to output").WithCause(err)
	}
	
	// Write the frontmatter
	if _, err := writer.Write(yamlBuf.Bytes()); err != nil {
		return ttmperrors.NewError(ttmperrors.ErrFile, "Error writing to output").WithCause(err)
	}
	
	// Write the frontmatter delimiter
	if _, err := writer.Write([]byte("---\n\n")); err != nil {
		return ttmperrors.NewError(ttmperrors.ErrFile, "Error writing to output").WithCause(err)
	}
	
	// Write the content
	if _, err := writer.Write([]byte(doc.Content)); err != nil {
		return ttmperrors.NewError(ttmperrors.ErrFile, "Error writing to output").WithCause(err)
	}
	
	return nil
}

// LoadCollection loads a collection of TTMP documents from a directory
func (p *TTMPParser) LoadCollection(baseDir string, recursive bool) (*model.TTMPCollection, error) {
	// Check if directory exists
	if _, err := os.Stat(baseDir); os.IsNotExist(err) {
		return nil, ttmperrors.NewError(ttmperrors.ErrFileNotFound, "Directory not found").WithFile(baseDir)
	}
	
	collection := model.NewTTMPCollection(baseDir)
	
	// Function to filter markdown files and parse them
	walkFunc := func(path string, info os.FileInfo, err error) error {
		if err != nil {
			p.logger.Warnf("Error accessing path %s: %v", path, err)
			return nil
		}
		
		// Skip directories if not recursive
		if !recursive && path != baseDir && info.IsDir() {
			return filepath.SkipDir
		}
		
		// Parse markdown files
		if !info.IsDir() && strings.HasSuffix(strings.ToLower(info.Name()), ".md") {
			doc, err := p.ParseFile(path)
			if err != nil {
				p.logger.Warnf("Error parsing %s: %v", path, err)
				return nil
			}
			
			collection.AddDocument(doc)
		}
		
		return nil
	}
	
	if err := filepath.Walk(baseDir, walkFunc); err != nil {
		return nil, ttmperrors.NewError(ttmperrors.ErrFile, "Error walking directory").WithFile(baseDir).WithCause(err)
	}
	
	p.logger.Infof("Loaded %d documents from %s", len(collection.Documents), baseDir)
	return collection, nil
}

// ExtractTags extracts tags from frontmatter in a markdown string
func (p *TTMPParser) ExtractTags(content string) ([]string, error) {
	doc, err := p.ParseString(content)
	if err != nil {
		return nil, err
	}
	return doc.Tags, nil
}

// ExtractRelatedCodes finds source code paths in a document
func (p *TTMPParser) ExtractRelatedCodes(doc *model.TTMPDocument) []string {
	var relatedCodes []string
	
	// Add source files
	relatedCodes = append(relatedCodes, doc.SourceFiles...)
	
	// Extract code paths from content using regex
	codePattern := regexp.MustCompile("`([^`]+\\.(go|py|js|ts|java|rb|c|cpp|h|hpp|cs))`")
	matches := codePattern.FindAllStringSubmatch(doc.Content, -1)
	
	for _, match := range matches {
		if len(match) > 1 {
			relatedCodes = append(relatedCodes, match[1])
		}
	}
	
	return relatedCodes
}

// FindRelatedDocuments finds other documents related to the given document
func (p *TTMPParser) FindRelatedDocuments(doc *model.TTMPDocument, collection *model.TTMPCollection) []*model.TTMPDocument {
	var related []*model.TTMPDocument
	
	// Check explicit see_also references
	for _, ref := range doc.SeeAlso {
		relDoc := collection.GetDocumentByPath(ref)
		if relDoc != nil {
			related = append(related, relDoc)
			continue
		}
		
		// Try as ID
		relDoc = collection.GetDocumentByID(ref)
		if relDoc != nil {
			related = append(related, relDoc)
		}
	}
	
	// Check predecessor/successor
	if doc.Predecessor != "" {
		predDoc := collection.GetDocumentByID(doc.Predecessor)
		if predDoc != nil {
			related = append(related, predDoc)
		}
	}
	
	if doc.Successor != "" {
		succDoc := collection.GetDocumentByID(doc.Successor)
		if succDoc != nil {
			related = append(related, succDoc)
		}
	}
	
	// Check shared source files
	for _, sourceFile := range doc.SourceFiles {
		docs := collection.GetDocumentsBySourceFile(sourceFile)
		for _, relDoc := range docs {
			if relDoc.ID != doc.ID {
				related = append(related, relDoc)
			}
		}
	}
	
	// Check shared concepts
	for _, concept := range doc.Concepts {
		docs := collection.GetDocumentsByConcept(concept)
		for _, relDoc := range docs {
			if relDoc.ID != doc.ID {
				related = append(related, relDoc)
			}
		}
	}
	
	// Remove duplicates
	uniqueMap := make(map[string]*model.TTMPDocument)
	for _, relDoc := range related {
		uniqueMap[relDoc.ID] = relDoc
	}
	
	unique := make([]*model.TTMPDocument, 0, len(uniqueMap))
	for _, relDoc := range uniqueMap {
		unique = append(unique, relDoc)
	}
	
	return unique
}
