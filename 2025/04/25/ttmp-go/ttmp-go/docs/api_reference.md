# TTMP API Reference

This document provides a comprehensive reference for the TTMP system's Go API, covering all exported types, functions, and methods that can be used in your own applications.

## Table of Contents

1. [Model Package](#model-package)
2. [Parser Package](#parser-package)
3. [Validator Package](#validator-package)
4. [Query Package](#query-package)
5. [Server Package](#server-package)
6. [Utility Package](#utility-package)
7. [Errors Package](#errors-package)
8. [Web API Endpoints](#web-api-endpoints)

## Model Package

`import "github.com/scrapybara/ttmp/pkg/model"`

The Model package defines the core data structures for representing TTMP documents.

### Types

#### TTMPDocument

```go
type TTMPDocument struct {
    // Required fields
    ID           string   `yaml:"id" json:"id"`
    Title        string   `yaml:"title" json:"title"`
    DocumentType string   `yaml:"document_type" json:"document_type"`
    Status       string   `yaml:"status" json:"status"`
    
    // Optional fields
    Owner        string     `yaml:"owner,omitempty" json:"owner,omitempty"`
    Created      *time.Time `yaml:"created,omitempty" json:"created,omitempty"`
    Updated      *time.Time `yaml:"updated,omitempty" json:"updated,omitempty"`
    Tags         []string   `yaml:"tags,omitempty" json:"tags,omitempty"`
    Category     string     `yaml:"category,omitempty" json:"category,omitempty"`
    Audience     string     `yaml:"audience,omitempty" json:"audience,omitempty"`
    Abstract     string     `yaml:"abstract,omitempty" json:"abstract,omitempty"`
    SeeAlso      []string   `yaml:"see_also,omitempty" json:"see_also,omitempty"`
    Concepts     []string   `yaml:"concepts,omitempty" json:"concepts,omitempty"`
    SourceFiles  []string   `yaml:"source_files,omitempty" json:"source_files,omitempty"`
    Longevity    string     `yaml:"longevity,omitempty" json:"longevity,omitempty"`
    
    // Content and metadata
    Content          string `yaml:"-" json:"content,omitempty"`
    RawYAML          string `yaml:"-" json:"-"`
    WordCount        int    `yaml:"-" json:"word_count,omitempty"`
    FilePath         string `yaml:"-" json:"-"`
    RelativeFilePath string `yaml:"-" json:"-"`
}
```

The main structure representing a TTMP document. Contains all document metadata, content, and file information.

#### Collection

```go
type Collection struct {
    Documents []*TTMPDocument
    BasePath  string
}
```

Represents a collection of TTMP documents with a common base path.

### Functions

#### NewCollection

```go
func NewCollection(basePath string) *Collection
```

Creates a new collection with the specified base path.

#### LoadCollection

```go
func LoadCollection(basePath string) (*Collection, error)
```

Loads all TTMP documents from the specified base path into a collection.

### Methods

#### Collection.AddDocument

```go
func (c *Collection) AddDocument(doc *TTMPDocument) 
```

Adds a document to the collection.

#### Collection.Query

```go
func (c *Collection) Query(queryStr string, keywords []string) ([]*TTMPDocument, error)
```

Queries the collection for documents matching the specified query and keywords.

#### Collection.GetStats

```go
func (c *Collection) GetStats() map[string]interface{}
```

Generates statistics about the documents in the collection.

## Parser Package

`import "github.com/scrapybara/ttmp/pkg/parser"`

The Parser package provides functionality for parsing TTMP documents.

### Types

#### TTMPParser

```go
type TTMPParser struct {
    // Unexported fields
}
```

Parser for TTMP documents.

### Functions

#### NewTTMPParser

```go
func NewTTMPParser(logger *logrus.Logger) *TTMPParser
```

Creates a new parser with the specified logger.

### Methods

#### ParseFile

```go
func (p *TTMPParser) ParseFile(path string) (*model.TTMPDocument, error)
```

Parses a TTMP document from a file.

#### ParseString

```go
func (p *TTMPParser) ParseString(content string) (*model.TTMPDocument, error)
```

Parses a TTMP document from a string.

#### ParseYAML

```go
func (p *TTMPParser) ParseYAML(yamlContent string) (*model.TTMPDocument, error)
```

Parses just the YAML frontmatter into a TTMPDocument.

## Validator Package

`import "github.com/scrapybara/ttmp/pkg/validator"`

The Validator package provides functionality for validating TTMP documents.

### Types

#### Validator

```go
type Validator struct {
    // Unexported fields
}
```

Validator for TTMP documents.

#### ValidationResult

```go
type ValidationResult struct {
    Valid  bool
    Errors []error
}
```

Result of a document validation operation.

### Functions

#### NewValidator

```go
func NewValidator(logger *logrus.Logger) *Validator
```

Creates a new validator with the specified logger.

### Methods

#### Validate

```go
func (v *Validator) Validate(doc *model.TTMPDocument) (*ValidationResult, error)
```

Validates a TTMP document.

#### ValidateStrict

```go
func (v *Validator) ValidateStrict(doc *model.TTMPDocument) (*ValidationResult, error)
```

Validates a TTMP document using strict validation rules.

#### ValidationResult.GetErrorStrings

```go
func (vr *ValidationResult) GetErrorStrings() []string
```

Returns a list of error messages from the validation result.

## Query Package

`import "github.com/scrapybara/ttmp/pkg/query"`

The Query package provides functionality for querying TTMP documents.

### Functions

#### QueryDocuments

```go
func QueryDocuments(docs []*model.TTMPDocument, queryStr string, keywords []string) ([]*model.TTMPDocument, error)
```

Searches for documents matching the specified query and keywords.

#### ParseQuery

```go
func ParseQuery(queryStr string) (Query, error)
```

Parses a query string into a query object.

### Types

#### Query

```go
type Query interface {
    Evaluate(doc *model.TTMPDocument) bool
}
```

Interface for a query that can be evaluated against a document.

## Server Package

`import "github.com/scrapybara/ttmp/pkg/server"`

The Server package provides the web server implementation.

### Types

#### Server

```go
type Server struct {
    // Unexported fields
}
```

TTMP web server.

### Functions

#### NewServer

```go
func NewServer(basePath string) (*Server, error)
```

Creates a new web server with the specified base path.

### Methods

#### Start

```go
func (s *Server) Start(addr string) error
```

Starts the web server at the specified address.

## Utility Package

`import "github.com/scrapybara/ttmp/pkg/util"`

The Utility package provides helper functions.

### Functions

#### FindMarkdownFiles

```go
func FindMarkdownFiles(basePath string) ([]string, error)
```

Finds all Markdown files in the specified directory.

#### CountWords

```go
func CountWords(text string) int
```

Counts the number of words in the specified text.

#### FormatDateTime

```go
func FormatDateTime(t time.Time) string
```

Formats a time as an ISO 8601 datetime string.

#### ParseDateTime

```go
func ParseDateTime(s string) (time.Time, error)
```

Parses an ISO 8601 datetime string into a time.

## Errors Package

`import "github.com/scrapybara/ttmp/pkg/errors"`

The Errors package provides custom error types.

### Types

#### ValidationError

```go
type ValidationError struct {
    Field   string
    Message string
}
```

Error representing a validation failure.

#### ParseError

```go
type ParseError struct {
    Line    int
    Message string
}
```

Error representing a parsing failure.

#### QueryError

```go
type QueryError struct {
    Position int
    Message  string
}
```

Error representing a query syntax error.

### Methods

#### ValidationError.Error

```go
func (e ValidationError) Error() string
```

Returns the error message for a ValidationError.

#### ParseError.Error

```go
func (e ParseError) Error() string
```

Returns the error message for a ParseError.

#### QueryError.Error

```go
func (e QueryError) Error() string
```

Returns the error message for a QueryError.

## Web API Endpoints

The TTMP web server provides the following HTTP API endpoints:

### List Documents

```
GET /api/documents
```

Lists all TTMP documents.

**Response**:

```json
[
    {
        "path": "path/to/document.md",
        "metadata": {
            "id": "doc-001",
            "title": "Document Title",
            "document_type": "article",
            "status": "published",
            "tags": ["tag1", "tag2"],
            "author": "John Doe",
            "created": "2022-01-01T00:00:00Z",
            "updated": "2022-01-02T00:00:00Z"
        }
    },
    // ...
]
```

### Get Document

```
GET /api/documents/{path}
```

Gets a specific TTMP document.

**Response**:

```json
{
    "Metadata": {
        "id": "doc-001",
        "title": "Document Title",
        "document_type": "article",
        "status": "published",
        "tags": ["tag1", "tag2"],
        "owner": "John Doe",
        "created": "2022-01-01T00:00:00Z",
        "updated": "2022-01-02T00:00:00Z",
        "category": "example",
        "audience": "developers",
        "abstract": "A sample document",
        "see_also": ["doc-002"],
        "concepts": ["concept1", "concept2"],
        "source_files": ["file1.go", "file2.go"],
        "longevity": "long-term",
        "word_count": 150
    },
    "Content": "# Document Title\n\nThis is the content of the document."
}
```

### Query Documents

```
POST /api/query
```

Queries TTMP documents.

**Request**:

```json
{
    "query": "document_type='tutorial' && status='published'",
    "keywords": ["golang", "tutorial"]
}
```

**Response**:

```json
[
    {
        "Metadata": {
            "id": "tutorial-001",
            "title": "Tutorial Title",
            "document_type": "tutorial",
            "status": "published",
            "tags": ["golang", "tutorial"],
            "owner": "Jane Smith",
            "_path": "tutorials/tutorial-001.md"
        },
        "Content": "# Tutorial Title\n\nThis is a tutorial about Golang."
    },
    // ...
]
```

### Validate Document

```
POST /api/validate
```

Validates a TTMP document.

**Request**:

```json
{
    "content": "---\nid: doc-001\ntitle: Document Title\ndocument_type: article\nstatus: published\n---\n\n# Document Title\n\nContent here."
}
```

**Response** (success):

```json
{
    "valid": true
}
```

**Response** (failure):

```json
{
    "valid": false,
    "error": "Missing required field: id"
}
```

### Get Statistics

```
GET /api/stats
```

Gets statistics about TTMP documents.

**Response**:

```json
{
    "totalDocuments": 25,
    "metadataKeys": {
        "id": 25,
        "title": 25,
        "document_type": 25,
        "status": 25,
        "owner": 20,
        "tags": 18,
        "category": 15,
        "abstract": 10
    },
    "documentTypes": {
        "article": 10,
        "tutorial": 8,
        "reference": 7
    }
}
```