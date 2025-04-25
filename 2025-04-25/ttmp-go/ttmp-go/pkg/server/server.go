package server

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"path/filepath"
	"strings"

	"github.com/gorilla/mux"
	"github.com/sirupsen/logrus"
	"github.com/scrapybara/ttmp/pkg/model"
	"github.com/scrapybara/ttmp/pkg/parser"
	"github.com/scrapybara/ttmp/pkg/query"
	"github.com/scrapybara/ttmp/pkg/util"
	"github.com/scrapybara/ttmp/pkg/validator"
)

// Server represents a TTMP web server
type Server struct {
	basePath  string
	router    *mux.Router
	parser    *parser.TTMPParser
	validator *validator.Validator
	logger    *logrus.Logger
}

// NewServer creates a new TTMP web server
func NewServer(basePath string) (*Server, error) {
	// Create logger
	logger := logrus.New()
	logger.SetLevel(logrus.InfoLevel)

	// Create a new router
	router := mux.NewRouter()

	// Create parser and validator
	p := parser.NewTTMPParser(logger)
	v := validator.NewValidator(logger)

	// Create the server
	s := &Server{
		basePath:  basePath,
		router:    router,
		parser:    p,
		validator: v,
		logger:    logger,
	}

	// Register routes
	s.registerRoutes()

	return s, nil
}

// Start starts the TTMP web server
func (s *Server) Start(addr string) error {
	log.Printf("Starting TTMP web server on %s\n", addr)
	log.Printf("Serving documents from %s\n", s.basePath)
	return http.ListenAndServe(addr, s.router)
}

// registerRoutes registers all the HTTP routes for the server
func (s *Server) registerRoutes() {
	// API routes
	s.router.HandleFunc("/api/documents", s.handleListDocuments).Methods("GET")
	s.router.HandleFunc("/api/documents/{path:.*}", s.handleGetDocument).Methods("GET")
	s.router.HandleFunc("/api/query", s.handleQueryDocuments).Methods("POST")
	s.router.HandleFunc("/api/validate", s.handleValidateDocument).Methods("POST")
	s.router.HandleFunc("/api/stats", s.handleGetStats).Methods("GET")

	// Static files
	staticFS := http.FileServer(http.Dir("./pkg/server/static"))
	s.router.PathPrefix("/static/").Handler(http.StripPrefix("/static/", staticFS))

	// SPA handling - serve index.html for all other routes
	s.router.PathPrefix("/").HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.ServeFile(w, r, "./pkg/server/static/index.html")
	})
}

// handleListDocuments handles requests to list all documents
func (s *Server) handleListDocuments(w http.ResponseWriter, r *http.Request) {
	files, err := util.FindMarkdownFiles(s.basePath)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error finding markdown files: %v", err), http.StatusInternalServerError)
		return
	}

	// Create a list of document metadata
	var documents []map[string]interface{}
	for _, file := range files {
		doc, err := s.parser.ParseFile(file)
		if err != nil {
			// Skip files that can't be parsed
			continue
		}

		// Get relative path from base path
		relPath, err := filepath.Rel(s.basePath, file)
		if err != nil {
			// Skip files with path issues
			continue
		}

		// Convert TTMPDocument to a map for JSON output
		metadata := make(map[string]interface{})
		metadata["id"] = doc.ID
		metadata["title"] = doc.Title
		metadata["status"] = doc.Status
		metadata["document_type"] = doc.DocumentType
		metadata["tags"] = doc.Tags
		metadata["category"] = doc.Category
		metadata["author"] = doc.Owner
		if doc.Created != nil {
			metadata["created"] = doc.Created
		}
		if doc.Updated != nil {
			metadata["updated"] = doc.Updated
		}

		// Add document metadata to the list
		documents = append(documents, map[string]interface{}{
			"path":     relPath,
			"metadata": metadata,
		})
	}

	// Send the response
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(documents)
}

// handleGetDocument handles requests to get a specific document
func (s *Server) handleGetDocument(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	path := vars["path"]

	// Ensure the path doesn't try to escape the base path
	fullPath := filepath.Join(s.basePath, path)
	if !strings.HasPrefix(fullPath, s.basePath) {
		http.Error(w, "Invalid path", http.StatusBadRequest)
		return
	}

	// Parse the document
	doc, err := s.parser.ParseFile(fullPath)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error parsing document: %v", err), http.StatusInternalServerError)
		return
	}

	// Create a response structure
	response := struct {
		Metadata map[string]interface{} `json:"Metadata"`
		Content  string                 `json:"Content"`
	}{
		Metadata: make(map[string]interface{}),
		Content:  doc.Content,
	}

	// Copy document fields to metadata map
	response.Metadata["id"] = doc.ID
	response.Metadata["title"] = doc.Title
	response.Metadata["status"] = doc.Status
	response.Metadata["document_type"] = doc.DocumentType
	response.Metadata["tags"] = doc.Tags
	response.Metadata["category"] = doc.Category
	response.Metadata["owner"] = doc.Owner
	response.Metadata["audience"] = doc.Audience
	response.Metadata["concepts"] = doc.Concepts
	response.Metadata["see_also"] = doc.SeeAlso
	response.Metadata["source_files"] = doc.SourceFiles
	response.Metadata["abstract"] = doc.Abstract
	response.Metadata["longevity"] = doc.Longevity
	if doc.Created != nil {
		response.Metadata["created"] = doc.Created
	}
	if doc.Updated != nil {
		response.Metadata["updated"] = doc.Updated
	}
	response.Metadata["word_count"] = doc.WordCount

	// Send the response
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(response)
}

// handleQueryDocuments handles requests to query documents
func (s *Server) handleQueryDocuments(w http.ResponseWriter, r *http.Request) {
	// Parse the query from the request body
	var queryParams struct {
		Query    string   `json:"query"`
		Keywords []string `json:"keywords,omitempty"`
	}
	if err := json.NewDecoder(r.Body).Decode(&queryParams); err != nil {
		http.Error(w, "Invalid query parameters", http.StatusBadRequest)
		return
	}

	// Find all markdown files
	files, err := util.FindMarkdownFiles(s.basePath)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error finding markdown files: %v", err), http.StatusInternalServerError)
		return
	}

	// Parse all documents
	var documents []*model.TTMPDocument
	for _, file := range files {
		doc, err := s.parser.ParseFile(file)
		if err != nil {
			// Skip files that can't be parsed
			continue
		}

		// Get relative path from base path
		relPath, err := filepath.Rel(s.basePath, file)
		if err != nil {
			// Skip files with path issues
			continue
		}

		// Add the path to the document metadata
		doc.FilePath = file
		doc.RelativeFilePath = relPath
        
		// Add custom metadata
		if doc.Tags == nil {
			doc.Tags = []string{}
		}
		documents = append(documents, doc)
	}

	// Perform the query
	results, err := query.QueryDocuments(documents, queryParams.Query, queryParams.Keywords)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error querying documents: %v", err), http.StatusInternalServerError)
		return
	}

	// Format results
	var formattedResults []struct {
		Metadata map[string]interface{} `json:"Metadata"`
		Content  string                 `json:"Content"`
	}

	for _, doc := range results {
		// Create metadata map
		metadata := make(map[string]interface{})
		metadata["id"] = doc.ID
		metadata["title"] = doc.Title
		metadata["status"] = doc.Status
		metadata["document_type"] = doc.DocumentType
		metadata["tags"] = doc.Tags
		metadata["category"] = doc.Category
		metadata["author"] = doc.Owner
		if doc.Created != nil {
			metadata["created"] = doc.Created
		}
		if doc.Updated != nil {
			metadata["updated"] = doc.Updated
		}
		metadata["_path"] = doc.RelativeFilePath

		formattedResults = append(formattedResults, struct {
			Metadata map[string]interface{} `json:"Metadata"`
			Content  string                 `json:"Content"`
		}{
			Metadata: metadata,
			Content:  doc.Content,
		})
	}

	// Send the response
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(formattedResults)
}

// handleValidateDocument handles requests to validate a document
func (s *Server) handleValidateDocument(w http.ResponseWriter, r *http.Request) {
	// Parse the document from the request body
	var docRequest struct {
		Content string `json:"content"`
	}
	if err := json.NewDecoder(r.Body).Decode(&docRequest); err != nil {
		http.Error(w, "Invalid document", http.StatusBadRequest)
		return
	}

	// Parse the document
	doc, err := s.parser.ParseString(docRequest.Content)
	if err != nil {
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]interface{}{
			"valid": false,
			"error": err.Error(),
		})
		return
	}

	// Validate the document
	result, err := s.validator.Validate(doc)
	if err != nil {
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]interface{}{
			"valid": false,
			"error": err.Error(),
		})
		return
	}

	// Check if there are any validation errors
	if !result.Valid {
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]interface{}{
			"valid": false,
			"error": strings.Join(result.GetErrorStrings(), "; "),
		})
		return
	}

	// Send the response
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"valid": true,
	})
}

// handleGetStats handles requests to get document statistics
func (s *Server) handleGetStats(w http.ResponseWriter, r *http.Request) {
	// Find all markdown files
	files, err := util.FindMarkdownFiles(s.basePath)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error finding markdown files: %v", err), http.StatusInternalServerError)
		return
	}

	// Parse all documents
	var documents []*model.TTMPDocument
	for _, file := range files {
		doc, err := s.parser.ParseFile(file)
		if err != nil {
			// Skip files that can't be parsed
			continue
		}
		documents = append(documents, doc)
	}

	// Calculate statistics
	stats := map[string]interface{}{
		"totalDocuments": len(documents),
	}

	// Count documents by each metadata key
	metadataCounts := make(map[string]int)
	for _, doc := range documents {
		// Count standard metadata fields
		if doc.ID != "" {
			metadataCounts["id"]++
		}
		if doc.Title != "" {
			metadataCounts["title"]++
		}
		if doc.Status != "" {
			metadataCounts["status"]++
		}
		if doc.DocumentType != "" {
			metadataCounts["document_type"]++
		}
		if doc.Category != "" {
			metadataCounts["category"]++
		}
		if doc.Owner != "" {
			metadataCounts["owner"]++
		}
		if doc.Abstract != "" {
			metadataCounts["abstract"]++
		}
		if doc.Audience != "" {
			metadataCounts["audience"]++
		}
		if len(doc.Tags) > 0 {
			metadataCounts["tags"]++
		}
		if len(doc.Concepts) > 0 {
			metadataCounts["concepts"]++
		}
		if len(doc.SeeAlso) > 0 {
			metadataCounts["see_also"]++
		}
		if len(doc.SourceFiles) > 0 {
			metadataCounts["source_files"]++
		}
	}
	stats["metadataKeys"] = metadataCounts

	// Count by document type
	docTypes := make(map[string]int)
	for _, doc := range documents {
		if doc.DocumentType != "" {
			docTypes[doc.DocumentType]++
		}
	}
	stats["documentTypes"] = docTypes

	// Send the response
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(stats)
}