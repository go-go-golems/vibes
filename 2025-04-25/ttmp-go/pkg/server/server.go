package server

import (
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"time"

	"github.com/gorilla/mux"
	"github.com/user/ttmp-go/pkg/errors"
	"github.com/user/ttmp-go/pkg/model"
	"github.com/user/ttmp-go/pkg/parser"
	"github.com/user/ttmp-go/pkg/query"
	"github.com/user/ttmp-go/pkg/util/fileutil"
	"github.com/user/ttmp-go/pkg/validator"
)

// Server represents the TTMP web server
type Server struct {
	router     *mux.Router
	parser     *parser.TTMPParser
	validator  *validator.TTMPValidator
	querier    *query.TTMPQuerier
	collection *model.TTMPCollection
	basePath   string
}

// NewServer creates a new TTMP web server
func NewServer(basePath string) (*Server, error) {
	// Check if basePath exists
	if !fileutil.DirectoryExists(basePath) {
		return nil, errors.NewIOError(fmt.Sprintf("Base directory does not exist: %s", basePath), nil)
	}

	router := mux.NewRouter()
	
	server := &Server{
		router:     router,
		parser:     parser.NewParser(),
		validator:  validator.NewValidator(),
		querier:    query.NewQuerier(),
		collection: model.NewCollection(),
		basePath:   basePath,
	}

	// Load documents from the base path
	err := server.loadDocuments()
	if err != nil {
		return nil, err
	}

	// Register routes
	server.registerRoutes()

	return server, nil
}

// loadDocuments loads all documents from the base path into the collection
func (s *Server) loadDocuments() error {
	files, err := fileutil.FindMarkdownFilesRecursive(s.basePath)
	if err != nil {
		return errors.NewIOError(fmt.Sprintf("Failed to find markdown files in: %s", s.basePath), err)
	}

	for _, file := range files {
		doc, err := s.parser.ParseFile(file)
		if err != nil {
			// Log the error but continue with other files
			fmt.Fprintf(os.Stderr, "Warning: Failed to parse %s: %v\n", file, err)
			continue
		}

		err = s.collection.AddDocument(doc)
		if err != nil {
			// Log the error but continue with other files
			fmt.Fprintf(os.Stderr, "Warning: Failed to add document %s: %v\n", file, err)
		}
	}

	return nil
}

// registerRoutes registers all API routes
func (s *Server) registerRoutes() {
	// Document routes
	s.router.HandleFunc("/api/documents", s.handleListDocuments).Methods("GET")
	s.router.HandleFunc("/api/documents/{id}", s.handleGetDocument).Methods("GET")
	s.router.HandleFunc("/api/documents", s.handleCreateDocument).Methods("POST")
	s.router.HandleFunc("/api/documents/{id}", s.handleUpdateDocument).Methods("PUT")
	s.router.HandleFunc("/api/documents/{id}", s.handleDeleteDocument).Methods("DELETE")

	// Search and filtering routes
	s.router.HandleFunc("/api/documents/search", s.handleSearchDocuments).Methods("GET")
	s.router.HandleFunc("/api/documents/type/{type}", s.handleGetDocumentsByType).Methods("GET")
	s.router.HandleFunc("/api/documents/tag/{tag}", s.handleGetDocumentsByTag).Methods("GET")

	// Validation routes
	s.router.HandleFunc("/api/validate", s.handleValidateDocument).Methods("POST")
	s.router.HandleFunc("/api/validate/{id}", s.handleValidateDocumentById).Methods("GET")

	// Statistics routes
	s.router.HandleFunc("/api/stats", s.handleGetStats).Methods("GET")
	
	// Enable CORS
	s.router.Use(corsMiddleware)
}

// corsMiddleware adds CORS headers to responses
func corsMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// Set CORS headers
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
		w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization")
		
		// Handle preflight requests
		if r.Method == "OPTIONS" {
			w.WriteHeader(http.StatusOK)
			return
		}
		
		// Call the next handler
		next.ServeHTTP(w, r)
	})
}

// ServeHTTP implements the http.Handler interface
func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	s.router.ServeHTTP(w, r)
}

// Start starts the server on the specified address
func (s *Server) Start(addr string) error {
	fmt.Printf("Starting TTMP web server on %s\n", addr)
	fmt.Printf("Base path: %s\n", s.basePath)
	fmt.Printf("Loaded %d documents\n", s.collection.Size())
	
	// Add static file routes
	s.AddStaticRoutes()
	
	return http.ListenAndServe(addr, s.router)
}

// respondWithJSON responds with a JSON payload
func respondWithJSON(w http.ResponseWriter, code int, payload interface{}) {
	response, err := json.Marshal(payload)
	if err != nil {
		w.WriteHeader(http.StatusInternalServerError)
		w.Write([]byte("Error encoding response"))
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(code)
	w.Write(response)
}

// respondWithError responds with an error message
func respondWithError(w http.ResponseWriter, code int, message string) {
	respondWithJSON(w, code, map[string]string{"error": message})
}

// handleListDocuments handles GET /api/documents
func (s *Server) handleListDocuments(w http.ResponseWriter, r *http.Request) {
	documents := s.collection.GetAllDocuments()
	
	// Convert to a simpler format for JSON output
	type documentResponse struct {
		ID          string                 `json:"id"`
		Type        string                 `json:"type"`
		Title       string                 `json:"title"`
		Description string                 `json:"description,omitempty"`
		Created     string                 `json:"created"`
		Modified    string                 `json:"modified,omitempty"`
		Tags        []string               `json:"tags,omitempty"`
		Links       []model.Link           `json:"links,omitempty"`
		Filename    string                 `json:"filename"`
		Custom      map[string]interface{} `json:"custom,omitempty"`
	}
	
	response := make([]documentResponse, 0, len(documents))
	
	for _, doc := range documents {
		modified := ""
		if doc.Modified != nil {
			modified = doc.Modified.Format(time.RFC3339)
		}
		
		response = append(response, documentResponse{
			ID:          doc.ID,
			Type:        doc.Type,
			Title:       doc.Title,
			Description: doc.Description,
			Created:     doc.Created.Format(time.RFC3339),
			Modified:    modified,
			Tags:        doc.Tags,
			Links:       doc.Links,
			Filename:    doc.Filename,
			Custom:      doc.Custom,
		})
	}
	
	respondWithJSON(w, http.StatusOK, response)
}

// handleGetDocument handles GET /api/documents/{id}
func (s *Server) handleGetDocument(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]
	
	doc, err := s.collection.GetDocumentByID(id)
	if err != nil {
		respondWithError(w, http.StatusNotFound, fmt.Sprintf("Document with ID '%s' not found", id))
		return
	}
	
	// Convert to a simpler format for JSON output
	type documentResponse struct {
		ID          string                 `json:"id"`
		Type        string                 `json:"type"`
		Title       string                 `json:"title"`
		Description string                 `json:"description,omitempty"`
		Created     string                 `json:"created"`
		Modified    string                 `json:"modified,omitempty"`
		Tags        []string               `json:"tags,omitempty"`
		Links       []model.Link           `json:"links,omitempty"`
		Content     string                 `json:"content"`
		Filename    string                 `json:"filename"`
		Custom      map[string]interface{} `json:"custom,omitempty"`
	}
	
	modified := ""
	if doc.Modified != nil {
		modified = doc.Modified.Format(time.RFC3339)
	}
	
	response := documentResponse{
		ID:          doc.ID,
		Type:        doc.Type,
		Title:       doc.Title,
		Description: doc.Description,
		Created:     doc.Created.Format(time.RFC3339),
		Modified:    modified,
		Tags:        doc.Tags,
		Links:       doc.Links,
		Content:     doc.Content,
		Filename:    doc.Filename,
		Custom:      doc.Custom,
	}
	
	respondWithJSON(w, http.StatusOK, response)
}

// handleCreateDocument handles POST /api/documents
func (s *Server) handleCreateDocument(w http.ResponseWriter, r *http.Request) {
	// Define the request structure
	type createRequest struct {
		ID          string                 `json:"id"`
		Type        string                 `json:"type"`
		Title       string                 `json:"title"`
		Description string                 `json:"description,omitempty"`
		Tags        []string               `json:"tags,omitempty"`
		Links       []model.Link           `json:"links,omitempty"`
		Content     string                 `json:"content"`
		Custom      map[string]interface{} `json:"custom,omitempty"`
		Filename    string                 `json:"filename,omitempty"`
	}
	
	// Parse request body
	var req createRequest
	decoder := json.NewDecoder(r.Body)
	if err := decoder.Decode(&req); err != nil {
		respondWithError(w, http.StatusBadRequest, "Invalid request payload")
		return
	}
	defer r.Body.Close()
	
	// Validate required fields
	if req.ID == "" {
		respondWithError(w, http.StatusBadRequest, "ID is required")
		return
	}
	
	if req.Type == "" {
		respondWithError(w, http.StatusBadRequest, "Type is required")
		return
	}
	
	if req.Title == "" {
		respondWithError(w, http.StatusBadRequest, "Title is required")
		return
	}
	
	// Check if document with the same ID already exists
	if _, err := s.collection.GetDocumentByID(req.ID); err == nil {
		respondWithError(w, http.StatusConflict, fmt.Sprintf("Document with ID '%s' already exists", req.ID))
		return
	}
	
	// Generate a filename if not provided
	filename := req.Filename
	if filename == "" {
		filename = filepath.Join(s.basePath, req.ID+".md")
	} else if !filepath.IsAbs(filename) {
		filename = filepath.Join(s.basePath, filename)
	}
	
	// Create a new TTMPDocument
	now := time.Now()
	doc := &model.TTMPDocument{
		ID:          req.ID,
		Type:        req.Type,
		Title:       req.Title,
		Description: req.Description,
		Created:     now,
		Tags:        req.Tags,
		Links:       req.Links,
		Content:     req.Content,
		Custom:      req.Custom,
		Filename:    filename,
	}
	
	// Validate the document
	if err := s.validator.ValidateDocument(doc); err != nil {
		respondWithError(w, http.StatusBadRequest, fmt.Sprintf("Document validation failed: %v", err))
		return
	}
	
	// Write the document to a file
	if err := s.parser.WriteDocumentToFile(doc, filename); err != nil {
		respondWithError(w, http.StatusInternalServerError, fmt.Sprintf("Failed to write document to file: %v", err))
		return
	}
	
	// Add the document to the collection
	if err := s.collection.AddDocument(doc); err != nil {
		respondWithError(w, http.StatusInternalServerError, fmt.Sprintf("Failed to add document to collection: %v", err))
		return
	}
	
	respondWithJSON(w, http.StatusCreated, doc)
}

// handleUpdateDocument handles PUT /api/documents/{id}
func (s *Server) handleUpdateDocument(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]
	
	// Check if document exists
	doc, err := s.collection.GetDocumentByID(id)
	if err != nil {
		respondWithError(w, http.StatusNotFound, fmt.Sprintf("Document with ID '%s' not found", id))
		return
	}
	
	// Define the request structure
	type updateRequest struct {
		Type        string                 `json:"type"`
		Title       string                 `json:"title"`
		Description string                 `json:"description,omitempty"`
		Tags        []string               `json:"tags,omitempty"`
		Links       []model.Link           `json:"links,omitempty"`
		Content     string                 `json:"content"`
		Custom      map[string]interface{} `json:"custom,omitempty"`
	}
	
	// Parse request body
	var req updateRequest
	decoder := json.NewDecoder(r.Body)
	if err := decoder.Decode(&req); err != nil {
		respondWithError(w, http.StatusBadRequest, "Invalid request payload")
		return
	}
	defer r.Body.Close()
	
	// Update the document
	if req.Type != "" {
		doc.Type = req.Type
	}
	
	if req.Title != "" {
		doc.Title = req.Title
	}
	
	if req.Description != "" {
		doc.Description = req.Description
	}
	
	if req.Tags != nil {
		doc.Tags = req.Tags
	}
	
	if req.Links != nil {
		doc.Links = req.Links
	}
	
	if req.Content != "" {
		doc.Content = req.Content
	}
	
	if req.Custom != nil {
		doc.Custom = req.Custom
	}
	
	// Set modified timestamp
	now := time.Now()
	doc.Modified = &now
	
	// Validate the document
	if err := s.validator.ValidateDocument(doc); err != nil {
		respondWithError(w, http.StatusBadRequest, fmt.Sprintf("Document validation failed: %v", err))
		return
	}
	
	// Write the document to a file
	if err := s.parser.WriteDocumentToFile(doc, doc.Filename); err != nil {
		respondWithError(w, http.StatusInternalServerError, fmt.Sprintf("Failed to write document to file: %v", err))
		return
	}
	
	respondWithJSON(w, http.StatusOK, doc)
}

// handleDeleteDocument handles DELETE /api/documents/{id}
func (s *Server) handleDeleteDocument(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]
	
	// Check if document exists
	doc, err := s.collection.GetDocumentByID(id)
	if err != nil {
		respondWithError(w, http.StatusNotFound, fmt.Sprintf("Document with ID '%s' not found", id))
		return
	}
	
	// Remove the file
	if err := os.Remove(doc.Filename); err != nil {
		respondWithError(w, http.StatusInternalServerError, fmt.Sprintf("Failed to delete file: %v", err))
		return
	}
	
	// Remove the document from the collection
	if err := s.collection.RemoveDocument(id); err != nil {
		respondWithError(w, http.StatusInternalServerError, fmt.Sprintf("Failed to remove document from collection: %v", err))
		return
	}
	
	respondWithJSON(w, http.StatusOK, map[string]string{"message": fmt.Sprintf("Document with ID '%s' was deleted", id)})
}

// handleSearchDocuments handles GET /api/documents/search
func (s *Server) handleSearchDocuments(w http.ResponseWriter, r *http.Request) {
	// Get query parameters
	field := r.URL.Query().Get("field")
	operator := r.URL.Query().Get("operator")
	value := r.URL.Query().Get("value")
	
	// Validate parameters
	if field == "" {
		respondWithError(w, http.StatusBadRequest, "Field parameter is required")
		return
	}
	
	if operator == "" {
		respondWithError(w, http.StatusBadRequest, "Operator parameter is required")
		return
	}
	
	if value == "" {
		respondWithError(w, http.StatusBadRequest, "Value parameter is required")
		return
	}
	
	// Validate operator
	op := query.Operator(operator)
	validOps := map[query.Operator]bool{
		query.Equal:              true,
		query.NotEqual:           true,
		query.Contains:           true,
		query.StartsWith:         true,
		query.EndsWith:           true,
		query.GreaterThan:        true,
		query.LessThan:           true,
		query.GreaterThanOrEqual: true,
		query.LessThanOrEqual:    true,
		query.Matches:            true,
	}
	
	if !validOps[op] {
		respondWithError(w, http.StatusBadRequest, fmt.Sprintf("Invalid operator: %s", operator))
		return
	}
	
	// Create a query condition
	condition := query.QueryCondition{
		Field:    field,
		Operator: op,
		Value:    value,
	}
	
	// Execute the query
	results, err := s.querier.Query(s.collection.GetAllDocuments(), []query.QueryCondition{condition})
	if err != nil {
		respondWithError(w, http.StatusInternalServerError, fmt.Sprintf("Query execution failed: %v", err))
		return
	}
	
	// Convert to a simpler format for JSON output
	type documentResponse struct {
		ID          string                 `json:"id"`
		Type        string                 `json:"type"`
		Title       string                 `json:"title"`
		Description string                 `json:"description,omitempty"`
		Created     string                 `json:"created"`
		Modified    string                 `json:"modified,omitempty"`
		Tags        []string               `json:"tags,omitempty"`
		Filename    string                 `json:"filename"`
		Custom      map[string]interface{} `json:"custom,omitempty"`
	}
	
	response := make([]documentResponse, 0, len(results))
	
	for _, result := range results {
		doc := result.Document
		
		modified := ""
		if doc.Modified != nil {
			modified = doc.Modified.Format(time.RFC3339)
		}
		
		response = append(response, documentResponse{
			ID:          doc.ID,
			Type:        doc.Type,
			Title:       doc.Title,
			Description: doc.Description,
			Created:     doc.Created.Format(time.RFC3339),
			Modified:    modified,
			Tags:        doc.Tags,
			Filename:    doc.Filename,
			Custom:      doc.Custom,
		})
	}
	
	respondWithJSON(w, http.StatusOK, response)
}

// handleGetDocumentsByType handles GET /api/documents/type/{type}
func (s *Server) handleGetDocumentsByType(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	docType := vars["type"]
	
	documents := s.collection.GetDocumentsByType(docType)
	
	// Convert to a simpler format for JSON output
	type documentResponse struct {
		ID          string                 `json:"id"`
		Type        string                 `json:"type"`
		Title       string                 `json:"title"`
		Description string                 `json:"description,omitempty"`
		Created     string                 `json:"created"`
		Modified    string                 `json:"modified,omitempty"`
		Tags        []string               `json:"tags,omitempty"`
		Filename    string                 `json:"filename"`
		Custom      map[string]interface{} `json:"custom,omitempty"`
	}
	
	response := make([]documentResponse, 0, len(documents))
	
	for _, doc := range documents {
		modified := ""
		if doc.Modified != nil {
			modified = doc.Modified.Format(time.RFC3339)
		}
		
		response = append(response, documentResponse{
			ID:          doc.ID,
			Type:        doc.Type,
			Title:       doc.Title,
			Description: doc.Description,
			Created:     doc.Created.Format(time.RFC3339),
			Modified:    modified,
			Tags:        doc.Tags,
			Filename:    doc.Filename,
			Custom:      doc.Custom,
		})
	}
	
	respondWithJSON(w, http.StatusOK, response)
}

// handleGetDocumentsByTag handles GET /api/documents/tag/{tag}
func (s *Server) handleGetDocumentsByTag(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	tag := vars["tag"]
	
	documents := s.collection.GetDocumentsByTag(tag)
	
	// Convert to a simpler format for JSON output
	type documentResponse struct {
		ID          string                 `json:"id"`
		Type        string                 `json:"type"`
		Title       string                 `json:"title"`
		Description string                 `json:"description,omitempty"`
		Created     string                 `json:"created"`
		Modified    string                 `json:"modified,omitempty"`
		Tags        []string               `json:"tags,omitempty"`
		Filename    string                 `json:"filename"`
		Custom      map[string]interface{} `json:"custom,omitempty"`
	}
	
	response := make([]documentResponse, 0, len(documents))
	
	for _, doc := range documents {
		modified := ""
		if doc.Modified != nil {
			modified = doc.Modified.Format(time.RFC3339)
		}
		
		response = append(response, documentResponse{
			ID:          doc.ID,
			Type:        doc.Type,
			Title:       doc.Title,
			Description: doc.Description,
			Created:     doc.Created.Format(time.RFC3339),
			Modified:    modified,
			Tags:        doc.Tags,
			Filename:    doc.Filename,
			Custom:      doc.Custom,
		})
	}
	
	respondWithJSON(w, http.StatusOK, response)
}

// handleValidateDocument handles POST /api/validate
func (s *Server) handleValidateDocument(w http.ResponseWriter, r *http.Request) {
	// Parse request body
	var doc model.TTMPDocument
	decoder := json.NewDecoder(r.Body)
	if err := decoder.Decode(&doc); err != nil {
		respondWithError(w, http.StatusBadRequest, "Invalid request payload")
		return
	}
	defer r.Body.Close()
	
	// Validate the document
	err := s.validator.ValidateDocument(&doc)
	if err != nil {
		// Format the validation errors
		var errorMessages []string
		if valErrs, ok := err.(*errors.ValidationErrors); ok {
			for _, err := range valErrs.Errors {
				errorMessages = append(errorMessages, err.Message)
			}
		} else {
			errorMessages = append(errorMessages, err.Error())
		}
		
		respondWithJSON(w, http.StatusOK, map[string]interface{}{
			"valid": false,
			"errors": errorMessages,
		})
		return
	}
	
	respondWithJSON(w, http.StatusOK, map[string]interface{}{
		"valid": true,
	})
}

// handleValidateDocumentById handles GET /api/validate/{id}
func (s *Server) handleValidateDocumentById(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]
	
	// Check if document exists
	doc, err := s.collection.GetDocumentByID(id)
	if err != nil {
		respondWithError(w, http.StatusNotFound, fmt.Sprintf("Document with ID '%s' not found", id))
		return
	}
	
	// Validate the document
	err = s.validator.ValidateDocument(doc)
	if err != nil {
		// Format the validation errors
		var errorMessages []string
		if valErrs, ok := err.(*errors.ValidationErrors); ok {
			for _, err := range valErrs.Errors {
				errorMessages = append(errorMessages, err.Message)
			}
		} else {
			errorMessages = append(errorMessages, err.Error())
		}
		
		respondWithJSON(w, http.StatusOK, map[string]interface{}{
			"valid": false,
			"errors": errorMessages,
		})
		return
	}
	
	respondWithJSON(w, http.StatusOK, map[string]interface{}{
		"valid": true,
	})
}

// handleGetStats handles GET /api/stats
func (s *Server) handleGetStats(w http.ResponseWriter, r *http.Request) {
	documents := s.collection.GetAllDocuments()
	
	// Count document types
	typeCount := make(map[string]int)
	for _, doc := range documents {
		typeCount[doc.Type]++
	}
	
	// Count tags
	tagCount := make(map[string]int)
	for _, doc := range documents {
		for _, tag := range doc.Tags {
			tagCount[tag]++
		}
	}
	
	// Get top tags
	type tagStat struct {
		Name  string `json:"name"`
		Count int    `json:"count"`
	}
	
	var topTags []tagStat
	for tag, count := range tagCount {
		topTags = append(topTags, tagStat{tag, count})
	}
	
	// Sort tags by count (descending)
	for i := 0; i < len(topTags); i++ {
		for j := i + 1; j < len(topTags); j++ {
			if topTags[i].Count < topTags[j].Count {
				topTags[i], topTags[j] = topTags[j], topTags[i]
			}
		}
	}
	
	// Limit to top 10
	if len(topTags) > 10 {
		topTags = topTags[:10]
	}
	
	// Count documents by creation date (last 30 days)
	now := time.Now()
	dateCount := make(map[string]int)
	for i := 0; i < 30; i++ {
		date := now.AddDate(0, 0, -i).Format("2006-01-02")
		dateCount[date] = 0
	}
	
	for _, doc := range documents {
		date := doc.Created.Format("2006-01-02")
		if _, ok := dateCount[date]; ok {
			dateCount[date]++
		}
	}
	
	// Convert date count to array
	type dateStat struct {
		Date  string `json:"date"`
		Count int    `json:"count"`
	}
	
	var dateStats []dateStat
	for date, count := range dateCount {
		dateStats = append(dateStats, dateStat{date, count})
	}
	
	// Sort date stats by date (ascending)
	for i := 0; i < len(dateStats); i++ {
		for j := i + 1; j < len(dateStats); j++ {
			if dateStats[i].Date > dateStats[j].Date {
				dateStats[i], dateStats[j] = dateStats[j], dateStats[i]
			}
		}
	}
	
	// Prepare response
	response := map[string]interface{}{
		"totalDocuments": len(documents),
		"documentTypes":  typeCount,
		"topTags":        topTags,
		"dateStats":      dateStats,
	}
	
	respondWithJSON(w, http.StatusOK, response)
}