package handlers

import (
	"bytes"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"strconv"
	"testing"

	"github.com/gin-gonic/gin"
	"gorm.io/driver/sqlite"
	"gorm.io/gorm"
	"pelican-farm/internal/database"
	"pelican-farm/internal/models"
)

func setupTestHandler(t *testing.T) (*PelicanHandler, *gin.Engine) {
	// Setup test database
	db, err := gorm.Open(sqlite.Open(":memory:"), &gorm.Config{})
	if err != nil {
		t.Fatalf("Failed to connect to test database: %v", err)
	}

	// Auto-migrate the schema
	err = db.AutoMigrate(&models.Pelican{})
	if err != nil {
		t.Fatalf("Failed to migrate database: %v", err)
	}

	// Create repository and handler
	repo := database.NewPelicanRepository(db)
	handler := NewPelicanHandler(repo)

	// Setup Gin router
	gin.SetMode(gin.TestMode)
	router := gin.New()
	
	// Setup routes
	router.POST("/pelicans", handler.CreatePelican)
	router.GET("/pelicans", handler.GetPelicans)
	router.GET("/pelicans/:id", handler.GetPelican)
	router.PUT("/pelicans/:id", handler.UpdatePelican)
	router.DELETE("/pelicans/:id", handler.DeletePelican)
	router.GET("/pelicans/stats", handler.GetPelicanStats)

	return handler, router
}

func createTestPelicanPayload() map[string]interface{} {
	return map[string]interface{}{
		"name":     "Test Pelican",
		"species":  "Brown Pelican",
		"age":      5,
		"weight":   4.5,
		"health":   "healthy",
		"location": "Pond A",
		"gender":   "male",
		"color":    "brown",
	}
}

func TestPelicanHandler_CreatePelican_Success(t *testing.T) {
	_, router := setupTestHandler(t)

	payload := createTestPelicanPayload()
	jsonPayload, _ := json.Marshal(payload)

	req, _ := http.NewRequest("POST", "/pelicans", bytes.NewBuffer(jsonPayload))
	req.Header.Set("Content-Type", "application/json")
	
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusCreated {
		t.Errorf("Expected status %d, got %d", http.StatusCreated, w.Code)
	}

	var response map[string]interface{}
	json.Unmarshal(w.Body.Bytes(), &response)
	
	if response["message"] != "Pelican created successfully" {
		t.Errorf("Expected success message, got %v", response["message"])
	}
	
	data := response["data"].(map[string]interface{})
	if data["name"] != payload["name"] {
		t.Errorf("Expected name %s, got %s", payload["name"], data["name"])
	}
}

func TestPelicanHandler_CreatePelican_InvalidJSON(t *testing.T) {
	_, router := setupTestHandler(t)

	req, _ := http.NewRequest("POST", "/pelicans", bytes.NewBuffer([]byte("invalid json")))
	req.Header.Set("Content-Type", "application/json")
	
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusBadRequest {
		t.Errorf("Expected status %d, got %d", http.StatusBadRequest, w.Code)
	}

	var response map[string]interface{}
	json.Unmarshal(w.Body.Bytes(), &response)
	
	if response["error"] != "Invalid request body" {
		t.Errorf("Expected error message, got %v", response["error"])
	}
}

func TestPelicanHandler_GetPelicans_Success(t *testing.T) {
	handler, router := setupTestHandler(t)

	// Create a test pelican first
	pelican := &models.Pelican{
		Name:     "Test Pelican",
		Species:  "Brown Pelican",
		Age:      5,
		Weight:   4.5,
		Health:   models.HealthStatusHealthy,
		Location: "Pond A",
		Gender:   "male",
		Color:    "brown",
	}
	handler.repo.Create(pelican)

	req, _ := http.NewRequest("GET", "/pelicans", nil)
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Errorf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	var response map[string]interface{}
	json.Unmarshal(w.Body.Bytes(), &response)
	
	data := response["data"].([]interface{})
	if len(data) != 1 {
		t.Errorf("Expected 1 pelican, got %d", len(data))
	}
}

func TestPelicanHandler_GetPelicans_WithFilter(t *testing.T) {
	handler, router := setupTestHandler(t)

	// Create test pelicans with different species
	pelican1 := &models.Pelican{
		Name:    "Brown Pelican",
		Species: "Brown Pelican",
		Health:  models.HealthStatusHealthy,
	}
	pelican2 := &models.Pelican{
		Name:    "White Pelican",
		Species: "White Pelican",
		Health:  models.HealthStatusHealthy,
	}
	handler.repo.Create(pelican1)
	handler.repo.Create(pelican2)

	req, _ := http.NewRequest("GET", "/pelicans?species=Brown Pelican", nil)
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Errorf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	var response map[string]interface{}
	json.Unmarshal(w.Body.Bytes(), &response)
	
	data := response["data"].([]interface{})
	if len(data) != 1 {
		t.Errorf("Expected 1 pelican, got %d", len(data))
	}
}

func TestPelicanHandler_GetPelican_Success(t *testing.T) {
	handler, router := setupTestHandler(t)

	// Create a test pelican first
	pelican := &models.Pelican{
		Name:     "Test Pelican",
		Species:  "Brown Pelican",
		Age:      5,
		Weight:   4.5,
		Health:   models.HealthStatusHealthy,
		Location: "Pond A",
		Gender:   "male",
		Color:    "brown",
	}
	handler.repo.Create(pelican)

	req, _ := http.NewRequest("GET", "/pelicans/"+strconv.Itoa(pelican.ID), nil)
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Errorf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	var response map[string]interface{}
	json.Unmarshal(w.Body.Bytes(), &response)
	
	data := response["data"].(map[string]interface{})
	if data["name"] != pelican.Name {
		t.Errorf("Expected name %s, got %s", pelican.Name, data["name"])
	}
}

func TestPelicanHandler_GetPelican_NotFound(t *testing.T) {
	_, router := setupTestHandler(t)

	req, _ := http.NewRequest("GET", "/pelicans/999", nil)
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusNotFound {
		t.Errorf("Expected status %d, got %d", http.StatusNotFound, w.Code)
	}
}

func TestPelicanHandler_GetPelican_InvalidID(t *testing.T) {
	_, router := setupTestHandler(t)

	req, _ := http.NewRequest("GET", "/pelicans/invalid", nil)
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusBadRequest {
		t.Errorf("Expected status %d, got %d", http.StatusBadRequest, w.Code)
	}
}

func TestPelicanHandler_UpdatePelican_Success(t *testing.T) {
	handler, router := setupTestHandler(t)

	// Create a test pelican first
	pelican := &models.Pelican{
		Name:     "Test Pelican",
		Species:  "Brown Pelican",
		Age:      5,
		Weight:   4.5,
		Health:   models.HealthStatusHealthy,
		Location: "Pond A",
		Gender:   "male",
		Color:    "brown",
	}
	handler.repo.Create(pelican)

	updatePayload := map[string]interface{}{
		"name":   "Updated Pelican",
		"age":    6,
		"health": "recovering",
	}
	jsonPayload, _ := json.Marshal(updatePayload)

	req, _ := http.NewRequest("PUT", "/pelicans/"+strconv.Itoa(pelican.ID), bytes.NewBuffer(jsonPayload))
	req.Header.Set("Content-Type", "application/json")
	
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Errorf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	var response map[string]interface{}
	json.Unmarshal(w.Body.Bytes(), &response)
	
	if response["message"] != "Pelican updated successfully" {
		t.Errorf("Expected success message, got %v", response["message"])
	}
}

func TestPelicanHandler_UpdatePelican_NotFound(t *testing.T) {
	_, router := setupTestHandler(t)

	updatePayload := map[string]interface{}{
		"name": "Updated Pelican",
	}
	jsonPayload, _ := json.Marshal(updatePayload)

	req, _ := http.NewRequest("PUT", "/pelicans/999", bytes.NewBuffer(jsonPayload))
	req.Header.Set("Content-Type", "application/json")
	
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusNotFound {
		t.Errorf("Expected status %d, got %d", http.StatusNotFound, w.Code)
	}
}

func TestPelicanHandler_DeletePelican_Success(t *testing.T) {
	handler, router := setupTestHandler(t)

	// Create a test pelican first
	pelican := &models.Pelican{
		Name:     "Test Pelican",
		Species:  "Brown Pelican",
		Age:      5,
		Weight:   4.5,
		Health:   models.HealthStatusHealthy,
		Location: "Pond A",
		Gender:   "male",
		Color:    "brown",
	}
	handler.repo.Create(pelican)

	req, _ := http.NewRequest("DELETE", "/pelicans/"+strconv.Itoa(pelican.ID), nil)
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Errorf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	var response map[string]interface{}
	json.Unmarshal(w.Body.Bytes(), &response)
	
	if response["message"] != "Pelican deleted successfully" {
		t.Errorf("Expected success message, got %v", response["message"])
	}
}

func TestPelicanHandler_DeletePelican_NotFound(t *testing.T) {
	_, router := setupTestHandler(t)

	req, _ := http.NewRequest("DELETE", "/pelicans/999", nil)
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusNotFound {
		t.Errorf("Expected status %d, got %d", http.StatusNotFound, w.Code)
	}
}

func TestPelicanHandler_GetPelicanStats_Success(t *testing.T) {
	handler, router := setupTestHandler(t)

	// Create test pelicans with different health statuses
	pelican1 := &models.Pelican{
		Name:   "Healthy Pelican",
		Health: models.HealthStatusHealthy,
	}
	pelican2 := &models.Pelican{
		Name:   "Sick Pelican",
		Health: models.HealthStatusSick,
	}
	handler.repo.Create(pelican1)
	handler.repo.Create(pelican2)

	req, _ := http.NewRequest("GET", "/pelicans/stats", nil)
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Errorf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	var response map[string]interface{}
	json.Unmarshal(w.Body.Bytes(), &response)
	
	data := response["data"].(map[string]interface{})
	if data["total"] != float64(2) {
		t.Errorf("Expected total 2, got %v", data["total"])
	}
	if data["healthy"] != float64(1) {
		t.Errorf("Expected healthy 1, got %v", data["healthy"])
	}
	if data["sick"] != float64(1) {
		t.Errorf("Expected sick 1, got %v", data["sick"])
	}
}

func BenchmarkPelicanHandler_CreatePelican(b *testing.B) {
	_, router := setupTestHandler(&testing.T{})

	payload := createTestPelicanPayload()
	jsonPayload, _ := json.Marshal(payload)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		req, _ := http.NewRequest("POST", "/pelicans", bytes.NewBuffer(jsonPayload))
		req.Header.Set("Content-Type", "application/json")
		
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)
	}
}

func BenchmarkPelicanHandler_GetPelicans(b *testing.B) {
	handler, router := setupTestHandler(&testing.T{})

	// Create some test data
	for i := 0; i < 100; i++ {
		pelican := &models.Pelican{
			Name:   "Test Pelican " + strconv.Itoa(i),
			Health: models.HealthStatusHealthy,
		}
		handler.repo.Create(pelican)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		req, _ := http.NewRequest("GET", "/pelicans", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)
	}
} 