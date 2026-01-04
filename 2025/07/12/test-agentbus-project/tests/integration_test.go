package tests

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"strconv"
	"testing"
	"time"

	"github.com/gin-gonic/gin"
	"gorm.io/driver/sqlite"
	"gorm.io/gorm"
	"pelican-farm/internal/database"
	"pelican-farm/internal/handlers"
	"pelican-farm/internal/models"
)

func setupIntegrationTest(t *testing.T) (*gin.Engine, *gorm.DB) {
	// Create temporary database file
	dbFile := fmt.Sprintf("test_%d.db", time.Now().UnixNano())
	
	// Setup database
	db, err := gorm.Open(sqlite.Open(dbFile), &gorm.Config{})
	if err != nil {
		t.Fatalf("Failed to connect to test database: %v", err)
	}

	// Auto-migrate the schema
	err = db.AutoMigrate(&models.Pelican{}, &models.Farm{}, &models.Assignment{})
	if err != nil {
		t.Fatalf("Failed to migrate database: %v", err)
	}

	// Create repositories and handlers
	pelicanRepo := database.NewPelicanRepository(db)
	pelicanHandler := handlers.NewPelicanHandler(pelicanRepo)
	farmHandler := handlers.NewFarmHandler(db)

	// Setup Gin router
	gin.SetMode(gin.TestMode)
	router := gin.New()
	
	// Setup routes
	v1 := router.Group("/api/v1")
	{
		// Pelican routes
		v1.POST("/pelicans", pelicanHandler.CreatePelican)
		v1.GET("/pelicans", pelicanHandler.GetPelicans)
		v1.GET("/pelicans/:id", pelicanHandler.GetPelican)
		v1.PUT("/pelicans/:id", pelicanHandler.UpdatePelican)
		v1.DELETE("/pelicans/:id", pelicanHandler.DeletePelican)
		v1.GET("/pelicans/stats", pelicanHandler.GetPelicanStats)

		// Farm routes
		v1.POST("/farms", farmHandler.CreateFarm)
		v1.GET("/farms", farmHandler.GetFarms)
		v1.GET("/farms/:id", farmHandler.GetFarm)
		v1.PUT("/farms/:id", farmHandler.UpdateFarm)
		v1.DELETE("/farms/:id", farmHandler.DeleteFarm)
		v1.GET("/farms/:id/stats", farmHandler.GetFarmStats)
		v1.POST("/farms/:id/pelicans/:pelican_id", farmHandler.AssignPelican)
		v1.DELETE("/farms/:id/pelicans/:pelican_id", farmHandler.UnassignPelican)
	}

	// Cleanup function
	t.Cleanup(func() {
		os.Remove(dbFile)
	})

	return router, db
}

func createTestPelican() map[string]interface{} {
	return map[string]interface{}{
		"name":     "Integration Test Pelican",
		"species":  "Brown Pelican",
		"age":      5,
		"weight":   4.5,
		"health":   "healthy",
		"location": "Pond A",
		"gender":   "male",
		"color":    "brown",
	}
}

func createTestFarm() map[string]interface{} {
	return map[string]interface{}{
		"name":         "Integration Test Farm",
		"type":         "sanctuary",
		"location":     "California Coast",
		"capacity":     100,
		"manager_name": "John Doe",
		"manager_email": "john@example.com",
	}
}

func TestIntegration_PelicanFullCRUD(t *testing.T) {
	router, _ := setupIntegrationTest(t)

	// Test CREATE
	pelicanPayload := createTestPelican()
	jsonPayload, _ := json.Marshal(pelicanPayload)

	req, _ := http.NewRequest("POST", "/api/v1/pelicans", bytes.NewBuffer(jsonPayload))
	req.Header.Set("Content-Type", "application/json")
	
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusCreated {
		t.Fatalf("Expected status %d, got %d", http.StatusCreated, w.Code)
	}

	var createResponse map[string]interface{}
	json.Unmarshal(w.Body.Bytes(), &createResponse)
	
	data := createResponse["data"].(map[string]interface{})
	pelicanID := int(data["id"].(float64))

	// Test READ (single)
	req, _ = http.NewRequest("GET", "/api/v1/pelicans/"+strconv.Itoa(pelicanID), nil)
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	var readResponse map[string]interface{}
	json.Unmarshal(w.Body.Bytes(), &readResponse)
	
	readData := readResponse["data"].(map[string]interface{})
	if readData["name"] != pelicanPayload["name"] {
		t.Errorf("Expected name %s, got %s", pelicanPayload["name"], readData["name"])
	}

	// Test READ (all)
	req, _ = http.NewRequest("GET", "/api/v1/pelicans", nil)
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	var listResponse map[string]interface{}
	json.Unmarshal(w.Body.Bytes(), &listResponse)
	
	listData := listResponse["data"].([]interface{})
	if len(listData) != 1 {
		t.Errorf("Expected 1 pelican, got %d", len(listData))
	}

	// Test UPDATE
	updatePayload := map[string]interface{}{
		"name":   "Updated Integration Pelican",
		"age":    6,
		"health": "recovering",
	}
	jsonPayload, _ = json.Marshal(updatePayload)

	req, _ = http.NewRequest("PUT", "/api/v1/pelicans/"+strconv.Itoa(pelicanID), bytes.NewBuffer(jsonPayload))
	req.Header.Set("Content-Type", "application/json")
	
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	// Verify update
	req, _ = http.NewRequest("GET", "/api/v1/pelicans/"+strconv.Itoa(pelicanID), nil)
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)

	json.Unmarshal(w.Body.Bytes(), &readResponse)
	readData = readResponse["data"].(map[string]interface{})
	if readData["name"] != updatePayload["name"] {
		t.Errorf("Expected updated name %s, got %s", updatePayload["name"], readData["name"])
	}

	// Test DELETE
	req, _ = http.NewRequest("DELETE", "/api/v1/pelicans/"+strconv.Itoa(pelicanID), nil)
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	// Verify deletion
	req, _ = http.NewRequest("GET", "/api/v1/pelicans/"+strconv.Itoa(pelicanID), nil)
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusNotFound {
		t.Errorf("Expected status %d after deletion, got %d", http.StatusNotFound, w.Code)
	}
}

func TestIntegration_FarmFullCRUD(t *testing.T) {
	router, _ := setupIntegrationTest(t)

	// Test CREATE
	farmPayload := createTestFarm()
	jsonPayload, _ := json.Marshal(farmPayload)

	req, _ := http.NewRequest("POST", "/api/v1/farms", bytes.NewBuffer(jsonPayload))
	req.Header.Set("Content-Type", "application/json")
	
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusCreated {
		t.Fatalf("Expected status %d, got %d", http.StatusCreated, w.Code)
	}

	var createResponse map[string]interface{}
	json.Unmarshal(w.Body.Bytes(), &createResponse)
	
	data := createResponse["data"].(map[string]interface{})
	farmID := int(data["id"].(float64))

	// Test READ (single)
	req, _ = http.NewRequest("GET", "/api/v1/farms/"+strconv.Itoa(farmID), nil)
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	var readResponse map[string]interface{}
	json.Unmarshal(w.Body.Bytes(), &readResponse)
	
	readData := readResponse["data"].(map[string]interface{})
	if readData["name"] != farmPayload["name"] {
		t.Errorf("Expected name %s, got %s", farmPayload["name"], readData["name"])
	}

	// Test UPDATE
	updatePayload := map[string]interface{}{
		"name":     "Updated Integration Farm",
		"capacity": 150,
	}
	jsonPayload, _ = json.Marshal(updatePayload)

	req, _ = http.NewRequest("PUT", "/api/v1/farms/"+strconv.Itoa(farmID), bytes.NewBuffer(jsonPayload))
	req.Header.Set("Content-Type", "application/json")
	
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	// Test DELETE
	req, _ = http.NewRequest("DELETE", "/api/v1/farms/"+strconv.Itoa(farmID), nil)
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("Expected status %d, got %d", http.StatusOK, w.Code)
	}
}

func TestIntegration_PelicanFarmAssignment(t *testing.T) {
	router, _ := setupIntegrationTest(t)

	// Create a farm
	farmPayload := createTestFarm()
	jsonPayload, _ := json.Marshal(farmPayload)

	req, _ := http.NewRequest("POST", "/api/v1/farms", bytes.NewBuffer(jsonPayload))
	req.Header.Set("Content-Type", "application/json")
	
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	var farmResponse map[string]interface{}
	json.Unmarshal(w.Body.Bytes(), &farmResponse)
	farmData := farmResponse["data"].(map[string]interface{})
	farmID := int(farmData["id"].(float64))

	// Create a pelican
	pelicanPayload := createTestPelican()
	jsonPayload, _ = json.Marshal(pelicanPayload)

	req, _ = http.NewRequest("POST", "/api/v1/pelicans", bytes.NewBuffer(jsonPayload))
	req.Header.Set("Content-Type", "application/json")
	
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)

	var pelicanResponse map[string]interface{}
	json.Unmarshal(w.Body.Bytes(), &pelicanResponse)
	pelicanData := pelicanResponse["data"].(map[string]interface{})
	pelicanID := int(pelicanData["id"].(float64))

	// Test assignment
	assignmentPayload := map[string]interface{}{
		"notes": "Integration test assignment",
	}
	jsonPayload, _ = json.Marshal(assignmentPayload)

	req, _ = http.NewRequest("POST", 
		fmt.Sprintf("/api/v1/farms/%d/pelicans/%d", farmID, pelicanID), 
		bytes.NewBuffer(jsonPayload))
	req.Header.Set("Content-Type", "application/json")
	
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusCreated {
		t.Fatalf("Expected status %d, got %d", http.StatusCreated, w.Code)
	}

	// Test unassignment
	req, _ = http.NewRequest("DELETE", 
		fmt.Sprintf("/api/v1/farms/%d/pelicans/%d", farmID, pelicanID), nil)
	
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("Expected status %d, got %d", http.StatusOK, w.Code)
	}
}

func TestIntegration_PelicanFiltering(t *testing.T) {
	router, _ := setupIntegrationTest(t)

	// Create multiple pelicans with different attributes
	pelicans := []map[string]interface{}{
		{
			"name":     "Brown Pelican 1",
			"species":  "Brown Pelican",
			"age":      3,
			"health":   "healthy",
			"location": "Pond A",
		},
		{
			"name":     "Brown Pelican 2",
			"species":  "Brown Pelican",
			"age":      7,
			"health":   "sick",
			"location": "Pond B",
		},
		{
			"name":     "White Pelican 1",
			"species":  "White Pelican",
			"age":      5,
			"health":   "healthy",
			"location": "Pond A",
		},
	}

	for _, pelican := range pelicans {
		jsonPayload, _ := json.Marshal(pelican)
		req, _ := http.NewRequest("POST", "/api/v1/pelicans", bytes.NewBuffer(jsonPayload))
		req.Header.Set("Content-Type", "application/json")
		
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		if w.Code != http.StatusCreated {
			t.Fatalf("Failed to create pelican: %d", w.Code)
		}
	}

	// Test filter by species
	req, _ := http.NewRequest("GET", "/api/v1/pelicans?species=Brown Pelican", nil)
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	var response map[string]interface{}
	json.Unmarshal(w.Body.Bytes(), &response)
	
	data := response["data"].([]interface{})
	if len(data) != 2 {
		t.Errorf("Expected 2 Brown Pelicans, got %d", len(data))
	}

	// Test filter by health
	req, _ = http.NewRequest("GET", "/api/v1/pelicans?health=healthy", nil)
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	json.Unmarshal(w.Body.Bytes(), &response)
	data = response["data"].([]interface{})
	if len(data) != 2 {
		t.Errorf("Expected 2 healthy pelicans, got %d", len(data))
	}

	// Test filter by location
	req, _ = http.NewRequest("GET", "/api/v1/pelicans?location=Pond A", nil)
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	json.Unmarshal(w.Body.Bytes(), &response)
	data = response["data"].([]interface{})
	if len(data) != 2 {
		t.Errorf("Expected 2 pelicans in Pond A, got %d", len(data))
	}

	// Test age range filter
	req, _ = http.NewRequest("GET", "/api/v1/pelicans?min_age=4&max_age=6", nil)
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	json.Unmarshal(w.Body.Bytes(), &response)
	data = response["data"].([]interface{})
	if len(data) != 1 {
		t.Errorf("Expected 1 pelican in age range 4-6, got %d", len(data))
	}
}

func TestIntegration_PelicanStats(t *testing.T) {
	router, _ := setupIntegrationTest(t)

	// Create pelicans with different health statuses
	pelicans := []map[string]interface{}{
		{"name": "Healthy 1", "health": "healthy"},
		{"name": "Healthy 2", "health": "healthy"},
		{"name": "Sick 1", "health": "sick"},
		{"name": "Injured 1", "health": "injured"},
	}

	for _, pelican := range pelicans {
		jsonPayload, _ := json.Marshal(pelican)
		req, _ := http.NewRequest("POST", "/api/v1/pelicans", bytes.NewBuffer(jsonPayload))
		req.Header.Set("Content-Type", "application/json")
		
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		if w.Code != http.StatusCreated {
			t.Fatalf("Failed to create pelican: %d", w.Code)
		}
	}

	// Test stats endpoint
	req, _ := http.NewRequest("GET", "/api/v1/pelicans/stats", nil)
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	var response map[string]interface{}
	json.Unmarshal(w.Body.Bytes(), &response)
	
	data := response["data"].(map[string]interface{})
	if data["total"] != float64(4) {
		t.Errorf("Expected total 4, got %v", data["total"])
	}
	if data["healthy"] != float64(2) {
		t.Errorf("Expected healthy 2, got %v", data["healthy"])
	}
	if data["sick"] != float64(1) {
		t.Errorf("Expected sick 1, got %v", data["sick"])
	}
	if data["injured"] != float64(1) {
		t.Errorf("Expected injured 1, got %v", data["injured"])
	}
}

func BenchmarkIntegration_PelicanCRUD(b *testing.B) {
	router, _ := setupIntegrationTest(&testing.T{})

	pelicanPayload := createTestPelican()
	jsonPayload, _ := json.Marshal(pelicanPayload)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		// Create
		req, _ := http.NewRequest("POST", "/api/v1/pelicans", bytes.NewBuffer(jsonPayload))
		req.Header.Set("Content-Type", "application/json")
		
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		if w.Code != http.StatusCreated {
			b.Fatalf("Failed to create pelican: %d", w.Code)
		}

		var response map[string]interface{}
		json.Unmarshal(w.Body.Bytes(), &response)
		data := response["data"].(map[string]interface{})
		pelicanID := int(data["id"].(float64))

		// Read
		req, _ = http.NewRequest("GET", "/api/v1/pelicans/"+strconv.Itoa(pelicanID), nil)
		w = httptest.NewRecorder()
		router.ServeHTTP(w, req)

		// Delete
		req, _ = http.NewRequest("DELETE", "/api/v1/pelicans/"+strconv.Itoa(pelicanID), nil)
		w = httptest.NewRecorder()
		router.ServeHTTP(w, req)
	}
} 