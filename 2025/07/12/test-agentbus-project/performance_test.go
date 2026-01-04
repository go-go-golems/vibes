package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"strconv"
	"sync"
	"testing"
	"time"

	"github.com/gin-gonic/gin"
	"gorm.io/driver/sqlite"
	"gorm.io/gorm"
	"pelican-farm/internal/database"
	"pelican-farm/internal/handlers"
	"pelican-farm/internal/models"
)

func setupPerformanceTest(b *testing.B) (*gin.Engine, *gorm.DB, func()) {
	// Create temporary database file
	dbFile := fmt.Sprintf("perf_test_%d.db", time.Now().UnixNano())
	
	// Setup database
	db, err := gorm.Open(sqlite.Open(dbFile), &gorm.Config{})
	if err != nil {
		b.Fatalf("Failed to connect to test database: %v", err)
	}

	// Auto-migrate the schema
	err = db.AutoMigrate(&models.Pelican{}, &models.Farm{}, &models.Assignment{})
	if err != nil {
		b.Fatalf("Failed to migrate database: %v", err)
	}

	// Create repositories and handlers
	pelicanRepo := database.NewPelicanRepository(db)
	pelicanHandler := handlers.NewPelicanHandler(pelicanRepo)
	farmHandler := handlers.NewFarmHandler(db)

	// Setup Gin router
	gin.SetMode(gin.TestMode)
	router := gin.New()
	
	// Setup routes
	router.POST("/pelicans", pelicanHandler.CreatePelican)
	router.GET("/pelicans", pelicanHandler.GetPelicans)
	router.GET("/pelicans/:id", pelicanHandler.GetPelican)
	router.PUT("/pelicans/:id", pelicanHandler.UpdatePelican)
	router.DELETE("/pelicans/:id", pelicanHandler.DeletePelican)
	router.GET("/pelicans/stats", pelicanHandler.GetPelicanStats)
	
	router.POST("/farms", farmHandler.CreateFarm)
	router.GET("/farms", farmHandler.GetFarms)
	router.GET("/farms/:id", farmHandler.GetFarm)

	// Cleanup function
	cleanup := func() {
		os.Remove(dbFile)
	}

	return router, db, cleanup
}

func createPerfTestPelican(id int) map[string]interface{} {
	return map[string]interface{}{
		"name":     fmt.Sprintf("Perf Test Pelican %d", id),
		"species":  "Brown Pelican",
		"age":      5 + (id % 10),
		"weight":   4.5 + float64(id%5),
		"health":   []string{"healthy", "sick", "injured", "recovering"}[id%4],
		"location": fmt.Sprintf("Pond %c", 'A'+byte(id%5)),
		"gender":   []string{"male", "female"}[id%2],
		"color":    "brown",
	}
}

func createPerfTestFarm(id int) map[string]interface{} {
	return map[string]interface{}{
		"name":         fmt.Sprintf("Perf Test Farm %d", id),
		"type":         []string{"sanctuary", "breeding", "rehabilitation"}[id%3],
		"location":     fmt.Sprintf("Location %d", id),
		"capacity":     100 + (id * 10),
		"manager_name": fmt.Sprintf("Manager %d", id),
		"manager_email": fmt.Sprintf("manager%d@example.com", id),
	}
}

// Database Performance Tests
func BenchmarkDatabase_PelicanCreate(b *testing.B) {
	router, db, cleanup := setupPerformanceTest(b)
	defer cleanup()

	repo := database.NewPelicanRepository(db)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		pelican := &models.Pelican{
			Name:     fmt.Sprintf("Benchmark Pelican %d", i),
			Species:  "Brown Pelican",
			Age:      5,
			Weight:   4.5,
			Health:   models.HealthStatusHealthy,
			Location: "Pond A",
			Gender:   "male",
			Color:    "brown",
		}
		repo.Create(pelican)
	}
}

func BenchmarkDatabase_PelicanBatchCreate(b *testing.B) {
	router, db, cleanup := setupPerformanceTest(b)
	defer cleanup()

	repo := database.NewPelicanRepository(db)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		// Create batch of 100 pelicans
		for j := 0; j < 100; j++ {
			pelican := &models.Pelican{
				Name:     fmt.Sprintf("Batch Pelican %d-%d", i, j),
				Species:  "Brown Pelican",
				Age:      5 + (j % 10),
				Weight:   4.5,
				Health:   models.HealthStatusHealthy,
				Location: "Pond A",
				Gender:   "male",
				Color:    "brown",
			}
			repo.Create(pelican)
		}
	}
}

func BenchmarkDatabase_PelicanRead(b *testing.B) {
	router, db, cleanup := setupPerformanceTest(b)
	defer cleanup()

	repo := database.NewPelicanRepository(db)

	// Create test data
	pelicans := make([]*models.Pelican, 1000)
	for i := 0; i < 1000; i++ {
		pelican := &models.Pelican{
			Name:     fmt.Sprintf("Read Test Pelican %d", i),
			Species:  "Brown Pelican",
			Age:      5,
			Weight:   4.5,
			Health:   models.HealthStatusHealthy,
			Location: "Pond A",
			Gender:   "male",
			Color:    "brown",
		}
		repo.Create(pelican)
		pelicans[i] = pelican
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		repo.GetByID(uint(pelicans[i%1000].ID))
	}
}

func BenchmarkDatabase_PelicanFilter(b *testing.B) {
	router, db, cleanup := setupPerformanceTest(b)
	defer cleanup()

	repo := database.NewPelicanRepository(db)

	// Create test data with varied attributes
	for i := 0; i < 1000; i++ {
		pelican := &models.Pelican{
			Name:     fmt.Sprintf("Filter Test Pelican %d", i),
			Species:  []string{"Brown Pelican", "White Pelican"}[i%2],
			Age:      5 + (i % 10),
			Weight:   4.5,
			Health:   []models.HealthStatus{models.HealthStatusHealthy, models.HealthStatusSick}[i%2],
			Location: fmt.Sprintf("Pond %c", 'A'+byte(i%5)),
			Gender:   "male",
			Color:    "brown",
		}
		repo.Create(pelican)
	}

	species := "Brown Pelican"
	filter := models.PelicanFilter{Species: &species}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		repo.Filter(filter)
	}
}

// API Performance Tests
func BenchmarkAPI_PelicanCreate(b *testing.B) {
	router, _, cleanup := setupPerformanceTest(b)
	defer cleanup()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		payload := createPerfTestPelican(i)
		jsonPayload, _ := json.Marshal(payload)

		req, _ := http.NewRequest("POST", "/pelicans", bytes.NewBuffer(jsonPayload))
		req.Header.Set("Content-Type", "application/json")
		
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)
	}
}

func BenchmarkAPI_PelicanRead(b *testing.B) {
	router, _, cleanup := setupPerformanceTest(b)
	defer cleanup()

	// Create test data
	pelicanIDs := make([]int, 100)
	for i := 0; i < 100; i++ {
		payload := createPerfTestPelican(i)
		jsonPayload, _ := json.Marshal(payload)

		req, _ := http.NewRequest("POST", "/pelicans", bytes.NewBuffer(jsonPayload))
		req.Header.Set("Content-Type", "application/json")
		
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		var response map[string]interface{}
		json.Unmarshal(w.Body.Bytes(), &response)
		data := response["data"].(map[string]interface{})
		pelicanIDs[i] = int(data["id"].(float64))
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		pelicanID := pelicanIDs[i%100]
		req, _ := http.NewRequest("GET", "/pelicans/"+strconv.Itoa(pelicanID), nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)
	}
}

func BenchmarkAPI_PelicanList(b *testing.B) {
	router, _, cleanup := setupPerformanceTest(b)
	defer cleanup()

	// Create test data
	for i := 0; i < 100; i++ {
		payload := createPerfTestPelican(i)
		jsonPayload, _ := json.Marshal(payload)

		req, _ := http.NewRequest("POST", "/pelicans", bytes.NewBuffer(jsonPayload))
		req.Header.Set("Content-Type", "application/json")
		
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		req, _ := http.NewRequest("GET", "/pelicans", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)
	}
}

func BenchmarkAPI_PelicanFilter(b *testing.B) {
	router, _, cleanup := setupPerformanceTest(b)
	defer cleanup()

	// Create test data
	for i := 0; i < 100; i++ {
		payload := createPerfTestPelican(i)
		jsonPayload, _ := json.Marshal(payload)

		req, _ := http.NewRequest("POST", "/pelicans", bytes.NewBuffer(jsonPayload))
		req.Header.Set("Content-Type", "application/json")
		
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		req, _ := http.NewRequest("GET", "/pelicans?species=Brown Pelican&health=healthy", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)
	}
}

func BenchmarkAPI_PelicanStats(b *testing.B) {
	router, _, cleanup := setupPerformanceTest(b)
	defer cleanup()

	// Create test data
	for i := 0; i < 100; i++ {
		payload := createPerfTestPelican(i)
		jsonPayload, _ := json.Marshal(payload)

		req, _ := http.NewRequest("POST", "/pelicans", bytes.NewBuffer(jsonPayload))
		req.Header.Set("Content-Type", "application/json")
		
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		req, _ := http.NewRequest("GET", "/pelicans/stats", nil)
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)
	}
}

// Concurrent Performance Tests
func BenchmarkConcurrent_PelicanCreate(b *testing.B) {
	router, _, cleanup := setupPerformanceTest(b)
	defer cleanup()

	b.ResetTimer()
	b.RunParallel(func(pb *testing.PB) {
		i := 0
		for pb.Next() {
			payload := createPerfTestPelican(i)
			jsonPayload, _ := json.Marshal(payload)

			req, _ := http.NewRequest("POST", "/pelicans", bytes.NewBuffer(jsonPayload))
			req.Header.Set("Content-Type", "application/json")
			
			w := httptest.NewRecorder()
			router.ServeHTTP(w, req)
			i++
		}
	})
}

func BenchmarkConcurrent_PelicanRead(b *testing.B) {
	router, _, cleanup := setupPerformanceTest(b)
	defer cleanup()

	// Create test data
	pelicanIDs := make([]int, 100)
	for i := 0; i < 100; i++ {
		payload := createPerfTestPelican(i)
		jsonPayload, _ := json.Marshal(payload)

		req, _ := http.NewRequest("POST", "/pelicans", bytes.NewBuffer(jsonPayload))
		req.Header.Set("Content-Type", "application/json")
		
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		var response map[string]interface{}
		json.Unmarshal(w.Body.Bytes(), &response)
		data := response["data"].(map[string]interface{})
		pelicanIDs[i] = int(data["id"].(float64))
	}

	b.ResetTimer()
	b.RunParallel(func(pb *testing.PB) {
		i := 0
		for pb.Next() {
			pelicanID := pelicanIDs[i%100]
			req, _ := http.NewRequest("GET", "/pelicans/"+strconv.Itoa(pelicanID), nil)
			w := httptest.NewRecorder()
			router.ServeHTTP(w, req)
			i++
		}
	})
}

// Memory Performance Tests
func BenchmarkMemory_PelicanStructCreation(b *testing.B) {
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = &models.Pelican{
			Name:     fmt.Sprintf("Memory Test Pelican %d", i),
			Species:  "Brown Pelican",
			Age:      5,
			Weight:   4.5,
			Health:   models.HealthStatusHealthy,
			Location: "Pond A",
			Gender:   "male",
			Color:    "brown",
		}
	}
}

func BenchmarkMemory_PelicanFilterCreation(b *testing.B) {
	species := "Brown Pelican"
	health := models.HealthStatusHealthy
	minAge := 2
	maxAge := 10

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = &models.PelicanFilter{
			Species: &species,
			Health:  &health,
			MinAge:  &minAge,
			MaxAge:  &maxAge,
		}
	}
}

// Stress Tests
func BenchmarkStress_HighVolumeCreation(b *testing.B) {
	router, _, cleanup := setupPerformanceTest(b)
	defer cleanup()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		// Create 1000 pelicans in each iteration
		for j := 0; j < 1000; j++ {
			payload := createPerfTestPelican(i*1000 + j)
			jsonPayload, _ := json.Marshal(payload)

			req, _ := http.NewRequest("POST", "/pelicans", bytes.NewBuffer(jsonPayload))
			req.Header.Set("Content-Type", "application/json")
			
			w := httptest.NewRecorder()
			router.ServeHTTP(w, req)
		}
	}
}

func BenchmarkStress_ConcurrentMixedOperations(b *testing.B) {
	router, _, cleanup := setupPerformanceTest(b)
	defer cleanup()

	// Pre-populate with some data
	pelicanIDs := make([]int, 50)
	for i := 0; i < 50; i++ {
		payload := createPerfTestPelican(i)
		jsonPayload, _ := json.Marshal(payload)

		req, _ := http.NewRequest("POST", "/pelicans", bytes.NewBuffer(jsonPayload))
		req.Header.Set("Content-Type", "application/json")
		
		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		var response map[string]interface{}
		json.Unmarshal(w.Body.Bytes(), &response)
		data := response["data"].(map[string]interface{})
		pelicanIDs[i] = int(data["id"].(float64))
	}

	b.ResetTimer()
	b.RunParallel(func(pb *testing.PB) {
		i := 0
		for pb.Next() {
			operation := i % 4
			switch operation {
			case 0: // Create
				payload := createPerfTestPelican(i)
				jsonPayload, _ := json.Marshal(payload)
				req, _ := http.NewRequest("POST", "/pelicans", bytes.NewBuffer(jsonPayload))
				req.Header.Set("Content-Type", "application/json")
				w := httptest.NewRecorder()
				router.ServeHTTP(w, req)
			case 1: // Read
				pelicanID := pelicanIDs[i%50]
				req, _ := http.NewRequest("GET", "/pelicans/"+strconv.Itoa(pelicanID), nil)
				w := httptest.NewRecorder()
				router.ServeHTTP(w, req)
			case 2: // List
				req, _ := http.NewRequest("GET", "/pelicans", nil)
				w := httptest.NewRecorder()
				router.ServeHTTP(w, req)
			case 3: // Stats
				req, _ := http.NewRequest("GET", "/pelicans/stats", nil)
				w := httptest.NewRecorder()
				router.ServeHTTP(w, req)
			}
			i++
		}
	})
} 