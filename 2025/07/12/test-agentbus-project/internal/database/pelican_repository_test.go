package database

import (
	"testing"
	"time"

	"gorm.io/driver/sqlite"
	"gorm.io/gorm"
	"pelican-farm/internal/models"
)

func setupTestDB(t *testing.T) *gorm.DB {
	db, err := gorm.Open(sqlite.Open(":memory:"), &gorm.Config{})
	if err != nil {
		t.Fatalf("Failed to connect to test database: %v", err)
	}

	// Auto-migrate the schema
	err = db.AutoMigrate(&models.Pelican{})
	if err != nil {
		t.Fatalf("Failed to migrate database: %v", err)
	}

	return db
}

func createTestPelican() *models.Pelican {
	return &models.Pelican{
		Name:      "Test Pelican",
		Species:   "Brown Pelican",
		Age:       5,
		Weight:    4.5,
		Health:    models.HealthStatusHealthy,
		Location:  "Pond A",
		Gender:    "male",
		Color:     "brown",
		CreatedAt: time.Now(),
		UpdatedAt: time.Now(),
	}
}

func TestPelicanRepository_Create(t *testing.T) {
	db := setupTestDB(t)
	repo := NewPelicanRepository(db)

	pelican := createTestPelican()
	err := repo.Create(pelican)

	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}
	if pelican.ID == 0 {
		t.Error("Expected ID to be set after creation")
	}
}

func TestPelicanRepository_GetByID(t *testing.T) {
	db := setupTestDB(t)
	repo := NewPelicanRepository(db)

	// Create a pelican first
	pelican := createTestPelican()
	err := repo.Create(pelican)
	if err != nil {
		t.Fatalf("Failed to create pelican: %v", err)
	}

	// Get by ID
	retrieved, err := repo.GetByID(uint(pelican.ID))
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}
	if retrieved.Name != pelican.Name {
		t.Errorf("Expected name %s, got %s", pelican.Name, retrieved.Name)
	}
	if retrieved.Species != pelican.Species {
		t.Errorf("Expected species %s, got %s", pelican.Species, retrieved.Species)
	}
}

func TestPelicanRepository_GetByID_NotFound(t *testing.T) {
	db := setupTestDB(t)
	repo := NewPelicanRepository(db)

	_, err := repo.GetByID(999)
	if err == nil {
		t.Error("Expected error for non-existent ID")
	}
}

func TestPelicanRepository_GetAll(t *testing.T) {
	db := setupTestDB(t)
	repo := NewPelicanRepository(db)

	// Create multiple pelicans
	pelican1 := createTestPelican()
	pelican1.Name = "Pelican 1"
	pelican2 := createTestPelican()
	pelican2.Name = "Pelican 2"

	repo.Create(pelican1)
	repo.Create(pelican2)

	pelicans, err := repo.GetAll()
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}
	if len(pelicans) != 2 {
		t.Errorf("Expected 2 pelicans, got %d", len(pelicans))
	}
}

func TestPelicanRepository_Filter_BySpecies(t *testing.T) {
	db := setupTestDB(t)
	repo := NewPelicanRepository(db)

	// Create pelicans with different species
	pelican1 := createTestPelican()
	pelican1.Species = "Brown Pelican"
	pelican2 := createTestPelican()
	pelican2.Species = "White Pelican"

	repo.Create(pelican1)
	repo.Create(pelican2)

	// Filter by species
	species := "Brown Pelican"
	filter := models.PelicanFilter{Species: &species}
	pelicans, err := repo.Filter(filter)

	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}
	if len(pelicans) != 1 {
		t.Errorf("Expected 1 pelican, got %d", len(pelicans))
	}
	if pelicans[0].Species != "Brown Pelican" {
		t.Errorf("Expected Brown Pelican, got %s", pelicans[0].Species)
	}
}

func TestPelicanRepository_Filter_ByHealth(t *testing.T) {
	db := setupTestDB(t)
	repo := NewPelicanRepository(db)

	// Create pelicans with different health statuses
	pelican1 := createTestPelican()
	pelican1.Health = models.HealthStatusHealthy
	pelican2 := createTestPelican()
	pelican2.Health = models.HealthStatusSick

	repo.Create(pelican1)
	repo.Create(pelican2)

	// Filter by health
	health := models.HealthStatusHealthy
	filter := models.PelicanFilter{Health: &health}
	pelicans, err := repo.Filter(filter)

	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}
	if len(pelicans) != 1 {
		t.Errorf("Expected 1 pelican, got %d", len(pelicans))
	}
	if pelicans[0].Health != models.HealthStatusHealthy {
		t.Errorf("Expected healthy, got %s", pelicans[0].Health)
	}
}

func TestPelicanRepository_Filter_ByAgeRange(t *testing.T) {
	db := setupTestDB(t)
	repo := NewPelicanRepository(db)

	// Create pelicans with different ages
	pelican1 := createTestPelican()
	pelican1.Age = 3
	pelican2 := createTestPelican()
	pelican2.Age = 7
	pelican3 := createTestPelican()
	pelican3.Age = 10

	repo.Create(pelican1)
	repo.Create(pelican2)
	repo.Create(pelican3)

	// Filter by age range
	minAge := 5
	maxAge := 9
	filter := models.PelicanFilter{MinAge: &minAge, MaxAge: &maxAge}
	pelicans, err := repo.Filter(filter)

	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}
	if len(pelicans) != 1 {
		t.Errorf("Expected 1 pelican, got %d", len(pelicans))
	}
	if pelicans[0].Age != 7 {
		t.Errorf("Expected age 7, got %d", pelicans[0].Age)
	}
}

func TestPelicanRepository_Filter_ByLocation(t *testing.T) {
	db := setupTestDB(t)
	repo := NewPelicanRepository(db)

	// Create pelicans in different locations
	pelican1 := createTestPelican()
	pelican1.Location = "Pond A"
	pelican2 := createTestPelican()
	pelican2.Location = "Pond B"

	repo.Create(pelican1)
	repo.Create(pelican2)

	// Filter by location
	location := "Pond A"
	filter := models.PelicanFilter{Location: &location}
	pelicans, err := repo.Filter(filter)

	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}
	if len(pelicans) != 1 {
		t.Errorf("Expected 1 pelican, got %d", len(pelicans))
	}
	if pelicans[0].Location != "Pond A" {
		t.Errorf("Expected Pond A, got %s", pelicans[0].Location)
	}
}

func TestPelicanRepository_Update(t *testing.T) {
	db := setupTestDB(t)
	repo := NewPelicanRepository(db)

	// Create a pelican
	pelican := createTestPelican()
	repo.Create(pelican)

	// Update the pelican
	newName := "Updated Pelican"
	newAge := 6
	newHealth := models.HealthStatusRecovering
	
	update := models.PelicanUpdate{
		Name:   &newName,
		Age:    &newAge,
		Health: &newHealth,
	}

	err := repo.Update(uint(pelican.ID), update)
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}

	// Verify the update
	updated, err := repo.GetByID(uint(pelican.ID))
	if err != nil {
		t.Errorf("Failed to get updated pelican: %v", err)
	}
	if updated.Name != newName {
		t.Errorf("Expected name %s, got %s", newName, updated.Name)
	}
	if updated.Age != newAge {
		t.Errorf("Expected age %d, got %d", newAge, updated.Age)
	}
	if updated.Health != newHealth {
		t.Errorf("Expected health %s, got %s", newHealth, updated.Health)
	}
}

func TestPelicanRepository_Delete(t *testing.T) {
	db := setupTestDB(t)
	repo := NewPelicanRepository(db)

	// Create a pelican
	pelican := createTestPelican()
	repo.Create(pelican)

	// Delete the pelican
	err := repo.Delete(uint(pelican.ID))
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}

	// Verify deletion
	_, err = repo.GetByID(uint(pelican.ID))
	if err == nil {
		t.Error("Expected error when getting deleted pelican")
	}
}

func TestPelicanRepository_GetStats(t *testing.T) {
	db := setupTestDB(t)
	repo := NewPelicanRepository(db)

	// Create pelicans with different health statuses
	pelican1 := createTestPelican()
	pelican1.Health = models.HealthStatusHealthy
	pelican2 := createTestPelican()
	pelican2.Health = models.HealthStatusHealthy
	pelican3 := createTestPelican()
	pelican3.Health = models.HealthStatusSick

	repo.Create(pelican1)
	repo.Create(pelican2)
	repo.Create(pelican3)

	stats, err := repo.GetStats()
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}
	if stats.Total != 3 {
		t.Errorf("Expected total 3, got %d", stats.Total)
	}
	if stats.Healthy != 2 {
		t.Errorf("Expected healthy 2, got %d", stats.Healthy)
	}
	if stats.Sick != 1 {
		t.Errorf("Expected sick 1, got %d", stats.Sick)
	}
}

func BenchmarkPelicanRepository_Create(b *testing.B) {
	db := setupTestDB(&testing.T{})
	repo := NewPelicanRepository(db)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		pelican := createTestPelican()
		pelican.Name = "Benchmark Pelican"
		repo.Create(pelican)
	}
}

func BenchmarkPelicanRepository_GetAll(b *testing.B) {
	db := setupTestDB(&testing.T{})
	repo := NewPelicanRepository(db)

	// Create some test data
	for i := 0; i < 100; i++ {
		pelican := createTestPelican()
		repo.Create(pelican)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		repo.GetAll()
	}
}

func BenchmarkPelicanRepository_Filter(b *testing.B) {
	db := setupTestDB(&testing.T{})
	repo := NewPelicanRepository(db)

	// Create some test data
	for i := 0; i < 100; i++ {
		pelican := createTestPelican()
		repo.Create(pelican)
	}

	species := "Brown Pelican"
	filter := models.PelicanFilter{Species: &species}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		repo.Filter(filter)
	}
} 