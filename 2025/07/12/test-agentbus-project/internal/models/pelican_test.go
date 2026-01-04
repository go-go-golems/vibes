package models

import (
	"testing"
	"time"
)

func TestHealthStatus_Constants(t *testing.T) {
	tests := []struct {
		name     string
		status   HealthStatus
		expected string
	}{
		{"Healthy", HealthStatusHealthy, "healthy"},
		{"Sick", HealthStatusSick, "sick"},
		{"Injured", HealthStatusInjured, "injured"},
		{"Recovering", HealthStatusRecovering, "recovering"},
		{"Critical", HealthStatusCritical, "critical"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if string(tt.status) != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, string(tt.status))
			}
		})
	}
}

func TestPelican_Creation(t *testing.T) {
	now := time.Now()
	pelican := Pelican{
		ID:        1,
		Name:      "Bob",
		Species:   "Brown Pelican",
		Age:       5,
		Weight:    4.5,
		Health:    HealthStatusHealthy,
		Location:  "Pond A",
		Gender:    "male",
		Color:     "brown",
		CreatedAt: now,
		UpdatedAt: now,
	}

	if pelican.ID != 1 {
		t.Errorf("Expected ID 1, got %d", pelican.ID)
	}
	if pelican.Name != "Bob" {
		t.Errorf("Expected name Bob, got %s", pelican.Name)
	}
	if pelican.Health != HealthStatusHealthy {
		t.Errorf("Expected health healthy, got %s", pelican.Health)
	}
}

func TestPelicanFilter_EmptyFilter(t *testing.T) {
	filter := PelicanFilter{}
	
	if filter.Species != nil {
		t.Error("Expected Species to be nil")
	}
	if filter.Health != nil {
		t.Error("Expected Health to be nil")
	}
	if filter.Location != nil {
		t.Error("Expected Location to be nil")
	}
}

func TestPelicanFilter_WithValues(t *testing.T) {
	species := "Brown Pelican"
	health := HealthStatusHealthy
	location := "Pond A"
	minAge := 2
	maxAge := 10
	minWeight := 3.0
	maxWeight := 6.0

	filter := PelicanFilter{
		Species:   &species,
		Health:    &health,
		Location:  &location,
		MinAge:    &minAge,
		MaxAge:    &maxAge,
		MinWeight: &minWeight,
		MaxWeight: &maxWeight,
	}

	if *filter.Species != species {
		t.Errorf("Expected species %s, got %s", species, *filter.Species)
	}
	if *filter.Health != health {
		t.Errorf("Expected health %s, got %s", health, *filter.Health)
	}
	if *filter.Location != location {
		t.Errorf("Expected location %s, got %s", location, *filter.Location)
	}
	if *filter.MinAge != minAge {
		t.Errorf("Expected min age %d, got %d", minAge, *filter.MinAge)
	}
	if *filter.MaxAge != maxAge {
		t.Errorf("Expected max age %d, got %d", maxAge, *filter.MaxAge)
	}
	if *filter.MinWeight != minWeight {
		t.Errorf("Expected min weight %f, got %f", minWeight, *filter.MinWeight)
	}
	if *filter.MaxWeight != maxWeight {
		t.Errorf("Expected max weight %f, got %f", maxWeight, *filter.MaxWeight)
	}
}

func TestPelicanUpdate_PartialUpdate(t *testing.T) {
	name := "Updated Bob"
	age := 6
	newHealth := HealthStatusRecovering

	update := PelicanUpdate{
		Name:   &name,
		Age:    &age,
		Health: &newHealth,
		// Weight and Location intentionally nil
	}

	if *update.Name != name {
		t.Errorf("Expected name %s, got %s", name, *update.Name)
	}
	if *update.Age != age {
		t.Errorf("Expected age %d, got %d", age, *update.Age)
	}
	if *update.Health != newHealth {
		t.Errorf("Expected health %s, got %s", newHealth, *update.Health)
	}
	if update.Weight != nil {
		t.Error("Expected Weight to be nil")
	}
	if update.Location != nil {
		t.Error("Expected Location to be nil")
	}
}

func TestPelicanStats_Structure(t *testing.T) {
	stats := PelicanStats{
		Total:   100,
		Healthy: 80,
		Sick:    15,
		Injured: 5,
	}

	if stats.Total != 100 {
		t.Errorf("Expected total 100, got %d", stats.Total)
	}
	if stats.Healthy != 80 {
		t.Errorf("Expected healthy 80, got %d", stats.Healthy)
	}
	if stats.Sick != 15 {
		t.Errorf("Expected sick 15, got %d", stats.Sick)
	}
	if stats.Injured != 5 {
		t.Errorf("Expected injured 5, got %d", stats.Injured)
	}
}

func BenchmarkPelican_Creation(b *testing.B) {
	now := time.Now()
	
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = Pelican{
			ID:        i,
			Name:      "Test Pelican",
			Species:   "Brown Pelican",
			Age:       5,
			Weight:    4.5,
			Health:    HealthStatusHealthy,
			Location:  "Pond A",
			Gender:    "male",
			Color:     "brown",
			CreatedAt: now,
			UpdatedAt: now,
		}
	}
}

func BenchmarkPelicanFilter_Creation(b *testing.B) {
	species := "Brown Pelican"
	health := HealthStatusHealthy
	minAge := 2
	maxAge := 10
	
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = PelicanFilter{
			Species: &species,
			Health:  &health,
			MinAge:  &minAge,
			MaxAge:  &maxAge,
		}
	}
} 