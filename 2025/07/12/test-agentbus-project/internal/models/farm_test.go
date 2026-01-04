package models

import (
	"testing"
	"time"
)

func TestFarmType_Constants(t *testing.T) {
	tests := []struct {
		name     string
		farmType FarmType
		expected string
	}{
		{"Breeding", FarmTypeBreeding, "breeding"},
		{"Rehab", FarmTypeRehab, "rehabilitation"},
		{"Conservation", FarmTypeConservation, "conservation"},
		{"Research", FarmTypeResearch, "research"},
		{"Sanctuary", FarmTypeSanctuary, "sanctuary"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if string(tt.farmType) != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, string(tt.farmType))
			}
		})
	}
}

func TestFarm_Creation(t *testing.T) {
	now := time.Now()
	farm := Farm{
		ID:           1,
		Name:         "Coastal Sanctuary",
		Type:         FarmTypeSanctuary,
		Location:     "California Coast",
		Capacity:     100,
		CurrentCount: 75,
		ManagerName:  "John Doe",
		ManagerEmail: "john@example.com",
		Established:  now.AddDate(-5, 0, 0),
		CreatedAt:    now,
		UpdatedAt:    now,
	}

	if farm.ID != 1 {
		t.Errorf("Expected ID 1, got %d", farm.ID)
	}
	if farm.Name != "Coastal Sanctuary" {
		t.Errorf("Expected name Coastal Sanctuary, got %s", farm.Name)
	}
	if farm.Type != FarmTypeSanctuary {
		t.Errorf("Expected type sanctuary, got %s", farm.Type)
	}
	if farm.Capacity != 100 {
		t.Errorf("Expected capacity 100, got %d", farm.Capacity)
	}
	if farm.CurrentCount != 75 {
		t.Errorf("Expected current count 75, got %d", farm.CurrentCount)
	}
}

func TestFarmStats_Structure(t *testing.T) {
	healthyCounts := map[HealthStatus]int{
		HealthStatusHealthy: 50,
		HealthStatusSick:    10,
		HealthStatusInjured: 5,
	}
	
	speciesCounts := map[string]int{
		"Brown Pelican": 40,
		"White Pelican": 25,
	}
	
	locationCounts := map[string]int{
		"Pond A": 30,
		"Pond B": 35,
	}

	stats := FarmStats{
		TotalPelicans:  65,
		HealthyCounts:  healthyCounts,
		SpeciesCounts:  speciesCounts,
		LocationCounts: locationCounts,
		AverageAge:     4.5,
		AverageWeight:  4.2,
		CapacityUsage:  0.65,
	}

	if stats.TotalPelicans != 65 {
		t.Errorf("Expected total pelicans 65, got %d", stats.TotalPelicans)
	}
	if stats.HealthyCounts[HealthStatusHealthy] != 50 {
		t.Errorf("Expected healthy count 50, got %d", stats.HealthyCounts[HealthStatusHealthy])
	}
	if stats.SpeciesCounts["Brown Pelican"] != 40 {
		t.Errorf("Expected Brown Pelican count 40, got %d", stats.SpeciesCounts["Brown Pelican"])
	}
	if stats.LocationCounts["Pond A"] != 30 {
		t.Errorf("Expected Pond A count 30, got %d", stats.LocationCounts["Pond A"])
	}
	if stats.AverageAge != 4.5 {
		t.Errorf("Expected average age 4.5, got %f", stats.AverageAge)
	}
	if stats.CapacityUsage != 0.65 {
		t.Errorf("Expected capacity usage 0.65, got %f", stats.CapacityUsage)
	}
}

func TestFarmFilter_EmptyFilter(t *testing.T) {
	filter := FarmFilter{}
	
	if filter.Type != nil {
		t.Error("Expected Type to be nil")
	}
	if filter.Location != nil {
		t.Error("Expected Location to be nil")
	}
	if filter.MinCapacity != nil {
		t.Error("Expected MinCapacity to be nil")
	}
	if filter.MaxCapacity != nil {
		t.Error("Expected MaxCapacity to be nil")
	}
	if filter.HasVacancy != nil {
		t.Error("Expected HasVacancy to be nil")
	}
}

func TestFarmFilter_WithValues(t *testing.T) {
	farmType := FarmTypeBreeding
	location := "California"
	minCapacity := 50
	maxCapacity := 200
	hasVacancy := true

	filter := FarmFilter{
		Type:        &farmType,
		Location:    &location,
		MinCapacity: &minCapacity,
		MaxCapacity: &maxCapacity,
		HasVacancy:  &hasVacancy,
	}

	if *filter.Type != farmType {
		t.Errorf("Expected type %s, got %s", farmType, *filter.Type)
	}
	if *filter.Location != location {
		t.Errorf("Expected location %s, got %s", location, *filter.Location)
	}
	if *filter.MinCapacity != minCapacity {
		t.Errorf("Expected min capacity %d, got %d", minCapacity, *filter.MinCapacity)
	}
	if *filter.MaxCapacity != maxCapacity {
		t.Errorf("Expected max capacity %d, got %d", maxCapacity, *filter.MaxCapacity)
	}
	if *filter.HasVacancy != hasVacancy {
		t.Errorf("Expected has vacancy %t, got %t", hasVacancy, *filter.HasVacancy)
	}
}

func TestFarmUpdate_PartialUpdate(t *testing.T) {
	name := "Updated Farm"
	farmType := FarmTypeRehab
	capacity := 150

	update := FarmUpdate{
		Name:     &name,
		Type:     &farmType,
		Capacity: &capacity,
		// Location, ManagerName, ManagerEmail intentionally nil
	}

	if *update.Name != name {
		t.Errorf("Expected name %s, got %s", name, *update.Name)
	}
	if *update.Type != farmType {
		t.Errorf("Expected type %s, got %s", farmType, *update.Type)
	}
	if *update.Capacity != capacity {
		t.Errorf("Expected capacity %d, got %d", capacity, *update.Capacity)
	}
	if update.Location != nil {
		t.Error("Expected Location to be nil")
	}
	if update.ManagerName != nil {
		t.Error("Expected ManagerName to be nil")
	}
	if update.ManagerEmail != nil {
		t.Error("Expected ManagerEmail to be nil")
	}
}

func TestAssignment_Structure(t *testing.T) {
	now := time.Now()
	assignment := Assignment{
		ID:         1,
		PelicanID:  5,
		FarmID:     2,
		AssignedAt: now,
		Notes:      "Temporary assignment for rehabilitation",
	}

	if assignment.ID != 1 {
		t.Errorf("Expected ID 1, got %d", assignment.ID)
	}
	if assignment.PelicanID != 5 {
		t.Errorf("Expected PelicanID 5, got %d", assignment.PelicanID)
	}
	if assignment.FarmID != 2 {
		t.Errorf("Expected FarmID 2, got %d", assignment.FarmID)
	}
	if assignment.Notes != "Temporary assignment for rehabilitation" {
		t.Errorf("Expected specific notes, got %s", assignment.Notes)
	}
}

func BenchmarkFarm_Creation(b *testing.B) {
	now := time.Now()
	
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = Farm{
			ID:           i,
			Name:         "Test Farm",
			Type:         FarmTypeBreeding,
			Location:     "Test Location",
			Capacity:     100,
			CurrentCount: 50,
			ManagerName:  "Test Manager",
			ManagerEmail: "test@example.com",
			Established:  now,
			CreatedAt:    now,
			UpdatedAt:    now,
		}
	}
}

func BenchmarkFarmFilter_Creation(b *testing.B) {
	farmType := FarmTypeBreeding
	location := "California"
	minCapacity := 50
	hasVacancy := true
	
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = FarmFilter{
			Type:        &farmType,
			Location:    &location,
			MinCapacity: &minCapacity,
			HasVacancy:  &hasVacancy,
		}
	}
}

func BenchmarkFarmStats_Creation(b *testing.B) {
	healthyCounts := map[HealthStatus]int{
		HealthStatusHealthy: 50,
		HealthStatusSick:    10,
	}
	speciesCounts := map[string]int{
		"Brown Pelican": 40,
		"White Pelican": 20,
	}
	locationCounts := map[string]int{
		"Pond A": 30,
		"Pond B": 30,
	}
	
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = FarmStats{
			TotalPelicans:  60,
			HealthyCounts:  healthyCounts,
			SpeciesCounts:  speciesCounts,
			LocationCounts: locationCounts,
			AverageAge:     4.0,
			AverageWeight:  4.5,
			CapacityUsage:  0.6,
		}
	}
} 