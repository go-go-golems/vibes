package models

import (
	"time"
)

// FarmType represents the type of farm facility
type FarmType string

const (
	FarmTypeBreeding    FarmType = "breeding"
	FarmTypeRehab       FarmType = "rehabilitation"
	FarmTypeConservation FarmType = "conservation"
	FarmTypeResearch    FarmType = "research"
	FarmTypeSanctuary   FarmType = "sanctuary"
)

// Farm represents a pelican farm facility
type Farm struct {
	ID          int       `json:"id" db:"id"`
	Name        string    `json:"name" db:"name"`
	Type        FarmType  `json:"type" db:"type"`
	Location    string    `json:"location" db:"location"`
	Capacity    int       `json:"capacity" db:"capacity"`
	CurrentCount int      `json:"current_count" db:"current_count"`
	ManagerName string    `json:"manager_name" db:"manager_name"`
	ManagerEmail string   `json:"manager_email" db:"manager_email"`
	Established time.Time `json:"established" db:"established"`
	CreatedAt   time.Time `json:"created_at" db:"created_at"`
	UpdatedAt   time.Time `json:"updated_at" db:"updated_at"`
}

// FarmStats represents statistics for a farm
type FarmStats struct {
	TotalPelicans     int                        `json:"total_pelicans"`
	HealthyCounts     map[HealthStatus]int       `json:"healthy_counts"`
	SpeciesCounts     map[string]int             `json:"species_counts"`
	LocationCounts    map[string]int             `json:"location_counts"`
	AverageAge        float64                    `json:"average_age"`
	AverageWeight     float64                    `json:"average_weight"`
	CapacityUsage     float64                    `json:"capacity_usage"`
}

// FarmFilter represents filtering options for farm queries
type FarmFilter struct {
	Type         *FarmType `json:"type,omitempty"`
	Location     *string   `json:"location,omitempty"`
	MinCapacity  *int      `json:"min_capacity,omitempty"`
	MaxCapacity  *int      `json:"max_capacity,omitempty"`
	HasVacancy   *bool     `json:"has_vacancy,omitempty"`
}

// FarmUpdate represents fields that can be updated for a farm
type FarmUpdate struct {
	Name         *string   `json:"name,omitempty"`
	Type         *FarmType `json:"type,omitempty"`
	Location     *string   `json:"location,omitempty"`
	Capacity     *int      `json:"capacity,omitempty"`
	ManagerName  *string   `json:"manager_name,omitempty"`
	ManagerEmail *string   `json:"manager_email,omitempty"`
}

// Assignment represents the relationship between a pelican and a farm
type Assignment struct {
	ID        int       `json:"id" db:"id"`
	PelicanID int       `json:"pelican_id" db:"pelican_id"`
	FarmID    int       `json:"farm_id" db:"farm_id"`
	AssignedAt time.Time `json:"assigned_at" db:"assigned_at"`
	Notes     string    `json:"notes" db:"notes"`
}
