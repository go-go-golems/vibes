package models

import (
	"time"
)

// HealthStatus represents the health condition of a pelican
type HealthStatus string

const (
	HealthStatusHealthy   HealthStatus = "healthy"
	HealthStatusSick      HealthStatus = "sick"
	HealthStatusInjured   HealthStatus = "injured"
	HealthStatusRecovering HealthStatus = "recovering"
	HealthStatusCritical  HealthStatus = "critical"
)

// Pelican represents a pelican in the farm
type Pelican struct {
	ID        int          `json:"id" db:"id"`
	Name      string       `json:"name" db:"name"`
	Species   string       `json:"species" db:"species"`
	Age       int          `json:"age" db:"age"`
	Weight    float64      `json:"weight" db:"weight"`
	Health    HealthStatus `json:"health" db:"health"`
	Location  string       `json:"location" db:"location"`
	Gender    string       `json:"gender" db:"gender"`
	Color     string       `json:"color" db:"color"`
	CreatedAt time.Time    `json:"created_at" db:"created_at"`
	UpdatedAt time.Time    `json:"updated_at" db:"updated_at"`
}

// PelicanFilter represents filtering options for pelican queries
type PelicanFilter struct {
	Species  *string       `json:"species,omitempty"`
	Health   *HealthStatus `json:"health,omitempty"`
	Location *string       `json:"location,omitempty"`
	MinAge   *int          `json:"min_age,omitempty"`
	MaxAge   *int          `json:"max_age,omitempty"`
	MinWeight *float64     `json:"min_weight,omitempty"`
	MaxWeight *float64     `json:"max_weight,omitempty"`
}

// PelicanUpdate represents fields that can be updated for a pelican
type PelicanUpdate struct {
	Name     *string       `json:"name,omitempty"`
	Age      *int          `json:"age,omitempty"`
	Weight   *float64      `json:"weight,omitempty"`
	Health   *HealthStatus `json:"health,omitempty"`
	Location *string       `json:"location,omitempty"`
	Color    *string       `json:"color,omitempty"`
}

// PelicanStats represents statistics about pelicans in the farm
type PelicanStats struct {
	Total    int64 `json:"total"`
	Healthy  int64 `json:"healthy"`
	Sick     int64 `json:"sick"`
	Injured  int64 `json:"injured"`
}
