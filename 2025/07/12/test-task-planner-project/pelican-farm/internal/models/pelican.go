package models

import "time"

type Pelican struct {
	ID           int       `json:"id"`
	Name         string    `json:"name"`
	Species      string    `json:"species"`
	Age          int       `json:"age"`
	Weight       *float64  `json:"weight,omitempty"`
	HealthStatus string    `json:"health_status"`
	ArrivalDate  string    `json:"arrival_date"`
	Notes        *string   `json:"notes,omitempty"`
	Created      time.Time `json:"created"`
	Modified     time.Time `json:"modified"`
}

type FeedingRecord struct {
	ID          int       `json:"id"`
	PelicanID   int       `json:"pelican_id"`
	FoodType    string    `json:"food_type"`
	AmountKg    float64   `json:"amount_kg"`
	FeedingTime time.Time `json:"feeding_time"`
	Notes       *string   `json:"notes,omitempty"`
	Created     time.Time `json:"created"`
}

type HealthCheck struct {
	ID           int       `json:"id"`
	PelicanID    int       `json:"pelican_id"`
	CheckDate    string    `json:"check_date"`
	Weight       *float64  `json:"weight,omitempty"`
	Temperature  *float64  `json:"temperature,omitempty"`
	Notes        *string   `json:"notes,omitempty"`
	Veterinarian *string   `json:"veterinarian,omitempty"`
	Created      time.Time `json:"created"`
}

type FeedingSchedule struct {
	ID            int       `json:"id"`
	PelicanID     int       `json:"pelican_id"`
	ScheduledTime time.Time `json:"scheduled_time"`
	FoodType      string    `json:"food_type"`
	AmountKg      float64   `json:"amount_kg"`
	Completed     bool      `json:"completed"`
	Created       time.Time `json:"created"`
}
