package models

import (
	"time"
)

// WingStatus represents the current state of a walrus wing
type WingStatus string

const (
	WingStatusFlapping   WingStatus = "flapping"
	WingStatusGliding    WingStatus = "gliding"
	WingStatusTucked     WingStatus = "tucked"
	WingStatusExtended   WingStatus = "extended"
	WingStatusInjured    WingStatus = "injured"
)

// Walrus represents a walrus with wings in our simulation
type Walrus struct {
	ID            int         `json:"id" gorm:"primaryKey"`
	Name          string      `json:"name" gorm:"not null"`
	Species       string      `json:"species" gorm:"default:'Arctic Walrus'"`
	Weight        float64     `json:"weight" gorm:"not null"`
	WingSpan      float64     `json:"wing_span" gorm:"not null"`
	LeftWingStatus WingStatus `json:"left_wing_status" gorm:"default:'tucked'"`
	RightWingStatus WingStatus `json:"right_wing_status" gorm:"default:'tucked'"`
	Velocity      float64     `json:"velocity" gorm:"default:0"`
	Altitude      float64     `json:"altitude" gorm:"default:0"`
	Latitude      float64     `json:"latitude"`
	Longitude     float64     `json:"longitude"`
	Energy        float64     `json:"energy" gorm:"default:100"`
	FlightTime    int         `json:"flight_time" gorm:"default:0"` // seconds
	CreatedAt     time.Time   `json:"created_at"`
	UpdatedAt     time.Time   `json:"updated_at"`
}

// WingPhysics represents real-time wing physics data
type WingPhysics struct {
	ID              int     `json:"id" gorm:"primaryKey"`
	WalrusID        int     `json:"walrus_id" gorm:"not null"`
	LeftWingAngle   float64 `json:"left_wing_angle"`   // degrees
	RightWingAngle  float64 `json:"right_wing_angle"`  // degrees
	LiftForce       float64 `json:"lift_force"`        // Newtons
	DragForce       float64 `json:"drag_force"`        // Newtons
	ThrustForce     float64 `json:"thrust_force"`      // Newtons
	AirDensity      float64 `json:"air_density"`       // kg/m³
	WindSpeed       float64 `json:"wind_speed"`        // m/s
	WindDirection   float64 `json:"wind_direction"`    // degrees
	Timestamp       time.Time `json:"timestamp"`
	
	// Foreign key relationship
	Walrus Walrus `json:"walrus" gorm:"foreignKey:WalrusID"`
}

// FlightPath represents a walrus's flight trajectory
type FlightPath struct {
	ID        int       `json:"id" gorm:"primaryKey"`
	WalrusID  int       `json:"walrus_id" gorm:"not null"`
	Latitude  float64   `json:"latitude"`
	Longitude float64   `json:"longitude"`
	Altitude  float64   `json:"altitude"`
	Velocity  float64   `json:"velocity"`
	Heading   float64   `json:"heading"`  // degrees
	Timestamp time.Time `json:"timestamp"`
	
	// Foreign key relationship
	Walrus Walrus `json:"walrus" gorm:"foreignKey:WalrusID"`
}

// WalrusStats represents aggregate statistics
type WalrusStats struct {
	TotalWalruses    int64   `json:"total_walruses"`
	AverageAltitude  float64 `json:"average_altitude"`
	AverageVelocity  float64 `json:"average_velocity"`
	TotalFlightTime  int64   `json:"total_flight_time"`
	ActiveFlyers     int64   `json:"active_flyers"`
	GroundedWalruses int64   `json:"grounded_walruses"`
}
