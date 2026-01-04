package physics

import (
	"math"
	"pelican-farm/internal/models"
)

// Vector3D represents a 3D vector for physics calculations
type Vector3D struct {
	X, Y, Z float64
}

// Add two vectors
func (v Vector3D) Add(other Vector3D) Vector3D {
	return Vector3D{v.X + other.X, v.Y + other.Y, v.Z + other.Z}
}

// Subtract two vectors
func (v Vector3D) Subtract(other Vector3D) Vector3D {
	return Vector3D{v.X - other.X, v.Y - other.Y, v.Z - other.Z}
}

// Multiply vector by scalar
func (v Vector3D) Multiply(scalar float64) Vector3D {
	return Vector3D{v.X * scalar, v.Y * scalar, v.Z * scalar}
}

// Magnitude of vector
func (v Vector3D) Magnitude() float64 {
	return math.Sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z)
}

// Normalize vector
func (v Vector3D) Normalize() Vector3D {
	mag := v.Magnitude()
	if mag == 0 {
		return Vector3D{0, 0, 0}
	}
	return Vector3D{v.X / mag, v.Y / mag, v.Z / mag}
}

// PelicanPhysicsEngine handles advanced pelican flight physics
type PelicanPhysicsEngine struct {
	CollisionDistance    float64 // meters
	SeparationForce      float64 // flocking separation strength
	AlignmentForce       float64 // flocking alignment strength  
	CohesionForce        float64 // flocking cohesion strength
	MaxSpeed             float64 // m/s
	MaxForce             float64 // maximum steering force
	WaterDensity         float64 // kg/m³ for water physics
	AirDensity           float64 // kg/m³ for air physics
}

// NewPelicanPhysicsEngine creates physics engine with realistic values
func NewPelicanPhysicsEngine() *PelicanPhysicsEngine {
	return &PelicanPhysicsEngine{
		CollisionDistance: 2.0,   // 2 meter collision radius
		SeparationForce:   1.5,   // separation strength
		AlignmentForce:    1.0,   // alignment strength
		CohesionForce:     1.0,   // cohesion strength
		MaxSpeed:          15.0,  // 15 m/s max flight speed
		MaxForce:          3.0,   // 3 N max steering force
		WaterDensity:      1000.0, // water density
		AirDensity:        1.225,  // air density at sea level
	}
}

// PelicanState represents current pelican physics state
type PelicanState struct {
	Position Vector3D
	Velocity Vector3D
	Pelican  *models.Pelican
}

// DetectCollisions checks for collisions between pelicans
func (ppe *PelicanPhysicsEngine) DetectCollisions(pelicans []*models.Pelican) []CollisionEvent {
	var collisions []CollisionEvent
	
	for i, p1 := range pelicans {
		for j, p2 := range pelicans {
			if i >= j { // avoid duplicate checks
				continue
			}
			
			// Calculate distance between pelicans
			pos1 := Vector3D{p1.Latitude, p1.Longitude, float64(p1.Age)} // simplified 3D position
			pos2 := Vector3D{p2.Latitude, p2.Longitude, float64(p2.Age)}
			distance := pos1.Subtract(pos2).Magnitude()
			
			if distance < ppe.CollisionDistance {
				collisions = append(collisions, CollisionEvent{
					Pelican1: p1,
					Pelican2: p2,
					Distance: distance,
					Force:    ppe.calculateCollisionForce(distance),
				})
			}
		}
	}
	
	return collisions
}

// CollisionEvent represents a collision between two pelicans
type CollisionEvent struct {
	Pelican1 *models.Pelican
	Pelican2 *models.Pelican
	Distance float64
	Force    float64
}

// calculateCollisionForce computes collision force based on distance
func (ppe *PelicanPhysicsEngine) calculateCollisionForce(distance float64) float64 {
	if distance >= ppe.CollisionDistance {
		return 0
	}
	// Inverse square law for collision force
	return ppe.MaxForce * math.Pow((ppe.CollisionDistance-distance)/ppe.CollisionDistance, 2)
}

// CalculateFlockingBehavior computes flocking forces for a pelican
func (ppe *PelicanPhysicsEngine) CalculateFlockingBehavior(pelican *models.Pelican, flock []*models.Pelican) Vector3D {
	if len(flock) <= 1 {
		return Vector3D{0, 0, 0}
	}
	
	position := Vector3D{pelican.Latitude, pelican.Longitude, float64(pelican.Age)}
	
	// Calculate separation, alignment, and cohesion
	separation := ppe.calculateSeparation(position, flock)
	alignment := ppe.calculateAlignment(pelican, flock)
	cohesion := ppe.calculateCohesion(position, flock)
	
	// Combine forces with weights
	totalForce := separation.Multiply(ppe.SeparationForce).
		Add(alignment.Multiply(ppe.AlignmentForce)).
		Add(cohesion.Multiply(ppe.CohesionForce))
	
	// Limit force magnitude
	if totalForce.Magnitude() > ppe.MaxForce {
		totalForce = totalForce.Normalize().Multiply(ppe.MaxForce)
	}
	
	return totalForce
}

// calculateSeparation computes separation force (avoid crowding neighbors)
func (ppe *PelicanPhysicsEngine) calculateSeparation(position Vector3D, flock []*models.Pelican) Vector3D {
	steer := Vector3D{0, 0, 0}
	count := 0
	
	for _, other := range flock {
		otherPos := Vector3D{other.Latitude, other.Longitude, float64(other.Age)}
		distance := position.Subtract(otherPos).Magnitude()
		
		if distance > 0 && distance < ppe.CollisionDistance*2 {
			// Calculate vector pointing away from neighbor
			diff := position.Subtract(otherPos).Normalize()
			diff = diff.Multiply(1.0 / distance) // weight by distance
			steer = steer.Add(diff)
			count++
		}
	}
	
	if count > 0 {
		steer = steer.Multiply(1.0 / float64(count)) // average
		steer = steer.Normalize().Multiply(ppe.MaxSpeed)
	}
	
	return steer
}

// calculateAlignment computes alignment force (steer towards average heading)
func (ppe *PelicanPhysicsEngine) calculateAlignment(pelican *models.Pelican, flock []*models.Pelican) Vector3D {
	sum := Vector3D{0, 0, 0}
	count := 0
	
	position := Vector3D{pelican.Latitude, pelican.Longitude, float64(pelican.Age)}
	
	for _, other := range flock {
		otherPos := Vector3D{other.Latitude, other.Longitude, float64(other.Age)}
		distance := position.Subtract(otherPos).Magnitude()
		
		if distance > 0 && distance < ppe.CollisionDistance*4 {
			// Use pelican weight as velocity proxy (simplified)
			velocity := Vector3D{other.Weight, 0, 0} // simplified velocity
			sum = sum.Add(velocity)
			count++
		}
	}
	
	if count > 0 {
		sum = sum.Multiply(1.0 / float64(count)) // average velocity
		sum = sum.Normalize().Multiply(ppe.MaxSpeed)
	}
	
	return sum
}

// calculateCohesion computes cohesion force (steer towards center of mass)
func (ppe *PelicanPhysicsEngine) calculateCohesion(position Vector3D, flock []*models.Pelican) Vector3D {
	sum := Vector3D{0, 0, 0}
	count := 0
	
	for _, other := range flock {
		otherPos := Vector3D{other.Latitude, other.Longitude, float64(other.Age)}
		distance := position.Subtract(otherPos).Magnitude()
		
		if distance > 0 && distance < ppe.CollisionDistance*4 {
			sum = sum.Add(otherPos)
			count++
		}
	}
	
	if count > 0 {
		sum = sum.Multiply(1.0 / float64(count)) // center of mass
		target := sum.Subtract(position)        // vector to center
		target = target.Normalize().Multiply(ppe.MaxSpeed)
		return target
	}
	
	return Vector3D{0, 0, 0}
}

// CalculateWaterPhysics computes water interaction forces
func (ppe *PelicanPhysicsEngine) CalculateWaterPhysics(pelican *models.Pelican, waterLevel float64) WaterInteraction {
	pelicanAltitude := float64(pelican.Age) // simplified altitude using age
	
	interaction := WaterInteraction{
		IsInWater:    pelicanAltitude <= waterLevel,
		WaterDepth:   math.Max(0, waterLevel-pelicanAltitude),
		BuoyantForce: 0,
		DragForce:    0,
		WakeEffect:   0,
	}
	
	if interaction.IsInWater {
		// Calculate buoyant force (Archimedes' principle)
		volume := pelican.Weight / 800.0 // simplified pelican density
		interaction.BuoyantForce = ppe.WaterDensity * 9.81 * volume
		
		// Water drag is much higher than air drag
		velocity := pelican.Weight // simplified velocity proxy
		interaction.DragForce = 0.5 * ppe.WaterDensity * velocity * velocity * 0.1 // simplified drag
		
		// Wake effect for nearby pelicans
		interaction.WakeEffect = interaction.DragForce * 0.3
	}
	
	return interaction
}

// WaterInteraction represents pelican-water physics
type WaterInteraction struct {
	IsInWater    bool
	WaterDepth   float64
	BuoyantForce float64
	DragForce    float64
	WakeEffect   float64
}
