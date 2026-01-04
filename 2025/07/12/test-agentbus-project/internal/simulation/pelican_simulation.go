package simulation

import (
	"sync"
	"time"
	"gorm.io/gorm"
	"pelican-farm/internal/models"
	"pelican-farm/internal/physics"
)

// PelicanSimulation manages real-time pelican physics simulation
type PelicanSimulation struct {
	db           *gorm.DB
	physicsEngine *physics.PelicanPhysicsEngine
	pelicans     map[int]*models.Pelican
	isRunning    bool
	mutex        sync.RWMutex
	tickRate     time.Duration
	waterLevel   float64
}

// NewPelicanSimulation creates a new simulation manager
func NewPelicanSimulation(db *gorm.DB) *PelicanSimulation {
	return &PelicanSimulation{
		db:            db,
		physicsEngine: physics.NewPelicanPhysicsEngine(),
		pelicans:      make(map[int]*models.Pelican),
		tickRate:      50 * time.Millisecond, // 20 FPS
		waterLevel:    0.0, // sea level
	}
}

// StartSimulation begins the physics simulation loop
func (ps *PelicanSimulation) StartSimulation() {
	ps.mutex.Lock()
	ps.isRunning = true
	ps.mutex.Unlock()
	
	// Load initial pelicans from database
	ps.loadPelicans()
	
	ticker := time.NewTicker(ps.tickRate)
	defer ticker.Stop()
	
	for {
		select {
		case <-ticker.C:
			if !ps.isRunning {
				return
			}
			ps.simulationTick()
		}
	}
}

// StopSimulation halts the physics simulation
func (ps *PelicanSimulation) StopSimulation() {
	ps.mutex.Lock()
	ps.isRunning = false
	ps.mutex.Unlock()
}

// IsRunning returns simulation status
func (ps *PelicanSimulation) IsRunning() bool {
	ps.mutex.RLock()
	defer ps.mutex.RUnlock()
	return ps.isRunning
}

// loadPelicans loads all pelicans from database into simulation
func (ps *PelicanSimulation) loadPelicans() {
	var pelicans []models.Pelican
	ps.db.Find(&pelicans)
	
	ps.mutex.Lock()
	for _, pelican := range pelicans {
		p := pelican // create copy
		ps.pelicans[p.ID] = &p
	}
	ps.mutex.Unlock()
}

// simulationTick performs one physics simulation step
func (ps *PelicanSimulation) simulationTick() {
	ps.mutex.Lock()
	defer ps.mutex.Unlock()
	
	if len(ps.pelicans) == 0 {
		return
	}
	
	// Convert to slice for physics calculations
	pelicanSlice := make([]*models.Pelican, 0, len(ps.pelicans))
	for _, p := range ps.pelicans {
		pelicanSlice = append(pelicanSlice, p)
	}
	
	// Detect collisions
	collisions := ps.physicsEngine.DetectCollisions(pelicanSlice)
	ps.handleCollisions(collisions)
	
	// Calculate flocking behavior for each pelican
	for _, pelican := range ps.pelicans {
		if pelican.Health == models.HealthStatusHealthy {
			flockingForce := ps.physicsEngine.CalculateFlockingBehavior(pelican, pelicanSlice)
			ps.applyFlockingForce(pelican, flockingForce)
		}
		
		// Calculate water physics
		waterInteraction := ps.physicsEngine.CalculateWaterPhysics(pelican, ps.waterLevel)
		ps.applyWaterPhysics(pelican, waterInteraction)
		
		// Update pelican position based on forces
		ps.updatePelicanPosition(pelican)
	}
	
	// Periodically save to database (every 100 ticks = 5 seconds)
	if time.Now().UnixMilli()%5000 < 50 {
		ps.savePelicansToDatabase()
	}
}

// handleCollisions processes collision events
func (ps *PelicanSimulation) handleCollisions(collisions []physics.CollisionEvent) {
	for _, collision := range collisions {
		// Apply collision forces
		force := collision.Force
		
		// Reduce velocity due to collision
		collision.Pelican1.Weight -= force * 0.1 // simplified momentum transfer
		collision.Pelican2.Weight -= force * 0.1
		
		// Potential injury from collision
		if force > 5.0 {
			if collision.Pelican1.Health == models.HealthStatusHealthy {
				collision.Pelican1.Health = models.HealthStatusInjured
			}
			if collision.Pelican2.Health == models.HealthStatusHealthy {
				collision.Pelican2.Health = models.HealthStatusInjured
			}
		}
		
		// Separate pelicans to prevent overlap
		ps.separatePelicans(collision.Pelican1, collision.Pelican2)
	}
}

// separatePelicans moves colliding pelicans apart
func (ps *PelicanSimulation) separatePelicans(p1, p2 *models.Pelican) {
	// Calculate separation vector
	deltaLat := p1.Latitude - p2.Latitude
	deltaLon := p1.Longitude - p2.Longitude
	
	// Normalize and apply separation
	length := math.Sqrt(deltaLat*deltaLat + deltaLon*deltaLon)
	if length > 0 {
		deltaLat /= length
		deltaLon /= length
		
		// Move pelicans apart
		separation := 0.001 // small coordinate adjustment
		p1.Latitude += deltaLat * separation
		p1.Longitude += deltaLon * separation
		p2.Latitude -= deltaLat * separation
		p2.Longitude -= deltaLon * separation
	}
}

// applyFlockingForce applies flocking behavior to pelican
func (ps *PelicanSimulation) applyFlockingForce(pelican *models.Pelican, force physics.Vector3D) {
	// Convert force to position changes (simplified)
	deltaTime := ps.tickRate.Seconds()
	
	// Apply force to position (simplified physics)
	pelican.Latitude += force.X * deltaTime * 0.0001  // small coordinate changes
	pelican.Longitude += force.Y * deltaTime * 0.0001
	
	// Age represents altitude in our simplified model
	newAltitude := float64(pelican.Age) + force.Z*deltaTime*0.1
	if newAltitude > 0 && newAltitude < 100 { // reasonable altitude limits
		pelican.Age = int(newAltitude)
	}
}

// applyWaterPhysics applies water interaction effects
func (ps *PelicanSimulation) applyWaterPhysics(pelican *models.Pelican, interaction physics.WaterInteraction) {
	if interaction.IsInWater {
		// Swimming pelican moves slower
		pelican.Weight *= 0.95 // reduce "velocity" (weight proxy)
		
		// Buoyancy affects altitude
		buoyancyEffect := interaction.BuoyantForce / 1000.0 // scaled down
		pelican.Age = int(math.Max(0, float64(pelican.Age)+buoyancyEffect))
		
		// Water exposure might affect health
		if interaction.WaterDepth > 5.0 && pelican.Health == models.HealthStatusHealthy {
			// Long submersion might cause stress
			if rand.Float64() < 0.001 { // 0.1% chance per tick
				pelican.Health = models.HealthStatusSick
			}
		}
	}
}

// updatePelicanPosition updates pelican's world position
func (ps *PelicanSimulation) updatePelicanPosition(pelican *models.Pelican) {
	// Natural pelican behavior - slight random movement
	if pelican.Health == models.HealthStatusHealthy {
		randomFactor := 0.00001 // very small random movement
		pelican.Latitude += (rand.Float64() - 0.5) * randomFactor
		pelican.Longitude += (rand.Float64() - 0.5) * randomFactor
	}
	
	// Update timestamp
	pelican.UpdatedAt = time.Now()
}

// savePelicansToDatabase saves current pelican states to database
func (ps *PelicanSimulation) savePelicansToDatabase() {
	for _, pelican := range ps.pelicans {
		ps.db.Save(pelican)
	}
}

// AddPelican adds a new pelican to the simulation
func (ps *PelicanSimulation) AddPelican(pelican *models.Pelican) {
	ps.mutex.Lock()
	ps.pelicans[pelican.ID] = pelican
	ps.mutex.Unlock()
}

// RemovePelican removes a pelican from the simulation
func (ps *PelicanSimulation) RemovePelican(pelicanID int) {
	ps.mutex.Lock()
	delete(ps.pelicans, pelicanID)
	ps.mutex.Unlock()
}

// GetPelicanCount returns current number of pelicans in simulation
func (ps *PelicanSimulation) GetPelicanCount() int {
	ps.mutex.RLock()
	defer ps.mutex.RUnlock()
	return len(ps.pelicans)
}

// SetWaterLevel adjusts the water level for physics calculations
func (ps *PelicanSimulation) SetWaterLevel(level float64) {
	ps.mutex.Lock()
	ps.waterLevel = level
	ps.mutex.Unlock()
}
