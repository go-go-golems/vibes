package physics

import (
	"math"
	"walrus-wing/internal/models"
)

// AerodynamicsEngine calculates wing physics for walrus flight
type AerodynamicsEngine struct {
	GravityAccel  float64 // m/s² (Earth: 9.81)
	AirDensity    float64 // kg/m³ (sea level: 1.225)
	DragCoeff     float64 // Cd for walrus (estimated: 0.8)
	LiftCoeff     float64 // Cl for walrus wings (estimated: 1.2)
}

// NewAerodynamicsEngine creates a physics engine with realistic values
func NewAerodynamicsEngine() *AerodynamicsEngine {
	return &AerodynamicsEngine{
		GravityAccel: 9.81,
		AirDensity:   1.225,
		DragCoeff:    0.8,
		LiftCoeff:    1.2,
	}
}

// CalculateWingForces computes aerodynamic forces for a walrus
func (ae *AerodynamicsEngine) CalculateWingForces(walrus *models.Walrus, windSpeed, windDirection float64) *models.WingPhysics {
	// Wing area calculation (simplified: wingSpan² * 0.6 for walrus wings)
	wingArea := walrus.WingSpan * walrus.WingSpan * 0.6
	
	// Effective airspeed (walrus velocity + wind)
	windVelX := windSpeed * math.Cos(windDirection*math.Pi/180)
	windVelY := windSpeed * math.Sin(windDirection*math.Pi/180)
	effectiveVel := math.Sqrt(math.Pow(walrus.Velocity+windVelX, 2) + math.Pow(windVelY, 2))
	
	// Lift force: F = 0.5 * ρ * v² * Cl * A
	liftForce := 0.5 * ae.AirDensity * effectiveVel * effectiveVel * ae.LiftCoeff * wingArea
	
	// Drag force: F = 0.5 * ρ * v² * Cd * A  
	dragForce := 0.5 * ae.AirDensity * effectiveVel * effectiveVel * ae.DragCoeff * wingArea
	
	// Thrust varies by wing flapping (simplified model)
	var thrustForce float64
	if walrus.LeftWingStatus == models.WingStatusFlapping || walrus.RightWingStatus == models.WingStatusFlapping {
		thrustForce = liftForce * 0.3 // 30% of lift as thrust when flapping
	} else {
		thrustForce = 0 // No thrust when gliding
	}
	
	// Wing angles (simplified: based on status)
	leftAngle := ae.getWingAngle(walrus.LeftWingStatus)
	rightAngle := ae.getWingAngle(walrus.RightWingStatus)
	
	return &models.WingPhysics{
		WalrusID:      walrus.ID,
		LeftWingAngle: leftAngle,
		RightWingAngle: rightAngle,
		LiftForce:     liftForce,
		DragForce:     dragForce,
		ThrustForce:   thrustForce,
		AirDensity:    ae.AirDensity,
		WindSpeed:     windSpeed,
		WindDirection: windDirection,
	}
}

// UpdateWalrusPosition calculates new position based on forces
func (ae *AerodynamicsEngine) UpdateWalrusPosition(walrus *models.Walrus, physics *models.WingPhysics, deltaTime float64) {
	mass := walrus.Weight // kg
	
	// Net forces
	netVerticalForce := physics.LiftForce - (mass * ae.GravityAccel)
	netHorizontalForce := physics.ThrustForce - physics.DragForce
	
	// Accelerations
	verticalAccel := netVerticalForce / mass
	horizontalAccel := netHorizontalForce / mass
	
	// Update velocity (simplified 1D physics)
	verticalVel := verticalAccel * deltaTime
	walrus.Velocity += horizontalAccel * deltaTime
	
	// Update position
	walrus.Altitude += verticalVel * deltaTime
	
	// Ensure walrus doesn't go underground
	if walrus.Altitude < 0 {
		walrus.Altitude = 0
		walrus.Velocity *= 0.5 // Landing impact
	}
	
	// Energy consumption based on activity
	energyConsumption := ae.calculateEnergyConsumption(walrus, deltaTime)
	walrus.Energy -= energyConsumption
	
	if walrus.Energy < 0 {
		walrus.Energy = 0
		// Force landing when exhausted
		walrus.LeftWingStatus = models.WingStatusTucked
		walrus.RightWingStatus = models.WingStatusTucked
	}
}

// getWingAngle returns wing angle based on status
func (ae *AerodynamicsEngine) getWingAngle(status models.WingStatus) float64 {
	switch status {
	case models.WingStatusFlapping:
		return 45.0 // Flapping position
	case models.WingStatusExtended:
		return 90.0 // Fully extended
	case models.WingStatusGliding:
		return 30.0 // Gliding position
	case models.WingStatusTucked:
		return 0.0  // Tucked against body
	case models.WingStatusInjured:
		return 15.0 // Partially extended
	default:
		return 0.0
	}
}

// calculateEnergyConsumption computes energy usage per second
func (ae *AerodynamicsEngine) calculateEnergyConsumption(walrus *models.Walrus, deltaTime float64) float64 {
	baseMetabolism := 0.1 * deltaTime // Base energy consumption
	
	// Flapping costs more energy
	var flappingCost float64
	if walrus.LeftWingStatus == models.WingStatusFlapping || walrus.RightWingStatus == models.WingStatusFlapping {
		flappingCost = 2.0 * deltaTime
	}
	
	// Velocity costs energy
	velocityCost := walrus.Velocity * 0.01 * deltaTime
	
	return baseMetabolism + flappingCost + velocityCost
}
