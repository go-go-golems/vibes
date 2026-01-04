package animations

import (
	"math/rand"
	"time"
)

// AnimationClock manages all animation timing
type AnimationClock struct {
	startTime    time.Time
	frameCounter int64
	lastTick     time.Time
}

// NewAnimationClock creates a new animation clock
func NewAnimationClock() *AnimationClock {
	now := time.Now()
	return &AnimationClock{
		startTime: now,
		lastTick:  now,
	}
}

// Tick updates the animation clock
func (ac *AnimationClock) Tick() {
	now := time.Now()
	if now.Sub(ac.lastTick) >= 100*time.Millisecond {
		ac.frameCounter++
		ac.lastTick = now
	}
}

// GetFrame returns the current frame counter
func (ac *AnimationClock) GetFrame() int64 {
	return ac.frameCounter
}

// GetElapsed returns elapsed time since start
func (ac *AnimationClock) GetElapsed() time.Duration {
	return time.Since(ac.startTime)
}

// IsFrameMultiple checks if current frame is a multiple of n
func (ac *AnimationClock) IsFrameMultiple(n int64) bool {
	return ac.frameCounter%n == 0
}

// GetPhaseOffset returns a phase-offset frame counter
func (ac *AnimationClock) GetPhaseOffset(offset int64) int64 {
	return (ac.frameCounter + offset) % 1000000 // Prevent overflow
}

// Animation speed constants (in 100ms ticks)
const (
	FastAnimation   = 1  // 100ms
	MediumAnimation = 5  // 500ms
	SlowAnimation   = 20 // 2 seconds
)

// AnimationState represents the state of an animation
type AnimationState struct {
	Frame    int
	MaxFrame int
	Speed    int64
	Offset   int64
}

// NewAnimationState creates a new animation state
func NewAnimationState(maxFrame int, speed int64, offset int64) *AnimationState {
	return &AnimationState{
		Frame:    0,
		MaxFrame: maxFrame,
		Speed:    speed,
		Offset:   offset,
	}
}

// Update updates the animation state based on the clock
func (as *AnimationState) Update(clock *AnimationClock) {
	frameWithOffset := clock.GetPhaseOffset(as.Offset)
	if frameWithOffset%as.Speed == 0 {
		as.Frame = (as.Frame + 1) % as.MaxFrame
	}
}

// GetFrame returns the current frame
func (as *AnimationState) GetFrame() int {
	return as.Frame
}

// GlitchState manages glitch effects
type GlitchState struct {
	Active       bool
	Duration     time.Duration
	StartTime    time.Time
	Intensity    float64 // 0.0 to 1.0
	NextTrigger  time.Time
}

// NewGlitchState creates a new glitch state
func NewGlitchState() *GlitchState {
	return &GlitchState{
		Active:      false,
		Duration:    150 * time.Millisecond,
		Intensity:   0.2, // 20% corruption
		NextTrigger: time.Now().Add(time.Duration(rand.Intn(3000)+2000) * time.Millisecond),
	}
}

// Update updates the glitch state
func (gs *GlitchState) Update() {
	now := time.Now()
	
	if gs.Active {
		if now.Sub(gs.StartTime) >= gs.Duration {
			gs.Active = false
			gs.NextTrigger = now.Add(time.Duration(rand.Intn(3000)+2000) * time.Millisecond)
		}
	} else {
		if now.After(gs.NextTrigger) {
			gs.TriggerGlitch()
		}
	}
}

// TriggerGlitch manually triggers a glitch effect
func (gs *GlitchState) TriggerGlitch() {
	gs.Active = true
	gs.StartTime = time.Now()
	gs.Intensity = 0.1 + rand.Float64()*0.2 // 10-30% corruption
}

// IsActive returns whether glitch is currently active
func (gs *GlitchState) IsActive() bool {
	return gs.Active
}

// GetIntensity returns the current glitch intensity
func (gs *GlitchState) GetIntensity() float64 {
	return gs.Intensity
}

