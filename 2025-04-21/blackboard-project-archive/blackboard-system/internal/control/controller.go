package control

import (
	"log"
	"sort"
	"sync"
	"time"

	"github.com/blackboard-system/internal/blackboard"
	"github.com/blackboard-system/internal/models"
)

// Controller manages the control flow of the blackboard system
type Controller struct {
	bb            *blackboard.Blackboard
	mu            sync.Mutex
	running       bool
	stopChan      chan struct{}
	knowledgeSources map[string]KnowledgeSource
}

// KnowledgeSource represents an agent that can interact with the blackboard
type KnowledgeSource interface {
	ID() string
	Activate(bb *blackboard.Blackboard) error
	CanActivate(state models.BlackboardState) (bool, float64)
}

// NewController creates a new controller
func NewController(bb *blackboard.Blackboard) *Controller {
	return &Controller{
		bb:            bb,
		knowledgeSources: make(map[string]KnowledgeSource),
		stopChan:      make(chan struct{}),
	}
}

// RegisterKnowledgeSource registers a knowledge source with the controller
func (c *Controller) RegisterKnowledgeSource(ks KnowledgeSource) {
	c.mu.Lock()
	defer c.mu.Unlock()
	
	c.knowledgeSources[ks.ID()] = ks
	log.Printf("Registered knowledge source: %s", ks.ID())
}

// Start starts the control cycle
func (c *Controller) Start() {
	c.mu.Lock()
	if c.running {
		c.mu.Unlock()
		return
	}
	c.running = true
	c.stopChan = make(chan struct{})
	c.mu.Unlock()
	
	go c.controlLoop()
}

// Stop stops the control cycle
func (c *Controller) Stop() {
	c.mu.Lock()
	defer c.mu.Unlock()
	
	if !c.running {
		return
	}
	
	c.running = false
	close(c.stopChan)
}

// IsRunning returns whether the controller is running
func (c *Controller) IsRunning() bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	
	return c.running
}

// controlLoop is the main control loop
func (c *Controller) controlLoop() {
	ticker := time.NewTicker(1 * time.Second)
	defer ticker.Stop()
	
	for {
		select {
		case <-c.stopChan:
			return
		case <-ticker.C:
			c.cycle()
		}
	}
}

// cycle performs one control cycle
func (c *Controller) cycle() {
	c.mu.Lock()
	defer c.mu.Unlock()
	
	// Get current state
	state := c.bb.GetState()
	
	// Collect bids from knowledge sources
	type bid struct {
		ksID  string
		value float64
	}
	
	var bids []bid
	for id, ks := range c.knowledgeSources {
		canActivate, bidValue := ks.CanActivate(state)
		if canActivate {
			bids = append(bids, bid{
				ksID:  id,
				value: bidValue,
			})
		}
	}
	
	// Sort bids by value (highest first)
	sort.Slice(bids, func(i, j int) bool {
		return bids[i].value > bids[j].value
	})
	
	// Activate highest bidding knowledge source
	if len(bids) > 0 {
		highestBid := bids[0]
		ks := c.knowledgeSources[highestBid.ksID]
		
		// Log the activation
		log.Printf("Activating knowledge source %s with bid value %.2f", highestBid.ksID, highestBid.value)
		
		// Create activity log entry
		al := models.ActivityLog{
			Time:              time.Now().Format("15:04:05.000"),
			KnowledgeSourceID: highestBid.ksID,
			Action:            "Activated with bid value " + formatFloat(highestBid.value),
			Level:             "CONTROL",
		}
		
		if err := c.bb.AddActivityLog(al); err != nil {
			log.Printf("Error adding activity log: %v", err)
		}
		
		// Activate the knowledge source
		if err := ks.Activate(c.bb); err != nil {
			log.Printf("Error activating knowledge source %s: %v", highestBid.ksID, err)
		}
	}
	
	// Increment cycle counter
	c.bb.IncrementCycle()
}

// formatFloat formats a float to 2 decimal places
func formatFloat(f float64) string {
	return time.Now().Format("0.00")
}
