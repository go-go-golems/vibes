package blackboard

import (
	"log"
	"sync"
	"time"

	"github.com/blackboard-system/internal/database"
	"github.com/blackboard-system/internal/models"
)

// Blackboard represents the central data structure of the system
type Blackboard struct {
	mu            sync.RWMutex
	state         models.BlackboardState
	eventHandlers []func(event Event)
	cycle         int
}

// Event represents a change in the blackboard system
type Event struct {
	Type             string          `json:"type"`
	KnowledgeSource  string          `json:"knowledge_source,omitempty"`
	Level            string          `json:"level,omitempty"`
	HypothesisID     string          `json:"hypothesis_id,omitempty"`
	Action           string          `json:"action,omitempty"`
	Data             interface{}     `json:"data,omitempty"`
	Timestamp        time.Time       `json:"timestamp"`
}

// NewBlackboard creates a new blackboard instance
func NewBlackboard() (*Blackboard, error) {
	bb := &Blackboard{
		eventHandlers: make([]func(event Event), 0),
		cycle:         0,
	}
	
	// Load initial state from database
	state, err := database.GetBlackboardState()
	if err != nil {
		return nil, err
	}
	
	bb.state = state
	return bb, nil
}

// GetState returns the current state of the blackboard
func (bb *Blackboard) GetState() models.BlackboardState {
	bb.mu.RLock()
	defer bb.mu.RUnlock()
	return bb.state
}

// AddHypothesis adds a new hypothesis to the blackboard
func (bb *Blackboard) AddHypothesis(h models.Hypothesis) error {
	bb.mu.Lock()
	defer bb.mu.Unlock()
	
	if err := database.CreateHypothesis(h); err != nil {
		return err
	}
	
	// Update in-memory state
	level := getLevelFromID(h.ID)
	switch level {
	case "PHRASE":
		bb.state.Phrase = append(bb.state.Phrase, h)
	case "WORD":
		bb.state.Word = append(bb.state.Word, h)
	case "SYLLABLE":
		bb.state.Syllable = append(bb.state.Syllable, h)
	case "SEGMENT":
		bb.state.Segment = append(bb.state.Segment, h)
	case "PARAMETER":
		bb.state.Parameter = append(bb.state.Parameter, h)
	}
	
	// Emit event
	bb.emitEvent(Event{
		Type:         "hypothesis_added",
		Level:        level,
		HypothesisID: h.ID,
		Data:         h,
		Timestamp:    time.Now(),
	})
	
	return nil
}

// UpdateHypothesis updates an existing hypothesis
func (bb *Blackboard) UpdateHypothesis(h models.Hypothesis) error {
	bb.mu.Lock()
	defer bb.mu.Unlock()
	
	if err := database.UpdateHypothesis(h); err != nil {
		return err
	}
	
	// Update in-memory state
	level := getLevelFromID(h.ID)
	var updated bool
	
	switch level {
	case "PHRASE":
		for i, hyp := range bb.state.Phrase {
			if hyp.ID == h.ID {
				bb.state.Phrase[i] = h
				updated = true
				break
			}
		}
	case "WORD":
		for i, hyp := range bb.state.Word {
			if hyp.ID == h.ID {
				bb.state.Word[i] = h
				updated = true
				break
			}
		}
	case "SYLLABLE":
		for i, hyp := range bb.state.Syllable {
			if hyp.ID == h.ID {
				bb.state.Syllable[i] = h
				updated = true
				break
			}
		}
	case "SEGMENT":
		for i, hyp := range bb.state.Segment {
			if hyp.ID == h.ID {
				bb.state.Segment[i] = h
				updated = true
				break
			}
		}
	case "PARAMETER":
		for i, hyp := range bb.state.Parameter {
			if hyp.ID == h.ID {
				bb.state.Parameter[i] = h
				updated = true
				break
			}
		}
	}
	
	if !updated {
		log.Printf("Warning: Hypothesis %s not found in memory, but updated in database", h.ID)
	}
	
	// Emit event
	bb.emitEvent(Event{
		Type:         "hypothesis_updated",
		Level:        level,
		HypothesisID: h.ID,
		Data:         h,
		Timestamp:    time.Now(),
	})
	
	return nil
}

// LockHypothesis locks a hypothesis for a knowledge source
func (bb *Blackboard) LockHypothesis(id, owner string) error {
	bb.mu.Lock()
	defer bb.mu.Unlock()
	
	// Find the hypothesis
	var h models.Hypothesis
	var found bool
	
	level := getLevelFromID(id)
	switch level {
	case "PHRASE":
		for i, hyp := range bb.state.Phrase {
			if hyp.ID == id {
				h = hyp
				h.Locked = true
				h.Owner = owner
				bb.state.Phrase[i] = h
				found = true
				break
			}
		}
	case "WORD":
		for i, hyp := range bb.state.Word {
			if hyp.ID == id {
				h = hyp
				h.Locked = true
				h.Owner = owner
				bb.state.Word[i] = h
				found = true
				break
			}
		}
	case "SYLLABLE":
		for i, hyp := range bb.state.Syllable {
			if hyp.ID == id {
				h = hyp
				h.Locked = true
				h.Owner = owner
				bb.state.Syllable[i] = h
				found = true
				break
			}
		}
	case "SEGMENT":
		for i, hyp := range bb.state.Segment {
			if hyp.ID == id {
				h = hyp
				h.Locked = true
				h.Owner = owner
				bb.state.Segment[i] = h
				found = true
				break
			}
		}
	case "PARAMETER":
		for i, hyp := range bb.state.Parameter {
			if hyp.ID == id {
				h = hyp
				h.Locked = true
				h.Owner = owner
				bb.state.Parameter[i] = h
				found = true
				break
			}
		}
	}
	
	if !found {
		return ErrHypothesisNotFound
	}
	
	// Update in database
	if err := database.UpdateHypothesis(h); err != nil {
		return err
	}
	
	// Emit event
	bb.emitEvent(Event{
		Type:             "hypothesis_locked",
		Level:            level,
		HypothesisID:     id,
		KnowledgeSource:  owner,
		Timestamp:        time.Now(),
	})
	
	return nil
}

// UnlockHypothesis unlocks a hypothesis
func (bb *Blackboard) UnlockHypothesis(id string) error {
	bb.mu.Lock()
	defer bb.mu.Unlock()
	
	// Find the hypothesis
	var h models.Hypothesis
	var found bool
	var owner string
	
	level := getLevelFromID(id)
	switch level {
	case "PHRASE":
		for i, hyp := range bb.state.Phrase {
			if hyp.ID == id {
				owner = hyp.Owner
				h = hyp
				h.Locked = false
				h.Owner = ""
				bb.state.Phrase[i] = h
				found = true
				break
			}
		}
	case "WORD":
		for i, hyp := range bb.state.Word {
			if hyp.ID == id {
				owner = hyp.Owner
				h = hyp
				h.Locked = false
				h.Owner = ""
				bb.state.Word[i] = h
				found = true
				break
			}
		}
	case "SYLLABLE":
		for i, hyp := range bb.state.Syllable {
			if hyp.ID == id {
				owner = hyp.Owner
				h = hyp
				h.Locked = false
				h.Owner = ""
				bb.state.Syllable[i] = h
				found = true
				break
			}
		}
	case "SEGMENT":
		for i, hyp := range bb.state.Segment {
			if hyp.ID == id {
				owner = hyp.Owner
				h = hyp
				h.Locked = false
				h.Owner = ""
				bb.state.Segment[i] = h
				found = true
				break
			}
		}
	case "PARAMETER":
		for i, hyp := range bb.state.Parameter {
			if hyp.ID == id {
				owner = hyp.Owner
				h = hyp
				h.Locked = false
				h.Owner = ""
				bb.state.Parameter[i] = h
				found = true
				break
			}
		}
	}
	
	if !found {
		return ErrHypothesisNotFound
	}
	
	// Update in database
	if err := database.UpdateHypothesis(h); err != nil {
		return err
	}
	
	// Emit event
	bb.emitEvent(Event{
		Type:             "hypothesis_unlocked",
		Level:            level,
		HypothesisID:     id,
		KnowledgeSource:  owner,
		Timestamp:        time.Now(),
	})
	
	return nil
}

// UpdateKnowledgeSource updates a knowledge source
func (bb *Blackboard) UpdateKnowledgeSource(ks models.KnowledgeSource) error {
	bb.mu.Lock()
	defer bb.mu.Unlock()
	
	if err := database.UpdateKnowledgeSource(ks); err != nil {
		return err
	}
	
	// Update in-memory state
	for i, source := range bb.state.KSources {
		if source.ID == ks.ID {
			bb.state.KSources[i] = ks
			break
		}
	}
	
	// Emit event
	bb.emitEvent(Event{
		Type:             "knowledge_source_updated",
		KnowledgeSource:  ks.ID,
		Data:             ks,
		Timestamp:        time.Now(),
	})
	
	return nil
}

// AddActivityLog adds a new activity log entry
func (bb *Blackboard) AddActivityLog(al models.ActivityLog) error {
	bb.mu.Lock()
	defer bb.mu.Unlock()
	
	if err := database.CreateActivityLog(al); err != nil {
		return err
	}
	
	// Update in-memory state
	bb.state.ActivityLog = append([]models.ActivityLog{al}, bb.state.ActivityLog...)
	if len(bb.state.ActivityLog) > 10 {
		bb.state.ActivityLog = bb.state.ActivityLog[:10]
	}
	
	// Emit event
	bb.emitEvent(Event{
		Type:             "activity_log_added",
		KnowledgeSource:  al.KnowledgeSourceID,
		Level:            al.Level,
		Action:           al.Action,
		Data:             al,
		Timestamp:        time.Now(),
	})
	
	return nil
}

// UpdateFocus updates the current focus of control
func (bb *Blackboard) UpdateFocus(focus string) error {
	bb.mu.Lock()
	defer bb.mu.Unlock()
	
	// Get current focus control
	fc, err := database.GetFocusControl()
	if err != nil {
		return err
	}
	
	// Update focus
	fc.CurrentFocus = focus
	if err := database.UpdateFocusControl(fc); err != nil {
		return err
	}
	
	// Update in-memory state
	bb.state.Focus.CurrentFocus = focus
	
	// Emit event
	bb.emitEvent(Event{
		Type:             "focus_updated",
		Level:            focus,
		Timestamp:        time.Now(),
	})
	
	return nil
}

// AddBid adds a new bid to the queue
func (bb *Blackboard) AddBid(bid models.BidQueue) error {
	bb.mu.Lock()
	defer bb.mu.Unlock()
	
	if err := database.CreateBid(bid); err != nil {
		return err
	}
	
	// Update in-memory state
	bb.state.Focus.BidQueue = append(bb.state.Focus.BidQueue, bid)
	
	// Emit event
	bb.emitEvent(Event{
		Type:             "bid_added",
		KnowledgeSource:  bid.KnowledgeSourceID,
		Action:           bid.Action,
		Data:             bid,
		Timestamp:        time.Now(),
	})
	
	return nil
}

// RemoveBid removes a bid from the queue
func (bb *Blackboard) RemoveBid(id int64) error {
	bb.mu.Lock()
	defer bb.mu.Unlock()
	
	if err := database.DeleteBid(id); err != nil {
		return err
	}
	
	// Update in-memory state
	for i, bid := range bb.state.Focus.BidQueue {
		if bid.ID == id {
			bb.state.Focus.BidQueue = append(bb.state.Focus.BidQueue[:i], bb.state.Focus.BidQueue[i+1:]...)
			break
		}
	}
	
	// Emit event
	bb.emitEvent(Event{
		Type:             "bid_removed",
		Data:             id,
		Timestamp:        time.Now(),
	})
	
	return nil
}

// IncrementCycle increments the cycle counter
func (bb *Blackboard) IncrementCycle() {
	bb.mu.Lock()
	defer bb.mu.Unlock()
	
	bb.cycle++
	
	// Emit event
	bb.emitEvent(Event{
		Type:             "cycle_incremented",
		Data:             bb.cycle,
		Timestamp:        time.Now(),
	})
}

// GetCycle returns the current cycle count
func (bb *Blackboard) GetCycle() int {
	bb.mu.RLock()
	defer bb.mu.RUnlock()
	
	return bb.cycle
}

// RegisterEventHandler registers a function to be called when events occur
func (bb *Blackboard) RegisterEventHandler(handler func(event Event)) {
	bb.mu.Lock()
	defer bb.mu.Unlock()
	
	bb.eventHandlers = append(bb.eventHandlers, handler)
}

// emitEvent sends an event to all registered handlers
func (bb *Blackboard) emitEvent(event Event) {
	for _, handler := range bb.eventHandlers {
		go handler(event)
	}
}

// Helper function to get level from hypothesis ID
func getLevelFromID(id string) string {
	if len(id) < 3 {
		return "UNKNOWN"
	}
	
	prefix := id[:3]
	switch prefix {
	case "PHR":
		return "PHRASE"
	case "WRD":
		return "WORD"
	case "SYL":
		return "SYLLABLE"
	case "SEG":
		return "SEGMENT"
	case "PAR":
		return "PARAMETER"
	default:
		return "UNKNOWN"
	}
}

// Error definitions
var (
	ErrHypothesisNotFound = NewError("hypothesis not found")
)

// Error represents a blackboard error
type Error struct {
	message string
}

// NewError creates a new error
func NewError(message string) *Error {
	return &Error{message: message}
}

// Error returns the error message
func (e *Error) Error() string {
	return e.message
}
