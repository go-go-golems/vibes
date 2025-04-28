package saga

import (
	"context"
	"fmt"
	"log"
	"sync"
	"time"

	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/google/uuid"
)

// SagaStep defines a step in a saga process
type SagaStep struct {
	// Name of the step for logging and debugging
	Name string

	// Handler is the function to execute for this step
	Handler func(ctx context.Context, data interface{}) (interface{}, error)

	// Compensation is the function to execute if the saga needs to be reversed
	Compensation func(ctx context.Context, data interface{}) error
}

// SagaDefinition defines the sequence of steps in a saga
type SagaDefinition struct {
	// Name of the saga
	Name string

	// Steps to execute in order
	Steps []SagaStep
}

// SagaInstance represents a running instance of a saga
type SagaInstance struct {
	// ID of this saga instance
	ID string

	// Definition is the saga definition
	Definition SagaDefinition

	// Data is the data being passed between steps
	Data interface{}

	// CurrentStep is the index of the current step
	CurrentStep int

	// CompensatingMode indicates if the saga is executing normally or compensating
	CompensatingMode bool

	// StartTime is when the saga started
	StartTime time.Time

	// EndTime is when the saga completed or failed
	EndTime time.Time

	// Error is the error that caused compensation, if any
	Error error

	// Publisher to publish saga events
	Publisher message.Publisher

	// Mutex to protect concurrent access
	mutex sync.Mutex
}

// NewSagaDefinition creates a new saga definition
func NewSagaDefinition(name string, steps ...SagaStep) SagaDefinition {
	return SagaDefinition{
		Name:  name,
		Steps: steps,
	}
}

// NewSagaInstance creates a new saga instance
func NewSagaInstance(definition SagaDefinition, initialData interface{}, publisher message.Publisher) *SagaInstance {
	return &SagaInstance{
		ID:               uuid.New().String(),
		Definition:       definition,
		Data:             initialData,
		CurrentStep:      0,
		CompensatingMode: false,
		StartTime:        time.Now(),
		Publisher:        publisher,
	}
}

// Start begins execution of the saga
func (s *SagaInstance) Start(ctx context.Context) error {
	s.mutex.Lock()
	defer s.mutex.Unlock()

	log.Printf("Starting saga %s (instance: %s)", s.Definition.Name, s.ID)

	if len(s.Definition.Steps) == 0 {
		return fmt.Errorf("saga definition has no steps")
	}

	return s.executeNextStep(ctx)
}

// executeNextStep executes the next step in the saga
func (s *SagaInstance) executeNextStep(ctx context.Context) error {
	if s.CompensatingMode {
		// In compensating mode, go backward
		if s.CurrentStep < 0 {
			s.EndTime = time.Now()
			log.Printf("Saga %s (instance: %s) compensation completed", s.Definition.Name, s.ID)
			return nil
		}

		step := s.Definition.Steps[s.CurrentStep]
		log.Printf("Executing compensation for step %s of saga %s (instance: %s)",
			step.Name, s.Definition.Name, s.ID)

		if step.Compensation != nil {
			if err := step.Compensation(ctx, s.Data); err != nil {
				log.Printf("Error executing compensation for step %s: %v", step.Name, err)
				// Continue compensation even if there's an error
			}
		}

		s.CurrentStep--
		return s.executeNextStep(ctx)
	} else {
		// In normal mode, go forward
		if s.CurrentStep >= len(s.Definition.Steps) {
			s.EndTime = time.Now()
			log.Printf("Saga %s (instance: %s) completed successfully", s.Definition.Name, s.ID)
			return nil
		}

		step := s.Definition.Steps[s.CurrentStep]
		log.Printf("Executing step %s of saga %s (instance: %s)",
			step.Name, s.Definition.Name, s.ID)

		result, err := step.Handler(ctx, s.Data)
		if err != nil {
			log.Printf("Error executing step %s: %v", step.Name, err)
			s.Error = err
			s.CompensatingMode = true
			s.CurrentStep--
			return s.executeNextStep(ctx)
		}

		// Update data for the next step
		s.Data = result
		s.CurrentStep++
		return s.executeNextStep(ctx)
	}
}

// SagaEvent represents an event emitted during saga execution
type SagaEvent struct {
	SagaID    string    `json:"saga_id"`
	SagaName  string    `json:"saga_name"`
	StepName  string    `json:"step_name"`
	StepIndex int       `json:"step_index"`
	EventType string    `json:"event_type"` // started, completed, failed, compensating
	Timestamp time.Time `json:"timestamp"`
	Error     string    `json:"error,omitempty"`
}

// publishEvent publishes a saga event
func (s *SagaInstance) publishEvent(eventType string, stepName string, err error) {
	if s.Publisher == nil {
		return
	}

	event := SagaEvent{
		SagaID:    s.ID,
		SagaName:  s.Definition.Name,
		StepName:  stepName,
		StepIndex: s.CurrentStep,
		EventType: eventType,
		Timestamp: time.Now(),
	}

	if err != nil {
		event.Error = err.Error()
	}

	// Publish the event (could use json.Marshal here)
	payload := []byte(fmt.Sprintf(`{
		"saga_id": "%s",
		"saga_name": "%s",
		"step_name": "%s",
		"step_index": %d,
		"event_type": "%s",
		"timestamp": "%s"
	}`, event.SagaID, event.SagaName, event.StepName, event.StepIndex, event.EventType, event.Timestamp))

	msg := message.NewMessage(uuid.New().String(), payload)

	// Publish to a topic specific to this saga
	topic := fmt.Sprintf("saga.%s.events", s.Definition.Name)
	if err := s.Publisher.Publish(topic, msg); err != nil {
		log.Printf("Error publishing saga event: %v", err)
	}
}
