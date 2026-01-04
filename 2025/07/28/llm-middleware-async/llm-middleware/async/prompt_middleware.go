package async

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"log"
	"math/rand"
	"strings"
	"text/template"
	"time"
)

// Example represents a user/assistant example pair with metadata
type Example struct {
	User      string                 `json:"user"`
	Assistant string                 `json:"assistant"`
	Metadata  map[string]interface{} `json:"metadata,omitempty"`
	Tags      []string               `json:"tags,omitempty"`
	Weight    float64                `json:"weight,omitempty"`
}

// ExampleSelectionMode defines how examples are selected
type ExampleSelectionMode int

const (
	ModeSequential ExampleSelectionMode = iota
	ModeRandom
	ModeWeighted
	ModeSemantic
)

// PersonaController manages persona state with async operations
type PersonaController struct {
	currentPersona string
	personas       map[string]string
	switchHistory  []PersonaSwitch
}

// PersonaSwitch records a persona change event
type PersonaSwitch struct {
	From      string    `json:"from"`
	To        string    `json:"to"`
	Timestamp time.Time `json:"timestamp"`
	TurnID    string    `json:"turn_id"`
}

// NewPersonaController creates a new persona controller
func NewPersonaController(initialPersona string, personas map[string]string) *PersonaController {
	return &PersonaController{
		currentPersona: initialPersona,
		personas:       personas,
		switchHistory:  make([]PersonaSwitch, 0),
	}
}

// SwitchPersona changes the current persona
func (pc *PersonaController) SwitchPersona(newPersona, turnID string) error {
	if _, exists := pc.personas[newPersona]; !exists {
		return fmt.Errorf("persona '%s' not found", newPersona)
	}
	
	oldPersona := pc.currentPersona
	pc.currentPersona = newPersona
	
	pc.switchHistory = append(pc.switchHistory, PersonaSwitch{
		From:      oldPersona,
		To:        newPersona,
		Timestamp: time.Now(),
		TurnID:    turnID,
	})
	
	log.Printf("Persona switched to: %s", newPersona)
	return nil
}

// GetCurrentPersona returns the current persona
func (pc *PersonaController) GetCurrentPersona() string {
	return pc.currentPersona
}

// GetPersonaPrompt returns the prompt for the current persona
func (pc *PersonaController) GetPersonaPrompt() string {
	return pc.personas[pc.currentPersona]
}

// AsyncPromptTemplating returns async prompt templating middleware
func AsyncPromptTemplating(templateText, variablesKey string) AsyncMiddleware {
	// Parse template once at middleware creation
	tmpl, err := template.New("prompt").Parse(templateText)
	if err != nil {
		log.Printf("Template parsing error: %v", err)
		return func(next AsyncHandler) AsyncHandler {
			return func(ctx context.Context, turn *Turn) <-chan AsyncResult {
				resultChan := make(chan AsyncResult, 1)
				go func() {
					defer close(resultChan)
					turn.Context.AddWarning("TEMPLATE_PARSE_ERROR", err.Error(), "template_middleware")
					nextResult := <-next(ctx, turn)
					resultChan <- nextResult
				}()
				return resultChan
			}
		}
	}
	
	return func(next AsyncHandler) AsyncHandler {
		return func(ctx context.Context, turn *Turn) <-chan AsyncResult {
			resultChan := make(chan AsyncResult, 1)
			
			go func() {
				defer close(resultChan)
				
				// Get template variables from context
				variables, exists := turn.Context.GetVariable(variablesKey)
				if !exists {
					turn.Context.AddWarning("TEMPLATE_VARS_MISSING", 
						fmt.Sprintf("Template variables not found at key: %s", variablesKey), 
						"template_middleware")
					nextResult := <-next(ctx, turn)
					resultChan <- nextResult
					return
				}
				
				// Render template asynchronously
				renderChan := make(chan string, 1)
				errorChan := make(chan error, 1)
				
				go func() {
					var buf bytes.Buffer
					if err := tmpl.Execute(&buf, variables); err != nil {
						errorChan <- err
					} else {
						renderChan <- buf.String()
					}
				}()
				
				select {
				case rendered := <-renderChan:
					// Create system message with rendered template
					templateMsg := Message{
						Role:      RoleSystem,
						Content:   rendered,
						Timestamp: time.Now(),
						Metadata:  Metadata{"source": "template_middleware"},
					}
					
					// Insert at beginning of messages
					turn.Messages = append([]Message{templateMsg}, turn.Messages...)
					
					// Store template metadata
					turn.Context.SetFlag("template_applied", true)
					turn.Context.SetVariable("template_content", rendered)
					turn.Context.SetVariable("template_vars", variables)
					
					// Create template artifact
					templateArtifact := &Artifact{
						ID:        "template_result",
						Type:      ArtifactTypeTemplate,
						Version:   1,
						Data:      rendered,
						CreatedAt: time.Now(),
						UpdatedAt: time.Now(),
						Metadata: map[string]interface{}{
							"template_text": templateText,
							"variables":     variables,
						},
					}
					turn.Context.SetArtifact(templateArtifact)
					
					nextResult := <-next(ctx, turn)
					resultChan <- nextResult
					
				case err := <-errorChan:
					turn.Context.AddWarning("TEMPLATE_RENDER_ERROR", err.Error(), "template_middleware")
					nextResult := <-next(ctx, turn)
					resultChan <- nextResult
					
				case <-ctx.Done():
					err := ctx.Err()
					turn.Fail(err, "template_timeout")
					resultChan <- AsyncResult{Turn: turn, Error: err}
				}
			}()
			
			return resultChan
		}
	}
}

// AsyncChainOfThoughtInjector returns async CoT middleware
func AsyncChainOfThoughtInjector(cotPrompt, enableKey, usedKey string) AsyncMiddleware {
	return func(next AsyncHandler) AsyncHandler {
		return func(ctx context.Context, turn *Turn) <-chan AsyncResult {
			resultChan := make(chan AsyncResult, 1)
			
			go func() {
				defer close(resultChan)
				
				// Check if CoT is enabled
				enabled := turn.Context.GetFlag(enableKey)
				turn.Context.SetFlag(usedKey, enabled)
				
				if enabled && len(turn.Messages) > 0 {
					// Find last user message and append CoT prompt
					for i := len(turn.Messages) - 1; i >= 0; i-- {
						if turn.Messages[i].Role == RoleUser {
							originalContent := turn.Messages[i].Content
							enhancedContent := fmt.Sprintf("%s\n\n%s", originalContent, cotPrompt)
							turn.Messages[i].Content = enhancedContent
							turn.Messages[i].Metadata = Metadata{"cot_enhanced": true}
							break
						}
					}
					
					// Create CoT artifact
					cotArtifact := &Artifact{
						ID:        "cot_enhancement",
						Type:      ArtifactTypeText,
						Version:   1,
						Data:      cotPrompt,
						CreatedAt: time.Now(),
						UpdatedAt: time.Now(),
						Metadata: map[string]interface{}{
							"enabled_key": enableKey,
							"used_key":    usedKey,
						},
					}
					turn.Context.SetArtifact(cotArtifact)
				}
				
				nextResult := <-next(ctx, turn)
				resultChan <- nextResult
			}()
			
			return resultChan
		}
	}
}

// AsyncExampleInjection returns async example injection middleware
func AsyncExampleInjection(examples []Example, count int, mode ExampleSelectionMode) AsyncMiddleware {
	return func(next AsyncHandler) AsyncHandler {
		return func(ctx context.Context, turn *Turn) <-chan AsyncResult {
			resultChan := make(chan AsyncResult, 1)
			
			go func() {
				defer close(resultChan)
				
				if len(examples) == 0 || count <= 0 {
					nextResult := <-next(ctx, turn)
					resultChan <- nextResult
					return
				}
				
				// Select examples asynchronously
				selectionChan := make(chan []Example, 1)
				
				go func() {
					selected := selectExamples(examples, count, mode, turn.Index)
					selectionChan <- selected
				}()
				
				select {
				case selectedExamples := <-selectionChan:
					// Inject examples before the last user message
					var injectionPoint int
					for i := len(turn.Messages) - 1; i >= 0; i-- {
						if turn.Messages[i].Role == RoleUser {
							injectionPoint = i
							break
						}
					}
					
					// Create example messages
					var exampleMessages []Message
					for _, example := range selectedExamples {
						userMsg := Message{
							Role:      RoleUser,
							Content:   example.User,
							Timestamp: time.Now(),
							Metadata:  Metadata{"source": "example_injection", "type": "example"},
						}
						assistantMsg := Message{
							Role:      RoleAssistant,
							Content:   example.Assistant,
							Timestamp: time.Now(),
							Metadata:  Metadata{"source": "example_injection", "type": "example"},
						}
						exampleMessages = append(exampleMessages, userMsg, assistantMsg)
					}
					
					// Insert examples at injection point
					turn.Messages = append(
						turn.Messages[:injectionPoint],
						append(exampleMessages, turn.Messages[injectionPoint:]...)...,
					)
					
					// Store example metadata
					turn.Context.SetVariable("examples_injected", len(selectedExamples))
					turn.Context.SetVariable("injection_mode", int(mode))
					turn.Context.SetVariable("selected_examples", selectedExamples)
					
					// Create examples artifact
					examplesArtifact := &Artifact{
						ID:        "injected_examples",
						Type:      ArtifactTypeExample,
						Version:   1,
						Data:      selectedExamples,
						CreatedAt: time.Now(),
						UpdatedAt: time.Now(),
						Metadata: map[string]interface{}{
							"count":          count,
							"mode":           mode,
							"total_examples": len(examples),
						},
					}
					turn.Context.SetArtifact(examplesArtifact)
					
					nextResult := <-next(ctx, turn)
					resultChan <- nextResult
					
				case <-ctx.Done():
					err := ctx.Err()
					turn.Fail(err, "example_selection_timeout")
					resultChan <- AsyncResult{Turn: turn, Error: err}
				}
			}()
			
			return resultChan
		}
	}
}

// AsyncStructuredSchema returns async structured schema middleware
func AsyncStructuredSchema(schemaText, outputKey string) AsyncMiddleware {
	return func(next AsyncHandler) AsyncHandler {
		return func(ctx context.Context, turn *Turn) <-chan AsyncResult {
			resultChan := make(chan AsyncResult, 1)
			
			go func() {
				defer close(resultChan)
				
				// Inject schema prompt
				if len(turn.Messages) > 0 {
					schemaPrompt := fmt.Sprintf("Please respond with valid JSON matching this schema: %s", schemaText)
					
					// Find last user message and inject schema before it
					for i := len(turn.Messages) - 1; i >= 0; i-- {
						if turn.Messages[i].Role == RoleUser {
							schemaMsg := Message{
								Role:      RoleSystem,
								Content:   schemaPrompt,
								Timestamp: time.Now(),
								Metadata:  Metadata{"source": "schema_middleware"},
							}
							// Insert schema message
							turn.Messages = append(turn.Messages[:i], append([]Message{schemaMsg}, turn.Messages[i:]...)...)
							break
						}
					}
				}
				
				// Execute next handler
				nextResultChan := next(ctx, turn)
				
				select {
				case result := <-nextResultChan:
					if result.Error != nil {
						resultChan <- result
						return
					}
					
					// Parse JSON response asynchronously
					parseChan := make(chan interface{}, 1)
					errorChan := make(chan error, 1)
					
					go func() {
						var parsed interface{}
						if err := json.Unmarshal([]byte(turn.Output.Raw), &parsed); err != nil {
							errorChan <- err
						} else {
							parseChan <- parsed
						}
					}()
					
					select {
					case parsed := <-parseChan:
						// Create schema artifact
						schemaArtifact := &Artifact{
							ID:        outputKey,
							Type:      ArtifactTypeJSON,
							Version:   1,
							Data:      parsed,
							Schema:    schemaText,
							CreatedAt: time.Now(),
							UpdatedAt: time.Now(),
						}
						turn.Output.Artifacts[outputKey] = schemaArtifact
						turn.Context.SetFlag("schema_valid", true)
						turn.Context.SetVariable("schema_text", schemaText)
						turn.Context.SetVariable("schema_output_key", outputKey)
						
					case err := <-errorChan:
						turn.Context.AddWarning("SCHEMA_PARSE_ERROR", err.Error(), "schema_middleware")
						turn.Context.SetFlag("schema_valid", false)
						turn.Context.SetVariable("schema_error", err.Error())
						turn.Context.SetVariable("schema_text", schemaText)
						turn.Context.SetVariable("schema_output_key", outputKey)
						
					case <-time.After(5 * time.Second):
						turn.Context.AddWarning("SCHEMA_PARSE_TIMEOUT", "JSON parsing timed out", "schema_middleware")
						turn.Context.SetFlag("schema_valid", false)
					}
					
					resultChan <- result
					
				case <-ctx.Done():
					err := ctx.Err()
					turn.Fail(err, "schema_context_cancelled")
					resultChan <- AsyncResult{Turn: turn, Error: err}
				}
			}()
			
			return resultChan
		}
	}
}

// AsyncPersonaSwitch returns async persona switch middleware and controller
func AsyncPersonaSwitch(initialPersona string, personas map[string]string) (*PersonaController, AsyncMiddleware) {
	controller := NewPersonaController(initialPersona, personas)
	
	middleware := func(next AsyncHandler) AsyncHandler {
		return func(ctx context.Context, turn *Turn) <-chan AsyncResult {
			resultChan := make(chan AsyncResult, 1)
			
			go func() {
				defer close(resultChan)
				
				// Handle persona switch commands
				if len(turn.Messages) > 0 {
					lastMsg := &turn.Messages[len(turn.Messages)-1]
					if lastMsg.Role == RoleUser && strings.HasPrefix(lastMsg.Content, "/persona ") {
						parts := strings.Fields(lastMsg.Content)
						if len(parts) >= 2 {
							newPersona := parts[1]
							if err := controller.SwitchPersona(newPersona, turn.ID); err != nil {
								turn.Context.AddWarning("PERSONA_SWITCH_ERROR", err.Error(), "persona_middleware")
							} else {
								turn.Context.SetFlag("persona_switched", true)
								turn.Context.SetVariable("current_persona", newPersona)
								
								// Remove the command message
								turn.Messages = turn.Messages[:len(turn.Messages)-1]
								
								// If no messages left, this is just a persona switch
								if len(turn.Messages) == 0 {
									turn.Fail(fmt.Errorf("no messages provided"), "persona_switch")
									resultChan <- AsyncResult{Turn: turn, Error: fmt.Errorf("no messages provided")}
									return
								}
							}
						}
					}
				}
				
				// Add persona banner on turn 0
				if turn.Index == 0 {
					var personaNames []string
					for name := range personas {
						personaNames = append(personaNames, name)
					}
					
					bannerMsg := Message{
						Role:      RoleSystem,
						Content:   fmt.Sprintf("Available personas: %s. Current persona: %s. Use '/persona <name>' to switch.", strings.Join(personaNames, ", "), controller.GetCurrentPersona()),
						Timestamp: time.Now(),
						Metadata:  Metadata{"source": "persona_middleware", "type": "banner"},
					}
					turn.Messages = append([]Message{bannerMsg}, turn.Messages...)
				}
				
				// Inject current persona prompt
				currentPersona := controller.GetCurrentPersona()
				personaPrompt := controller.GetPersonaPrompt()
				
				if personaPrompt != "" {
					personaMsg := Message{
						Role:      RoleSystem,
						Content:   fmt.Sprintf("Persona: %s - %s", currentPersona, personaPrompt),
						Timestamp: time.Now(),
						Metadata:  Metadata{"source": "persona_middleware", "persona": currentPersona},
					}
					
					// Insert before last user message
					if len(turn.Messages) > 0 {
						for i := len(turn.Messages) - 1; i >= 0; i-- {
							if turn.Messages[i].Role == RoleUser {
								turn.Messages = append(turn.Messages[:i], append([]Message{personaMsg}, turn.Messages[i:]...)...)
								break
							}
						}
					}
				}
				
				// Store persona state
				turn.Context.SetVariable("current_persona", currentPersona)
				
				// Create persona artifact
				personaArtifact := &Artifact{
					ID:        "current_persona",
					Type:      ArtifactTypePersona,
					Version:   1,
					Data: map[string]interface{}{
						"name":    currentPersona,
						"prompt":  personaPrompt,
						"history": controller.switchHistory,
					},
					CreatedAt: time.Now(),
					UpdatedAt: time.Now(),
				}
				turn.Context.SetArtifact(personaArtifact)
				
				nextResult := <-next(ctx, turn)
				resultChan <- nextResult
			}()
			
			return resultChan
		}
	}
	
	return controller, middleware
}

// selectExamples selects examples based on the specified mode
func selectExamples(examples []Example, count int, mode ExampleSelectionMode, turnIndex int) []Example {
	if count >= len(examples) {
		return examples
	}
	
	switch mode {
	case ModeSequential:
		start := turnIndex % len(examples)
		selected := make([]Example, 0, count)
		for i := 0; i < count; i++ {
			idx := (start + i) % len(examples)
			selected = append(selected, examples[idx])
		}
		return selected
		
	case ModeRandom:
		// Use turn index as seed for deterministic randomness
		rng := rand.New(rand.NewSource(int64(turnIndex)))
		indices := rng.Perm(len(examples))
		selected := make([]Example, 0, count)
		for i := 0; i < count; i++ {
			selected = append(selected, examples[indices[i]])
		}
		return selected
		
	case ModeWeighted:
		// Implement weighted selection based on example weights
		return selectWeightedExamples(examples, count, turnIndex)
		
	default:
		return examples[:count]
	}
}

// selectWeightedExamples selects examples based on their weights
func selectWeightedExamples(examples []Example, count int, turnIndex int) []Example {
	// Calculate total weight
	totalWeight := 0.0
	for _, example := range examples {
		weight := example.Weight
		if weight <= 0 {
			weight = 1.0 // Default weight
		}
		totalWeight += weight
	}
	
	if totalWeight == 0 {
		return examples[:count]
	}
	
	// Use turn index as seed for deterministic randomness
	rng := rand.New(rand.NewSource(int64(turnIndex)))
	selected := make([]Example, 0, count)
	usedIndices := make(map[int]bool)
	
	for len(selected) < count && len(selected) < len(examples) {
		target := rng.Float64() * totalWeight
		current := 0.0
		
		for i, example := range examples {
			if usedIndices[i] {
				continue
			}
			
			weight := example.Weight
			if weight <= 0 {
				weight = 1.0
			}
			current += weight
			
			if current >= target {
				selected = append(selected, example)
				usedIndices[i] = true
				break
			}
		}
	}
	
	return selected
}

