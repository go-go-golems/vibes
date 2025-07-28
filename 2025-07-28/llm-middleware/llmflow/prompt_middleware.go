package llmflow

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

// PromptTemplating returns an InputMiddleware that
// • takes tmpl (a text/template string),
// • looks up varsKey in turn.Context (expects map[string]interface{}),
// • renders and injects the result.
func PromptTemplating(tmpl string, varsKey string) InputMiddleware {
	// Parse template once at middleware creation
	parsedTemplate, err := template.New("prompt").Parse(tmpl)
	if err != nil {
		log.Printf("Error parsing template: %v", err)
		// Return a no-op middleware if template parsing fails
		return func(next InputHandler) InputHandler {
			return func(ctx context.Context, turn *Turn) error {
				if turn.Context["warnings"] == nil {
					turn.Context["warnings"] = []string{}
				}
				warnings := turn.Context["warnings"].([]string)
				turn.Context["warnings"] = append(warnings, fmt.Sprintf("Template parsing failed: %v", err))
				return next(ctx, turn)
			}
		}
	}

	return func(next InputHandler) InputHandler {
		return func(ctx context.Context, turn *Turn) error {
			// Look up variables in turn context
			vars, ok := turn.Context[varsKey]
			if !ok {
				// No variables found, add warning
				if turn.Context["warnings"] == nil {
					turn.Context["warnings"] = []string{}
				}
				warnings := turn.Context["warnings"].([]string)
				turn.Context["warnings"] = append(warnings, fmt.Sprintf("Template variables not found at key: %s", varsKey))
				return next(ctx, turn)
			}

			// Render template with variables
			var buf bytes.Buffer
			if err := parsedTemplate.Execute(&buf, vars); err != nil {
				// Template execution failed, add warning
				if turn.Context["warnings"] == nil {
					turn.Context["warnings"] = []string{}
				}
				warnings := turn.Context["warnings"].([]string)
				turn.Context["warnings"] = append(warnings, fmt.Sprintf("Template execution failed: %v", err))
				return next(ctx, turn)
			}

			renderedPrompt := buf.String()

			// Inject the rendered prompt as a system message before the last user message
			if len(turn.Messages) > 0 {
				// Find the last user message and inject before it
				for i := len(turn.Messages) - 1; i >= 0; i-- {
					if turn.Messages[i].Role == "user" {
						templateMsg := Message{
							Role:    "system",
							Content: renderedPrompt,
						}
						// Insert the template message
						turn.Messages = append(turn.Messages[:i], append([]Message{templateMsg}, turn.Messages[i:]...)...)
						break
					}
				}
			}

			// Record that templating was applied
			turn.Context["template_applied"] = true
			turn.Context["template_content"] = renderedPrompt

			return next(ctx, turn)
		}
	}
}

// ChainOfThoughtInjector returns a middleware that
// • inspects enableFlagKey in turn.Context,
// • if true, appends cotPrompt to the final message,
// • and records cotUsedKey=true afterwards.
func ChainOfThoughtInjector(cotPrompt, enableFlagKey, cotUsedKey string) InputMiddleware {
	return func(next InputHandler) InputHandler {
		return func(ctx context.Context, turn *Turn) error {
			// Check if CoT is enabled
			enableCoT := false
			if flag, ok := turn.Context[enableFlagKey]; ok {
				if enabled, ok := flag.(bool); ok {
					enableCoT = enabled
				}
			}

			if enableCoT && len(turn.Messages) > 0 {
				// Find the last user message and append CoT prompt
				for i := len(turn.Messages) - 1; i >= 0; i-- {
					if turn.Messages[i].Role == "user" {
						// Append CoT prompt to the user message
						turn.Messages[i].Content = turn.Messages[i].Content + "\n\n" + cotPrompt
						break
					}
				}
				// Record that CoT was used
				turn.Context[cotUsedKey] = true
			} else {
				// Record that CoT was not used
				turn.Context[cotUsedKey] = false
			}

			return next(ctx, turn)
		}
	}
}

// Example is a user/assistant pair for few-shot learning.
type Example struct {
	User      string `json:"user"`
	Assistant string `json:"assistant"`
}

// InjectionMode controls selection strategy.
type InjectionMode int

const (
	ModeSequential InjectionMode = iota
	ModeRandom
)

// ExampleInjection returns a middleware that
// • holds examples,
// • chooses `k` per turn via mode,
// • and injects them at position n–1.
func ExampleInjection(examples []Example, k int, mode InjectionMode) InputMiddleware {
	return func(next InputHandler) InputHandler {
		return func(ctx context.Context, turn *Turn) error {
			if len(examples) == 0 || k <= 0 {
				return next(ctx, turn)
			}

			// Select examples based on mode
			var selectedExamples []Example
			switch mode {
			case ModeSequential:
				// Use turn index to cycle through examples
				start := turn.Index % len(examples)
				for i := 0; i < k && i < len(examples); i++ {
					idx := (start + i) % len(examples)
					selectedExamples = append(selectedExamples, examples[idx])
				}
			case ModeRandom:
				// Use random selection (with seed based on turn for reproducibility)
				rng := rand.New(rand.NewSource(int64(turn.Index) + time.Now().UnixNano()))
				indices := rng.Perm(len(examples))
				for i := 0; i < k && i < len(examples); i++ {
					selectedExamples = append(selectedExamples, examples[indices[i]])
				}
			}

			// Inject examples before the last user message
			if len(turn.Messages) > 0 && len(selectedExamples) > 0 {
				// Find the last user message
				for i := len(turn.Messages) - 1; i >= 0; i-- {
					if turn.Messages[i].Role == "user" {
						// Create example messages
						var exampleMessages []Message
						for _, example := range selectedExamples {
							exampleMessages = append(exampleMessages,
								Message{Role: "user", Content: example.User},
								Message{Role: "assistant", Content: example.Assistant},
							)
						}
						// Insert examples before the current user message
						turn.Messages = append(turn.Messages[:i], append(exampleMessages, turn.Messages[i:]...)...)
						break
					}
				}
			}

			// Record example injection details
			turn.Context["examples_injected"] = len(selectedExamples)
			turn.Context["injection_mode"] = mode
			turn.Context["selected_examples"] = selectedExamples

			return next(ctx, turn)
		}
	}
}


// StructuredSchema returns a single middleware that
// • implements both InputRenderer and OutputParser,
// • uses schemaText as the JSON spec,
// • and writes parsed value into turn.Output[outputKey].
func StructuredSchema(schemaText, outputKey string) InputMiddleware {
	return func(next InputHandler) InputHandler {
		return func(ctx context.Context, turn *Turn) error {
			// Inject schema prompt before the last user message
			if len(turn.Messages) > 0 {
				schemaPrompt := fmt.Sprintf("Please respond with valid JSON matching this schema: %s", schemaText)
				
				// Find the last user message and inject schema before it
				for i := len(turn.Messages) - 1; i >= 0; i-- {
					if turn.Messages[i].Role == "user" {
						schemaMsg := Message{
							Role:    "system",
							Content: schemaPrompt,
						}
						// Insert the schema message
						turn.Messages = append(turn.Messages[:i], append([]Message{schemaMsg}, turn.Messages[i:]...)...)
						break
					}
				}
			}

			// Call next in chain
			err := next(ctx, turn)
			if err != nil {
				return err
			}

			// Parse the output if available
			if raw, ok := turn.Output["raw"].(string); ok {
				var parsed interface{}
				if jsonErr := json.Unmarshal([]byte(raw), &parsed); jsonErr == nil {
					turn.Output[outputKey] = parsed
					turn.Context["schema_valid"] = true
				} else {
					turn.Context["schema_error"] = jsonErr.Error()
					turn.Context["schema_valid"] = false
				}
			}

			// Record schema details
			turn.Context["schema_text"] = schemaText
			turn.Context["schema_output_key"] = outputKey

			return nil
		}
	}
}

// PersonaController lets callers switch the active persona.
type PersonaController interface {
	// SetPersona changes the voice for future turns.
	SetPersona(name string) error
	// GetPersona returns the current active persona.
	GetPersona() string
	// ListPersonas returns all available persona names.
	ListPersonas() []string
}

// personaController implements PersonaController
type personaController struct {
	currentPersona string
	prompts        map[string]string
}

// SetPersona changes the voice for future turns
func (pc *personaController) SetPersona(name string) error {
	if _, exists := pc.prompts[name]; !exists {
		return fmt.Errorf("unknown persona: %s", name)
	}
	pc.currentPersona = name
	log.Printf("Persona switched to: %s", name)
	return nil
}

// GetPersona returns the current active persona
func (pc *personaController) GetPersona() string {
	return pc.currentPersona
}

// ListPersonas returns all available persona names
func (pc *personaController) ListPersonas() []string {
	var personas []string
	for name := range pc.prompts {
		personas = append(personas, name)
	}
	return personas
}

// PersonaSwitch returns both a controller and its middleware.
// • defaultName is the starting persona,
// • prompts maps persona→system message.
func PersonaSwitch(defaultName string, prompts map[string]string) (PersonaController, InputMiddleware) {
	controller := &personaController{
		currentPersona: defaultName,
		prompts:        prompts,
	}

	middleware := func(next InputHandler) InputHandler {
		return func(ctx context.Context, turn *Turn) error {
			// Check for persona switch commands in user messages
			if len(turn.Messages) > 0 {
				for i := len(turn.Messages) - 1; i >= 0; i-- {
					if turn.Messages[i].Role == "user" {
						content := strings.TrimSpace(turn.Messages[i].Content)
						
						// Check for persona switch command (e.g., "/persona expert")
						if strings.HasPrefix(content, "/persona ") {
							personaName := strings.TrimSpace(strings.TrimPrefix(content, "/persona "))
							if err := controller.SetPersona(personaName); err != nil {
								// Invalid persona, add warning but continue
								if turn.Context["warnings"] == nil {
									turn.Context["warnings"] = []string{}
								}
								warnings := turn.Context["warnings"].([]string)
								turn.Context["warnings"] = append(warnings, fmt.Sprintf("Invalid persona: %s", personaName))
							} else {
								// Valid persona switch, remove this message from going to LLM
								turn.Messages = append(turn.Messages[:i], turn.Messages[i+1:]...)
								turn.Context["persona_switched"] = true
								turn.Context["new_persona"] = personaName
							}
							break
						}
					}
				}
			}

			// On turn 0, emit a banner listing available personas
			if turn.Index == 0 {
				var personaList []string
				for persona := range prompts {
					personaList = append(personaList, persona)
				}
				banner := fmt.Sprintf("Available personas: %s. Current persona: %s. Use '/persona <name>' to switch.",
					strings.Join(personaList, ", "), controller.currentPersona)

				bannerMsg := Message{
					Role:    "system",
					Content: banner,
				}
				turn.Messages = append([]Message{bannerMsg}, turn.Messages...)
			}

			// Inject current persona's prompt before the last user message
			if personaPrompt, exists := prompts[controller.currentPersona]; exists && len(turn.Messages) > 0 {
				// Find the last user message and inject persona prompt before it
				for i := len(turn.Messages) - 1; i >= 0; i-- {
					if turn.Messages[i].Role == "user" {
						personaMsg := Message{
							Role:    "system",
							Content: fmt.Sprintf("Persona: %s - %s", controller.currentPersona, personaPrompt),
						}
						// Insert the persona message
						turn.Messages = append(turn.Messages[:i], append([]Message{personaMsg}, turn.Messages[i:]...)...)
						break
					}
				}
			}

			// Record current persona in turn context
			turn.Context["current_persona"] = controller.currentPersona

			return next(ctx, turn)
		}
	}

	return controller, middleware
}

