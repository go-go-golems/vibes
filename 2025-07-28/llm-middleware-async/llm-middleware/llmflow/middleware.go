package llmflow

import (
	"context"
	"crypto/md5"
	"encoding/json"
	"fmt"
	"log"
	"strings"
	"time"
)

// Logging returns a middleware that writes a log line
// "[prefix] ⏵ START turn X" before calling next,
// and "[prefix] ⏴ DONE turn X (err=nil|err=…)" afterwards.
func Logging(prefix string) InputMiddleware {
	return func(next InputHandler) InputHandler {
		return func(ctx context.Context, turn *Turn) error {
			log.Printf("[%s] ⏵ START turn %d", prefix, turn.Index)
			start := time.Now()
			
			err := next(ctx, turn)
			
			duration := time.Since(start)
			if err != nil {
				log.Printf("[%s] ⏴ DONE turn %d (err=%v) [%v]", prefix, turn.Index, err, duration)
			} else {
				log.Printf("[%s] ⏴ DONE turn %d (err=nil) [%v]", prefix, turn.Index, duration)
			}
			
			return err
		}
	}
}

// Retry returns a middleware that, upon error, will
// re-invoke next() up to maxAttempts times, sleeping
// backoff between each try.
func Retry(maxAttempts int, backoff time.Duration) InputMiddleware {
	return func(next InputHandler) InputHandler {
		return func(ctx context.Context, turn *Turn) error {
			var lastErr error
			
			for attempt := 1; attempt <= maxAttempts; attempt++ {
				err := next(ctx, turn)
				if err == nil {
					if attempt > 1 {
						log.Printf("Retry succeeded on attempt %d", attempt)
					}
					return nil
				}
				
				lastErr = err
				log.Printf("Attempt %d failed: %v", attempt, err)
				
				if attempt < maxAttempts {
					log.Printf("Retrying in %v...", backoff)
					time.Sleep(backoff)
				}
			}
			
			return fmt.Errorf("all %d attempts failed, last error: %w", maxAttempts, lastErr)
		}
	}
}

// CacheStore is the minimal interface for any cache backend.
type CacheStore interface {
	// Get returns value+true if present.
	Get(key string) (val string, ok bool)
	// Set stores the value for key.
	Set(key, val string)
}

// InMemoryCache is a simple in-memory implementation of CacheStore
type InMemoryCache struct {
	data map[string]string
}

// NewInMemoryCache creates a new in-memory cache
func NewInMemoryCache() *InMemoryCache {
	return &InMemoryCache{
		data: make(map[string]string),
	}
}

// Get retrieves a value from the cache
func (c *InMemoryCache) Get(key string) (string, bool) {
	val, ok := c.data[key]
	return val, ok
}

// Set stores a value in the cache
func (c *InMemoryCache) Set(key, val string) {
	c.data[key] = val
}

// Cache returns a middleware that checks the cache
// *before* calling next. On hit, it populates turn.Output["raw"]
// and skips the rest of the chain.
func Cache(store CacheStore) InputMiddleware {
	return func(next InputHandler) InputHandler {
		return func(ctx context.Context, turn *Turn) error {
			// Generate cache key from messages
			key := generateCacheKey(turn.Messages)
			
			// Check cache first
			if cached, ok := store.Get(key); ok {
				log.Printf("Cache hit for key: %s", key[:8]+"...")
				turn.Output["raw"] = cached
				turn.Output["cache_hit"] = true
				return nil
			}
			
			// Cache miss, call next
			log.Printf("Cache miss for key: %s", key[:8]+"...")
			err := next(ctx, turn)
			
			// Store result in cache if successful
			if err == nil {
				if raw, ok := turn.Output["raw"].(string); ok {
					store.Set(key, raw)
					turn.Output["cache_hit"] = false
				}
			}
			
			return err
		}
	}
}

// generateCacheKey creates a hash key from the messages
func generateCacheKey(messages []Message) string {
	// Create a deterministic string representation of messages
	var parts []string
	for _, msg := range messages {
		parts = append(parts, fmt.Sprintf("%s:%s", msg.Role, msg.Content))
	}
	content := strings.Join(parts, "|")
	
	// Generate MD5 hash
	hash := md5.Sum([]byte(content))
	return fmt.Sprintf("%x", hash)
}

// Schema enforces that the LLM reply conforms to `schema` (e.g. a JSON spec).
// It injects the spec text at n−2 and, on output, tries to unmarshal raw → v.
// Successful parses are stored under turn.Output[outputKey].
func Schema(schema string, outputKey string) InputMiddleware {
	return func(next InputHandler) InputHandler {
		return func(ctx context.Context, turn *Turn) error {
			// Inject schema prompt before the last user message
			if len(turn.Messages) > 0 {
				// Find the last user message and inject schema before it
				for i := len(turn.Messages) - 1; i >= 0; i-- {
					if turn.Messages[i].Role == "user" {
						// Insert schema instruction before this user message
						schemaMsg := Message{
							Role:    "system",
							Content: fmt.Sprintf("Return JSON matching this schema: %s", schema),
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
			
			// Try to parse the raw output as JSON
			if raw, ok := turn.Output["raw"].(string); ok {
				var parsed interface{}
				if jsonErr := json.Unmarshal([]byte(raw), &parsed); jsonErr == nil {
					turn.Output[outputKey] = parsed
					turn.Output["schema_valid"] = true
				} else {
					turn.Output["schema_valid"] = false
					turn.Output["schema_error"] = jsonErr.Error()
				}
			}
			
			return nil
		}
	}
}

// ModeController holds state + exposes SetMode().
type ModeController interface {
	// SetMode switches the active mode for future turns.
	SetMode(name string)
	// GetMode returns the current mode
	GetMode() string
}

// modeController implements ModeController
type modeController struct {
	currentMode string
	prompts     map[string]string
}

// SetMode switches the active mode for future turns
func (mc *modeController) SetMode(name string) {
	if _, exists := mc.prompts[name]; exists {
		mc.currentMode = name
		log.Printf("Mode switched to: %s", name)
	} else {
		log.Printf("Warning: Unknown mode '%s', keeping current mode '%s'", name, mc.currentMode)
	}
}

// GetMode returns the current mode
func (mc *modeController) GetMode() string {
	return mc.currentMode
}

// ThinkingMode returns both:
//   - a ModeController you can call SetMode on,
//   - the corresponding middleware to inject mode prompts.
func ThinkingMode(defaultMode string, prompts map[string]string) (ModeController, InputMiddleware) {
	controller := &modeController{
		currentMode: defaultMode,
		prompts:     prompts,
	}
	
	middleware := func(next InputHandler) InputHandler {
		return func(ctx context.Context, turn *Turn) error {
			// On turn 0, emit a banner listing available modes
			if turn.Index == 0 {
				var modeList []string
				for mode := range prompts {
					modeList = append(modeList, mode)
				}
				banner := fmt.Sprintf("Available modes: %s. Current mode: %s", 
					strings.Join(modeList, ", "), controller.currentMode)
				
				bannerMsg := Message{
					Role:    "system",
					Content: banner,
				}
				turn.Messages = append([]Message{bannerMsg}, turn.Messages...)
			}
			
			// Inject current mode's guidance before the last user message
			if modePrompt, exists := prompts[controller.currentMode]; exists {
				modeMsg := Message{
					Role:    "system",
					Content: fmt.Sprintf("Mode: %s - %s", controller.currentMode, modePrompt),
				}
				
				// Insert before the last user message
				if len(turn.Messages) > 0 {
					for i := len(turn.Messages) - 1; i >= 0; i-- {
						if turn.Messages[i].Role == "user" {
							turn.Messages = append(turn.Messages[:i], append([]Message{modeMsg}, turn.Messages[i:]...)...)
							break
						}
					}
				}
			}
			
			// Store current mode in turn output
			turn.Output["current_mode"] = controller.currentMode
			
			return next(ctx, turn)
		}
	}
	
	return controller, middleware
}

