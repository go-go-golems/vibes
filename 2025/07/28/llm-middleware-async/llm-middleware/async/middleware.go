package async

import (
	"context"
	"crypto/md5"
	"encoding/json"
	"fmt"
	"log"
	"strings"
	"time"
)

// AsyncLogging returns async logging middleware with detailed metrics
func AsyncLogging(prefix string) AsyncMiddleware {
	return func(next AsyncHandler) AsyncHandler {
		return func(ctx context.Context, turn *Turn) <-chan AsyncResult {
			resultChan := make(chan AsyncResult, 1)
			
			go func() {
				defer close(resultChan)
				
				log.Printf("[%s] ⏵ START turn %d (ID: %s)", prefix, turn.Index, turn.ID)
				start := time.Now()
				
				// Execute next handler
				nextResultChan := next(ctx, turn)
				
				select {
				case result := <-nextResultChan:
					duration := time.Since(start)
					
					// Add timing metrics to turn
					if turn.Output.Metrics == nil {
						turn.Output.Metrics = &Metrics{}
					}
					if turn.Output.Metrics.MiddlewareTimings == nil {
						turn.Output.Metrics.MiddlewareTimings = make(map[string]time.Duration)
					}
					turn.Output.Metrics.MiddlewareTimings[prefix] = duration
					
					if result.Error != nil {
						log.Printf("[%s] ⏴ DONE turn %d (err=%v) [%v]", prefix, turn.Index, result.Error, duration)
					} else {
						log.Printf("[%s] ⏴ DONE turn %d (err=nil) [%v]", prefix, turn.Index, duration)
					}
					
					resultChan <- result
					
				case <-ctx.Done():
					duration := time.Since(start)
					err := ctx.Err()
					log.Printf("[%s] ⏴ TIMEOUT turn %d (err=%v) [%v]", prefix, turn.Index, err, duration)
					turn.Fail(err, prefix)
					resultChan <- AsyncResult{Turn: turn, Error: err}
				}
			}()
			
			return resultChan
		}
	}
}

// AsyncRetry returns async retry middleware with exponential backoff
func AsyncRetry(maxAttempts int, backoff time.Duration) AsyncMiddleware {
	return func(next AsyncHandler) AsyncHandler {
		return func(ctx context.Context, turn *Turn) <-chan AsyncResult {
			resultChan := make(chan AsyncResult, 1)
			
			go func() {
				defer close(resultChan)
				
				var lastResult AsyncResult
				
				for attempt := 1; attempt <= maxAttempts; attempt++ {
					// Create attempt-specific context with timeout
					attemptCtx, cancel := context.WithCancel(ctx)
					
					// Execute next handler
					nextResultChan := next(attemptCtx, turn)
					
					select {
					case result := <-nextResultChan:
						cancel()
						
						if result.Error == nil {
							if attempt > 1 {
								log.Printf("Retry succeeded on attempt %d for turn %d", attempt, turn.Index)
								// Add retry metrics
								turn.Context.SetVariable("retry_attempts", attempt)
								turn.Context.SetVariable("retry_succeeded", true)
							}
							resultChan <- result
							return
						}
						
						lastResult = result
						log.Printf("Attempt %d failed for turn %d: %v", attempt, turn.Index, result.Error)
						
						if attempt < maxAttempts {
							log.Printf("Retrying in %v...", backoff)
							select {
							case <-time.After(backoff):
								// Continue to next attempt
								backoff *= 2 // Exponential backoff
							case <-ctx.Done():
								cancel()
								err := ctx.Err()
								turn.Fail(err, "retry_timeout")
								resultChan <- AsyncResult{Turn: turn, Error: err}
								return
							}
						}
						
					case <-ctx.Done():
						cancel()
						err := ctx.Err()
						turn.Fail(err, "retry_context_cancelled")
						resultChan <- AsyncResult{Turn: turn, Error: err}
						return
					}
				}
				
				// All attempts failed
				turn.Context.SetVariable("retry_attempts", maxAttempts)
				turn.Context.SetVariable("retry_succeeded", false)
				err := fmt.Errorf("all %d attempts failed, last error: %w", maxAttempts, lastResult.Error)
				turn.Fail(err, "retry_exhausted")
				resultChan <- AsyncResult{Turn: turn, Error: err}
			}()
			
			return resultChan
		}
	}
}

// AsyncCacheStore interface for async cache operations
type AsyncCacheStore interface {
	GetAsync(ctx context.Context, key string) <-chan CacheResult
	SetAsync(ctx context.Context, key, value string) <-chan error
}

// CacheResult represents the result of a cache operation
type CacheResult struct {
	Value string
	Found bool
	Error error
}

// InMemoryAsyncCache implements AsyncCacheStore
type InMemoryAsyncCache struct {
	data map[string]CacheEntry
}

// CacheEntry represents a cached item with metadata
type CacheEntry struct {
	Value     string
	CreatedAt time.Time
	TTL       time.Duration
}

// NewInMemoryAsyncCache creates a new async in-memory cache
func NewInMemoryAsyncCache() *InMemoryAsyncCache {
	return &InMemoryAsyncCache{
		data: make(map[string]CacheEntry),
	}
}

// GetAsync retrieves a value from the cache asynchronously
func (c *InMemoryAsyncCache) GetAsync(ctx context.Context, key string) <-chan CacheResult {
	resultChan := make(chan CacheResult, 1)
	
	go func() {
		defer close(resultChan)
		
		// Simulate async operation
		select {
		case <-time.After(1 * time.Millisecond):
			entry, exists := c.data[key]
			if !exists {
				resultChan <- CacheResult{Found: false}
				return
			}
			
			// Check TTL if set
			if entry.TTL > 0 && time.Since(entry.CreatedAt) > entry.TTL {
				delete(c.data, key)
				resultChan <- CacheResult{Found: false}
				return
			}
			
			resultChan <- CacheResult{Value: entry.Value, Found: true}
			
		case <-ctx.Done():
			resultChan <- CacheResult{Error: ctx.Err()}
		}
	}()
	
	return resultChan
}

// SetAsync stores a value in the cache asynchronously
func (c *InMemoryAsyncCache) SetAsync(ctx context.Context, key, value string) <-chan error {
	resultChan := make(chan error, 1)
	
	go func() {
		defer close(resultChan)
		
		// Simulate async operation
		select {
		case <-time.After(1 * time.Millisecond):
			c.data[key] = CacheEntry{
				Value:     value,
				CreatedAt: time.Now(),
			}
			resultChan <- nil
			
		case <-ctx.Done():
			resultChan <- ctx.Err()
		}
	}()
	
	return resultChan
}

// AsyncCache returns async caching middleware
func AsyncCache(store AsyncCacheStore) AsyncMiddleware {
	return func(next AsyncHandler) AsyncHandler {
		return func(ctx context.Context, turn *Turn) <-chan AsyncResult {
			resultChan := make(chan AsyncResult, 1)
			
			go func() {
				defer close(resultChan)
				
				// Generate cache key
				key := generateAsyncCacheKey(turn.Messages)
				
				// Check cache
				cacheResultChan := store.GetAsync(ctx, key)
				
				select {
				case cacheResult := <-cacheResultChan:
					if cacheResult.Error != nil {
						log.Printf("Cache error for turn %d: %v", turn.Index, cacheResult.Error)
						// Continue without cache
					} else if cacheResult.Found {
						log.Printf("Cache hit for turn %d, key: %s", turn.Index, key[:8]+"...")
						turn.Output.Raw = cacheResult.Value
						turn.Context.SetFlag("cache_hit", true)
						
						// Create cache hit artifact
						cacheArtifact := &Artifact{
							ID:        "cache_result",
							Type:      ArtifactTypeText,
							Version:   1,
							Data:      cacheResult.Value,
							CreatedAt: time.Now(),
							UpdatedAt: time.Now(),
						}
						turn.Output.Artifacts["cache_result"] = cacheArtifact
						
						turn.Complete()
						resultChan <- AsyncResult{Turn: turn, Error: nil}
						return
					}
					
					// Cache miss, continue to next handler
					log.Printf("Cache miss for turn %d, key: %s", turn.Index, key[:8]+"...")
					turn.Context.SetFlag("cache_hit", false)
					
					nextResultChan := next(ctx, turn)
					
					select {
					case result := <-nextResultChan:
						if result.Error == nil && turn.Output.Raw != "" {
							// Store in cache asynchronously (fire and forget)
							go func() {
								cacheCtx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
								defer cancel()
								
								setChan := store.SetAsync(cacheCtx, key, turn.Output.Raw)
								select {
								case err := <-setChan:
									if err != nil {
										log.Printf("Failed to cache result for turn %d: %v", turn.Index, err)
									}
								case <-cacheCtx.Done():
									log.Printf("Cache set timeout for turn %d", turn.Index)
								}
							}()
						}
						
						resultChan <- result
						
					case <-ctx.Done():
						err := ctx.Err()
						turn.Fail(err, "cache_context_cancelled")
						resultChan <- AsyncResult{Turn: turn, Error: err}
					}
					
				case <-ctx.Done():
					err := ctx.Err()
					turn.Fail(err, "cache_lookup_timeout")
					resultChan <- AsyncResult{Turn: turn, Error: err}
				}
			}()
			
			return resultChan
		}
	}
}

// AsyncSchema returns async schema enforcement middleware
func AsyncSchema(schemaText, outputKey string) AsyncMiddleware {
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
					go func() {
						parseCtx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
						defer cancel()
						
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
							
						case err := <-errorChan:
							turn.Context.AddWarning("SCHEMA_PARSE_ERROR", err.Error(), "schema_middleware")
							turn.Context.SetFlag("schema_valid", false)
							
						case <-parseCtx.Done():
							turn.Context.AddWarning("SCHEMA_PARSE_TIMEOUT", "JSON parsing timed out", "schema_middleware")
							turn.Context.SetFlag("schema_valid", false)
						}
					}()
					
					// Store schema metadata
					turn.Context.SetVariable("schema_text", schemaText)
					turn.Context.SetVariable("schema_output_key", outputKey)
					
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

// generateAsyncCacheKey creates a cache key from messages
func generateAsyncCacheKey(messages []Message) string {
	var parts []string
	for _, msg := range messages {
		parts = append(parts, fmt.Sprintf("%s:%s", msg.Role, msg.Content))
	}
	content := strings.Join(parts, "|")
	
	hash := md5.Sum([]byte(content))
	return fmt.Sprintf("%x", hash)
}

