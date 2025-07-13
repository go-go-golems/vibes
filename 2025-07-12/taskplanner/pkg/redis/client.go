package redis

import (
	"context"
	"encoding/json"
	"fmt"
	"time"

	"github.com/pkg/errors"
	"github.com/redis/go-redis/v9"
	"github.com/taskplanner/taskplanner/pkg/models"
)

// Client wraps redis.Client with taskplanner-specific functionality
type Client struct {
	*redis.Client
	prefix string
}

// NewClient creates a new Redis client for taskplanner
func NewClient(redisURL string) (*Client, error) {
	opts, err := redis.ParseURL(redisURL)
	if err != nil {
		return nil, errors.Wrap(err, "failed to parse redis URL")
	}

	client := redis.NewClient(opts)

	// Test connection
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	_, err = client.Ping(ctx).Result()
	if err != nil {
		return nil, errors.Wrap(err, "failed to connect to redis")
	}

	return &Client{
		Client: client,
		prefix: "taskplanner:",
	}, nil
}

// Key returns a prefixed key for taskplanner
func (c *Client) Key(parts ...string) string {
	key := c.prefix
	for _, part := range parts {
		key += part
	}
	return key
}

// Task-related keys
func (c *Client) TaskStatusKey(taskID string) string {
	return c.Key("task:", taskID, ":status")
}

func (c *Client) TaskAssignedKey(taskID string) string {
	return c.Key("task:", taskID, ":assigned")
}

func (c *Client) TaskClaimedKey(taskID string) string {
	return c.Key("task:", taskID, ":claimed")
}

// Plan-related keys
func (c *Client) PlanUpdatesKey(planID string) string {
	return c.Key("plan:", planID, ":updates")
}

func (c *Client) PlanTasksKey(planID string) string {
	return c.Key("plan:", planID, ":tasks")
}

// Agent-related keys
func (c *Client) AgentTasksKey(agentID string) string {
	return c.Key("agent:", agentID, ":tasks")
}

func (c *Client) AgentStatusKey(agentID string) string {
	return c.Key("agent:", agentID, ":status")
}

func (c *Client) AgentHeartbeatKey(agentID string) string {
	return c.Key("agent:", agentID, ":heartbeat")
}

// Coordination keys
func (c *Client) CoordinationStreamKey() string {
	return c.Key("coordination:stream")
}

func (c *Client) NotificationsStreamKey() string {
	return c.Key("notifications:stream")
}

func (c *Client) LastReadKey(agentID string) string {
	return c.Key("last_read:", agentID)
}

// Data structures for Redis operations

// TaskUpdate represents a task status update
type TaskUpdate struct {
	TaskID    string    `json:"task_id"`
	PlanID    string    `json:"plan_id"`
	AgentID   string    `json:"agent_id"`
	Action    string    `json:"action"`
	OldStatus string    `json:"old_status,omitempty"`
	NewStatus string    `json:"new_status,omitempty"`
	Message   string    `json:"message"`
	Timestamp time.Time `json:"timestamp"`
}

// PlanUpdate represents a plan update
type PlanUpdate struct {
	PlanID    string    `json:"plan_id"`
	AgentID   string    `json:"agent_id"`
	Action    string    `json:"action"`
	Message   string    `json:"message"`
	Timestamp time.Time `json:"timestamp"`
}

// AgentStatus represents an agent's current status
type AgentStatus struct {
	AgentID      string    `json:"agent_id"`
	Status       string    `json:"status"`
	Capabilities []string  `json:"capabilities"`
	LastSeen     time.Time `json:"last_seen"`
}

// CoordinationMessage represents a coordination message
type CoordinationMessage struct {
	ID        string    `json:"id"`
	AgentID   string    `json:"agent_id"`
	Type      string    `json:"type"` // task_update, plan_update, agent_status, notification
	Data      string    `json:"data"` // JSON-encoded specific data
	Timestamp time.Time `json:"timestamp"`
}

// Task coordination operations

// PublishTaskUpdate publishes a task update to the coordination stream
func (c *Client) PublishTaskUpdate(ctx context.Context, agentID, taskID, action, message string) error {
	update := TaskUpdate{
		TaskID:    taskID,
		AgentID:   agentID,
		Action:    action,
		Message:   message,
		Timestamp: time.Now(),
	}

	data, err := json.Marshal(update)
	if err != nil {
		return errors.Wrap(err, "failed to marshal task update")
	}

	coordMsg := CoordinationMessage{
		ID:        fmt.Sprintf("%s-%d", agentID, time.Now().UnixNano()),
		AgentID:   agentID,
		Type:      "task_update",
		Data:      string(data),
		Timestamp: time.Now(),
	}

	return c.publishCoordinationMessage(ctx, coordMsg)
}

// PublishPlanUpdate publishes a plan update to the coordination stream
func (c *Client) PublishPlanUpdate(ctx context.Context, agentID, planID, action, message string) error {
	update := PlanUpdate{
		PlanID:    planID,
		AgentID:   agentID,
		Action:    action,
		Message:   message,
		Timestamp: time.Now(),
	}

	data, err := json.Marshal(update)
	if err != nil {
		return errors.Wrap(err, "failed to marshal plan update")
	}

	coordMsg := CoordinationMessage{
		ID:        fmt.Sprintf("%s-%d", agentID, time.Now().UnixNano()),
		AgentID:   agentID,
		Type:      "plan_update",
		Data:      string(data),
		Timestamp: time.Now(),
	}

	return c.publishCoordinationMessage(ctx, coordMsg)
}

// publishCoordinationMessage publishes a coordination message to the stream
func (c *Client) publishCoordinationMessage(ctx context.Context, msg CoordinationMessage) error {
	streamKey := c.CoordinationStreamKey()
	values := map[string]interface{}{
		"id":        msg.ID,
		"agent_id":  msg.AgentID,
		"type":      msg.Type,
		"data":      msg.Data,
		"timestamp": msg.Timestamp.Format(time.RFC3339),
	}

	_, err := c.XAdd(ctx, &redis.XAddArgs{
		Stream: streamKey,
		Values: values,
	}).Result()

	return errors.Wrap(err, "failed to publish coordination message")
}

// Task claiming and assignment

// ClaimTask attempts to claim a task for an agent
func (c *Client) ClaimTask(ctx context.Context, taskID, agentID string, ttl time.Duration) (bool, error) {
	claimKey := c.TaskClaimedKey(taskID)
	claimValue := fmt.Sprintf("%s@%s", agentID, time.Now().Format(time.RFC3339))

	// Use SET with NX (only if not exists) and EX (expiration)
	result, err := c.SetNX(ctx, claimKey, claimValue, ttl).Result()
	if err != nil {
		return false, errors.Wrap(err, "failed to claim task")
	}

	if result {
		// Successfully claimed, publish update
		err = c.PublishTaskUpdate(ctx, agentID, taskID, "claimed", fmt.Sprintf("Task claimed by agent %s", agentID))
		if err != nil {
			// Log error but don't fail the claim
			// In a real implementation, you might want to use a logger here
		}
	}

	return result, nil
}

// ReleaseTask releases a task claim
func (c *Client) ReleaseTask(ctx context.Context, taskID, agentID string) error {
	claimKey := c.TaskClaimedKey(taskID)
	
	// Use a Lua script to ensure atomic check and delete
	script := `
		local current = redis.call('GET', KEYS[1])
		if current and string.find(current, ARGV[1]) then
			redis.call('DEL', KEYS[1])
			return 1
		else
			return 0
		end
	`
	
	result, err := c.Eval(ctx, script, []string{claimKey}, agentID).Result()
	if err != nil {
		return errors.Wrap(err, "failed to release task")
	}

	if result.(int64) == 1 {
		// Successfully released, publish update
		err = c.PublishTaskUpdate(ctx, agentID, taskID, "released", fmt.Sprintf("Task released by agent %s", agentID))
		if err != nil {
			// Log error but don't fail the release
		}
	}

	return nil
}

// GetTaskClaim gets the current claim on a task
func (c *Client) GetTaskClaim(ctx context.Context, taskID string) (string, error) {
	claimKey := c.TaskClaimedKey(taskID)
	claim, err := c.Get(ctx, claimKey).Result()
	if err == redis.Nil {
		return "", nil // No claim
	}
	if err != nil {
		return "", errors.Wrap(err, "failed to get task claim")
	}
	return claim, nil
}

// Agent status management

// UpdateAgentStatus updates an agent's status in Redis
func (c *Client) UpdateAgentStatus(ctx context.Context, agentID, status string, capabilities []string) error {
	statusKey := c.AgentStatusKey(agentID)
	heartbeatKey := c.AgentHeartbeatKey(agentID)

	agentStatus := AgentStatus{
		AgentID:      agentID,
		Status:       status,
		Capabilities: capabilities,
		LastSeen:     time.Now(),
	}

	statusData, err := json.Marshal(agentStatus)
	if err != nil {
		return errors.Wrap(err, "failed to marshal agent status")
	}

	// Use a pipeline for atomic operations
	pipe := c.Pipeline()
	pipe.Set(ctx, statusKey, statusData, time.Hour) // Status expires after 1 hour
	pipe.Set(ctx, heartbeatKey, time.Now().Unix(), time.Minute*5) // Heartbeat expires after 5 minutes
	
	_, err = pipe.Exec(ctx)
	if err != nil {
		return errors.Wrap(err, "failed to update agent status")
	}

	// Publish agent status update
	coordMsg := CoordinationMessage{
		ID:        fmt.Sprintf("%s-status-%d", agentID, time.Now().UnixNano()),
		AgentID:   agentID,
		Type:      "agent_status",
		Data:      string(statusData),
		Timestamp: time.Now(),
	}

	return c.publishCoordinationMessage(ctx, coordMsg)
}

// GetAgentStatus gets an agent's current status
func (c *Client) GetAgentStatus(ctx context.Context, agentID string) (*AgentStatus, error) {
	statusKey := c.AgentStatusKey(agentID)
	statusData, err := c.Get(ctx, statusKey).Result()
	if err == redis.Nil {
		return nil, nil // Agent not found
	}
	if err != nil {
		return nil, errors.Wrap(err, "failed to get agent status")
	}

	var status AgentStatus
	err = json.Unmarshal([]byte(statusData), &status)
	if err != nil {
		return nil, errors.Wrap(err, "failed to unmarshal agent status")
	}

	return &status, nil
}

// ListOnlineAgents lists all currently online agents
func (c *Client) ListOnlineAgents(ctx context.Context) ([]*AgentStatus, error) {
	pattern := c.Key("agent:*:status")
	keys, err := c.Keys(ctx, pattern).Result()
	if err != nil {
		return nil, errors.Wrap(err, "failed to get agent status keys")
	}

	var agents []*AgentStatus
	for _, key := range keys {
		statusData, err := c.Get(ctx, key).Result()
		if err != nil {
			continue // Skip failed reads
		}

		var status AgentStatus
		err = json.Unmarshal([]byte(statusData), &status)
		if err != nil {
			continue // Skip invalid data
		}

		agents = append(agents, &status)
	}

	return agents, nil
}

// Monitoring and streaming

// ReadCoordinationStream reads messages from the coordination stream
func (c *Client) ReadCoordinationStream(ctx context.Context, agentID string, count int64) ([]CoordinationMessage, error) {
	streamKey := c.CoordinationStreamKey()
	lastReadKey := c.LastReadKey(agentID)

	// Get the last read position
	lastID, err := c.Get(ctx, lastReadKey).Result()
	if err == redis.Nil {
		lastID = "0" // Start from beginning if no last read position
	} else if err != nil {
		return nil, errors.Wrap(err, "failed to get last read position")
	}

	// Read from stream
	streams, err := c.XRead(ctx, &redis.XReadArgs{
		Streams: []string{streamKey, lastID},
		Count:   count,
		Block:   0, // Non-blocking
	}).Result()

	if err != nil {
		return nil, errors.Wrap(err, "failed to read coordination stream")
	}

	var messages []CoordinationMessage
	if len(streams) > 0 && len(streams[0].Messages) > 0 {
		for _, msg := range streams[0].Messages {
			coordMsg := CoordinationMessage{
				ID: msg.Values["id"].(string),
				AgentID: msg.Values["agent_id"].(string),
				Type: msg.Values["type"].(string),
				Data: msg.Values["data"].(string),
			}

			if ts, ok := msg.Values["timestamp"].(string); ok {
				if parsed, err := time.Parse(time.RFC3339, ts); err == nil {
					coordMsg.Timestamp = parsed
				}
			}

			messages = append(messages, coordMsg)
		}

		// Update last read position
		lastMessage := streams[0].Messages[len(streams[0].Messages)-1]
		err = c.Set(ctx, lastReadKey, lastMessage.ID, time.Hour*24).Err()
		if err != nil {
			// Log error but don't fail the read
		}
	}

	return messages, nil
}

// SubscribeToCoordinationStream subscribes to real-time coordination updates
func (c *Client) SubscribeToCoordinationStream(ctx context.Context, agentID string) (<-chan CoordinationMessage, error) {
	streamKey := c.CoordinationStreamKey()
	lastReadKey := c.LastReadKey(agentID)
	
	msgChan := make(chan CoordinationMessage, 100)

	go func() {
		defer close(msgChan)
		
		for {
			select {
			case <-ctx.Done():
				return
			default:
				// Get the last read position
				lastID, err := c.Get(ctx, lastReadKey).Result()
				if err == redis.Nil {
					lastID = "$" // Start from latest if no last read position
				} else if err != nil {
					continue
				}

				// Read from stream with blocking
				streams, err := c.XRead(ctx, &redis.XReadArgs{
					Streams: []string{streamKey, lastID},
					Count:   10,
					Block:   time.Second * 5, // Block for 5 seconds
				}).Result()

				if err != nil {
					if err != redis.Nil {
						// Log error in real implementation
					}
					continue
				}

				if len(streams) > 0 && len(streams[0].Messages) > 0 {
					for _, msg := range streams[0].Messages {
						coordMsg := CoordinationMessage{
							ID: msg.Values["id"].(string),
							AgentID: msg.Values["agent_id"].(string),
							Type: msg.Values["type"].(string),
							Data: msg.Values["data"].(string),
						}

						if ts, ok := msg.Values["timestamp"].(string); ok {
							if parsed, err := time.Parse(time.RFC3339, ts); err == nil {
								coordMsg.Timestamp = parsed
							}
						}

						select {
						case msgChan <- coordMsg:
						case <-ctx.Done():
							return
						}
					}

					// Update last read position
					lastMessage := streams[0].Messages[len(streams[0].Messages)-1]
					c.Set(ctx, lastReadKey, lastMessage.ID, time.Hour*24)
				}
			}
		}
	}()

	return msgChan, nil
}

// Cleanup operations

// CleanupExpiredClaims removes expired task claims
func (c *Client) CleanupExpiredClaims(ctx context.Context) error {
	pattern := c.Key("task:*:claimed")
	keys, err := c.Keys(ctx, pattern).Result()
	if err != nil {
		return errors.Wrap(err, "failed to get claim keys")
	}

	// Check each claim for expiration
	for _, key := range keys {
		ttl, err := c.TTL(ctx, key).Result()
		if err != nil {
			continue
		}
		
		// If TTL is -1, the key exists but has no expiration (shouldn't happen)
		// If TTL is -2, the key doesn't exist
		if ttl == -1 {
			// Set a default expiration
			c.Expire(ctx, key, time.Hour)
		}
	}

	return nil
}

// TrimCoordinationStream trims old messages from the coordination stream
func (c *Client) TrimCoordinationStream(ctx context.Context, maxLen int64) error {
	streamKey := c.CoordinationStreamKey()
	_, err := c.XTrimMaxLen(ctx, streamKey, maxLen).Result()
	return errors.Wrap(err, "failed to trim coordination stream")
}


// ReleaseTaskClaim releases a task claim if it belongs to the specified agent
func (c *Client) ReleaseTaskClaim(ctx context.Context, taskID, agentID string) (bool, error) {
	key := c.Key("task:", taskID, ":claimed")
	
	// Use Lua script to atomically check and delete if the claim belongs to this agent
	script := `
		local key = KEYS[1]
		local agent_id = ARGV[1]
		local current_claim = redis.call('GET', key)
		
		if current_claim then
			-- Extract agent ID from claim (format: "agent@timestamp")
			local claim_agent = string.match(current_claim, "^([^@]+)@")
			if claim_agent == agent_id then
				redis.call('DEL', key)
				return 1
			end
		end
		return 0
	`
	
	result, err := c.Eval(ctx, script, []string{key}, agentID).Result()
	if err != nil {
		return false, errors.Wrap(err, "failed to release task claim")
	}
	
	return result.(int64) == 1, nil
}

// SubscribeToCoordinationEvents subscribes to real-time coordination events
func (c *Client) SubscribeToCoordinationEvents(ctx context.Context) (<-chan models.CoordinationEvent, error) {
	streamKey := c.Key("coordination:stream")
	eventChan := make(chan models.CoordinationEvent, 100)
	
	go func() {
		defer close(eventChan)
		
		// Start reading from the latest events
		lastID := "$"
		
		for {
			select {
			case <-ctx.Done():
				return
			default:
				// Read new events from stream
				streams, err := c.XRead(ctx, &redis.XReadArgs{
					Streams: []string{streamKey, lastID},
					Block:   time.Second * 5,
					Count:   10,
				}).Result()
				
				if err != nil {
					if err != redis.Nil {
						// Log error but continue
						time.Sleep(time.Second)
					}
					continue
				}
				
				for _, stream := range streams {
					for _, message := range stream.Messages {
						event := models.CoordinationEvent{}
						
						// Parse event from stream message
						if id, ok := message.Values["id"].(string); ok {
							event.ID = id
						}
						if agentID, ok := message.Values["agent_id"].(string); ok {
							event.AgentID = agentID
						}
						if eventType, ok := message.Values["type"].(string); ok {
							event.Type = eventType
						}
						if data, ok := message.Values["data"].(string); ok {
							event.Data = data
						}
						if timestamp, ok := message.Values["timestamp"].(string); ok {
							if t, err := time.Parse(time.RFC3339, timestamp); err == nil {
								event.Timestamp = t
							}
						}
						
						select {
						case eventChan <- event:
						case <-ctx.Done():
							return
						}
						
						lastID = message.ID
					}
				}
			}
		}
	}()
	
	return eventChan, nil
}

// Ping tests the Redis connection
func (c *Client) Ping(ctx context.Context) error {
	_, err := c.Client.Ping(ctx).Result()
	return err
}

// GetActiveClaims returns all active task claims
func (c *Client) GetActiveClaims(ctx context.Context) (map[string]string, error) {
	pattern := c.Key("task:*:claimed")
	keys, err := c.Keys(ctx, pattern).Result()
	if err != nil {
		return nil, errors.Wrap(err, "failed to get claim keys")
	}
	
	claims := make(map[string]string)
	for _, key := range keys {
		// Extract task ID from key
		taskID := ""
		if len(key) > len(c.prefix+"task:") {
			parts := key[len(c.prefix+"task:"):]
			if idx := len(parts) - len(":claimed"); idx > 0 {
				taskID = parts[:idx]
			}
		}
		
		claimInfo, err := c.Get(ctx, key).Result()
		if err != nil {
			continue // Skip expired or deleted claims
		}
		
		claims[taskID] = claimInfo
	}
	
	return claims, nil
}

// GetCoordinationEventCount returns the total number of coordination events
func (c *Client) GetCoordinationEventCount(ctx context.Context) (int64, error) {
	streamKey := c.Key("coordination:stream")
	length, err := c.XLen(ctx, streamKey).Result()
	if err != nil {
		return 0, errors.Wrap(err, "failed to get event count")
	}
	return length, nil
}


// PublishCoordinationEvent publishes a coordination event to the Redis stream
func (c *Client) PublishCoordinationEvent(ctx context.Context, event models.CoordinationEvent) error {
	streamKey := c.Key("coordination:stream")
	
	// Add event to stream
	_, err := c.XAdd(ctx, &redis.XAddArgs{
		Stream: streamKey,
		Values: map[string]interface{}{
			"id":        event.ID,
			"agent_id":  event.AgentID,
			"type":      event.Type,
			"data":      event.Data,
			"timestamp": event.Timestamp.Format(time.RFC3339),
		},
	}).Result()
	
	if err != nil {
		return errors.Wrap(err, "failed to publish coordination event")
	}
	
	return nil
}

