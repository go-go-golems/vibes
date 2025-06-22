package pubsub

import (
	"context"
	"fmt"
	"sync"
	"time"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/ThreeDotsLabs/watermill/pubsub/gochannel"
	"github.com/rs/zerolog"
)

// MemoryPubSubConfig holds configuration for memory-based pub/sub
type MemoryPubSubConfig struct {
	// OutputChannelBuffer is the buffer size for output channels
	OutputChannelBuffer int64 `json:"output_channel_buffer"`
	
	// Persistent determines if messages should be buffered until a subscriber is attached
	Persistent bool `json:"persistent"`
	
	// BlockPublishUntilSubscriberAck determines if publishing should block until subscriber acks
	BlockPublishUntilSubscriberAck bool `json:"block_publish_until_subscriber_ack"`
	
	// MaxRetries is the maximum number of retries for failed messages
	MaxRetries int `json:"max_retries"`
	
	// RetryDelay is the delay between retries
	RetryDelay time.Duration `json:"retry_delay"`
	
	// EnableMetrics determines if metrics collection is enabled
	EnableMetrics bool `json:"enable_metrics"`
}

// DefaultMemoryPubSubConfig returns default configuration
func DefaultMemoryPubSubConfig() MemoryPubSubConfig {
	return MemoryPubSubConfig{
		OutputChannelBuffer:            64,
		Persistent:                     false,
		BlockPublishUntilSubscriberAck: false,
		MaxRetries:                     3,
		RetryDelay:                     100 * time.Millisecond,
		EnableMetrics:                  true,
	}
}

// MemoryPubSub wraps GoChannel with enhanced features
type MemoryPubSub struct {
	gochannel *gochannel.GoChannel
	config    MemoryPubSubConfig
	logger    zerolog.Logger
	metrics   *PubSubMetrics
	
	// Message tracking
	messageTracker *MessageTracker
	
	// Lifecycle
	ctx    context.Context
	cancel context.CancelFunc
	closed bool
	mutex  sync.RWMutex
}

// PubSubMetrics holds metrics for pub/sub operations
type PubSubMetrics struct {
	MessagesPublished   int64         `json:"messages_published"`
	MessagesConsumed    int64         `json:"messages_consumed"`
	MessagesFailed      int64         `json:"messages_failed"`
	MessagesRetried     int64         `json:"messages_retried"`
	AverageProcessTime  time.Duration `json:"average_process_time"`
	TotalProcessTime    time.Duration `json:"total_process_time"`
	ActiveSubscribers   int64         `json:"active_subscribers"`
	TopicCounts         map[string]int64 `json:"topic_counts"`
	mutex               sync.RWMutex
}

// MessageTracker tracks message lifecycle
type MessageTracker struct {
	messages map[string]*MessageInfo
	mutex    sync.RWMutex
	logger   zerolog.Logger
}

// MessageInfo holds information about a message
type MessageInfo struct {
	UUID        string            `json:"uuid"`
	Topic       string            `json:"topic"`
	PublishedAt time.Time         `json:"published_at"`
	ConsumedAt  *time.Time        `json:"consumed_at,omitempty"`
	CompletedAt *time.Time        `json:"completed_at,omitempty"`
	FailedAt    *time.Time        `json:"failed_at,omitempty"`
	RetryCount  int               `json:"retry_count"`
	Status      MessageStatus     `json:"status"`
	Metadata    map[string]string `json:"metadata"`
	Error       string            `json:"error,omitempty"`
}

// MessageStatus represents the status of a message
type MessageStatus string

const (
	MessageStatusPublished MessageStatus = "published"
	MessageStatusConsumed  MessageStatus = "consumed"
	MessageStatusCompleted MessageStatus = "completed"
	MessageStatusFailed    MessageStatus = "failed"
	MessageStatusRetrying  MessageStatus = "retrying"
)

// NewMemoryPubSub creates a new enhanced memory pub/sub
func NewMemoryPubSub(config MemoryPubSubConfig, logger zerolog.Logger) (*MemoryPubSub, error) {
	ctx, cancel := context.WithCancel(context.Background())
	
	// Create Watermill logger adapter
	watermillLogger := &WatermillZerologAdapter{logger: logger.With().Str("component", "watermill").Logger()}
	
	// Create GoChannel with configuration
	goChanConfig := gochannel.Config{
		OutputChannelBuffer:            config.OutputChannelBuffer,
		Persistent:                     config.Persistent,
		BlockPublishUntilSubscriberAck: config.BlockPublishUntilSubscriberAck,
	}
	
	goChan := gochannel.NewGoChannel(goChanConfig, watermillLogger)
	
	// Initialize metrics
	metrics := &PubSubMetrics{
		TopicCounts: make(map[string]int64),
	}
	
	// Initialize message tracker
	messageTracker := &MessageTracker{
		messages: make(map[string]*MessageInfo),
		logger:   logger.With().Str("component", "message_tracker").Logger(),
	}
	
	pubsub := &MemoryPubSub{
		gochannel:      goChan,
		config:         config,
		logger:         logger.With().Str("component", "memory_pubsub").Logger(),
		metrics:        metrics,
		messageTracker: messageTracker,
		ctx:            ctx,
		cancel:         cancel,
	}
	
	logger.Info().
		Interface("config", config).
		Msg("Created enhanced memory pub/sub")
	
	return pubsub, nil
}

// WatermillZerologAdapter adapts zerolog to Watermill's logger interface
type WatermillZerologAdapter struct {
	logger zerolog.Logger
}

// Error logs an error message
func (w *WatermillZerologAdapter) Error(msg string, err error, fields watermill.LogFields) {
	event := w.logger.Error().Err(err)
	for key, value := range fields {
		event = event.Interface(key, value)
	}
	event.Msg(msg)
}

// Info logs an info message
func (w *WatermillZerologAdapter) Info(msg string, fields watermill.LogFields) {
	event := w.logger.Info()
	for key, value := range fields {
		event = event.Interface(key, value)
	}
	event.Msg(msg)
}

// Debug logs a debug message
func (w *WatermillZerologAdapter) Debug(msg string, fields watermill.LogFields) {
	event := w.logger.Debug()
	for key, value := range fields {
		event = event.Interface(key, value)
	}
	event.Msg(msg)
}

// Trace logs a trace message
func (w *WatermillZerologAdapter) Trace(msg string, fields watermill.LogFields) {
	event := w.logger.Trace()
	for key, value := range fields {
		event = event.Interface(key, value)
	}
	event.Msg(msg)
}

// With returns a logger with additional fields
func (w *WatermillZerologAdapter) With(fields watermill.LogFields) watermill.LoggerAdapter {
	ctx := w.logger.With()
	for key, value := range fields {
		ctx = ctx.Interface(key, value)
	}
	return &WatermillZerologAdapter{logger: ctx.Logger()}
}

// Publish publishes a message to a topic
func (mp *MemoryPubSub) Publish(topic string, messages ...*message.Message) error {
	mp.mutex.RLock()
	if mp.closed {
		mp.mutex.RUnlock()
		return fmt.Errorf("pub/sub is closed")
	}
	mp.mutex.RUnlock()
	
	for _, msg := range messages {
		// Track message
		mp.messageTracker.TrackMessage(msg, topic, MessageStatusPublished)
		
		// Update metrics
		mp.updateMetrics(func(m *PubSubMetrics) {
			m.MessagesPublished++
			m.TopicCounts[topic]++
		})
		
		mp.logger.Debug().
			Str("message_uuid", msg.UUID).
			Str("topic", topic).
			Int("payload_size", len(msg.Payload)).
			Msg("Publishing message to memory pub/sub")
	}
	
	// Publish to underlying GoChannel
	err := mp.gochannel.Publish(topic, messages...)
	if err != nil {
		// Mark messages as failed
		for _, msg := range messages {
			mp.messageTracker.MarkMessageFailed(msg.UUID, err.Error())
		}
		
		mp.updateMetrics(func(m *PubSubMetrics) {
			m.MessagesFailed += int64(len(messages))
		})
		
		mp.logger.Error().
			Err(err).
			Str("topic", topic).
			Int("message_count", len(messages)).
			Msg("Failed to publish messages")
		
		return err
	}
	
	mp.logger.Info().
		Str("topic", topic).
		Int("message_count", len(messages)).
		Msg("Successfully published messages to memory pub/sub")
	
	return nil
}

// Subscribe subscribes to a topic
func (mp *MemoryPubSub) Subscribe(ctx context.Context, topic string) (<-chan *message.Message, error) {
	mp.mutex.RLock()
	if mp.closed {
		mp.mutex.RUnlock()
		return nil, fmt.Errorf("pub/sub is closed")
	}
	mp.mutex.RUnlock()
	
	mp.logger.Info().
		Str("topic", topic).
		Msg("Subscribing to topic in memory pub/sub")
	
	// Update metrics
	mp.updateMetrics(func(m *PubSubMetrics) {
		m.ActiveSubscribers++
	})
	
	// Subscribe to underlying GoChannel
	msgChan, err := mp.gochannel.Subscribe(ctx, topic)
	if err != nil {
		mp.logger.Error().
			Err(err).
			Str("topic", topic).
			Msg("Failed to subscribe to topic")
		return nil, err
	}
	
	// Wrap the channel to add tracking
	wrappedChan := make(chan *message.Message)
	
	go func() {
		defer close(wrappedChan)
		defer func() {
			mp.updateMetrics(func(m *PubSubMetrics) {
				m.ActiveSubscribers--
			})
		}()
		
		for {
			select {
			case msg, ok := <-msgChan:
				if !ok {
					mp.logger.Debug().
						Str("topic", topic).
						Msg("Message channel closed")
					return
				}
				
				// Track message consumption
				mp.messageTracker.MarkMessageConsumed(msg.UUID)
				
				// Update metrics
				mp.updateMetrics(func(m *PubSubMetrics) {
					m.MessagesConsumed++
				})
				
				mp.logger.Debug().
					Str("message_uuid", msg.UUID).
					Str("topic", topic).
					Msg("Received message from memory pub/sub")
				
				// Forward message
				select {
				case wrappedChan <- msg:
				case <-ctx.Done():
					mp.logger.Debug().
						Str("topic", topic).
						Msg("Subscription context cancelled")
					return
				case <-mp.ctx.Done():
					mp.logger.Debug().
						Str("topic", topic).
						Msg("PubSub context cancelled")
					return
				}
				
			case <-ctx.Done():
				mp.logger.Debug().
					Str("topic", topic).
					Msg("Subscription context cancelled")
				return
			case <-mp.ctx.Done():
				mp.logger.Debug().
					Str("topic", topic).
					Msg("PubSub context cancelled")
				return
			}
		}
	}()
	
	return wrappedChan, nil
}

// Close closes the pub/sub
func (mp *MemoryPubSub) Close() error {
	mp.mutex.Lock()
	defer mp.mutex.Unlock()
	
	if mp.closed {
		return nil
	}
	
	mp.logger.Info().Msg("Closing memory pub/sub")
	
	mp.closed = true
	mp.cancel()
	
	err := mp.gochannel.Close()
	if err != nil {
		mp.logger.Error().Err(err).Msg("Error closing underlying GoChannel")
		return err
	}
	
	mp.logger.Info().Msg("Memory pub/sub closed successfully")
	return nil
}

// GetMetrics returns current metrics
func (mp *MemoryPubSub) GetMetrics() PubSubMetrics {
	mp.metrics.mutex.RLock()
	defer mp.metrics.mutex.RUnlock()
	
	// Create a copy to avoid race conditions
	metrics := PubSubMetrics{
		MessagesPublished:  mp.metrics.MessagesPublished,
		MessagesConsumed:   mp.metrics.MessagesConsumed,
		MessagesFailed:     mp.metrics.MessagesFailed,
		MessagesRetried:    mp.metrics.MessagesRetried,
		AverageProcessTime: mp.metrics.AverageProcessTime,
		TotalProcessTime:   mp.metrics.TotalProcessTime,
		ActiveSubscribers:  mp.metrics.ActiveSubscribers,
		TopicCounts:        make(map[string]int64),
	}
	
	for topic, count := range mp.metrics.TopicCounts {
		metrics.TopicCounts[topic] = count
	}
	
	return metrics
}

// GetMessageInfo returns information about a specific message
func (mp *MemoryPubSub) GetMessageInfo(messageUUID string) (*MessageInfo, bool) {
	return mp.messageTracker.GetMessageInfo(messageUUID)
}

// GetAllMessages returns information about all tracked messages
func (mp *MemoryPubSub) GetAllMessages() map[string]*MessageInfo {
	return mp.messageTracker.GetAllMessages()
}

// updateMetrics safely updates metrics
func (mp *MemoryPubSub) updateMetrics(updateFn func(*PubSubMetrics)) {
	if !mp.config.EnableMetrics {
		return
	}
	
	mp.metrics.mutex.Lock()
	defer mp.metrics.mutex.Unlock()
	updateFn(mp.metrics)
}

// TrackMessage tracks a new message
func (mt *MessageTracker) TrackMessage(msg *message.Message, topic string, status MessageStatus) {
	mt.mutex.Lock()
	defer mt.mutex.Unlock()
	
	now := time.Now()
	info := &MessageInfo{
		UUID:        msg.UUID,
		Topic:       topic,
		PublishedAt: now,
		Status:      status,
		Metadata:    make(map[string]string),
	}
	
	// Copy metadata
	for k, v := range msg.Metadata {
		info.Metadata[k] = v
	}
	
	mt.messages[msg.UUID] = info
	
	mt.logger.Trace().
		Str("message_uuid", msg.UUID).
		Str("topic", topic).
		Str("status", string(status)).
		Msg("Tracking message")
}

// MarkMessageConsumed marks a message as consumed
func (mt *MessageTracker) MarkMessageConsumed(messageUUID string) {
	mt.mutex.Lock()
	defer mt.mutex.Unlock()
	
	if info, exists := mt.messages[messageUUID]; exists {
		now := time.Now()
		info.ConsumedAt = &now
		info.Status = MessageStatusConsumed
		
		mt.logger.Trace().
			Str("message_uuid", messageUUID).
			Msg("Marked message as consumed")
	}
}

// MarkMessageCompleted marks a message as completed
func (mt *MessageTracker) MarkMessageCompleted(messageUUID string) {
	mt.mutex.Lock()
	defer mt.mutex.Unlock()
	
	if info, exists := mt.messages[messageUUID]; exists {
		now := time.Now()
		info.CompletedAt = &now
		info.Status = MessageStatusCompleted
		
		mt.logger.Trace().
			Str("message_uuid", messageUUID).
			Msg("Marked message as completed")
	}
}

// MarkMessageFailed marks a message as failed
func (mt *MessageTracker) MarkMessageFailed(messageUUID string, errorMsg string) {
	mt.mutex.Lock()
	defer mt.mutex.Unlock()
	
	if info, exists := mt.messages[messageUUID]; exists {
		now := time.Now()
		info.FailedAt = &now
		info.Status = MessageStatusFailed
		info.Error = errorMsg
		
		mt.logger.Warn().
			Str("message_uuid", messageUUID).
			Str("error", errorMsg).
			Msg("Marked message as failed")
	}
}

// GetMessageInfo returns information about a specific message
func (mt *MessageTracker) GetMessageInfo(messageUUID string) (*MessageInfo, bool) {
	mt.mutex.RLock()
	defer mt.mutex.RUnlock()
	
	info, exists := mt.messages[messageUUID]
	if !exists {
		return nil, false
	}
	
	// Return a copy to avoid race conditions
	infoCopy := *info
	infoCopy.Metadata = make(map[string]string)
	for k, v := range info.Metadata {
		infoCopy.Metadata[k] = v
	}
	
	return &infoCopy, true
}

// GetAllMessages returns information about all tracked messages
func (mt *MessageTracker) GetAllMessages() map[string]*MessageInfo {
	mt.mutex.RLock()
	defer mt.mutex.RUnlock()
	
	result := make(map[string]*MessageInfo)
	for uuid, info := range mt.messages {
		// Create a copy
		infoCopy := *info
		infoCopy.Metadata = make(map[string]string)
		for k, v := range info.Metadata {
			infoCopy.Metadata[k] = v
		}
		result[uuid] = &infoCopy
	}
	
	return result
}

