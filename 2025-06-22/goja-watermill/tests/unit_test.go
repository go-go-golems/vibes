package tests

import (
	"context"
	"fmt"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/example/goja-watermill/pkg/pubsub"
	watermillpkg "github.com/example/goja-watermill/pkg/watermill"
	"github.com/rs/zerolog"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)


// TestMemoryPubSubUnit tests the memory pub/sub component in isolation
func TestMemoryPubSubUnit(t *testing.T) {
	logger := zerolog.New(zerolog.NewTestWriter(t)).With().Timestamp().Logger()
	
	config := pubsub.DefaultMemoryPubSubConfig()
	config.EnableMetrics = true
	
	memPubSub, err := pubsub.NewMemoryPubSub(config, logger)
	require.NoError(t, err)
	defer memPubSub.Close()
	
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	
	// Test subscription
	msgChan, err := memPubSub.Subscribe(ctx, "test.topic")
	require.NoError(t, err)
	
	// Test publishing
	msg := &message.Message{
		UUID:     "test-uuid",
		Payload:  []byte("test payload"),
		Metadata: map[string]string{"test": "value"},
	}
	
	err = memPubSub.Publish("test.topic", msg)
	require.NoError(t, err)
	
	// Test receiving
	select {
	case receivedMsg := <-msgChan:
		assert.Equal(t, msg.UUID, receivedMsg.UUID)
		assert.Equal(t, msg.Payload, receivedMsg.Payload)
		assert.Equal(t, msg.Metadata["test"], receivedMsg.Metadata["test"])
	case <-time.After(1 * time.Second):
		t.Fatal("Message not received within timeout")
	}
	
	// Test metrics
	metrics := memPubSub.GetMetrics()
	assert.Equal(t, int64(1), metrics.MessagesPublished)
	assert.Equal(t, int64(1), metrics.MessagesConsumed)
	assert.Equal(t, int64(1), metrics.TopicCounts["test.topic"])
}

// TestMemoryPubSubMetrics tests metrics collection
func TestMemoryPubSubMetrics(t *testing.T) {
	logger := zerolog.New(zerolog.NewTestWriter(t)).With().Timestamp().Logger()
	
	config := pubsub.DefaultMemoryPubSubConfig()
	config.EnableMetrics = true
	
	memPubSub, err := pubsub.NewMemoryPubSub(config, logger)
	require.NoError(t, err)
	defer memPubSub.Close()
	
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	
	// Subscribe to multiple topics
	_, err = memPubSub.Subscribe(ctx, "topic1")
	require.NoError(t, err)
	
	_, err = memPubSub.Subscribe(ctx, "topic2")
	require.NoError(t, err)
	
	// Publish multiple messages
	for i := 0; i < 5; i++ {
		msg := &message.Message{
			UUID:    fmt.Sprintf("msg-%d", i),
			Payload: []byte(fmt.Sprintf("payload %d", i)),
		}
		err = memPubSub.Publish("topic1", msg)
		require.NoError(t, err)
	}
	
	for i := 0; i < 3; i++ {
		msg := &message.Message{
			UUID:    fmt.Sprintf("msg2-%d", i),
			Payload: []byte(fmt.Sprintf("payload2 %d", i)),
		}
		err = memPubSub.Publish("topic2", msg)
		require.NoError(t, err)
	}
	
	// Wait for processing
	time.Sleep(100 * time.Millisecond)
	
	// Check metrics
	metrics := memPubSub.GetMetrics()
	assert.Equal(t, int64(8), metrics.MessagesPublished)
	assert.Equal(t, int64(5), metrics.TopicCounts["topic1"])
	assert.Equal(t, int64(3), metrics.TopicCounts["topic2"])
	assert.Equal(t, int64(2), metrics.ActiveSubscribers)
}

// TestMessageTracker tests message tracking functionality
func TestMessageTracker(t *testing.T) {
	logger := zerolog.New(zerolog.NewTestWriter(t)).With().Timestamp().Logger()
	
	config := pubsub.DefaultMemoryPubSubConfig()
	memPubSub, err := pubsub.NewMemoryPubSub(config, logger)
	require.NoError(t, err)
	defer memPubSub.Close()
	
	// Create a test message
	msg := &message.Message{
		UUID:     "tracked-message",
		Payload:  []byte("tracked payload"),
		Metadata: map[string]string{"source": "test"},
	}
	
	// Publish message
	err = memPubSub.Publish("tracked.topic", msg)
	require.NoError(t, err)
	
	// Check message info
	info, exists := memPubSub.GetMessageInfo("tracked-message")
	require.True(t, exists)
	assert.Equal(t, "tracked-message", info.UUID)
	assert.Equal(t, "tracked.topic", info.Topic)
	assert.Equal(t, pubsub.MessageStatusPublished, info.Status)
	assert.Equal(t, "test", info.Metadata["source"])
	
	// Get all messages
	allMessages := memPubSub.GetAllMessages()
	assert.Contains(t, allMessages, "tracked-message")
}

// TestLogConfiguration tests logging configuration
func TestLogConfiguration(t *testing.T) {
	config := watermillpkg.LogConfig{
		Level:      watermillpkg.LogLevelDebug,
		Pretty:     false,
		TimeFormat: time.RFC3339,
		Output:     "stdout",
	}
	
	logger, err := watermillpkg.ConfigureLogger(config)
	require.NoError(t, err)
	
	// Test that logger is configured
	logger.Info().Msg("Test log message")
	
	// Test invalid log level
	invalidConfig := config
	invalidConfig.Level = "invalid"
	
	_, err = watermillpkg.ConfigureLogger(invalidConfig)
	assert.Error(t, err)
}

// TestLogAnalyzer tests log analysis functionality
func TestLogAnalyzer(t *testing.T) {
	logger := zerolog.New(zerolog.NewTestWriter(t)).With().Timestamp().Logger()
	analyzer := watermillpkg.NewLogAnalyzer(logger)
	
	// Add test events
	events := []watermillpkg.LogEvent{
		{
			Timestamp:   time.Now(),
			Level:       "info",
			Component:   "test",
			Message:     "Test message 1",
			MessageUUID: "msg-1",
			Topic:       "topic1",
		},
		{
			Timestamp:   time.Now(),
			Level:       "error",
			Component:   "test",
			Message:     "Test error",
			MessageUUID: "msg-2",
			Topic:       "topic1",
		},
		{
			Timestamp:   time.Now(),
			Level:       "info",
			Component:   "other",
			Message:     "Other message",
			MessageUUID: "msg-1",
			Topic:       "topic2",
		},
	}
	
	for _, event := range events {
		analyzer.AddEvent(event)
	}
	
	// Test filtering
	infoEvents := analyzer.GetEventsByLevel("info")
	assert.Len(t, infoEvents, 2)
	
	testEvents := analyzer.GetEventsByComponent("test")
	assert.Len(t, testEvents, 2)
	
	topic1Events := analyzer.GetEventsByTopic("topic1")
	assert.Len(t, topic1Events, 2)
	
	// Test message flow
	msg1Flow := analyzer.GetMessageFlow("msg-1")
	assert.Len(t, msg1Flow, 2)
	
	// Test statistics
	stats := analyzer.GetStatistics()
	assert.Equal(t, 3, stats["total_events"])
	
	levelCounts := stats["by_level"].(map[string]int)
	assert.Equal(t, 2, levelCounts["info"])
	assert.Equal(t, 1, levelCounts["error"])
}

// TestWatermillZerologAdapter tests the Watermill logger adapter
func TestWatermillZerologAdapter(t *testing.T) {
	logger := zerolog.New(zerolog.NewTestWriter(t)).With().Timestamp().Logger()
	adapter := watermillpkg.NewWatermillZerologAdapter(logger)
	
	// Test different log levels
	adapter.Info("Info message", watermill.LogFields{"key": "value"})
	adapter.Debug("Debug message", watermill.LogFields{"debug": true})
	adapter.Error("Error message", fmt.Errorf("test error"), watermill.LogFields{"error": true})
	adapter.Trace("Trace message", watermill.LogFields{"trace": "data"})
	
	// Test With method
	childAdapter := adapter.With(watermill.LogFields{"child": "logger"})
	childAdapter.Info("Child message", watermill.LogFields{"additional": "field"})
}

// TestConcurrentAccess tests concurrent access to pub/sub
func TestConcurrentAccess(t *testing.T) {
	logger := zerolog.New(zerolog.NewTestWriter(t)).With().Timestamp().Logger()
	
	config := pubsub.DefaultMemoryPubSubConfig()
	memPubSub, err := pubsub.NewMemoryPubSub(config, logger)
	require.NoError(t, err)
	defer memPubSub.Close()
	
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	
	// Subscribe
	msgChan, err := memPubSub.Subscribe(ctx, "concurrent.topic")
	require.NoError(t, err)
	
	const numGoroutines = 10
	const messagesPerGoroutine = 10
	
	var wg sync.WaitGroup
	receivedCount := int64(0)
	
	// Start receiver
	go func() {
		for {
			select {
			case <-msgChan:
				atomic.AddInt64(&receivedCount, 1)
			case <-ctx.Done():
				return
			}
		}
	}()
	
	// Start publishers
	for i := 0; i < numGoroutines; i++ {
		wg.Add(1)
		go func(goroutineID int) {
			defer wg.Done()
			
			for j := 0; j < messagesPerGoroutine; j++ {
				msg := &message.Message{
					UUID:    fmt.Sprintf("msg-%d-%d", goroutineID, j),
					Payload: []byte(fmt.Sprintf("payload from goroutine %d, message %d", goroutineID, j)),
				}
				
				err := memPubSub.Publish("concurrent.topic", msg)
				if err != nil {
					t.Errorf("Failed to publish message: %v", err)
				}
			}
		}(i)
	}
	
	wg.Wait()
	
	// Wait for all messages to be processed
	time.Sleep(500 * time.Millisecond)
	
	expectedCount := int64(numGoroutines * messagesPerGoroutine)
	actualCount := atomic.LoadInt64(&receivedCount)
	
	assert.Equal(t, expectedCount, actualCount, "Should receive all published messages")
	
	// Check metrics
	metrics := memPubSub.GetMetrics()
	assert.Equal(t, expectedCount, metrics.MessagesPublished)
	assert.Equal(t, expectedCount, metrics.MessagesConsumed)
}

// TestPubSubClose tests proper cleanup when closing pub/sub
func TestPubSubClose(t *testing.T) {
	logger := zerolog.New(zerolog.NewTestWriter(t)).With().Timestamp().Logger()
	
	config := pubsub.DefaultMemoryPubSubConfig()
	memPubSub, err := pubsub.NewMemoryPubSub(config, logger)
	require.NoError(t, err)
	
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	
	// Subscribe
	msgChan, err := memPubSub.Subscribe(ctx, "close.topic")
	require.NoError(t, err)
	
	// Publish a message
	msg := &message.Message{
		UUID:    "close-test",
		Payload: []byte("test"),
	}
	err = memPubSub.Publish("close.topic", msg)
	require.NoError(t, err)
	
	// Receive message
	select {
	case <-msgChan:
		// Message received
	case <-time.After(1 * time.Second):
		t.Fatal("Message not received")
	}
	
	// Close pub/sub
	err = memPubSub.Close()
	require.NoError(t, err)
	
	// Try to publish after close (should fail)
	err = memPubSub.Publish("close.topic", msg)
	assert.Error(t, err)
	
	// Message channel should be closed
	select {
	case _, ok := <-msgChan:
		assert.False(t, ok, "Channel should be closed")
	case <-time.After(1 * time.Second):
		t.Fatal("Channel should be closed")
	}
}

