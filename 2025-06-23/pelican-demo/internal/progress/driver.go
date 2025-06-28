package progress

import (
	"context"
	"fmt"
	"log"
	"os"
	"time"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/ThreeDotsLabs/watermill-redisstream/pkg/redisstream"
	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/ThreeDotsLabs/watermill/pubsub/gochannel"
	"github.com/redis/go-redis/v9"
)

// WatermillSink implements Sink using Watermill publisher
type WatermillSink struct {
	publisher message.Publisher
	logger    watermill.LoggerAdapter
}

// WatermillSource implements Source using Watermill subscriber
type WatermillSource struct {
	subscriber message.Subscriber
	logger     watermill.LoggerAdapter
}

// Global variable to store the shared GoChannel instance
var sharedGoChannel *gochannel.GoChannel

// NewPublisher creates a new Watermill publisher based on environment
func NewPublisher() (message.Publisher, error) {
	logger := watermill.NewStdLogger(false, false)
	
	if os.Getenv("REDIS") == "1" {
		rdb := redis.NewClient(&redis.Options{
			Addr: "localhost:6379",
		})
		
		return redisstream.NewPublisher(
			redisstream.PublisherConfig{
				Client: rdb,
			},
			logger,
		)
	}
	
	// Default to in-memory for development - use shared instance
	if sharedGoChannel == nil {
		sharedGoChannel = gochannel.NewGoChannel(
			gochannel.Config{},
			logger,
		)
	}
	return sharedGoChannel, nil
}

// NewSubscriber creates a new Watermill subscriber based on environment
func NewSubscriber() (message.Subscriber, error) {
	logger := watermill.NewStdLogger(false, false)
	
	if os.Getenv("REDIS") == "1" {
		rdb := redis.NewClient(&redis.Options{
			Addr: "localhost:6379",
		})
		
		return redisstream.NewSubscriber(
			redisstream.SubscriberConfig{
				Client:        rdb,
				ConsumerGroup: "pelican-demo",
			},
			logger,
		)
	}
	
	// Default to in-memory for development - use shared instance
	if sharedGoChannel == nil {
		sharedGoChannel = gochannel.NewGoChannel(
			gochannel.Config{},
			logger,
		)
	}
	return sharedGoChannel, nil
}

// NewSink creates a new progress sink
func NewSink(publisher message.Publisher) *WatermillSink {
	return &WatermillSink{
		publisher: publisher,
		logger:    watermill.NewStdLogger(false, false),
	}
}

// Send publishes a progress event
func (s *WatermillSink) Send(event Event) error {
	data, err := event.ToJSON()
	if err != nil {
		return fmt.Errorf("failed to marshal event: %w", err)
	}
	
	topic := TopicName("") // Use single topic
	
	msg := message.NewMessage(watermill.NewUUID(), data)
	
	return s.publisher.Publish(topic, msg)
}

// NewSource creates a new progress source
func NewSource(subscriber message.Subscriber) *WatermillSource {
	return &WatermillSource{
		subscriber: subscriber,
		logger:     watermill.NewStdLogger(false, false),
	}
}

// Subscribe subscribes to progress events for a specific job
func (s *WatermillSource) Subscribe(jobID string) (<-chan Event, error) {
	topic := TopicName("") // Use single topic
	
	messages, err := s.subscriber.Subscribe(context.Background(), topic)
	if err != nil {
		return nil, fmt.Errorf("failed to subscribe to topic %s: %w", topic, err)
	}
	
	events := make(chan Event, 10)
	
	go func() {
		defer close(events)
		
		for msg := range messages {
			event, err := FromJSON(msg.Payload)
			if err != nil {
				log.Printf("Failed to unmarshal event: %v", err)
				msg.Nack()
				continue
			}
			
			// Filter events by job ID
			if event.JobID != jobID {
				msg.Ack() // Acknowledge but don't forward
				continue
			}
			
			select {
			case events <- event:
				msg.Ack()
			case <-time.After(5 * time.Second):
				log.Printf("Timeout sending event to channel")
				msg.Nack()
			}
		}
	}()
	
	return events, nil
}

// Close closes the subscriber
func (s *WatermillSource) Close() error {
	return s.subscriber.Close()
}

