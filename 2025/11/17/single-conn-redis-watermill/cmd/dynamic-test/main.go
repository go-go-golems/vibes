package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"os/signal"
	"sync"
	"sync/atomic"
	"time"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/ThreeDotsLabs/watermill/message"

	"github.com/ThreeDotsLabs/watermill-redisstream/pkg/redisstream"
	"github.com/redis/go-redis/v9"

	sc "example.com/single-conn-redis-watermill/internal/singleconnredis"
)

const (
	numTopics       = 15
	subscribeDelay  = 300 * time.Millisecond // Delay between subscribing to each topic
	publishInterval = 200 * time.Millisecond // How often to publish messages
	testDuration    = 12 * time.Second        // Total test duration
)

var (
	messagesReceived atomic.Int64
	messagesSent     atomic.Int64
)

func main() {
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt)
	defer stop()

	fmt.Println("=== Dynamic Topic Subscription Test ===")
	fmt.Printf("Topics: %d\n", numTopics)
	fmt.Printf("Subscribe delay: %v\n", subscribeDelay)
	fmt.Printf("Test duration: %v\n\n", testDuration)

	// ---- Redis client (single node) ----
	rdb := redis.NewClient(&redis.Options{
		Addr: "127.0.0.1:6379",
	})

	// Test connection
	if err := rdb.Ping(ctx).Err(); err != nil {
		log.Fatalf("Redis connection failed: %v", err)
	}
	fmt.Println("✓ Connected to Redis")

	// ---- Single-connection Subscriber ----
	unm := redisstream.DefaultMarshallerUnmarshaller{}
	sub, err := sc.NewSubscriber(sc.SubscriberConfig{
		Client:        rdb,
		ConsumerGroup: "dynamic-test-cg",
		Consumer:      "test-consumer-1",
		BlockTime:     300 * time.Millisecond,
		Unmarshaler:   unm,
	})
	if err != nil {
		log.Fatalf("subscriber: %v", err)
	}
	defer sub.Close()
	fmt.Println("✓ Created single-connection subscriber")

	// ---- Publisher ----
	wlog := watermill.NewStdLogger(false, false)
	pub, err := redisstream.NewPublisher(redisstream.PublisherConfig{
		Client: rdb,
	}, wlog)
	if err != nil {
		log.Fatalf("publisher: %v", err)
	}
	defer pub.Close()
	fmt.Println("✓ Created publisher\n")

	// Track active topics
	var topicsMu sync.Mutex
	activeTopics := make(map[string]bool)

	// Subscribe to topics gradually over time
	fmt.Println("--- Starting Dynamic Subscription ---")
	go func() {
		for i := 1; i <= numTopics; i++ {
			topic := fmt.Sprintf("topic.%02d", i)
			
			// Subscribe to the topic
			msgCh, err := sub.Subscribe(ctx, topic)
			if err != nil {
				log.Printf("Error subscribing to %s: %v", topic, err)
				continue
			}

			// Mark topic as active
			topicsMu.Lock()
			activeTopics[topic] = true
			topicsMu.Unlock()

			// Start handler for this topic
			go handleMessages(topic, msgCh)

			fmt.Printf("[%s] Subscribed to %s (total: %d/%d)\n", 
				time.Now().Format("15:04:05.000"), topic, i, numTopics)

			// Wait before subscribing to next topic
			if i < numTopics {
				time.Sleep(subscribeDelay)
			}
		}
		fmt.Println("\n✓ All topics subscribed\n")
	}()

	// Wait a bit for first few subscriptions to be ready
	time.Sleep(500 * time.Millisecond)

	// Start publishing to active topics
	fmt.Println("--- Starting Message Publishing ---")
	publishCtx, cancelPublish := context.WithCancel(ctx)
	defer cancelPublish()

	go func() {
		ticker := time.NewTicker(publishInterval)
		defer ticker.Stop()

		msgNum := 1
		for {
			select {
			case <-publishCtx.Done():
				return
			case <-ticker.C:
				// Get current active topics
				topicsMu.Lock()
				topics := make([]string, 0, len(activeTopics))
				for t := range activeTopics {
					topics = append(topics, t)
				}
				topicsMu.Unlock()

				// Publish to all active topics
				for _, topic := range topics {
					payload := fmt.Sprintf(`{"msg_num": %d, "topic": "%s", "timestamp": "%s"}`, 
						msgNum, topic, time.Now().Format(time.RFC3339Nano))
					
					msg := message.NewMessage(watermill.NewUUID(), []byte(payload))
					if err := pub.Publish(topic, msg); err != nil {
						log.Printf("Publish error to %s: %v", topic, err)
					} else {
						messagesSent.Add(1)
					}
				}
				msgNum++
			}
		}
	}()

	// Monitor and report progress
	go func() {
		ticker := time.NewTicker(2 * time.Second)
		defer ticker.Stop()

		for {
			select {
			case <-ctx.Done():
				return
			case <-ticker.C:
				sent := messagesSent.Load()
				received := messagesReceived.Load()
				topicsMu.Lock()
				numActive := len(activeTopics)
				topicsMu.Unlock()
				
				fmt.Printf("[STATS] Active topics: %d | Sent: %d | Received: %d | Delivery rate: %.1f%%\n",
					numActive, sent, received, float64(received)/float64(sent)*100)
			}
		}
	}()

	// Run for specified duration
	fmt.Printf("\n--- Running test for %v ---\n\n", testDuration)
	select {
	case <-time.After(testDuration):
		fmt.Println("\n=== Test Duration Completed ===")
	case <-ctx.Done():
		fmt.Println("\n=== Test Interrupted ===")
	}

	// Final stats
	time.Sleep(500 * time.Millisecond) // Let final messages process
	sent := messagesSent.Load()
	received := messagesReceived.Load()
	
	fmt.Println("\n=== Final Statistics ===")
	fmt.Printf("Topics subscribed: %d\n", numTopics)
	fmt.Printf("Messages sent: %d\n", sent)
	fmt.Printf("Messages received: %d\n", received)
	fmt.Printf("Delivery rate: %.2f%%\n", float64(received)/float64(sent)*100)
	
	if received == sent {
		fmt.Println("\n✓ SUCCESS: All messages delivered!")
	} else {
		fmt.Printf("\n⚠ WARNING: %d messages not delivered\n", sent-received)
	}
}

func handleMessages(topic string, messages <-chan *message.Message) {
	for msg := range messages {
		messagesReceived.Add(1)
		// Uncomment for detailed message logging:
		// fmt.Printf("[RECV %s] %s\n", topic, string(msg.Payload))
		msg.Ack()
	}
}
