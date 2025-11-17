package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"os/signal"
	"time"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/ThreeDotsLabs/watermill/message"

	"github.com/ThreeDotsLabs/watermill-redisstream/pkg/redisstream"
	"github.com/redis/go-redis/v9"

	sc "example.com/single-conn-redis-watermill/internal/singleconnredis"
)

func main() {
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt)
	defer stop()

	// ---- Redis client (single node) ----
	rdb := redis.NewClient(&redis.Options{
		Addr: "127.0.0.1:6379",
	})

	// ---- Single-connection Subscriber ----
	unm := redisstream.DefaultMarshallerUnmarshaller{}
	sub, err := sc.NewSubscriber(sc.SubscriberConfig{
		Client:        rdb,
		ConsumerGroup: "orders-cg",
		Consumer:      "demo-consumer-1",
		BlockTime:     400 * time.Millisecond,
		Unmarshaler:   unm,
	})
	if err != nil {
		log.Fatalf("subscriber: %v", err)
	}
	defer sub.Close()

	// ---- Publisher (uses watermill-redisstream) ----
	// Create a simple logger
	wlog := watermill.NewStdLogger(false, false)
	pub, err := redisstream.NewPublisher(redisstream.PublisherConfig{
		Client: rdb, // uses pooled client internally
	}, wlog)
	if err != nil {
		log.Fatalf("publisher: %v", err)
	}
	defer pub.Close()

	// Subscribe to topics
	createdCh, err := sub.Subscribe(ctx, "orders.created")
	if err != nil {
		log.Fatalf("subscribe to orders.created: %v", err)
	}

	cancelledCh, err := sub.Subscribe(ctx, "orders.cancelled")
	if err != nil {
		log.Fatalf("subscribe to orders.cancelled: %v", err)
	}

	// Start handlers
	go handleMessages("orders-created-handler", createdCh)
	go handleMessages("orders-cancelled-handler", cancelledCh)

	// Give subscribers time to start
	time.Sleep(500 * time.Millisecond)

	// Publish some demo messages
	go func() {
		tick := time.NewTicker(700 * time.Millisecond)
		defer tick.Stop()

		for i := 1; i <= 6; i++ {
			<-tick.C
			send(pub, "orders.created", fmt.Sprintf(`{"order_id": %d, "status":"created"}`, i))
			send(pub, "orders.cancelled", fmt.Sprintf(`{"order_id": %d, "status":"cancelled"}`, i))
		}
	}()

	// Run for ~6 seconds
	select {
	case <-time.After(6 * time.Second):
		fmt.Println("\n=== Demo completed successfully ===")
	case <-ctx.Done():
		fmt.Println("\n=== Demo interrupted ===")
	}
}

func handleMessages(name string, messages <-chan *message.Message) {
	for msg := range messages {
		fmt.Printf("[HANDLER %s] got: %s | payload=%s\n", name, msg.UUID, string(msg.Payload))
		msg.Ack()
	}
}

func send(pub message.Publisher, topic, json string) {
	msg := message.NewMessage(watermill.NewUUID(), []byte(json))
	if err := pub.Publish(topic, msg); err != nil {
		log.Printf("publish err: %v", err)
	} else {
		fmt.Printf("[PUBLISHER] sent to %s: %s\n", topic, json)
	}
}
