package singleconnredis

import (
	"context"
	"errors"
	"sync"
	"time"

	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/redis/go-redis/v9"
)

// Unmarshaler decodes Redis Stream fields into a Watermill message.
type Unmarshaler interface {
	Unmarshal(values map[string]interface{}) (*message.Message, error)
}

// SubscriberConfig controls the single-connection subscriber.
type SubscriberConfig struct {
	Client        *redis.Client // single-node client; for Cluster see README constraints
	ConsumerGroup string        // required
	Consumer      string        // required
	BlockTime     time.Duration // e.g., 500 * time.Millisecond (must be > 0)
	GroupStartID  string        // default "$"
	Unmarshaler   Unmarshaler   // required (e.g., redisstream.DefaultMarshallerUnmarshaller{})
}

// ackReq is queued so ACKs go over the same single connection.
type ackReq struct {
	stream string
	id     string
}

// Subscriber implements watermill's message.Subscriber over a single Redis connection.
type Subscriber struct {
	cfg   SubscriberConfig
	conn  *redis.Conn // the ONE TCP connection
	once  sync.Once   // start read loop once

	mu   sync.Mutex
	subs map[string][]chan *message.Message // topic -> channels

	acks   chan ackReq
	closed chan struct{}

	notify chan struct{} // optional "new topic" signal (loop still relies on BlockTime)
}

// NewSubscriber initializes the subscriber; it does NOT start the loop until first Subscribe.
func NewSubscriber(cfg SubscriberConfig) (*Subscriber, error) {
	if cfg.Client == nil {
		return nil, errors.New("Client is required")
	}
	if cfg.ConsumerGroup == "" || cfg.Consumer == "" {
		return nil, errors.New("ConsumerGroup and Consumer are required")
	}
	if cfg.Unmarshaler == nil {
		return nil, errors.New("Unmarshaler is required")
	}
	if cfg.BlockTime <= 0 {
		cfg.BlockTime = 500 * time.Millisecond
	}
	if cfg.GroupStartID == "" {
		cfg.GroupStartID = "$"
	}

	return &Subscriber{
		cfg:    cfg,
		conn:   cfg.Client.Conn(), // SINGLE dedicated connection
		subs:   make(map[string][]chan *message.Message),
		acks:   make(chan ackReq, 4096),
		closed: make(chan struct{}),
		notify: make(chan struct{}, 1),
	}, nil
}

// Subscribe adds a topic and returns a channel of messages for it.
// The single read loop will pick up new topics on its next Block timeout.
func (s *Subscriber) Subscribe(ctx context.Context, topic string) (<-chan *message.Message, error) {
	// Ensure group exists; ignore BUSYGROUP errors.
	_ = s.conn.XGroupCreateMkStream(ctx, topic, s.cfg.ConsumerGroup, s.cfg.GroupStartID).Err()

	out := make(chan *message.Message, 64)

	s.mu.Lock()
	s.subs[topic] = append(s.subs[topic], out)
	s.mu.Unlock()

	// Start the read loop once.
	s.once.Do(func() {
		go s.readLoop()
	})

	// Best-effort notify; loop still relies on BlockTime to rebuild list.
	select { case s.notify <- struct{}{}: default: }

	// Cleanup when caller cancels.
	go func() {
		select {
		case <-ctx.Done():
		case <-s.closed:
			return
		}
		s.mu.Lock()
		chs := s.subs[topic]
		for i := range chs {
			if chs[i] == out {
				chs[i] = chs[len(chs)-1]
				chs = chs[:len(chs)-1]
				break
			}
		}
		if len(chs) == 0 {
			delete(s.subs, topic)
		} else {
			s.subs[topic] = chs
		}
		s.mu.Unlock()
		close(out)
	}()

	return out, nil
}

func (s *Subscriber) readLoop() {
	ctx := context.Background()
	block := s.cfg.BlockTime

	for {
		select {
		case <-s.closed:
			return
		default:
		}

		// Snapshot topics
		s.mu.Lock()
		topics := make([]string, 0, len(s.subs))
		for t := range s.subs {
			topics = append(topics, t)
		}
		s.mu.Unlock()

		if len(topics) == 0 {
			// No topics yet; wait for either notify or small sleep
			select {
			case <-s.closed:
				return
			case <-s.notify:
			case <-time.After(50 * time.Millisecond):
			}
			continue
		}

		// Drain queued ACKs before issuing a blocking read.
	DrainAcks:
		for {
			select {
			case a := <-s.acks:
				_ = s.conn.XAck(ctx, a.stream, s.cfg.ConsumerGroup, a.id).Err()
			default:
				break DrainAcks
			}
		}

		// Build XREADGROUP arguments: [t1 t2 ...] + [">" ...]
		streams := make([]string, 0, len(topics)*2)
		streams = append(streams, topics...)
		for range topics {
			streams = append(streams, ">")
		}

		res, err := s.conn.XReadGroup(ctx, &redis.XReadGroupArgs{
			Group:    s.cfg.ConsumerGroup,
			Consumer: s.cfg.Consumer,
			Streams:  streams,
			Block:    block,
			Count:    0, // unlimited per call
			NoAck:    false,
		}).Result()

		// Timeout is redis.Nil; other errors: brief backoff.
		if err != nil {
			if errors.Is(err, redis.Nil) {
				// woke up due to Block timeout; loop will rebuild topics and continue
				continue
			}
			time.Sleep(100 * time.Millisecond)
			continue
		}

		// Fan-out to topic subscribers and wire ACK behavior.
		for _, xs := range res { // xs.Stream, xs.Messages
			stream := xs.Stream
			for _, xm := range xs.Messages {
				msg, uerr := s.cfg.Unmarshaler.Unmarshal(xm.Values)
				if uerr != nil {
					// bad payload; skip
					continue
				}

				// Observe Ack/Nack and queue XACK via the same connection.
				go func(m *message.Message, stream, id string) {
					select {
					case <-m.Acked():
						select {
						case s.acks <- ackReq{stream: stream, id: id}:
						case <-s.closed:
						}
					case <-m.Nacked():
						// leave in PEL for redelivery/claim outside
					case <-s.closed:
					}
				}(msg, stream, xm.ID)

				// Deliver to all subscribers for this stream
				s.mu.Lock()
				chs := append([]chan *message.Message(nil), s.subs[stream]...)
				s.mu.Unlock()
				for _, ch := range chs {
					ch <- msg
				}
			}
		}
	}
}

// Close stops the loop, closes the single connection, and closes all subscriber channels.
func (s *Subscriber) Close() error {
	select {
	case <-s.closed:
		// already closed
	default:
		close(s.closed)
	}
	_ = s.conn.Close()

	s.mu.Lock()
	for _, chs := range s.subs {
		for _, ch := range chs {
			close(ch)
		}
	}
	s.subs = map[string][]chan *message.Message{}
	s.mu.Unlock()
	return nil
}
