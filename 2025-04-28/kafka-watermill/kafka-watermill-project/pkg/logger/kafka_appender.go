package logger

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"strings"
	"sync"

	"github.com/IBM/sarama"
)

// KafkaAppender sends logs to a Kafka topic
type KafkaAppender struct {
	producer   sarama.SyncProducer
			topic      string
	brokers    []string
	bufferSize int
	logChan    chan []byte
	wg         sync.WaitGroup
	done       chan struct{}
}

// NewKafkaAppender creates a new Kafka appender
func NewKafkaAppender(brokers []string, topic string, bufferSize int) (*KafkaAppender, error) {
	fmt.Println("NewKafkaAppender %v", brokers)
	config := sarama.NewConfig()
	config.Producer.RequiredAcks = sarama.WaitForLocal
	config.Producer.Retry.Max = 3
	config.Producer.Return.Successes = true

	producer, err := sarama.NewSyncProducer(brokers, config)
	if err != nil {
		return nil, err
	}

	appender := &KafkaAppender{
		producer:   producer,
		topic:      topic,
		bufferSize: bufferSize,
		brokers:    brokers,
		logChan:    make(chan []byte, bufferSize),
		done:       make(chan struct{}),
	}

	appender.wg.Add(1)
	go appender.processLogs()

	return appender, nil
}

// Write implements io.Writer
func (a *KafkaAppender) Write(p []byte) (n int, err error) {
	// Make a copy of the log message
	logCopy := make([]byte, len(p))
	copy(logCopy, p)

	select {
	case a.logChan <- logCopy:
		// Successfully sent to channel
	default:
		// Channel is full, log to stderr
		os.Stderr.Write([]byte("Kafka appender buffer full, dropping log message\n"))
	}

	return len(p), nil
}

// processLogs sends logs to Kafka in a background goroutine
func (a *KafkaAppender) processLogs() {
	defer a.wg.Done()

	for {
		select {
		case <-a.done:
			return
		case logMessage := <-a.logChan:
			a.sendToKafka(logMessage)
		}
	}
}

// sendToKafka sends a log message to Kafka
func (a *KafkaAppender) sendToKafka(logMessage []byte) {
	msg := &sarama.ProducerMessage{
		Topic: a.topic,
		Value: sarama.ByteEncoder(logMessage),
	}

	// Try to extract a key from the log message
	var logEntry map[string]interface{}
	if err := json.Unmarshal(logMessage, &logEntry); err == nil {
		// Look for a correlation ID, order ID, or other field to use as a key
		for _, keyField := range []string{"correlation_id", "order_id", "request_id", "trace_id"} {
			if key, ok := logEntry[keyField].(string); ok && key != "" {
				msg.Key = sarama.StringEncoder(key)
				break
			}
		}
	}


	// Send to Kafka
	_, _, err := a.producer.SendMessage(msg)
	if err != nil {
		os.Stderr.Write([]byte("Failed to send log to Kafka: " + err.Error() + ", brokers: " + strings.Join(a.brokers, ",") + "\n"))
	}
}

// Close stops the appender and waits for pending logs to be processed
func (a *KafkaAppender) Close() error {
	close(a.done)
	a.wg.Wait()
	return a.producer.Close()
}

// WriterWithClose combines an io.Writer with an optional io.Closer
type WriterWithClose struct {
	Writer io.Writer
	Closer io.Closer
}

// MultiWriter is an io.Writer that writes to multiple writers
type MultiWriter struct {
	Writers []WriterWithClose
}

// Write implements io.Writer
func (w *MultiWriter) Write(p []byte) (n int, err error) {
	for _, writer := range w.Writers {
		n, err = writer.Writer.Write(p)
		if err != nil {
			return n, err
		}
	}
	return len(p), nil
}

// Close closes all io.Closers
func (w *MultiWriter) Close() error {
	var err error
	for _, writer := range w.Writers {
		if writer.Closer != nil {
			if e := writer.Closer.Close(); e != nil {
				err = e
			}
		}
	}
	return err
}
