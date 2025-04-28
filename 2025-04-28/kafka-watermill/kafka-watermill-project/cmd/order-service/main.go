package main

import (
	"context"
	"log"
	"os"
	"os/signal"
	"strings"
	"syscall"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/ThreeDotsLabs/watermill-kafka/v2/pkg/kafka"
	customlogger "github.com/scrapybara/kafka-watermill-project/pkg/logger"
)

var (
	logger       *customlogger.StructuredLogger
	kafkaAppender *customlogger.KafkaAppender
)

func main() {
	// Get brokers from environment or use default
	brokers := os.Getenv("KAFKA_BROKERS")
	if brokers == "" {
		brokers = "kafka:9092"
	}
	brokerList := strings.Split(brokers, ",")

	// Create structured logger
	var err error
	serviceName := "order-service"
	
	// First create a standard logger that writes to stdout
	logger = customlogger.NewStructuredLogger(serviceName)
	
	// Try to create a Kafka appender for logs
	kafkaAppender, err = customlogger.NewKafkaAppender(brokerList, "service.logs", 1000)
	if err != nil {
		logger.Warn("Failed to create Kafka appender for logs, using stdout only", map[string]interface{}{
			"error": err.Error(),
		})
	} else {
		// Set logger to write to both stdout and Kafka
		multiWriter := &customlogger.MultiWriter{
			Writers: []customlogger.WriterWithClose{
				{Writer: os.Stdout},
				{Writer: kafkaAppender, Closer: kafkaAppender},
			},
		}
		logger.SetOutput(multiWriter)
	}

	// Create watermill logger that wraps our structured logger
	watermillLogger := watermill.NewStdLoggerAdapter(logger.Info, logger.Error, 
		func(msg string, fields watermill.LogFields) {
			// Convert watermill fields to our logger fields
			ourFields := make(map[string]interface{}, len(fields))
			for k, v := range fields {
				ourFields[k] = v
			}
			logger.Debug(msg, ourFields)
		})

	// Create Kafka publisher
	publisher, err := kafka.NewPublisher(
		kafka.PublisherConfig{
			Brokers:   []string{brokers},
			Marshaler: kafka.DefaultMarshaler{},
		},
		watermillLogger,
	)
	if err != nil {
		logger.Fatal("Failed to create publisher", map[string]interface{}{
			"error": err.Error(),
		})
	}

	// Create API and start HTTP server
	api := NewAPI(publisher, logger)
	go api.StartServer()

	logger.Info("Order service started", map[string]interface{}{
		"brokers": brokers,
	})

	// Wait for interrupt signal to gracefully shutdown
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, syscall.SIGTERM)
	<-c
	
	logger.Info("Shutting down")
	
	if err := publisher.Close(); err != nil {
		logger.Error("Error closing publisher", map[string]interface{}{
			"error": err.Error(),
		})
	}
	
	if kafkaAppender != nil {
		if err := kafkaAppender.Close(); err != nil {
			log.Printf("Error closing Kafka appender: %v", err)
		}
	}
}