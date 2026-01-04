package messaging

import (
	"context"
	"encoding/json"
	"fmt"
	"time"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/ThreeDotsLabs/watermill-redisstream/pkg/redisstream"
	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/redis/go-redis/v9"

	pb "github.com/drone-workflow-platform/github.com/drone-workflow-platform/pkg/models"
)

type SimpleService struct {
	publisher   message.Publisher
	redisClient *redis.Client
	logger      watermill.LoggerAdapter
}

func NewSimpleService(redisAddr, redisPassword string, redisDB int, logger watermill.LoggerAdapter) (*SimpleService, error) {
	// Create Redis client
	redisClient := redis.NewClient(&redis.Options{
		Addr:     redisAddr,
		Password: redisPassword,
		DB:       redisDB,
	})

	// Test Redis connection
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	
	_, err := redisClient.Ping(ctx).Result()
	if err != nil {
		return nil, fmt.Errorf("failed to connect to Redis: %w", err)
	}

	// Create Watermill publisher
	publisher, err := redisstream.NewPublisher(
		redisstream.PublisherConfig{
			Client: redisClient,
		},
		logger,
	)
	if err != nil {
		return nil, fmt.Errorf("failed to create publisher: %w", err)
	}

	return &SimpleService{
		publisher:   publisher,
		redisClient: redisClient,
		logger:      logger,
	}, nil
}

func (s *SimpleService) PublishWorkflowStarted(event *pb.WorkflowStartedEvent) error {
	return s.publishEvent("workflow.started", event)
}

func (s *SimpleService) PublishWorkflowCompleted(event *pb.WorkflowCompletedEvent) error {
	return s.publishEvent("workflow.completed", event)
}

func (s *SimpleService) PublishStageStarted(event *pb.StageStartedEvent) error {
	return s.publishEvent("stage.started", event)
}

func (s *SimpleService) PublishStageCompleted(event *pb.StageCompletedEvent) error {
	return s.publishEvent("stage.completed", event)
}

func (s *SimpleService) PublishQualityCheckCompleted(event *pb.QualityCheckCompletedEvent) error {
	return s.publishEvent("quality.check.completed", event)
}

func (s *SimpleService) publishEvent(topic string, event interface{}) error {
	eventData, err := json.Marshal(event)
	if err != nil {
		return fmt.Errorf("failed to marshal event: %w", err)
	}

	msg := message.NewMessage(watermill.NewUUID(), eventData)
	msg.Metadata.Set("event_type", topic)
	msg.Metadata.Set("timestamp", time.Now().Format(time.RFC3339))

	err = s.publisher.Publish(topic, msg)
	if err != nil {
		return fmt.Errorf("failed to publish event to topic %s: %w", topic, err)
	}

	s.logger.Info("Event published", watermill.LogFields{
		"topic": topic,
		"message_id": msg.UUID,
	})

	return nil
}

func (s *SimpleService) Close() error {
	if s.publisher != nil {
		return s.publisher.Close()
	}
	return nil
}

// Helper methods for getting execution status and metrics
func (s *SimpleService) GetExecutionStatus(executionID string) (map[string]interface{}, error) {
	statusKey := fmt.Sprintf("execution_status:%s", executionID)
	statusData, err := s.redisClient.Get(context.Background(), statusKey).Result()
	if err != nil {
		return nil, err
	}

	var status map[string]interface{}
	err = json.Unmarshal([]byte(statusData), &status)
	return status, err
}

func (s *SimpleService) GetStageTracking(executionID string) (map[string]string, error) {
	trackingKey := fmt.Sprintf("stage_tracking:%s", executionID)
	return s.redisClient.HGetAll(context.Background(), trackingKey).Result()
}

func (s *SimpleService) GetQualityMetrics(stageExecutionID string) (map[string]interface{}, error) {
	metricsKey := fmt.Sprintf("quality_metrics:%s", stageExecutionID)
	metricsData, err := s.redisClient.Get(context.Background(), metricsKey).Result()
	if err != nil {
		return nil, err
	}

	var metrics map[string]interface{}
	err = json.Unmarshal([]byte(metricsData), &metrics)
	return metrics, err
}

