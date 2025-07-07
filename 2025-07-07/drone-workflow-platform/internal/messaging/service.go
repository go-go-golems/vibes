package messaging

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"time"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/ThreeDotsLabs/watermill-redisstream/pkg/redisstream"
	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/redis/go-redis/v9"

	pb "github.com/drone-workflow-platform/github.com/drone-workflow-platform/pkg/models"
)

type Service struct {
	publisher message.Publisher
	subscriber message.Subscriber
	redisClient *redis.Client
	logger watermill.LoggerAdapter
	router *message.Router
}

type EventHandler func(msg *message.Message) error

func NewService(redisAddr, redisPassword string, redisDB int, logger watermill.LoggerAdapter) (*Service, error) {
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

	// Create Watermill subscriber
	subscriber, err := redisstream.NewSubscriber(
		redisstream.SubscriberConfig{
			Client:        redisClient,
			ConsumerGroup: "drone-workflow-platform",
		},
		logger,
	)
	if err != nil {
		return nil, fmt.Errorf("failed to create subscriber: %w", err)
	}

	// Create message router
	router, err := message.NewRouter(message.RouterConfig{}, logger)
	if err != nil {
		return nil, fmt.Errorf("failed to create router: %w", err)
	}

	service := &Service{
		publisher:   publisher,
		subscriber:  subscriber,
		redisClient: redisClient,
		logger:      logger,
		router:      router,
	}

	// Set up event handlers
	service.setupEventHandlers()

	return service, nil
}

func (s *Service) Start(ctx context.Context) error {
	s.logger.Info("Starting messaging service", nil)
	
	// Start the router
	go func() {
		err := s.router.Run(ctx)
		if err != nil {
			s.logger.Error("Router stopped with error", err, nil)
		}
	}()

	// Wait for router to be ready
	<-s.router.Running()
	s.logger.Info("Messaging service started successfully", nil)
	
	return nil
}

func (s *Service) Stop() error {
	s.logger.Info("Stopping messaging service", nil)
	
	// Close router
	err := s.router.Close()
	if err != nil {
		s.logger.Error("Error closing router", err, nil)
	}

	// Close publisher
	err = s.publisher.Close()
	if err != nil {
		s.logger.Error("Error closing publisher", err, nil)
	}

	// Close subscriber
	err = s.subscriber.Close()
	if err != nil {
		s.logger.Error("Error closing subscriber", err, nil)
	}

	// Close Redis client
	err = s.redisClient.Close()
	if err != nil {
		s.logger.Error("Error closing Redis client", err, nil)
	}

	s.logger.Info("Messaging service stopped", nil)
	return nil
}

func (s *Service) PublishWorkflowStarted(event *pb.WorkflowStartedEvent) error {
	return s.publishEvent("workflow.started", event)
}

func (s *Service) PublishWorkflowCompleted(event *pb.WorkflowCompletedEvent) error {
	return s.publishEvent("workflow.completed", event)
}

func (s *Service) PublishStageStarted(event *pb.StageStartedEvent) error {
	return s.publishEvent("stage.started", event)
}

func (s *Service) PublishStageCompleted(event *pb.StageCompletedEvent) error {
	return s.publishEvent("stage.completed", event)
}

func (s *Service) PublishQualityCheckCompleted(event *pb.QualityCheckCompletedEvent) error {
	return s.publishEvent("quality.check.completed", event)
}

func (s *Service) publishEvent(topic string, event interface{}) error {
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

func (s *Service) setupEventHandlers() {
	// Workflow events
	s.router.AddHandler(
		"workflow_started_handler",
		"workflow.started",
		s.subscriber,
		"workflow.started.processed",
		s.publisher,
		s.handleWorkflowStarted,
	)

	s.router.AddHandler(
		"workflow_completed_handler",
		"workflow.completed",
		s.subscriber,
		"workflow.completed.processed",
		s.publisher,
		s.handleWorkflowCompleted,
	)

	// Stage events
	s.router.AddHandler(
		"stage_started_handler",
		"stage.started",
		s.subscriber,
		"stage.started.processed",
		s.publisher,
		s.handleStageStarted,
	)

	s.router.AddHandler(
		"stage_completed_handler",
		"stage.completed",
		s.subscriber,
		"stage.completed.processed",
		s.publisher,
		s.handleStageCompleted,
	)

	// Quality events
	s.router.AddHandler(
		"quality_check_completed_handler",
		"quality.check.completed",
		s.subscriber,
		"quality.check.completed.processed",
		s.publisher,
		s.handleQualityCheckCompleted,
	)

	// Add middleware for logging
	s.router.AddMiddleware(s.loggingMiddleware)
	s.router.AddMiddleware(s.recoveryMiddleware)
}

func (s *Service) handleWorkflowStarted(msg *message.Message) ([]*message.Message, error) {
	var event pb.WorkflowStartedEvent
	err := json.Unmarshal(msg.Payload, &event)
	if err != nil {
		s.logger.Error("Failed to unmarshal workflow started event", err, watermill.LogFields{
			"message_id": msg.UUID,
		})
		return nil, err
	}

	s.logger.Info("Processing workflow started event", watermill.LogFields{
		"workflow_id": event.WorkflowId,
		"execution_id": event.ExecutionId,
		"batch_id": event.BatchId,
	})

	// Store event in Redis for audit trail
	eventKey := fmt.Sprintf("event:workflow_started:%s", event.ExecutionId)
	eventData, _ := json.Marshal(event)
	s.redisClient.Set(context.Background(), eventKey, eventData, 24*time.Hour)

	// Update workflow execution status
	statusKey := fmt.Sprintf("execution_status:%s", event.ExecutionId)
	status := map[string]interface{}{
		"execution_id": event.ExecutionId,
		"workflow_id": event.WorkflowId,
		"status": "started",
		"started_at": event.Timestamp,
	}
	statusData, _ := json.Marshal(status)
	s.redisClient.Set(context.Background(), statusKey, statusData, 24*time.Hour)

	return nil, nil
}

func (s *Service) handleWorkflowCompleted(msg *message.Message) error {
	var event pb.WorkflowCompletedEvent
	err := json.Unmarshal(msg.Payload, &event)
	if err != nil {
		s.logger.Error("Failed to unmarshal workflow completed event", err, watermill.LogFields{
			"message_id": msg.UUID,
		})
		return err
	}

	s.logger.Info("Processing workflow completed event", watermill.LogFields{
		"workflow_id": event.WorkflowId,
		"execution_id": event.ExecutionId,
		"status": event.Status,
	})

	// Store event in Redis for audit trail
	eventKey := fmt.Sprintf("event:workflow_completed:%s", event.ExecutionId)
	eventData, _ := json.Marshal(event)
	s.redisClient.Set(context.Background(), eventKey, eventData, 24*time.Hour)

	// Update workflow execution status
	statusKey := fmt.Sprintf("execution_status:%s", event.ExecutionId)
	status := map[string]interface{}{
		"execution_id": event.ExecutionId,
		"workflow_id": event.WorkflowId,
		"status": event.Status,
		"completed_at": event.Timestamp,
	}
	statusData, _ := json.Marshal(status)
	s.redisClient.Set(context.Background(), statusKey, statusData, 24*time.Hour)

	// Generate completion notification
	notification := map[string]interface{}{
		"type": "workflow_completed",
		"execution_id": event.ExecutionId,
		"workflow_id": event.WorkflowId,
		"status": event.Status,
		"timestamp": event.Timestamp,
	}
	notificationData, _ := json.Marshal(notification)
	notificationMsg := message.NewMessage(watermill.NewUUID(), notificationData)
	s.publisher.Publish("notifications", notificationMsg)

	return nil
}

func (s *Service) handleStageStarted(msg *message.Message) error {
	var event pb.StageStartedEvent
	err := json.Unmarshal(msg.Payload, &event)
	if err != nil {
		s.logger.Error("Failed to unmarshal stage started event", err, watermill.LogFields{
			"message_id": msg.UUID,
		})
		return err
	}

	s.logger.Info("Processing stage started event", watermill.LogFields{
		"workflow_id": event.WorkflowId,
		"stage_id": event.StageId,
		"execution_id": event.ExecutionId,
		"stage_execution_id": event.StageExecutionId,
	})

	// Store event in Redis for audit trail
	eventKey := fmt.Sprintf("event:stage_started:%s", event.StageExecutionId)
	eventData, _ := json.Marshal(event)
	s.redisClient.Set(context.Background(), eventKey, eventData, 24*time.Hour)

	// Update stage execution tracking
	trackingKey := fmt.Sprintf("stage_tracking:%s", event.ExecutionId)
	s.redisClient.HSet(context.Background(), trackingKey, event.StageId, "started")
	s.redisClient.Expire(context.Background(), trackingKey, 24*time.Hour)

	return nil
}

func (s *Service) handleStageCompleted(msg *message.Message) error {
	var event pb.StageCompletedEvent
	err := json.Unmarshal(msg.Payload, &event)
	if err != nil {
		s.logger.Error("Failed to unmarshal stage completed event", err, watermill.LogFields{
			"message_id": msg.UUID,
		})
		return err
	}

	s.logger.Info("Processing stage completed event", watermill.LogFields{
		"workflow_id": event.WorkflowId,
		"stage_id": event.StageId,
		"execution_id": event.ExecutionId,
		"stage_execution_id": event.StageExecutionId,
		"status": event.Status.Status,
	})

	// Store event in Redis for audit trail
	eventKey := fmt.Sprintf("event:stage_completed:%s", event.StageExecutionId)
	eventData, _ := json.Marshal(event)
	s.redisClient.Set(context.Background(), eventKey, eventData, 24*time.Hour)

	// Update stage execution tracking
	trackingKey := fmt.Sprintf("stage_tracking:%s", event.ExecutionId)
	s.redisClient.HSet(context.Background(), trackingKey, event.StageId, event.Status.Status)
	s.redisClient.Expire(context.Background(), trackingKey, 24*time.Hour)

	// Check if this triggers next stages (simplified dependency checking)
	s.checkAndTriggerNextStages(event.WorkflowId, event.ExecutionId, event.StageId)

	return nil
}

func (s *Service) handleQualityCheckCompleted(msg *message.Message) error {
	var event pb.QualityCheckCompletedEvent
	err := json.Unmarshal(msg.Payload, &event)
	if err != nil {
		s.logger.Error("Failed to unmarshal quality check completed event", err, watermill.LogFields{
			"message_id": msg.UUID,
		})
		return err
	}

	s.logger.Info("Processing quality check completed event", watermill.LogFields{
		"stage_execution_id": event.StageExecutionId,
		"check_id": event.Result.CheckId,
		"check_type": event.Result.Type,
		"passed": event.Result.Passed,
	})

	// Store event in Redis for audit trail
	eventKey := fmt.Sprintf("event:quality_check:%s", event.Result.CheckId)
	eventData, _ := json.Marshal(event)
	s.redisClient.Set(context.Background(), eventKey, eventData, 24*time.Hour)

	// Update quality metrics
	metricsKey := fmt.Sprintf("quality_metrics:%s", event.StageExecutionId)
	metrics := map[string]interface{}{
		"total_checks": 1,
		"passed_checks": 0,
		"last_updated": event.Timestamp,
	}
	if event.Result.Passed {
		metrics["passed_checks"] = 1
	}

	// Increment existing metrics
	existingData, err := s.redisClient.Get(context.Background(), metricsKey).Result()
	if err == nil {
		var existingMetrics map[string]interface{}
		json.Unmarshal([]byte(existingData), &existingMetrics)
		if totalChecks, ok := existingMetrics["total_checks"].(float64); ok {
			metrics["total_checks"] = int(totalChecks) + 1
		}
		if passedChecks, ok := existingMetrics["passed_checks"].(float64); ok && event.Result.Passed {
			metrics["passed_checks"] = int(passedChecks) + 1
		}
	}

	metricsData, _ := json.Marshal(metrics)
	s.redisClient.Set(context.Background(), metricsKey, metricsData, 24*time.Hour)

	return nil
}

func (s *Service) checkAndTriggerNextStages(workflowID, executionID, completedStageID string) {
	// This is a simplified implementation
	// In a real system, this would check the workflow definition and dependencies
	s.logger.Info("Checking for next stages to trigger", watermill.LogFields{
		"workflow_id": workflowID,
		"execution_id": executionID,
		"completed_stage": completedStageID,
	})

	// For now, just log that we would trigger next stages
	// In a full implementation, this would:
	// 1. Load the workflow definition
	// 2. Check which stages depend on the completed stage
	// 3. Check if all dependencies for those stages are met
	// 4. Trigger execution of ready stages
}

func (s *Service) loggingMiddleware(h message.HandlerFunc) message.HandlerFunc {
	return func(msg *message.Message) ([]*message.Message, error) {
		start := time.Now()
		
		s.logger.Info("Processing message", watermill.LogFields{
			"message_id": msg.UUID,
			"topic": msg.Metadata.Get("topic"),
		})
		
		msgs, err := h(msg)
		
		duration := time.Since(start)
		logFields := watermill.LogFields{
			"message_id": msg.UUID,
			"duration": duration.String(),
		}
		
		if err != nil {
			s.logger.Error("Message processing failed", err, logFields)
		} else {
			s.logger.Info("Message processed successfully", logFields)
		}
		
		return msgs, err
	}
}

func (s *Service) recoveryMiddleware(h message.HandlerFunc) message.HandlerFunc {
	return func(msg *message.Message) (msgs []*message.Message, err error) {
		defer func() {
			if r := recover(); r != nil {
				s.logger.Error("Message handler panicked", fmt.Errorf("%v", r), watermill.LogFields{
					"message_id": msg.UUID,
				})
				err = fmt.Errorf("handler panicked: %v", r)
			}
		}()
		
		return h(msg)
	}
}

// Helper methods for getting execution status and metrics
func (s *Service) GetExecutionStatus(executionID string) (map[string]interface{}, error) {
	statusKey := fmt.Sprintf("execution_status:%s", executionID)
	statusData, err := s.redisClient.Get(context.Background(), statusKey).Result()
	if err != nil {
		return nil, err
	}

	var status map[string]interface{}
	err = json.Unmarshal([]byte(statusData), &status)
	return status, err
}

func (s *Service) GetStageTracking(executionID string) (map[string]string, error) {
	trackingKey := fmt.Sprintf("stage_tracking:%s", executionID)
	return s.redisClient.HGetAll(context.Background(), trackingKey).Result()
}

func (s *Service) GetQualityMetrics(stageExecutionID string) (map[string]interface{}, error) {
	metricsKey := fmt.Sprintf("quality_metrics:%s", stageExecutionID)
	metricsData, err := s.redisClient.Get(context.Background(), metricsKey).Result()
	if err != nil {
		return nil, err
	}

	var metrics map[string]interface{}
	err = json.Unmarshal([]byte(metricsData), &metrics)
	return metrics, err
}

