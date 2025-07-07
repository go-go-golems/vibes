package stage

import (
	"context"
	"encoding/json"
	"fmt"
	"sync"
	"time"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/redis/go-redis/v9"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	pb "github.com/drone-workflow-platform/github.com/drone-workflow-platform/pkg/models"
)

type Service struct {
	pb.UnimplementedStageServiceServer
	stageExecutions map[string]*pb.StageStatus
	publisher message.Publisher
	redisClient *redis.Client
	logger watermill.LoggerAdapter
	mu sync.RWMutex
}

func NewService(publisher message.Publisher, redisClient *redis.Client, logger watermill.LoggerAdapter) *Service {
	return &Service{
		stageExecutions: make(map[string]*pb.StageStatus),
		publisher: publisher,
		redisClient: redisClient,
		logger: logger,
	}
}

func (s *Service) ExecuteStage(ctx context.Context, req *pb.ExecuteStageRequest) (*pb.ExecuteStageResponse, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	stageExecutionID := fmt.Sprintf("stage_exec_%s_%s_%d", req.WorkflowId, req.StageId, time.Now().Unix())
	
	stageStatus := &pb.StageStatus{
		StageExecutionId: stageExecutionID,
		StageId: req.StageId,
		Status: "pending",
		StartedAt: time.Now().Format(time.RFC3339),
		QualityResults: []*pb.QualityCheckResult{},
	}

	s.stageExecutions[stageExecutionID] = stageStatus

	// Store in Redis for persistence
	statusData, err := json.Marshal(stageStatus)
	if err != nil {
		return &pb.ExecuteStageResponse{
			Success: false,
			Message: fmt.Sprintf("failed to serialize stage status: %v", err),
		}, status.Error(codes.Internal, "failed to serialize stage status")
	}

	err = s.redisClient.Set(ctx, fmt.Sprintf("stage_status:%s", stageExecutionID), statusData, 0).Err()
	if err != nil {
		return &pb.ExecuteStageResponse{
			Success: false,
			Message: fmt.Sprintf("failed to store stage status: %v", err),
		}, status.Error(codes.Internal, "failed to store stage status")
	}

	// Start stage execution in a goroutine
	go s.executeStageAsync(ctx, req, stageStatus)

	s.logger.Info("Started stage execution", watermill.LogFields{
		"stage_execution_id": stageExecutionID,
		"stage_id": req.StageId,
		"workflow_id": req.WorkflowId,
		"execution_id": req.ExecutionId,
	})

	return &pb.ExecuteStageResponse{
		StageExecutionId: stageExecutionID,
		Status: stageStatus,
		Success: true,
		Message: "stage execution started",
	}, nil
}

func (s *Service) GetStageStatus(ctx context.Context, req *pb.GetStageStatusRequest) (*pb.GetStageStatusResponse, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	stageStatus, exists := s.stageExecutions[req.StageExecutionId]
	if !exists {
		// Try to load from Redis
		statusData, err := s.redisClient.Get(ctx, fmt.Sprintf("stage_status:%s", req.StageExecutionId)).Result()
		if err != nil {
			return &pb.GetStageStatusResponse{
				Success: false,
				Message: "stage execution not found",
			}, status.Error(codes.NotFound, "stage execution not found")
		}

		stageStatus = &pb.StageStatus{}
		err = json.Unmarshal([]byte(statusData), stageStatus)
		if err != nil {
			return &pb.GetStageStatusResponse{
				Success: false,
				Message: "failed to deserialize stage status",
			}, status.Error(codes.Internal, "failed to deserialize stage status")
		}

		s.stageExecutions[req.StageExecutionId] = stageStatus
	}

	return &pb.GetStageStatusResponse{
		Status: stageStatus,
		Success: true,
		Message: "stage status retrieved successfully",
	}, nil
}

func (s *Service) UpdateStageStatus(ctx context.Context, req *pb.UpdateStageStatusRequest) (*pb.UpdateStageStatusResponse, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	if req.Status == nil {
		return &pb.UpdateStageStatusResponse{
			Success: false,
			Message: "status is required",
		}, status.Error(codes.InvalidArgument, "status is required")
	}

	_, exists := s.stageExecutions[req.Status.StageExecutionId]
	if !exists {
		return &pb.UpdateStageStatusResponse{
			Success: false,
			Message: "stage execution not found",
		}, status.Error(codes.NotFound, "stage execution not found")
	}

	s.stageExecutions[req.Status.StageExecutionId] = req.Status

	// Update in Redis
	statusData, err := json.Marshal(req.Status)
	if err != nil {
		return &pb.UpdateStageStatusResponse{
			Success: false,
			Message: fmt.Sprintf("failed to serialize stage status: %v", err),
		}, status.Error(codes.Internal, "failed to serialize stage status")
	}

	err = s.redisClient.Set(ctx, fmt.Sprintf("stage_status:%s", req.Status.StageExecutionId), statusData, 0).Err()
	if err != nil {
		return &pb.UpdateStageStatusResponse{
			Success: false,
			Message: fmt.Sprintf("failed to update stage status: %v", err),
		}, status.Error(codes.Internal, "failed to update stage status")
	}

	s.logger.Info("Updated stage status", watermill.LogFields{
		"stage_execution_id": req.Status.StageExecutionId,
		"status": req.Status.Status,
	})

	return &pb.UpdateStageStatusResponse{
		Success: true,
		Message: "stage status updated successfully",
	}, nil
}

func (s *Service) executeStageAsync(ctx context.Context, req *pb.ExecuteStageRequest, stageStatus *pb.StageStatus) {
	defer func() {
		if r := recover(); r != nil {
			s.logger.Error("Stage execution panicked", fmt.Errorf("%v", r), watermill.LogFields{
				"stage_execution_id": stageStatus.StageExecutionId,
			})
			
			// Update status to failed
			s.mu.Lock()
			stageStatus.Status = "failed"
			stageStatus.ErrorMessage = fmt.Sprintf("execution panicked: %v", r)
			stageStatus.CompletedAt = time.Now().Format(time.RFC3339)
			s.mu.Unlock()
		}
	}()

	// Update status to in_progress
	s.mu.Lock()
	stageStatus.Status = "in_progress"
	stageStatus.WorkerId = fmt.Sprintf("worker_%d", time.Now().Unix()%1000)
	s.mu.Unlock()

	s.logger.Info("Executing stage", watermill.LogFields{
		"stage_execution_id": stageStatus.StageExecutionId,
		"stage_id": req.StageId,
		"worker_id": stageStatus.WorkerId,
	})

	// Simulate stage execution work
	s.simulateStageWork(ctx, req, stageStatus)

	// Perform quality checks
	s.performQualityChecks(ctx, req, stageStatus)

	// Update final status
	s.mu.Lock()
	if stageStatus.Status != "failed" {
		stageStatus.Status = "completed"
	}
	stageStatus.CompletedAt = time.Now().Format(time.RFC3339)
	s.mu.Unlock()

	// Publish stage completed event
	event := &pb.StageCompletedEvent{
		WorkflowId: req.WorkflowId,
		StageId: req.StageId,
		ExecutionId: req.ExecutionId,
		StageExecutionId: stageStatus.StageExecutionId,
		Status: stageStatus,
		Timestamp: time.Now().Format(time.RFC3339),
	}

	eventData, err := json.Marshal(event)
	if err != nil {
		s.logger.Error("Failed to marshal stage completed event", err, watermill.LogFields{
			"stage_execution_id": stageStatus.StageExecutionId,
		})
	} else {
		msg := message.NewMessage(watermill.NewUUID(), eventData)
		err = s.publisher.Publish("stage.completed", msg)
		if err != nil {
			s.logger.Error("Failed to publish stage completed event", err, watermill.LogFields{
				"stage_execution_id": stageStatus.StageExecutionId,
			})
		}
	}

	// Update in Redis
	statusData, err := json.Marshal(stageStatus)
	if err != nil {
		s.logger.Error("Failed to serialize final stage status", err, watermill.LogFields{
			"stage_execution_id": stageStatus.StageExecutionId,
		})
	} else {
		err = s.redisClient.Set(ctx, fmt.Sprintf("stage_status:%s", stageStatus.StageExecutionId), statusData, 0).Err()
		if err != nil {
			s.logger.Error("Failed to update final stage status in Redis", err, watermill.LogFields{
				"stage_execution_id": stageStatus.StageExecutionId,
			})
		}
	}

	s.logger.Info("Completed stage execution", watermill.LogFields{
		"stage_execution_id": stageStatus.StageExecutionId,
		"stage_id": req.StageId,
		"status": stageStatus.Status,
	})
}

func (s *Service) simulateStageWork(ctx context.Context, req *pb.ExecuteStageRequest, stageStatus *pb.StageStatus) {
	// Simulate different types of work based on stage ID
	var workDuration time.Duration
	
	switch {
	case contains(req.StageId, "prep"):
		workDuration = 2 * time.Second
	case contains(req.StageId, "mount"):
		workDuration = 3 * time.Second
	case contains(req.StageId, "install"):
		workDuration = 4 * time.Second
	case contains(req.StageId, "test"):
		workDuration = 5 * time.Second
	case contains(req.StageId, "inspection"):
		workDuration = 2 * time.Second
	case contains(req.StageId, "packaging"):
		workDuration = 1 * time.Second
	default:
		workDuration = 3 * time.Second
	}

	s.logger.Info("Simulating stage work", watermill.LogFields{
		"stage_execution_id": stageStatus.StageExecutionId,
		"duration": workDuration.String(),
	})

	time.Sleep(workDuration)
}

func (s *Service) performQualityChecks(ctx context.Context, req *pb.ExecuteStageRequest, stageStatus *pb.StageStatus) {
	// Simulate quality checks based on stage type
	qualityChecks := s.getQualityChecksForStage(req.StageId)
	
	for i, checkType := range qualityChecks {
		checkID := fmt.Sprintf("qc_%s_%d", stageStatus.StageExecutionId, i)
		
		// Simulate quality check execution
		time.Sleep(500 * time.Millisecond)
		
		// 95% pass rate simulation
		passed := time.Now().Unix()%100 < 95
		
		result := &pb.QualityCheckResult{
			CheckId: checkID,
			Type: checkType,
			Passed: passed,
			Criteria: fmt.Sprintf("Standard criteria for %s check", checkType),
			ResultDetails: fmt.Sprintf("Check performed by worker %s", stageStatus.WorkerId),
			InspectorId: stageStatus.WorkerId,
			Timestamp: time.Now().Format(time.RFC3339),
		}
		
		stageStatus.QualityResults = append(stageStatus.QualityResults, result)
		
		if !passed {
			s.mu.Lock()
			stageStatus.Status = "failed"
			stageStatus.ErrorMessage = fmt.Sprintf("Quality check failed: %s", checkType)
			s.mu.Unlock()
			
			s.logger.Warn("Quality check failed", watermill.LogFields{
				"stage_execution_id": stageStatus.StageExecutionId,
				"check_type": checkType,
				"check_id": checkID,
			})
			
			// Publish quality check completed event
			event := &pb.QualityCheckCompletedEvent{
				StageExecutionId: stageStatus.StageExecutionId,
				Result: result,
				Timestamp: time.Now().Format(time.RFC3339),
			}
			
			eventData, err := json.Marshal(event)
			if err != nil {
				s.logger.Error("Failed to marshal quality check event", err, watermill.LogFields{
					"stage_execution_id": stageStatus.StageExecutionId,
				})
			} else {
				msg := message.NewMessage(watermill.NewUUID(), eventData)
				err = s.publisher.Publish("quality.check.completed", msg)
				if err != nil {
					s.logger.Error("Failed to publish quality check event", err, watermill.LogFields{
						"stage_execution_id": stageStatus.StageExecutionId,
					})
				}
			}
			
			return // Stop on first failure
		}
		
		s.logger.Info("Quality check passed", watermill.LogFields{
			"stage_execution_id": stageStatus.StageExecutionId,
			"check_type": checkType,
			"check_id": checkID,
		})
	}
}

func (s *Service) getQualityChecksForStage(stageID string) []string {
	switch {
	case contains(stageID, "prep"):
		return []string{"visual"}
	case contains(stageID, "mount"):
		return []string{"mechanical", "visual"}
	case contains(stageID, "install"):
		return []string{"electrical", "visual"}
	case contains(stageID, "test"):
		return []string{"electrical", "software"}
	case contains(stageID, "inspection"):
		return []string{"visual", "mechanical"}
	case contains(stageID, "packaging"):
		return []string{"visual"}
	default:
		return []string{"visual"}
	}
}

func contains(s, substr string) bool {
	return len(s) >= len(substr) && (s == substr || 
		(len(s) > len(substr) && (s[:len(substr)] == substr || s[len(s)-len(substr):] == substr)) ||
		(len(s) > len(substr) && len(substr) > 0 && s[1:len(substr)+1] == substr))
}

