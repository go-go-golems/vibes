package workflow

import (
	"context"
	"fmt"
	"sync"
	"time"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/redis/go-redis/v9"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	pb "github.com/drone-workflow-platform/github.com/drone-workflow-platform/pkg/models"
)

type Service struct {
	pb.UnimplementedWorkflowServiceServer
	workflows map[string]*pb.Workflow
	executions map[string]*WorkflowExecution
	publisher interface{
		PublishWorkflowStarted(*pb.WorkflowStartedEvent) error
		PublishWorkflowCompleted(*pb.WorkflowCompletedEvent) error
		PublishStageStarted(*pb.StageStartedEvent) error
		PublishStageCompleted(*pb.StageCompletedEvent) error
	}
	redisClient *redis.Client
	logger watermill.LoggerAdapter
	mu sync.RWMutex
}

type WorkflowExecution struct {
	ID string
	WorkflowID string
	BatchID string
	Status string
	StartedAt time.Time
	CompletedAt *time.Time
	StageExecutions map[string]*StageExecution
}

type StageExecution struct {
	ID string
	StageID string
	Status string
	StartedAt time.Time
	CompletedAt *time.Time
	WorkerID string
	QualityResults []*pb.QualityCheckResult
	ErrorMessage string
}

func NewService(publisher interface{
	PublishWorkflowStarted(*pb.WorkflowStartedEvent) error
	PublishWorkflowCompleted(*pb.WorkflowCompletedEvent) error
	PublishStageStarted(*pb.StageStartedEvent) error
	PublishStageCompleted(*pb.StageCompletedEvent) error
}, redisClient *redis.Client, logger watermill.LoggerAdapter) *Service {
	return &Service{
		workflows:   make(map[string]*pb.Workflow),
		executions:  make(map[string]*WorkflowExecution),
		publisher:   publisher,
		redisClient: redisClient,
		logger:      logger,
	}
}

func (s *Service) CreateWorkflow(ctx context.Context, req *pb.CreateWorkflowRequest) (*pb.CreateWorkflowResponse, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	if req.Workflow == nil {
		return &pb.CreateWorkflowResponse{
			Success: false,
			Message: "workflow is required",
		}, status.Error(codes.InvalidArgument, "workflow is required")
	}

	// Generate workflow ID
	workflowID := fmt.Sprintf("workflow_%s_%d", req.Workflow.Name, time.Now().Unix())
	
	// Store workflow
	s.workflows[workflowID] = req.Workflow

	s.logger.Info("Workflow created", watermill.LogFields{
		"workflow_id": workflowID,
		"name": req.Workflow.Name,
		"version": req.Workflow.Version,
	})

	return &pb.CreateWorkflowResponse{
		Success:    true,
		WorkflowId: workflowID,
		Message:    "Workflow created successfully",
	}, nil
}

func (s *Service) GetWorkflow(ctx context.Context, req *pb.GetWorkflowRequest) (*pb.GetWorkflowResponse, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	workflow, exists := s.workflows[req.WorkflowId]
	if !exists {
		return &pb.GetWorkflowResponse{
			Success: false,
			Message: "Workflow not found",
		}, status.Error(codes.NotFound, "workflow not found")
	}

	return &pb.GetWorkflowResponse{
		Success:  true,
		Workflow: workflow,
		Message:  "Workflow retrieved successfully",
	}, nil
}

func (s *Service) UpdateWorkflow(ctx context.Context, req *pb.UpdateWorkflowRequest) (*pb.UpdateWorkflowResponse, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	if _, exists := s.workflows[req.WorkflowId]; !exists {
		return &pb.UpdateWorkflowResponse{
			Success: false,
			Message: "Workflow not found",
		}, status.Error(codes.NotFound, "workflow not found")
	}

	s.workflows[req.WorkflowId] = req.Workflow

	s.logger.Info("Workflow updated", watermill.LogFields{
		"workflow_id": req.WorkflowId,
	})

	return &pb.UpdateWorkflowResponse{
		Success: true,
		Message: "Workflow updated successfully",
	}, nil
}

func (s *Service) DeleteWorkflow(ctx context.Context, req *pb.DeleteWorkflowRequest) (*pb.DeleteWorkflowResponse, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	if _, exists := s.workflows[req.WorkflowId]; !exists {
		return &pb.DeleteWorkflowResponse{
			Success: false,
			Message: "Workflow not found",
		}, status.Error(codes.NotFound, "workflow not found")
	}

	delete(s.workflows, req.WorkflowId)

	s.logger.Info("Workflow deleted", watermill.LogFields{
		"workflow_id": req.WorkflowId,
	})

	return &pb.DeleteWorkflowResponse{
		Success: true,
		Message: "Workflow deleted successfully",
	}, nil
}

func (s *Service) ListWorkflows(ctx context.Context, req *pb.ListWorkflowsRequest) (*pb.ListWorkflowsResponse, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	var workflows []*pb.Workflow
	for _, workflow := range s.workflows {
		workflows = append(workflows, workflow)
	}

	// Simple pagination
	start := int((req.Page - 1) * req.PageSize)
	end := int(req.Page * req.PageSize)
	
	if start >= len(workflows) {
		workflows = []*pb.Workflow{}
	} else if end > len(workflows) {
		workflows = workflows[start:]
	} else {
		workflows = workflows[start:end]
	}

	return &pb.ListWorkflowsResponse{
		Success:    true,
		Workflows:  workflows,
		TotalCount: int32(len(s.workflows)),
		Message:    "Workflows retrieved successfully",
	}, nil
}

func (s *Service) ExecuteWorkflow(ctx context.Context, req *pb.ExecuteWorkflowRequest) (*pb.ExecuteWorkflowResponse, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	workflow, exists := s.workflows[req.WorkflowId]
	if !exists {
		return &pb.ExecuteWorkflowResponse{
			Success: false,
			Message: "Workflow not found",
		}, status.Error(codes.NotFound, "workflow not found")
	}

	// Generate execution ID
	executionID := fmt.Sprintf("exec_%s_%d", req.WorkflowId, time.Now().Unix())

	// Create execution record
	execution := &WorkflowExecution{
		ID:              executionID,
		WorkflowID:      req.WorkflowId,
		BatchID:         req.BatchId,
		Status:          "started",
		StartedAt:       time.Now(),
		StageExecutions: make(map[string]*StageExecution),
	}

	s.executions[executionID] = execution

	// Publish workflow started event
	startedEvent := &pb.WorkflowStartedEvent{
		WorkflowId:  req.WorkflowId,
		ExecutionId: executionID,
		BatchId:     req.BatchId,
		Timestamp:   time.Now().Format(time.RFC3339),
	}

	err := s.publisher.PublishWorkflowStarted(startedEvent)
	if err != nil {
		s.logger.Error("Failed to publish workflow started event", err, watermill.LogFields{
			"execution_id": executionID,
		})
	}

	s.logger.Info("Workflow execution started", watermill.LogFields{
		"workflow_id":  req.WorkflowId,
		"execution_id": executionID,
		"batch_id":     req.BatchId,
		"stages":       len(workflow.Stages),
	})

	return &pb.ExecuteWorkflowResponse{
		Success:     true,
		ExecutionId: executionID,
		Message:     "Workflow execution started successfully",
	}, nil
}

