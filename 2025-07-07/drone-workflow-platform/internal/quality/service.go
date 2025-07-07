package quality

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
	pb.UnimplementedQualityServiceServer
	qualityResults map[string]*pb.QualityCheckResult
	qualityReports map[string]*pb.QualityReport
	publisher message.Publisher
	redisClient *redis.Client
	logger watermill.LoggerAdapter
	mu sync.RWMutex
}

func NewService(publisher message.Publisher, redisClient *redis.Client, logger watermill.LoggerAdapter) *Service {
	return &Service{
		qualityResults: make(map[string]*pb.QualityCheckResult),
		qualityReports: make(map[string]*pb.QualityReport),
		publisher: publisher,
		redisClient: redisClient,
		logger: logger,
	}
}

func (s *Service) PerformQualityCheck(ctx context.Context, req *pb.PerformQualityCheckRequest) (*pb.PerformQualityCheckResponse, error) {
	if req.QualityCheck == nil {
		return &pb.PerformQualityCheckResponse{
			Success: false,
			Message: "quality check is required",
		}, status.Error(codes.InvalidArgument, "quality check is required")
	}

	checkID := fmt.Sprintf("qc_%s_%d", req.StageExecutionId, time.Now().Unix())
	
	s.logger.Info("Performing quality check", watermill.LogFields{
		"check_id": checkID,
		"stage_execution_id": req.StageExecutionId,
		"check_type": req.QualityCheck.Type,
	})

	// Simulate quality check execution
	result := s.executeQualityCheck(ctx, checkID, req.StageExecutionId, req.QualityCheck)

	// Store result
	s.mu.Lock()
	s.qualityResults[checkID] = result
	s.mu.Unlock()

	// Store in Redis for persistence
	resultData, err := json.Marshal(result)
	if err != nil {
		return &pb.PerformQualityCheckResponse{
			Success: false,
			Message: fmt.Sprintf("failed to serialize quality result: %v", err),
		}, status.Error(codes.Internal, "failed to serialize quality result")
	}

	err = s.redisClient.Set(ctx, fmt.Sprintf("quality_result:%s", checkID), resultData, 0).Err()
	if err != nil {
		return &pb.PerformQualityCheckResponse{
			Success: false,
			Message: fmt.Sprintf("failed to store quality result: %v", err),
		}, status.Error(codes.Internal, "failed to store quality result")
	}

	// Publish quality check completed event
	event := &pb.QualityCheckCompletedEvent{
		StageExecutionId: req.StageExecutionId,
		Result: result,
		Timestamp: time.Now().Format(time.RFC3339),
	}

	eventData, err := json.Marshal(event)
	if err != nil {
		s.logger.Error("Failed to marshal quality check completed event", err, watermill.LogFields{
			"check_id": checkID,
		})
	} else {
		msg := message.NewMessage(watermill.NewUUID(), eventData)
		err = s.publisher.Publish("quality.check.completed", msg)
		if err != nil {
			s.logger.Error("Failed to publish quality check completed event", err, watermill.LogFields{
				"check_id": checkID,
			})
		}
	}

	s.logger.Info("Quality check completed", watermill.LogFields{
		"check_id": checkID,
		"stage_execution_id": req.StageExecutionId,
		"passed": result.Passed,
	})

	return &pb.PerformQualityCheckResponse{
		Result: result,
		Success: true,
		Message: "quality check completed successfully",
	}, nil
}

func (s *Service) GetQualityReport(ctx context.Context, req *pb.GetQualityReportRequest) (*pb.GetQualityReportResponse, error) {
	s.mu.RLock()
	report, exists := s.qualityReports[req.ExecutionId]
	s.mu.RUnlock()

	if !exists {
		// Try to load from Redis
		reportData, err := s.redisClient.Get(ctx, fmt.Sprintf("quality_report:%s", req.ExecutionId)).Result()
		if err != nil {
			// Generate report if it doesn't exist
			report = s.generateQualityReport(ctx, req.ExecutionId)
			if report == nil {
				return &pb.GetQualityReportResponse{
					Success: false,
					Message: "quality report not found and could not be generated",
				}, status.Error(codes.NotFound, "quality report not found")
			}
		} else {
			report = &pb.QualityReport{}
			err = json.Unmarshal([]byte(reportData), report)
			if err != nil {
				return &pb.GetQualityReportResponse{
					Success: false,
					Message: "failed to deserialize quality report",
				}, status.Error(codes.Internal, "failed to deserialize quality report")
			}
		}

		s.mu.Lock()
		s.qualityReports[req.ExecutionId] = report
		s.mu.Unlock()
	}

	return &pb.GetQualityReportResponse{
		Report: report,
		Success: true,
		Message: "quality report retrieved successfully",
	}, nil
}

func (s *Service) executeQualityCheck(ctx context.Context, checkID, stageExecutionID string, qualityCheck *pb.QualityCheck) *pb.QualityCheckResult {
	// Simulate different check types with different execution times and pass rates
	var executionTime time.Duration
	var passRate float64
	var resultDetails string

	switch qualityCheck.Type {
	case "visual":
		executionTime = 1 * time.Second
		passRate = 0.95
		resultDetails = "Visual inspection completed - checking for defects, proper alignment, and cosmetic quality"
	case "electrical":
		executionTime = 3 * time.Second
		passRate = 0.90
		resultDetails = "Electrical testing completed - checking continuity, voltage levels, and signal integrity"
	case "mechanical":
		executionTime = 2 * time.Second
		passRate = 0.92
		resultDetails = "Mechanical testing completed - checking torque specifications, fit, and structural integrity"
	case "software":
		executionTime = 4 * time.Second
		passRate = 0.88
		resultDetails = "Software testing completed - checking firmware functionality, calibration, and response"
	default:
		executionTime = 2 * time.Second
		passRate = 0.90
		resultDetails = "Generic quality check completed"
	}

	s.logger.Info("Executing quality check", watermill.LogFields{
		"check_id": checkID,
		"type": qualityCheck.Type,
		"duration": executionTime.String(),
	})

	// Simulate execution time
	time.Sleep(executionTime)

	// Determine pass/fail based on pass rate
	passed := float64(time.Now().Unix()%100)/100.0 < passRate

	inspectorID := fmt.Sprintf("inspector_%d", time.Now().Unix()%10+1)

	result := &pb.QualityCheckResult{
		CheckId: checkID,
		Type: qualityCheck.Type,
		Passed: passed,
		Criteria: qualityCheck.Criteria,
		ResultDetails: resultDetails,
		InspectorId: inspectorID,
		Timestamp: time.Now().Format(time.RFC3339),
	}

	if !passed {
		result.ResultDetails += " - FAILED: Does not meet quality standards"
		s.logger.Warn("Quality check failed", watermill.LogFields{
			"check_id": checkID,
			"type": qualityCheck.Type,
			"criteria": qualityCheck.Criteria,
		})
	} else {
		result.ResultDetails += " - PASSED: Meets all quality standards"
		s.logger.Info("Quality check passed", watermill.LogFields{
			"check_id": checkID,
			"type": qualityCheck.Type,
		})
	}

	return result
}

func (s *Service) generateQualityReport(ctx context.Context, executionID string) *pb.QualityReport {
	s.logger.Info("Generating quality report", watermill.LogFields{
		"execution_id": executionID,
	})

	// Collect all quality results for this execution
	var results []*pb.QualityCheckResult
	var totalScore float64
	var passedCount int

	// Search through stored results (in a real implementation, this would query Redis more efficiently)
	s.mu.RLock()
	for _, result := range s.qualityResults {
		// In a real implementation, we would have a better way to associate results with executions
		results = append(results, result)
		if result.Passed {
			passedCount++
			totalScore += 1.0
		}
	}
	s.mu.RUnlock()

	if len(results) == 0 {
		s.logger.Warn("No quality results found for execution", watermill.LogFields{
			"execution_id": executionID,
		})
		return nil
	}

	overallScore := totalScore / float64(len(results))
	passed := overallScore >= 0.95 // 95% pass threshold

	report := &pb.QualityReport{
		ExecutionId: executionID,
		WorkflowId: fmt.Sprintf("workflow_for_%s", executionID), // In real implementation, this would be properly tracked
		Results: results,
		OverallScore: float32(overallScore),
		Passed: passed,
		GeneratedAt: time.Now().Format(time.RFC3339),
	}

	// Store report in Redis
	reportData, err := json.Marshal(report)
	if err != nil {
		s.logger.Error("Failed to serialize quality report", err, watermill.LogFields{
			"execution_id": executionID,
		})
	} else {
		err = s.redisClient.Set(ctx, fmt.Sprintf("quality_report:%s", executionID), reportData, 0).Err()
		if err != nil {
			s.logger.Error("Failed to store quality report in Redis", err, watermill.LogFields{
				"execution_id": executionID,
			})
		}
	}

	s.logger.Info("Quality report generated", watermill.LogFields{
		"execution_id": executionID,
		"overall_score": overallScore,
		"passed": passed,
		"total_checks": len(results),
		"passed_checks": passedCount,
	})

	return report
}

// Helper method to get quality statistics
func (s *Service) GetQualityStatistics(ctx context.Context, executionID string) map[string]interface{} {
	s.mu.RLock()
	defer s.mu.RUnlock()

	var totalChecks, passedChecks int
	checkTypes := make(map[string]int)
	passedByType := make(map[string]int)

	for _, result := range s.qualityResults {
		totalChecks++
		checkTypes[result.Type]++
		
		if result.Passed {
			passedChecks++
			passedByType[result.Type]++
		}
	}

	stats := map[string]interface{}{
		"total_checks": totalChecks,
		"passed_checks": passedChecks,
		"pass_rate": float64(passedChecks) / float64(totalChecks),
		"check_types": checkTypes,
		"passed_by_type": passedByType,
	}

	return stats
}

