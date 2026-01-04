package tests

import (
	"context"
	"encoding/json"
	"fmt"
	"testing"
	"time"

	"github.com/redis/go-redis/v9"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"

	pb "github.com/drone-workflow-platform/github.com/drone-workflow-platform/pkg/models"
)

const (
	workflowServiceAddr = "localhost:50051"
	stageServiceAddr    = "localhost:50052"
	qualityServiceAddr  = "localhost:50053"
	docServiceAddr      = "localhost:50054"
	redisAddr           = "localhost:6379"
	testTimeout         = 30 * time.Second
)

func TestWorkflowServiceIntegration(t *testing.T) {
	// Connect to workflow service
	conn, err := grpc.Dial(workflowServiceAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	require.NoError(t, err)
	defer conn.Close()

	client := pb.NewWorkflowServiceClient(conn)
	ctx, cancel := context.WithTimeout(context.Background(), testTimeout)
	defer cancel()

	// Test workflow creation
	t.Run("CreateWorkflow", func(t *testing.T) {
		workflow := createTestWorkflow()
		
		resp, err := client.CreateWorkflow(ctx, &pb.CreateWorkflowRequest{
			Workflow: workflow,
		})
		
		require.NoError(t, err)
		assert.True(t, resp.Success)
		assert.NotEmpty(t, resp.WorkflowId)
		assert.Contains(t, resp.Message, "successfully")
	})

	// Test workflow listing
	t.Run("ListWorkflows", func(t *testing.T) {
		resp, err := client.ListWorkflows(ctx, &pb.ListWorkflowsRequest{
			Page:     1,
			PageSize: 10,
		})
		
		require.NoError(t, err)
		assert.True(t, resp.Success)
		assert.GreaterOrEqual(t, resp.TotalCount, int32(0))
	})
}

func TestStageServiceIntegration(t *testing.T) {
	// Connect to stage service
	conn, err := grpc.Dial(stageServiceAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	require.NoError(t, err)
	defer conn.Close()

	client := pb.NewStageServiceClient(conn)
	ctx, cancel := context.WithTimeout(context.Background(), testTimeout)
	defer cancel()

	// Test stage execution
	t.Run("ExecuteStage", func(t *testing.T) {
		resp, err := client.ExecuteStage(ctx, &pb.ExecuteStageRequest{
			WorkflowId:     "test-workflow-123",
			StageId:        "test-stage-001",
			ExecutionId:    "test-execution-456",
			InputMaterials: []string{"material1", "material2"},
		})
		
		require.NoError(t, err)
		assert.True(t, resp.Success)
		assert.NotEmpty(t, resp.StageExecutionId)
		assert.NotNil(t, resp.Status)
	})
}

func TestQualityServiceIntegration(t *testing.T) {
	// Connect to quality service
	conn, err := grpc.Dial(qualityServiceAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	require.NoError(t, err)
	defer conn.Close()

	client := pb.NewQualityServiceClient(conn)
	ctx, cancel := context.WithTimeout(context.Background(), testTimeout)
	defer cancel()

	// Test quality check
	t.Run("PerformQualityCheck", func(t *testing.T) {
		qualityCheck := &pb.QualityCheck{
			Type:     "visual",
			Criteria: "No visible defects",
			Required: true,
		}
		
		resp, err := client.PerformQualityCheck(ctx, &pb.PerformQualityCheckRequest{
			StageExecutionId: "test-stage-exec-123",
			QualityCheck:     qualityCheck,
		})
		
		require.NoError(t, err)
		assert.True(t, resp.Success)
		assert.NotNil(t, resp.Result)
		assert.Equal(t, "visual", resp.Result.Type)
	})
}

func TestDocumentationServiceIntegration(t *testing.T) {
	// Connect to documentation service
	conn, err := grpc.Dial(docServiceAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	require.NoError(t, err)
	defer conn.Close()

	client := pb.NewDocumentationServiceClient(conn)
	ctx, cancel := context.WithTimeout(context.Background(), testTimeout)
	defer cancel()

	// Test document retrieval
	t.Run("GetDocument", func(t *testing.T) {
		resp, err := client.GetDocument(ctx, &pb.GetDocumentRequest{
			DocumentId:    "iso_9001",
			UserClearance: "internal",
		})
		
		require.NoError(t, err)
		assert.True(t, resp.Success)
		assert.NotNil(t, resp.Document)
		assert.Equal(t, "iso_9001", resp.Document.Id)
	})

	// Test document access validation
	t.Run("ValidateDocumentAccess", func(t *testing.T) {
		resp, err := client.ValidateDocumentAccess(ctx, &pb.ValidateDocumentAccessRequest{
			DocumentId:    "iso_9001",
			UserClearance: "internal",
			RequiredAccess: &pb.DocumentAccess{
				RequiredClearance: []string{"internal"},
				DigitalSignature:  false,
				PrintRequired:     false,
				Verification:      false,
			},
		})
		
		require.NoError(t, err)
		assert.True(t, resp.AccessGranted)
	})
}

func TestRedisIntegration(t *testing.T) {
	// Connect to Redis
	redisClient := redis.NewClient(&redis.Options{
		Addr: redisAddr,
	})
	defer redisClient.Close()

	ctx, cancel := context.WithTimeout(context.Background(), testTimeout)
	defer cancel()

	// Test Redis connectivity
	t.Run("RedisConnectivity", func(t *testing.T) {
		pong, err := redisClient.Ping(ctx).Result()
		require.NoError(t, err)
		assert.Equal(t, "PONG", pong)
	})

	// Test data storage and retrieval
	t.Run("RedisDataOperations", func(t *testing.T) {
		testKey := "test:integration:key"
		testValue := "test-value-123"
		
		// Set value
		err := redisClient.Set(ctx, testKey, testValue, time.Minute).Err()
		require.NoError(t, err)
		
		// Get value
		result, err := redisClient.Get(ctx, testKey).Result()
		require.NoError(t, err)
		assert.Equal(t, testValue, result)
		
		// Clean up
		redisClient.Del(ctx, testKey)
	})
}

func TestEndToEndWorkflow(t *testing.T) {
	// This test simulates a complete workflow execution
	t.Run("CompleteWorkflowExecution", func(t *testing.T) {
		// Connect to workflow service
		workflowConn, err := grpc.Dial(workflowServiceAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
		require.NoError(t, err)
		defer workflowConn.Close()

		workflowClient := pb.NewWorkflowServiceClient(workflowConn)
		ctx, cancel := context.WithTimeout(context.Background(), testTimeout)
		defer cancel()

		// Step 1: Create workflow
		workflow := createTestWorkflow()
		createResp, err := workflowClient.CreateWorkflow(ctx, &pb.CreateWorkflowRequest{
			Workflow: workflow,
		})
		require.NoError(t, err)
		assert.True(t, createResp.Success)
		workflowId := createResp.WorkflowId

		// Step 2: Execute workflow
		execResp, err := workflowClient.ExecuteWorkflow(ctx, &pb.ExecuteWorkflowRequest{
			WorkflowId: workflowId,
			BatchId:    "test-batch-001",
		})
		require.NoError(t, err)
		assert.True(t, execResp.Success)
		executionId := execResp.ExecutionId

		// Step 3: Wait for execution to start
		time.Sleep(2 * time.Second)

		// Step 4: Verify execution in Redis
		redisClient := redis.NewClient(&redis.Options{
			Addr: redisAddr,
		})
		defer redisClient.Close()

		statusKey := fmt.Sprintf("execution_status:%s", executionId)
		statusData, err := redisClient.Get(ctx, statusKey).Result()
		if err == nil {
			var status map[string]interface{}
			err = json.Unmarshal([]byte(statusData), &status)
			require.NoError(t, err)
			assert.Equal(t, executionId, status["execution_id"])
			assert.Equal(t, workflowId, status["workflow_id"])
		}

		// Step 5: Clean up
		_, err = workflowClient.DeleteWorkflow(ctx, &pb.DeleteWorkflowRequest{
			WorkflowId: workflowId,
		})
		assert.NoError(t, err)
	})
}

func TestServiceHealthChecks(t *testing.T) {
	services := map[string]string{
		"workflow":      workflowServiceAddr,
		"stage":         stageServiceAddr,
		"quality":       qualityServiceAddr,
		"documentation": docServiceAddr,
	}

	for serviceName, addr := range services {
		t.Run(fmt.Sprintf("%sServiceHealth", serviceName), func(t *testing.T) {
			conn, err := grpc.Dial(addr, grpc.WithTransportCredentials(insecure.NewCredentials()))
			require.NoError(t, err, "Failed to connect to %s service", serviceName)
			conn.Close()
		})
	}
}

// Helper function to create a test workflow
func createTestWorkflow() *pb.Workflow {
	return &pb.Workflow{
		Name:       "Test_Workflow",
		Version:    "1.0",
		DroneModel: "TEST-001",
		Settings: &pb.WorkflowSettings{
			ParallelStations: 2,
			QualityThreshold: 0.95,
			BatchSize:        5,
		},
		Documentation: &pb.Documentation{
			BasePath:       "/test/docs/",
			DocumentServer: "https://test.docs.com/",
		},
		Materials: []*pb.Material{
			{
				Id:                 "test_material_1",
				Name:               "Test Material 1",
				Type:               "component",
				Quantity:           1,
				Supplier:           "TestSupplier",
				InspectionRequired: true,
			},
		},
		Stages: []*pb.Stage{
			{
				Id:        "test_stage_1",
				Name:      "Test Stage 1",
				Type:      "assembly",
				DependsOn: []string{},
				Inputs: &pb.StageInputs{
					Materials: []string{"test_material_1"},
					Tools:     []string{"test_tool_1"},
				},
				Process: &pb.StageProcess{
					Duration:     "5m",
					Workers:      1,
					SkillLevel:   "basic",
					Instructions: "Test assembly instructions",
				},
				QualityChecks: []*pb.QualityCheck{
					{
						Type:     "visual",
						Criteria: "Test quality criteria",
						Required: true,
					},
				},
				Outputs: []*pb.StageOutput{
					{
						Id:       "test_output_1",
						Type:     "subassembly",
						Quantity: 1,
					},
				},
				OnFailure: &pb.FailureHandling{
					Action:     "retry",
					MaxRetries: 2,
				},
			},
		},
	}
}

