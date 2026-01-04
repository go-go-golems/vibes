package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"strconv"
	"time"

	"github.com/spf13/cobra"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"

	pb "github.com/drone-workflow-platform/github.com/drone-workflow-platform/pkg/models"
)

var (
	serverAddr string
	timeout    time.Duration
)

func main() {
	var rootCmd = &cobra.Command{
		Use:   "workflow-cli",
		Short: "CLI for managing drone manufacturing workflows",
		Long:  "A command line interface for creating, managing, and executing drone manufacturing workflows",
	}

	// Global flags
	rootCmd.PersistentFlags().StringVar(&serverAddr, "server", "localhost:50051", "Workflow service address")
	rootCmd.PersistentFlags().DurationVar(&timeout, "timeout", 30*time.Second, "Request timeout")

	// Add subcommands
	rootCmd.AddCommand(createWorkflowCmd())
	rootCmd.AddCommand(getWorkflowCmd())
	rootCmd.AddCommand(updateWorkflowCmd())
	rootCmd.AddCommand(deleteWorkflowCmd())
	rootCmd.AddCommand(listWorkflowsCmd())
	rootCmd.AddCommand(executeWorkflowCmd())
	rootCmd.AddCommand(createSampleWorkflowCmd())

	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func createWorkflowCmd() *cobra.Command {
	var workflowFile string

	cmd := &cobra.Command{
		Use:   "create",
		Short: "Create a new workflow",
		Long:  "Create a new workflow from a JSON file",
		RunE: func(cmd *cobra.Command, args []string) error {
			client, conn, err := createClient()
			if err != nil {
				return err
			}
			defer conn.Close()

			// Read workflow from file
			data, err := os.ReadFile(workflowFile)
			if err != nil {
				return fmt.Errorf("failed to read workflow file: %w", err)
			}

			var workflow pb.Workflow
			err = json.Unmarshal(data, &workflow)
			if err != nil {
				return fmt.Errorf("failed to parse workflow JSON: %w", err)
			}

			ctx, cancel := context.WithTimeout(context.Background(), timeout)
			defer cancel()

			resp, err := client.CreateWorkflow(ctx, &pb.CreateWorkflowRequest{
				Workflow: &workflow,
			})
			if err != nil {
				return fmt.Errorf("failed to create workflow: %w", err)
			}

			fmt.Printf("Workflow created successfully!\n")
			fmt.Printf("Workflow ID: %s\n", resp.WorkflowId)
			fmt.Printf("Message: %s\n", resp.Message)

			return nil
		},
	}

	cmd.Flags().StringVarP(&workflowFile, "file", "f", "", "Path to workflow JSON file (required)")
	cmd.MarkFlagRequired("file")

	return cmd
}

func getWorkflowCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "get [workflow-id]",
		Short: "Get a workflow by ID",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			client, conn, err := createClient()
			if err != nil {
				return err
			}
			defer conn.Close()

			ctx, cancel := context.WithTimeout(context.Background(), timeout)
			defer cancel()

			resp, err := client.GetWorkflow(ctx, &pb.GetWorkflowRequest{
				WorkflowId: args[0],
			})
			if err != nil {
				return fmt.Errorf("failed to get workflow: %w", err)
			}

			// Pretty print the workflow
			workflowJSON, err := json.MarshalIndent(resp.Workflow, "", "  ")
			if err != nil {
				return fmt.Errorf("failed to marshal workflow: %w", err)
			}

			fmt.Printf("Workflow Details:\n")
			fmt.Printf("%s\n", workflowJSON)

			return nil
		},
	}

	return cmd
}

func updateWorkflowCmd() *cobra.Command {
	var workflowFile string

	cmd := &cobra.Command{
		Use:   "update [workflow-id]",
		Short: "Update an existing workflow",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			client, conn, err := createClient()
			if err != nil {
				return err
			}
			defer conn.Close()

			// Read workflow from file
			data, err := os.ReadFile(workflowFile)
			if err != nil {
				return fmt.Errorf("failed to read workflow file: %w", err)
			}

			var workflow pb.Workflow
			err = json.Unmarshal(data, &workflow)
			if err != nil {
				return fmt.Errorf("failed to parse workflow JSON: %w", err)
			}

			ctx, cancel := context.WithTimeout(context.Background(), timeout)
			defer cancel()

			resp, err := client.UpdateWorkflow(ctx, &pb.UpdateWorkflowRequest{
				WorkflowId: args[0],
				Workflow:   &workflow,
			})
			if err != nil {
				return fmt.Errorf("failed to update workflow: %w", err)
			}

			fmt.Printf("Workflow updated successfully!\n")
			fmt.Printf("Message: %s\n", resp.Message)

			return nil
		},
	}

	cmd.Flags().StringVarP(&workflowFile, "file", "f", "", "Path to workflow JSON file (required)")
	cmd.MarkFlagRequired("file")

	return cmd
}

func deleteWorkflowCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "delete [workflow-id]",
		Short: "Delete a workflow by ID",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			client, conn, err := createClient()
			if err != nil {
				return err
			}
			defer conn.Close()

			ctx, cancel := context.WithTimeout(context.Background(), timeout)
			defer cancel()

			resp, err := client.DeleteWorkflow(ctx, &pb.DeleteWorkflowRequest{
				WorkflowId: args[0],
			})
			if err != nil {
				return fmt.Errorf("failed to delete workflow: %w", err)
			}

			fmt.Printf("Workflow deleted successfully!\n")
			fmt.Printf("Message: %s\n", resp.Message)

			return nil
		},
	}

	return cmd
}

func listWorkflowsCmd() *cobra.Command {
	var page, pageSize int

	cmd := &cobra.Command{
		Use:   "list",
		Short: "List all workflows",
		RunE: func(cmd *cobra.Command, args []string) error {
			client, conn, err := createClient()
			if err != nil {
				return err
			}
			defer conn.Close()

			ctx, cancel := context.WithTimeout(context.Background(), timeout)
			defer cancel()

			resp, err := client.ListWorkflows(ctx, &pb.ListWorkflowsRequest{
				Page:     int32(page),
				PageSize: int32(pageSize),
			})
			if err != nil {
				return fmt.Errorf("failed to list workflows: %w", err)
			}

			fmt.Printf("Workflows (Total: %d):\n", resp.TotalCount)
			fmt.Printf("%-20s %-30s %-10s %-15s\n", "Name", "Drone Model", "Version", "Stages")
			fmt.Printf("%s\n", "--------------------------------------------------------------------------------")

			for _, workflow := range resp.Workflows {
				fmt.Printf("%-20s %-30s %-10s %-15d\n",
					workflow.Name,
					workflow.DroneModel,
					workflow.Version,
					len(workflow.Stages))
			}

			return nil
		},
	}

	cmd.Flags().IntVar(&page, "page", 1, "Page number")
	cmd.Flags().IntVar(&pageSize, "page-size", 10, "Page size")

	return cmd
}

func executeWorkflowCmd() *cobra.Command {
	var batchID string

	cmd := &cobra.Command{
		Use:   "execute [workflow-id]",
		Short: "Execute a workflow",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			client, conn, err := createClient()
			if err != nil {
				return err
			}
			defer conn.Close()

			if batchID == "" {
				batchID = fmt.Sprintf("batch_%d", time.Now().Unix())
			}

			ctx, cancel := context.WithTimeout(context.Background(), timeout)
			defer cancel()

			resp, err := client.ExecuteWorkflow(ctx, &pb.ExecuteWorkflowRequest{
				WorkflowId: args[0],
				BatchId:    batchID,
			})
			if err != nil {
				return fmt.Errorf("failed to execute workflow: %w", err)
			}

			fmt.Printf("Workflow execution started!\n")
			fmt.Printf("Execution ID: %s\n", resp.ExecutionId)
			fmt.Printf("Batch ID: %s\n", batchID)
			fmt.Printf("Message: %s\n", resp.Message)

			return nil
		},
	}

	cmd.Flags().StringVar(&batchID, "batch-id", "", "Batch ID for execution (auto-generated if not provided)")

	return cmd
}

func createSampleWorkflowCmd() *cobra.Command {
	var outputFile string

	cmd := &cobra.Command{
		Use:   "sample",
		Short: "Create a sample workflow JSON file",
		RunE: func(cmd *cobra.Command, args []string) error {
			sampleWorkflow := createSampleWorkflow()

			workflowJSON, err := json.MarshalIndent(sampleWorkflow, "", "  ")
			if err != nil {
				return fmt.Errorf("failed to marshal sample workflow: %w", err)
			}

			err = os.WriteFile(outputFile, workflowJSON, 0644)
			if err != nil {
				return fmt.Errorf("failed to write sample workflow file: %w", err)
			}

			fmt.Printf("Sample workflow created: %s\n", outputFile)
			return nil
		},
	}

	cmd.Flags().StringVarP(&outputFile, "output", "o", "sample-workflow.json", "Output file for sample workflow")

	return cmd
}

func createClient() (pb.WorkflowServiceClient, *grpc.ClientConn, error) {
	conn, err := grpc.Dial(serverAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		return nil, nil, fmt.Errorf("failed to connect to server: %w", err)
	}

	client := pb.NewWorkflowServiceClient(conn)
	return client, conn, nil
}

func createSampleWorkflow() *pb.Workflow {
	return &pb.Workflow{
		Name:       "QuadCopter_Basic_Assembly",
		Version:    "1.2",
		DroneModel: "QC-200",
		Settings: &pb.WorkflowSettings{
			ParallelStations: 4,
			QualityThreshold: 0.95,
			BatchSize:        10,
		},
		Documentation: &pb.Documentation{
			BasePath:       "/docs/manufacturing/",
			DocumentServer: "https://docs.company.com/manufacturing/",
		},
		Materials: []*pb.Material{
			{
				Id:                 "frame_main",
				Name:               "Carbon Fiber Frame",
				Type:               "component",
				Quantity:           1,
				Supplier:           "AeroTech",
				InspectionRequired: true,
			},
			{
				Id:                 "motor_brushless",
				Name:               "Brushless Motor 2205",
				Type:               "component",
				Quantity:           4,
				Supplier:           "MotorCorp",
				InspectionRequired: true,
			},
			{
				Id:                 "propeller_8045",
				Name:               "8045 Propeller",
				Type:               "component",
				Quantity:           4,
				Supplier:           "PropTech",
				InspectionRequired: false,
			},
		},
		Stages: []*pb.Stage{
			{
				Id:        "frame_prep",
				Name:      "Frame Preparation",
				Type:      "assembly",
				DependsOn: []string{},
				Inputs: &pb.StageInputs{
					Materials: []string{"frame_main"},
					Tools:     []string{"screwdriver_set"},
				},
				Process: &pb.StageProcess{
					Duration:     "10m",
					Workers:      1,
					SkillLevel:   "basic",
					Instructions: "Clean frame, check for defects, prepare mounting points",
				},
				QualityChecks: []*pb.QualityCheck{
					{
						Type:     "visual",
						Criteria: "No cracks, clean surfaces, all mounting holes clear",
						Required: true,
					},
				},
				Outputs: []*pb.StageOutput{
					{
						Id:       "prepared_frame",
						Type:     "subassembly",
						Quantity: 1,
					},
				},
				OnFailure: &pb.FailureHandling{
					Action:      "rework",
					MaxRetries:  2,
					ReworkStage: "frame_prep",
				},
			},
			{
				Id:        "motor_mount",
				Name:      "Motor Mounting",
				Type:      "assembly",
				DependsOn: []string{"frame_prep"},
				Inputs: &pb.StageInputs{
					Materials: []string{"motor_brushless"},
					Tools:     []string{"screwdriver_set", "torque_wrench"},
				},
				Process: &pb.StageProcess{
					Duration:     "20m",
					Workers:      1,
					SkillLevel:   "intermediate",
					Instructions: "Mount motors to frame arms, torque to 2.5 Nm",
				},
				QualityChecks: []*pb.QualityCheck{
					{
						Type:     "mechanical",
						Criteria: "Motors secure, no wobble, proper torque",
						Required: true,
					},
				},
				Outputs: []*pb.StageOutput{
					{
						Id:       "frame_with_motors",
						Type:     "subassembly",
						Quantity: 1,
					},
				},
				OnFailure: &pb.FailureHandling{
					Action:      "rework",
					MaxRetries:  1,
					ReworkStage: "motor_mount",
				},
			},
			{
				Id:        "propeller_install",
				Name:      "Propeller Installation",
				Type:      "assembly",
				DependsOn: []string{"motor_mount"},
				Inputs: &pb.StageInputs{
					Materials: []string{"propeller_8045"},
					Tools:     []string{"prop_wrench"},
				},
				Process: &pb.StageProcess{
					Duration:     "5m",
					Workers:      1,
					SkillLevel:   "basic",
					Instructions: "Install props: CW on motors 1&3, CCW on motors 2&4",
				},
				QualityChecks: []*pb.QualityCheck{
					{
						Type:     "visual",
						Criteria: "Correct rotation direction, secure attachment",
						Required: true,
					},
				},
				Outputs: []*pb.StageOutput{
					{
						Id:       "completed_drone",
						Type:     "completed_unit",
						Quantity: 1,
					},
				},
				OnFailure: &pb.FailureHandling{
					Action:     "retry",
					MaxRetries: 3,
				},
			},
		},
	}
}

