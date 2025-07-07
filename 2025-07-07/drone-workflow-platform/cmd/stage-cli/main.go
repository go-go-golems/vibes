package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
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
		Use:   "stage-cli",
		Short: "CLI for managing stage executions",
		Long:  "A command line interface for executing and monitoring manufacturing stages",
	}

	// Global flags
	rootCmd.PersistentFlags().StringVar(&serverAddr, "server", "localhost:50052", "Stage service address")
	rootCmd.PersistentFlags().DurationVar(&timeout, "timeout", 30*time.Second, "Request timeout")

	// Add subcommands
	rootCmd.AddCommand(executeStageCmd())
	rootCmd.AddCommand(getStageStatusCmd())
	rootCmd.AddCommand(updateStageStatusCmd())
	rootCmd.AddCommand(watchStageCmd())

	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func executeStageCmd() *cobra.Command {
	var workflowID, stageID, executionID string
	var inputMaterials []string

	cmd := &cobra.Command{
		Use:   "execute",
		Short: "Execute a stage",
		Long:  "Execute a specific stage within a workflow execution",
		RunE: func(cmd *cobra.Command, args []string) error {
			client, conn, err := createClient()
			if err != nil {
				return err
			}
			defer conn.Close()

			ctx, cancel := context.WithTimeout(context.Background(), timeout)
			defer cancel()

			resp, err := client.ExecuteStage(ctx, &pb.ExecuteStageRequest{
				WorkflowId:     workflowID,
				StageId:        stageID,
				ExecutionId:    executionID,
				InputMaterials: inputMaterials,
			})
			if err != nil {
				return fmt.Errorf("failed to execute stage: %w", err)
			}

			fmt.Printf("Stage execution started!\n")
			fmt.Printf("Stage Execution ID: %s\n", resp.StageExecutionId)
			fmt.Printf("Status: %s\n", resp.Status.Status)
			fmt.Printf("Message: %s\n", resp.Message)

			return nil
		},
	}

	cmd.Flags().StringVar(&workflowID, "workflow-id", "", "Workflow ID (required)")
	cmd.Flags().StringVar(&stageID, "stage-id", "", "Stage ID (required)")
	cmd.Flags().StringVar(&executionID, "execution-id", "", "Execution ID (required)")
	cmd.Flags().StringSliceVar(&inputMaterials, "materials", []string{}, "Input materials")

	cmd.MarkFlagRequired("workflow-id")
	cmd.MarkFlagRequired("stage-id")
	cmd.MarkFlagRequired("execution-id")

	return cmd
}

func getStageStatusCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "status [stage-execution-id]",
		Short: "Get stage execution status",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			client, conn, err := createClient()
			if err != nil {
				return err
			}
			defer conn.Close()

			ctx, cancel := context.WithTimeout(context.Background(), timeout)
			defer cancel()

			resp, err := client.GetStageStatus(ctx, &pb.GetStageStatusRequest{
				StageExecutionId: args[0],
			})
			if err != nil {
				return fmt.Errorf("failed to get stage status: %w", err)
			}

			// Pretty print the status
			statusJSON, err := json.MarshalIndent(resp.Status, "", "  ")
			if err != nil {
				return fmt.Errorf("failed to marshal status: %w", err)
			}

			fmt.Printf("Stage Status:\n")
			fmt.Printf("%s\n", statusJSON)

			// Display quality results if any
			if len(resp.Status.QualityResults) > 0 {
				fmt.Printf("\nQuality Check Results:\n")
				fmt.Printf("%-15s %-12s %-8s %-20s\n", "Check ID", "Type", "Passed", "Inspector")
				fmt.Printf("%s\n", "---------------------------------------------------------------")
				for _, result := range resp.Status.QualityResults {
					passedStr := "FAIL"
					if result.Passed {
						passedStr = "PASS"
					}
					fmt.Printf("%-15s %-12s %-8s %-20s\n",
						result.CheckId,
						result.Type,
						passedStr,
						result.InspectorId)
				}
			}

			return nil
		},
	}

	return cmd
}

func updateStageStatusCmd() *cobra.Command {
	var statusFile string

	cmd := &cobra.Command{
		Use:   "update [stage-execution-id]",
		Short: "Update stage execution status",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			client, conn, err := createClient()
			if err != nil {
				return err
			}
			defer conn.Close()

			// Read status from file
			data, err := os.ReadFile(statusFile)
			if err != nil {
				return fmt.Errorf("failed to read status file: %w", err)
			}

			var status pb.StageStatus
			err = json.Unmarshal(data, &status)
			if err != nil {
				return fmt.Errorf("failed to parse status JSON: %w", err)
			}

			// Ensure the stage execution ID matches
			status.StageExecutionId = args[0]

			ctx, cancel := context.WithTimeout(context.Background(), timeout)
			defer cancel()

			resp, err := client.UpdateStageStatus(ctx, &pb.UpdateStageStatusRequest{
				Status: &status,
			})
			if err != nil {
				return fmt.Errorf("failed to update stage status: %w", err)
			}

			fmt.Printf("Stage status updated successfully!\n")
			fmt.Printf("Message: %s\n", resp.Message)

			return nil
		},
	}

	cmd.Flags().StringVarP(&statusFile, "file", "f", "", "Path to status JSON file (required)")
	cmd.MarkFlagRequired("file")

	return cmd
}

func watchStageCmd() *cobra.Command {
	var interval time.Duration

	cmd := &cobra.Command{
		Use:   "watch [stage-execution-id]",
		Short: "Watch stage execution status",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			client, conn, err := createClient()
			if err != nil {
				return err
			}
			defer conn.Close()

			stageExecutionID := args[0]
			fmt.Printf("Watching stage execution: %s\n", stageExecutionID)
			fmt.Printf("Press Ctrl+C to stop watching\n\n")

			ticker := time.NewTicker(interval)
			defer ticker.Stop()

			for {
				ctx, cancel := context.WithTimeout(context.Background(), timeout)
				
				resp, err := client.GetStageStatus(ctx, &pb.GetStageStatusRequest{
					StageExecutionId: stageExecutionID,
				})
				cancel()

				if err != nil {
					fmt.Printf("Error getting status: %v\n", err)
				} else {
					fmt.Printf("[%s] Status: %s", 
						time.Now().Format("15:04:05"), 
						resp.Status.Status)
					
					if resp.Status.WorkerId != "" {
						fmt.Printf(" | Worker: %s", resp.Status.WorkerId)
					}
					
					if resp.Status.ErrorMessage != "" {
						fmt.Printf(" | Error: %s", resp.Status.ErrorMessage)
					}
					
					fmt.Printf("\n")

					// Stop watching if completed or failed
					if resp.Status.Status == "completed" || resp.Status.Status == "failed" {
						fmt.Printf("\nStage execution finished with status: %s\n", resp.Status.Status)
						
						if len(resp.Status.QualityResults) > 0 {
							fmt.Printf("\nFinal Quality Results:\n")
							passedCount := 0
							for _, result := range resp.Status.QualityResults {
								status := "FAIL"
								if result.Passed {
									status = "PASS"
									passedCount++
								}
								fmt.Printf("  %s: %s (%s)\n", result.Type, status, result.Criteria)
							}
							fmt.Printf("Overall: %d/%d checks passed\n", passedCount, len(resp.Status.QualityResults))
						}
						break
					}
				}

				select {
				case <-ticker.C:
					continue
				}
			}

			return nil
		},
	}

	cmd.Flags().DurationVar(&interval, "interval", 2*time.Second, "Watch interval")

	return cmd
}

func createClient() (pb.StageServiceClient, *grpc.ClientConn, error) {
	conn, err := grpc.Dial(serverAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		return nil, nil, fmt.Errorf("failed to connect to server: %w", err)
	}

	client := pb.NewStageServiceClient(conn)
	return client, conn, nil
}

