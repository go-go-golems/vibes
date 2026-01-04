package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"time"

	"github.com/redis/go-redis/v9"
	"github.com/spf13/cobra"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"

	pb "github.com/drone-workflow-platform/github.com/drone-workflow-platform/pkg/models"
)

var (
	workflowAddr string
	qualityAddr  string
	redisAddr    string
	timeout      time.Duration
)

func main() {
	var rootCmd = &cobra.Command{
		Use:   "monitor-cli",
		Short: "CLI for monitoring the drone workflow platform",
		Long:  "A command line interface for monitoring workflows, stages, and quality metrics",
	}

	// Global flags
	rootCmd.PersistentFlags().StringVar(&workflowAddr, "workflow-server", "localhost:50051", "Workflow service address")
	rootCmd.PersistentFlags().StringVar(&qualityAddr, "quality-server", "localhost:50053", "Quality service address")
	rootCmd.PersistentFlags().StringVar(&redisAddr, "redis", "localhost:6379", "Redis address")
	rootCmd.PersistentFlags().DurationVar(&timeout, "timeout", 30*time.Second, "Request timeout")

	// Add subcommands
	rootCmd.AddCommand(dashboardCmd())
	rootCmd.AddCommand(workflowStatusCmd())
	rootCmd.AddCommand(qualityReportCmd())
	rootCmd.AddCommand(systemHealthCmd())
	rootCmd.AddCommand(eventsCmd())
	rootCmd.AddCommand(metricsCmd())

	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func dashboardCmd() *cobra.Command {
	var refresh time.Duration

	cmd := &cobra.Command{
		Use:   "dashboard",
		Short: "Display real-time dashboard",
		RunE: func(cmd *cobra.Command, args []string) error {
			fmt.Printf("Drone Workflow Platform Dashboard\n")
			fmt.Printf("=================================\n\n")

			ticker := time.NewTicker(refresh)
			defer ticker.Stop()

			for {
				// Clear screen (simple version)
				fmt.Printf("\033[H\033[2J")
				fmt.Printf("Drone Workflow Platform Dashboard - %s\n", time.Now().Format("2006-01-02 15:04:05"))
				fmt.Printf("=================================\n\n")

				// Get system health
				health := getSystemHealth()
				fmt.Printf("System Health:\n")
				for service, status := range health {
					statusIcon := "❌"
					if status {
						statusIcon = "✅"
					}
					fmt.Printf("  %s %s\n", statusIcon, service)
				}
				fmt.Printf("\n")

				// Get workflow statistics
				stats := getWorkflowStatistics()
				fmt.Printf("Workflow Statistics:\n")
				fmt.Printf("  Total Workflows: %d\n", stats["total_workflows"])
				fmt.Printf("  Active Executions: %d\n", stats["active_executions"])
				fmt.Printf("  Completed Today: %d\n", stats["completed_today"])
				fmt.Printf("  Failed Today: %d\n", stats["failed_today"])
				fmt.Printf("\n")

				// Get quality metrics
				quality := getQualityMetrics()
				fmt.Printf("Quality Metrics:\n")
				fmt.Printf("  Overall Pass Rate: %.1f%%\n", quality["pass_rate"])
				fmt.Printf("  Total Checks Today: %d\n", quality["total_checks"])
				fmt.Printf("  Failed Checks: %d\n", quality["failed_checks"])
				fmt.Printf("\n")

				// Get recent events
				events := getRecentEvents(5)
				fmt.Printf("Recent Events:\n")
				for _, event := range events {
					fmt.Printf("  [%s] %s\n", event["timestamp"], event["message"])
				}

				fmt.Printf("\nPress Ctrl+C to exit\n")

				select {
				case <-ticker.C:
					continue
				}
			}
		},
	}

	cmd.Flags().DurationVar(&refresh, "refresh", 5*time.Second, "Dashboard refresh interval")

	return cmd
}

func workflowStatusCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "workflow-status [execution-id]",
		Short: "Get detailed workflow execution status",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			executionID := args[0]

			// Connect to Redis to get execution status
			redisClient := redis.NewClient(&redis.Options{
				Addr: redisAddr,
			})
			defer redisClient.Close()

			ctx := context.Background()

			// Get execution status
			statusKey := fmt.Sprintf("execution_status:%s", executionID)
			statusData, err := redisClient.Get(ctx, statusKey).Result()
			if err != nil {
				return fmt.Errorf("execution not found: %w", err)
			}

			var status map[string]interface{}
			err = json.Unmarshal([]byte(statusData), &status)
			if err != nil {
				return fmt.Errorf("failed to parse execution status: %w", err)
			}

			fmt.Printf("Workflow Execution Status\n")
			fmt.Printf("========================\n")
			fmt.Printf("Execution ID: %s\n", executionID)
			fmt.Printf("Workflow ID: %s\n", status["workflow_id"])
			fmt.Printf("Status: %s\n", status["status"])
			fmt.Printf("Started At: %s\n", status["started_at"])
			if completedAt, ok := status["completed_at"]; ok {
				fmt.Printf("Completed At: %s\n", completedAt)
			}
			fmt.Printf("\n")

			// Get stage tracking
			trackingKey := fmt.Sprintf("stage_tracking:%s", executionID)
			stageStatus, err := redisClient.HGetAll(ctx, trackingKey).Result()
			if err == nil && len(stageStatus) > 0 {
				fmt.Printf("Stage Progress:\n")
				fmt.Printf("%-20s %-15s\n", "Stage ID", "Status")
				fmt.Printf("%s\n", "-----------------------------------")
				for stageID, status := range stageStatus {
					statusIcon := getStatusIcon(status)
					fmt.Printf("%-20s %s %-15s\n", stageID, statusIcon, status)
				}
				fmt.Printf("\n")
			}

			return nil
		},
	}

	return cmd
}

func qualityReportCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "quality-report [execution-id]",
		Short: "Get quality report for an execution",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			client, conn, err := createQualityClient()
			if err != nil {
				return err
			}
			defer conn.Close()

			ctx, cancel := context.WithTimeout(context.Background(), timeout)
			defer cancel()

			resp, err := client.GetQualityReport(ctx, &pb.GetQualityReportRequest{
				ExecutionId: args[0],
			})
			if err != nil {
				return fmt.Errorf("failed to get quality report: %w", err)
			}

			report := resp.Report
			fmt.Printf("Quality Report\n")
			fmt.Printf("=============\n")
			fmt.Printf("Execution ID: %s\n", report.ExecutionId)
			fmt.Printf("Workflow ID: %s\n", report.WorkflowId)
			fmt.Printf("Overall Score: %.2f\n", report.OverallScore)
			fmt.Printf("Passed: %t\n", report.Passed)
			fmt.Printf("Generated At: %s\n", report.GeneratedAt)
			fmt.Printf("\n")

			if len(report.Results) > 0 {
				fmt.Printf("Quality Check Results:\n")
				fmt.Printf("%-15s %-12s %-8s %-20s %-30s\n", "Check ID", "Type", "Passed", "Inspector", "Criteria")
				fmt.Printf("%s\n", "-----------------------------------------------------------------------------------------")
				
				for _, result := range report.Results {
					passedStr := "FAIL"
					if result.Passed {
						passedStr = "PASS"
					}
					fmt.Printf("%-15s %-12s %-8s %-20s %-30s\n",
						result.CheckId,
						result.Type,
						passedStr,
						result.InspectorId,
						truncateString(result.Criteria, 30))
				}
			}

			return nil
		},
	}

	return cmd
}

func systemHealthCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "health",
		Short: "Check system health",
		RunE: func(cmd *cobra.Command, args []string) error {
			fmt.Printf("System Health Check\n")
			fmt.Printf("==================\n\n")

			health := getSystemHealth()
			allHealthy := true

			for service, status := range health {
				statusIcon := "❌ UNHEALTHY"
				if status {
					statusIcon = "✅ HEALTHY"
				} else {
					allHealthy = false
				}
				fmt.Printf("%-20s %s\n", service+":", statusIcon)
			}

			fmt.Printf("\n")
			if allHealthy {
				fmt.Printf("🎉 All services are healthy!\n")
			} else {
				fmt.Printf("⚠️  Some services are unhealthy. Please check the logs.\n")
			}

			return nil
		},
	}

	return cmd
}

func eventsCmd() *cobra.Command {
	var count int
	var eventType string

	cmd := &cobra.Command{
		Use:   "events",
		Short: "Show recent system events",
		RunE: func(cmd *cobra.Command, args []string) error {
			redisClient := redis.NewClient(&redis.Options{
				Addr: redisAddr,
			})
			defer redisClient.Close()

			ctx := context.Background()

			// Get event keys based on type
			var pattern string
			if eventType != "" {
				pattern = fmt.Sprintf("event:%s:*", eventType)
			} else {
				pattern = "event:*"
			}

			keys, err := redisClient.Keys(ctx, pattern).Result()
			if err != nil {
				return fmt.Errorf("failed to get events: %w", err)
			}

			fmt.Printf("Recent System Events\n")
			fmt.Printf("===================\n\n")

			if len(keys) == 0 {
				fmt.Printf("No events found.\n")
				return nil
			}

			// Limit the number of events shown
			if len(keys) > count {
				keys = keys[:count]
			}

			for _, key := range keys {
				eventData, err := redisClient.Get(ctx, key).Result()
				if err != nil {
					continue
				}

				var event map[string]interface{}
				err = json.Unmarshal([]byte(eventData), &event)
				if err != nil {
					continue
				}

				fmt.Printf("Event: %s\n", key)
				eventJSON, _ := json.MarshalIndent(event, "  ", "  ")
				fmt.Printf("  %s\n\n", eventJSON)
			}

			return nil
		},
	}

	cmd.Flags().IntVar(&count, "count", 10, "Number of events to show")
	cmd.Flags().StringVar(&eventType, "type", "", "Filter by event type (workflow_started, stage_completed, etc.)")

	return cmd
}

func metricsCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "metrics",
		Short: "Show system metrics",
		RunE: func(cmd *cobra.Command, args []string) error {
			fmt.Printf("System Metrics\n")
			fmt.Printf("=============\n\n")

			// Workflow metrics
			workflowStats := getWorkflowStatistics()
			fmt.Printf("Workflow Metrics:\n")
			for key, value := range workflowStats {
				fmt.Printf("  %s: %v\n", key, value)
			}
			fmt.Printf("\n")

			// Quality metrics
			qualityStats := getQualityMetrics()
			fmt.Printf("Quality Metrics:\n")
			for key, value := range qualityStats {
				fmt.Printf("  %s: %v\n", key, value)
			}
			fmt.Printf("\n")

			// Redis metrics
			redisClient := redis.NewClient(&redis.Options{
				Addr: redisAddr,
			})
			defer redisClient.Close()

			ctx := context.Background()
			info, err := redisClient.Info(ctx, "memory").Result()
			if err == nil {
				fmt.Printf("Redis Metrics:\n")
				fmt.Printf("  %s\n", info)
			}

			return nil
		},
	}

	return cmd
}

// Helper functions

func getSystemHealth() map[string]bool {
	health := make(map[string]bool)

	// Check workflow service
	conn, err := grpc.Dial(workflowAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err == nil {
		conn.Close()
		health["Workflow Service"] = true
	} else {
		health["Workflow Service"] = false
	}

	// Check quality service
	conn, err = grpc.Dial(qualityAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err == nil {
		conn.Close()
		health["Quality Service"] = true
	} else {
		health["Quality Service"] = false
	}

	// Check Redis
	redisClient := redis.NewClient(&redis.Options{
		Addr: redisAddr,
	})
	defer redisClient.Close()

	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()

	_, err = redisClient.Ping(ctx).Result()
	health["Redis"] = err == nil

	return health
}

func getWorkflowStatistics() map[string]int {
	// Simplified statistics - in a real implementation, this would query the services
	return map[string]int{
		"total_workflows":   5,
		"active_executions": 2,
		"completed_today":   8,
		"failed_today":      1,
	}
}

func getQualityMetrics() map[string]float64 {
	// Simplified metrics - in a real implementation, this would query the quality service
	return map[string]float64{
		"pass_rate":      94.5,
		"total_checks":   156,
		"failed_checks":  9,
	}
}

func getRecentEvents(count int) []map[string]string {
	// Simplified events - in a real implementation, this would query Redis
	return []map[string]string{
		{"timestamp": "15:30:45", "message": "Workflow QC-200 execution completed successfully"},
		{"timestamp": "15:28:12", "message": "Stage motor_mount started for execution exec_123"},
		{"timestamp": "15:25:33", "message": "Quality check passed for stage frame_prep"},
		{"timestamp": "15:22:18", "message": "New workflow created: QuadCopter_Basic_Assembly"},
		{"timestamp": "15:20:05", "message": "System health check completed - all services healthy"},
	}
}

func getStatusIcon(status string) string {
	switch status {
	case "completed":
		return "✅"
	case "failed":
		return "❌"
	case "in_progress":
		return "🔄"
	case "pending":
		return "⏳"
	default:
		return "❓"
	}
}

func truncateString(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen-3] + "..."
}

func createQualityClient() (pb.QualityServiceClient, *grpc.ClientConn, error) {
	conn, err := grpc.Dial(qualityAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		return nil, nil, fmt.Errorf("failed to connect to quality service: %w", err)
	}

	client := pb.NewQualityServiceClient(conn)
	return client, conn, nil
}

