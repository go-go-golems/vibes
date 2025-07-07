package main

import (
	"context"
	"fmt"
	"log"
	"net"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/ThreeDotsLabs/watermill"
	"github.com/redis/go-redis/v9"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"

	pb "github.com/drone-workflow-platform/github.com/drone-workflow-platform/pkg/models"
	"github.com/drone-workflow-platform/internal/messaging"
	"github.com/drone-workflow-platform/internal/workflow"
)

const (
	defaultPort = "50051"
	defaultRedisAddr = "localhost:6379"
)

func main() {
	// Get configuration from environment variables
	port := os.Getenv("PORT")
	if port == "" {
		port = defaultPort
	}

	redisAddr := os.Getenv("REDIS_ADDR")
	if redisAddr == "" {
		redisAddr = defaultRedisAddr
	}

	redisPassword := os.Getenv("REDIS_PASSWORD")
	redisDB := 0

	// Create logger
	logger := watermill.NewStdLogger(false, false)

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
		log.Fatalf("Failed to connect to Redis: %v", err)
	}
	logger.Info("Connected to Redis", watermill.LogFields{"addr": redisAddr})

	// Create messaging service
	messagingService, err := messaging.NewSimpleService(redisAddr, redisPassword, redisDB, logger)
	if err != nil {
		log.Fatalf("Failed to create messaging service: %v", err)
	}
	defer messagingService.Close()

	// Create workflow service
	workflowService := workflow.NewService(messagingService, redisClient, logger)

	// Create gRPC server
	lis, err := net.Listen("tcp", ":"+port)
	if err != nil {
		log.Fatalf("Failed to listen: %v", err)
	}

	grpcServer := grpc.NewServer()
	pb.RegisterWorkflowServiceServer(grpcServer, workflowService)

	// Enable reflection for testing
	reflection.Register(grpcServer)

	// Start server in a goroutine
	go func() {
		logger.Info("Starting workflow service", watermill.LogFields{
			"port": port,
			"addr": lis.Addr().String(),
		})
		
		if err := grpcServer.Serve(lis); err != nil {
			log.Fatalf("Failed to serve: %v", err)
		}
	}()

	// Wait for interrupt signal to gracefully shutdown
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
	<-quit

	logger.Info("Shutting down workflow service", nil)

	// Graceful shutdown
	grpcServer.GracefulStop()
	logger.Info("Workflow service stopped", nil)
}

// Helper function to create a publisher for the workflow service
func createPublisher(redisAddr, redisPassword string, redisDB int, logger watermill.LoggerAdapter) (*messaging.Service, error) {
	return messaging.NewService(redisAddr, redisPassword, redisDB, logger)
}

