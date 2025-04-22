package main

import (
	"log"
	"net/http"
	"os"
	"os/signal"
	"path/filepath"
	"syscall"

	"github.com/blackboard-system/internal/api"
	"github.com/blackboard-system/internal/blackboard"
	"github.com/blackboard-system/internal/control"
	"github.com/blackboard-system/internal/database"
	"github.com/blackboard-system/internal/agents"
)

func main() {
	// Set up database
	dbPath := filepath.Join(".", "data", "blackboard.db")
	log.Printf("Initializing database at %s", dbPath)
	
	if err := database.InitDB(dbPath); err != nil {
		log.Fatalf("Failed to initialize database: %v", err)
	}
	defer database.CloseDB()
	
	// Initialize default data
	if err := database.InitializeDefaultData(); err != nil {
		log.Fatalf("Failed to initialize default data: %v", err)
	}
	
	// Create blackboard
	bb, err := blackboard.NewBlackboard()
	if err != nil {
		log.Fatalf("Failed to create blackboard: %v", err)
	}
	
	// Create controller
	ctrl := control.NewController(bb)
	
	// Register knowledge sources (agents)
	ctrl.RegisterKnowledgeSource(agents.NewSegmentCreator())
	ctrl.RegisterKnowledgeSource(agents.NewPhoneToSyllable())
	ctrl.RegisterKnowledgeSource(agents.NewSyllableToWord())
	ctrl.RegisterKnowledgeSource(agents.NewSyntaxParser())
	ctrl.RegisterKnowledgeSource(agents.NewWordPredictor())
	ctrl.RegisterKnowledgeSource(agents.NewHypothesisRater())
	
	// Create API server
	server := api.NewServer(bb)
	
	// Start controller
	ctrl.Start()
	
	// Start server
	go func() {
		log.Println("Starting server on :8080")
		if err := http.ListenAndServe(":8080", server.Handler()); err != nil {
			log.Fatalf("Failed to start server: %v", err)
		}
	}()
	
	// Wait for interrupt signal
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)
	<-sigChan
	
	// Shutdown
	log.Println("Shutting down...")
	ctrl.Stop()
}
