package main

import (
	"log"
	"net/http"
	"time"

	"github.com/gin-gonic/gin"
	"walrus-wing/internal/database"
	"walrus-wing/internal/handlers"
	"walrus-wing/internal/physics"
	"walrus-wing/internal/simulation"
)

func main() {
	// Initialize database
	db, err := database.Initialize()
	if err != nil {
		log.Fatal("Failed to initialize database:", err)
	}

	// Initialize physics engine
	physicsEngine := physics.NewAerodynamicsEngine()

	// Initialize simulation manager
	simManager := simulation.NewSimulationManager(db, physicsEngine)

	// Start physics simulation loop
	go simManager.StartSimulation()

	// Initialize Gin router
	r := gin.Default()

	// CORS middleware for React frontend
	r.Use(func(c *gin.Context) {
		c.Header("Access-Control-Allow-Origin", "*")
		c.Header("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
		c.Header("Access-Control-Allow-Headers", "Content-Type, Authorization")
		
		if c.Request.Method == "OPTIONS" {
			c.AbortWithStatus(204)
			return
		}
		
		c.Next()
	})

	// Serve static files for React frontend
	r.Static("/static", "./frontend/build/static")
	r.StaticFile("/", "./frontend/build/index.html")
	r.StaticFile("/favicon.ico", "./frontend/build/favicon.ico")

	// WebSocket endpoint for real-time updates
	r.GET("/ws", handlers.HandleWebSocket)

	// API routes
	api := r.Group("/api/v1")
	{
		// Walrus routes
		walruses := api.Group("/walruses")
		{
			walruses.GET("", handlers.GetWalruses)
			walruses.POST("", handlers.CreateWalrus)
			walruses.GET("/:id", handlers.GetWalrus)
			walruses.PUT("/:id", handlers.UpdateWalrus)
			walruses.DELETE("/:id", handlers.DeleteWalrus)
			walruses.GET("/stats", handlers.GetWalrusStats)
		}

		// Physics routes
		physics := api.Group("/physics")
		{
			physics.GET("/walrus/:id", handlers.GetWalrusPhysics)
			physics.GET("/realtime", handlers.GetRealtimePhysics)
		}

		// Flight path routes
		paths := api.Group("/flightpaths")
		{
			paths.GET("/walrus/:id", handlers.GetWalrusFlightPath)
			paths.GET("/realtime", handlers.GetRealtimeFlightPaths)
		}

		// Simulation control
		simulation := api.Group("/simulation")
		{
			simulation.POST("/start", handlers.StartSimulation)
			simulation.POST("/stop", handlers.StopSimulation)
			simulation.POST("/reset", handlers.ResetSimulation)
			simulation.GET("/status", handlers.GetSimulationStatus)
		}
	}

	// Health check
	r.GET("/health", func(c *gin.Context) {
		c.JSON(200, gin.H{
			"status":    "healthy",
			"timestamp": time.Now(),
			"service":   "walrus-wing-api",
		})
	})

	// Start server
	log.Println("🦭 Walrus Wing Simulation Server starting on :8080")
	log.Println("🌐 Frontend: http://localhost:8080")
	log.Println("🔗 API: http://localhost:8080/api/v1")
	log.Println("📡 WebSocket: ws://localhost:8080/ws")
	
	log.Fatal(http.ListenAndServe(":8080", r))
}
