package main

import (
	"log"
	"net/http"

	"github.com/gin-gonic/gin"
	"pelican-farm/internal/database"
	"pelican-farm/internal/handlers"
)

func main() {
	// Initialize database
	db, err := database.Initialize()
	if err != nil {
		log.Fatal("Failed to initialize database:", err)
	}

	// Initialize Gin router
	r := gin.Default()
	
	// Load HTML templates
	r.LoadHTMLGlob("templates/*")
	r.Static("/static", "./static")

	// Initialize handlers
	handlers.SetupRoutes(r, db)

	// Start server
	log.Println("🐦 Pelican Farm Management Server starting on :8080")
	log.Fatal(http.ListenAndServe(":8080", r))
}
