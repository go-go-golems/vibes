package main

import (
	"context"
	"database/sql"
	"fmt"
	"html/template"
	"log"
	"net/http"
	"os"
	"os/signal"

	"syscall"
	"time"

	"pelican-farm/internal/handlers"

	"github.com/gorilla/mux"
	_ "github.com/mattn/go-sqlite3"
)

type Server struct {
	db             *sql.DB
	router         *mux.Router
	pelicanHandler *handlers.PelicanHandler
	feedingHandler *handlers.FeedingHandler
	healthHandler  *handlers.HealthHandler
}

func main() {
	db, err := initDatabase()
	if err != nil {
		log.Fatal("Failed to initialize database:", err)
	}
	defer db.Close()

	server := &Server{
		db:             db,
		router:         mux.NewRouter(),
		pelicanHandler: handlers.NewPelicanHandler(db),
		feedingHandler: handlers.NewFeedingHandler(db),
		healthHandler:  handlers.NewHealthHandler(db),
	}

	server.setupRoutes()
	server.setupMiddleware()

	httpServer := &http.Server{
		Addr:         ":8080",
		Handler:      server.router,
		ReadTimeout:  15 * time.Second,
		WriteTimeout: 15 * time.Second,
		IdleTimeout:  60 * time.Second,
	}

	go func() {
		fmt.Println("Pelican Farm Management System starting on :8080")
		if err := httpServer.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Fatal("Server failed to start:", err)
		}
	}()

	// Graceful shutdown
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
	<-quit
	log.Println("Shutting down server...")

	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	if err := httpServer.Shutdown(ctx); err != nil {
		log.Fatal("Server forced to shutdown:", err)
	}
	log.Println("Server exited")
}

func initDatabase() (*sql.DB, error) {
	db, err := sql.Open("sqlite3", "./pelican-farm.db")
	if err != nil {
		return nil, err
	}

	if err := db.Ping(); err != nil {
		return nil, err
	}

	// Run migrations
	if err := runMigrations(db); err != nil {
		return nil, fmt.Errorf("failed to run migrations: %w", err)
	}

	return db, nil
}

func runMigrations(db *sql.DB) error {
	migrationFiles := []string{
		"migrations/001_create_pelicans_table.sql",
		"migrations/002_create_feeding_records_table.sql",
		"migrations/003_create_health_checks_table.sql",
		"migrations/004_create_feeding_schedules_table.sql",
		"migrations/005_insert_sample_data.sql",
	}

	for _, file := range migrationFiles {
		if err := executeMigration(db, file); err != nil {
			return fmt.Errorf("failed to execute migration %s: %w", file, err)
		}
		log.Printf("Executed migration: %s", file)
	}

	return nil
}

func executeMigration(db *sql.DB, filename string) error {
	content, err := os.ReadFile(filename)
	if err != nil {
		// Skip if file doesn't exist
		if os.IsNotExist(err) {
			log.Printf("Migration file %s not found, skipping", filename)
			return nil
		}
		return err
	}

	_, err = db.Exec(string(content))
	return err
}

func (s *Server) setupMiddleware() {
	s.router.Use(loggingMiddleware)
	s.router.Use(recoveryMiddleware)
	s.router.Use(corsMiddleware)
}

func (s *Server) setupRoutes() {
	// Dashboard/Home
	s.router.HandleFunc("/", s.handleDashboard).Methods("GET")

	// Pelican routes
	s.router.HandleFunc("/pelicans", s.pelicanHandler.ListPelicans).Methods("GET")
	s.router.HandleFunc("/pelicans/new", s.pelicanHandler.NewPelican).Methods("GET")
	s.router.HandleFunc("/pelicans", s.pelicanHandler.CreatePelican).Methods("POST")
	s.router.HandleFunc("/pelicans/{id:[0-9]+}", s.pelicanHandler.ShowPelican).Methods("GET")
	s.router.HandleFunc("/pelicans/{id:[0-9]+}/edit", s.pelicanHandler.EditPelican).Methods("GET")
	s.router.HandleFunc("/pelicans/{id:[0-9]+}", s.pelicanHandler.UpdatePelican).Methods("PUT", "POST")
	s.router.HandleFunc("/pelicans/{id:[0-9]+}", s.pelicanHandler.DeletePelican).Methods("DELETE", "POST")

	// Feeding routes
	s.router.HandleFunc("/feedings", s.feedingHandler.ListFeedings).Methods("GET")
	s.router.HandleFunc("/feedings/new", s.feedingHandler.NewFeeding).Methods("GET")
	s.router.HandleFunc("/feedings", s.feedingHandler.CreateFeeding).Methods("POST")
	s.router.HandleFunc("/feedings/{id:[0-9]+}", s.feedingHandler.ShowFeeding).Methods("GET")
	s.router.HandleFunc("/feedings/{id:[0-9]+}/edit", s.feedingHandler.EditFeeding).Methods("GET")
	s.router.HandleFunc("/feedings/{id:[0-9]+}", s.feedingHandler.UpdateFeeding).Methods("PUT", "POST")
	s.router.HandleFunc("/feedings/{id:[0-9]+}", s.feedingHandler.DeleteFeeding).Methods("DELETE", "POST")
	s.router.HandleFunc("/feedings/schedules", s.feedingHandler.ScheduledFeedings).Methods("GET")

	// Health check routes
	s.router.HandleFunc("/health", s.healthHandler.ListHealthChecks).Methods("GET")
	s.router.HandleFunc("/health/new", s.healthHandler.NewHealthCheck).Methods("GET")
	s.router.HandleFunc("/health", s.healthHandler.CreateHealthCheck).Methods("POST")
	s.router.HandleFunc("/health/{id:[0-9]+}", s.healthHandler.ShowHealthCheck).Methods("GET")
	s.router.HandleFunc("/health/{id:[0-9]+}/edit", s.healthHandler.EditHealthCheck).Methods("GET")
	s.router.HandleFunc("/health/{id:[0-9]+}", s.healthHandler.UpdateHealthCheck).Methods("PUT", "POST")
	s.router.HandleFunc("/health/{id:[0-9]+}", s.healthHandler.DeleteHealthCheck).Methods("DELETE", "POST")

	// Static files
	s.router.PathPrefix("/static/").Handler(http.StripPrefix("/static/", http.FileServer(http.Dir("./static/"))))
}

// Middleware functions
func loggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		start := time.Now()
		next.ServeHTTP(w, r)
		log.Printf("%s %s %v", r.Method, r.URL.Path, time.Since(start))
	})
}

func recoveryMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			if err := recover(); err != nil {
				log.Printf("Panic recovered: %v", err)
				http.Error(w, "Internal Server Error", http.StatusInternalServerError)
			}
		}()
		next.ServeHTTP(w, r)
	})
}

func corsMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
		w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization")

		if r.Method == "OPTIONS" {
			w.WriteHeader(http.StatusOK)
			return
		}

		next.ServeHTTP(w, r)
	})
}

// Route handlers
func (s *Server) handleDashboard(w http.ResponseWriter, r *http.Request) {
	// Get stats for dashboard
	var pelicanCount int
	err := s.db.QueryRow("SELECT COUNT(*) FROM pelicans").Scan(&pelicanCount)
	if err != nil {
		http.Error(w, "Failed to get pelican count", http.StatusInternalServerError)
		return
	}

	var feedingCount int
	err = s.db.QueryRow("SELECT COUNT(*) FROM feeding_records WHERE DATE(feeding_time) = DATE('now')").Scan(&feedingCount)
	if err != nil {
		feedingCount = 0 // Default if query fails
	}

	var healthCount int
	err = s.db.QueryRow("SELECT COUNT(*) FROM health_checks WHERE DATE(check_date) >= DATE('now', '-7 days')").Scan(&healthCount)
	if err != nil {
		healthCount = 0 // Default if query fails
	}

	// Check if templates exist, otherwise use inline HTML
	tmplPath := "templates/dashboard.html"
	if _, err := os.Stat(tmplPath); err == nil {
		tmpl, err := template.ParseFiles(tmplPath)
		if err != nil {
			http.Error(w, "Template error: "+err.Error(), http.StatusInternalServerError)
			return
		}
		
		data := map[string]interface{}{
			"PelicanCount": pelicanCount,
			"FeedingCount": feedingCount,
			"HealthCount":  healthCount,
		}
		
		w.Header().Set("Content-Type", "text/html")
		tmpl.Execute(w, data)
		return
	}

	// Fallback inline HTML
	w.Header().Set("Content-Type", "text/html")
	fmt.Fprintf(w, `
<!DOCTYPE html>
<html>
<head>
    <title>Pelican Farm Management</title>
    <link rel="stylesheet" href="/static/style.css">
    <meta name="viewport" content="width=device-width, initial-scale=1">
</head>
<body>
    <h1>Pelican Farm Management System</h1>
    <div class="dashboard-stats">
        <div class="stat-card">
            <h3>Total Pelicans</h3>
            <p class="stat-number">%d</p>
        </div>
        <div class="stat-card">
            <h3>Feedings Today</h3>
            <p class="stat-number">%d</p>
        </div>
        <div class="stat-card">
            <h3>Health Checks (Last 7 Days)</h3>
            <p class="stat-number">%d</p>
        </div>
    </div>
    <nav class="main-nav">
        <a href="/pelicans" class="nav-button">View All Pelicans</a>
        <a href="/pelicans/new" class="nav-button">Add New Pelican</a>
        <a href="/feedings" class="nav-button">Feeding Records</a>
        <a href="/feedings/new" class="nav-button">Record Feeding</a>
        <a href="/health" class="nav-button">Health Checks</a>
        <a href="/health/new" class="nav-button">New Health Check</a>
    </nav>
</body>
</html>`, pelicanCount, feedingCount, healthCount)
}


