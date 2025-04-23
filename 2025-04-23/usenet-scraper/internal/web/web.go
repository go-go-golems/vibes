package web

import (
	"context"
	"fmt"
	"net/http"
	"sync"
	"time"

	"github.com/gin-gonic/gin"
	"github.com/usenet-scraper/internal/config"
	"github.com/usenet-scraper/internal/parser"
	"github.com/usenet-scraper/internal/scraper"
)

// Server represents a web interface server
type Server struct {
	config     *config.WebInterfaceConfig
	router     *gin.Engine
	httpServer *http.Server
	scraper    *scraper.Scraper
	parser     *parser.Parser
	wg         sync.WaitGroup
	mu         sync.Mutex
	jobs       map[string]*Job
}

// Job represents a scraping job
type Job struct {
	ID        string
	Status    string
	StartTime time.Time
	EndTime   time.Time
	Config    *config.Config
	Posts     int
	Errors    int
}

// NewServer creates a new web interface server
func NewServer(cfg *config.WebInterfaceConfig, scraper *scraper.Scraper, parser *parser.Parser) *Server {
	// Set up Gin router
	gin.SetMode(gin.ReleaseMode)
	router := gin.Default()

	server := &Server{
		config:  cfg,
		router:  router,
		scraper: scraper,
		parser:  parser,
		jobs:    make(map[string]*Job),
	}

	// Set up routes
	server.setupRoutes()

	return server
}

// setupRoutes sets up the HTTP routes
func (s *Server) setupRoutes() {
	// Add authentication middleware if configured
	if s.config.Auth != nil {
		s.router.Use(gin.BasicAuth(gin.Accounts{
			s.config.Auth.Username: s.config.Auth.Password,
		}))
	}

	// API routes
	api := s.router.Group("/api")
	{
		api.GET("/status", s.getStatus)
		api.POST("/jobs", s.createJob)
		api.GET("/jobs", s.listJobs)
		api.GET("/jobs/:id", s.getJob)
		api.DELETE("/jobs/:id", s.deleteJob)
	}

	// Serve static files for the web UI
	s.router.Static("/", "./web/static")
	s.router.NoRoute(func(c *gin.Context) {
		c.File("./web/static/index.html")
	})
}

// Start starts the web interface server
func (s *Server) Start() error {
	addr := fmt.Sprintf("%s:%d", s.config.Host, s.config.Port)
	s.httpServer = &http.Server{
		Addr:    addr,
		Handler: s.router,
	}

	s.wg.Add(1)
	go func() {
		defer s.wg.Done()
		if err := s.httpServer.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			fmt.Printf("Error starting web server: %v\n", err)
		}
	}()

	fmt.Printf("Web interface started at http://%s\n", addr)
	return nil
}

// Stop stops the web interface server
func (s *Server) Stop() error {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	if err := s.httpServer.Shutdown(ctx); err != nil {
		return err
	}

	s.wg.Wait()
	return nil
}

// getStatus handles the status endpoint
func (s *Server) getStatus(c *gin.Context) {
	c.JSON(http.StatusOK, gin.H{
		"status": "running",
		"jobs":   len(s.jobs),
	})
}

// createJob handles the create job endpoint
func (s *Server) createJob(c *gin.Context) {
	var cfg config.Config
	if err := c.ShouldBindJSON(&cfg); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
		return
	}

	// Validate the configuration
	if err := validateConfig(&cfg); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
		return
	}

	// Create a new job
	job := &Job{
		ID:        fmt.Sprintf("job-%d", time.Now().Unix()),
		Status:    "running",
		StartTime: time.Now(),
		Config:    &cfg,
	}

	// Store the job
	s.mu.Lock()
	s.jobs[job.ID] = job
	s.mu.Unlock()

	// Start the job in a goroutine
	go s.runJob(job)

	c.JSON(http.StatusCreated, gin.H{
		"id":     job.ID,
		"status": job.Status,
	})
}

// listJobs handles the list jobs endpoint
func (s *Server) listJobs(c *gin.Context) {
	s.mu.Lock()
	defer s.mu.Unlock()

	jobs := make([]gin.H, 0, len(s.jobs))
	for _, job := range s.jobs {
		jobs = append(jobs, gin.H{
			"id":         job.ID,
			"status":     job.Status,
			"start_time": job.StartTime,
			"end_time":   job.EndTime,
			"posts":      job.Posts,
			"errors":     job.Errors,
		})
	}

	c.JSON(http.StatusOK, jobs)
}

// getJob handles the get job endpoint
func (s *Server) getJob(c *gin.Context) {
	id := c.Param("id")

	s.mu.Lock()
	job, ok := s.jobs[id]
	s.mu.Unlock()

	if !ok {
		c.JSON(http.StatusNotFound, gin.H{"error": "Job not found"})
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"id":         job.ID,
		"status":     job.Status,
		"start_time": job.StartTime,
		"end_time":   job.EndTime,
		"posts":      job.Posts,
		"errors":     job.Errors,
		"config":     job.Config,
	})
}

// deleteJob handles the delete job endpoint
func (s *Server) deleteJob(c *gin.Context) {
	id := c.Param("id")

	s.mu.Lock()
	job, ok := s.jobs[id]
	if !ok {
		s.mu.Unlock()
		c.JSON(http.StatusNotFound, gin.H{"error": "Job not found"})
		return
	}

	// Only allow deleting completed jobs
	if job.Status == "running" {
		s.mu.Unlock()
		c.JSON(http.StatusBadRequest, gin.H{"error": "Cannot delete a running job"})
		return
	}

	delete(s.jobs, id)
	s.mu.Unlock()

	c.JSON(http.StatusOK, gin.H{"status": "deleted"})
}

// runJob runs a scraping job
func (s *Server) runJob(job *Job) {
	// In a real implementation, this would create a scraper and run it
	// For demonstration purposes, we'll just simulate a job
	time.Sleep(2 * time.Second)

	// Update the job status
	s.mu.Lock()
	job.Status = "completed"
	job.EndTime = time.Now()
	job.Posts = 100
	job.Errors = 0
	s.mu.Unlock()
}

// validateConfig validates a configuration
func validateConfig(config *config.Config) error {
	// Validate connection
	if config.Connection.Server == "" {
		return fmt.Errorf("connection.server is required")
	}

	// Validate sources
	if len(config.Sources) == 0 {
		return fmt.Errorf("at least one source is required")
	}
	for i, source := range config.Sources {
		if source.Group == "" {
			return fmt.Errorf("sources[%d].group is required", i)
		}
	}

	// Validate output
	if config.Output.Format == "" {
		return fmt.Errorf("output.format is required")
	}
	if config.Output.File == "" {
		return fmt.Errorf("output.file is required")
	}

	return nil
}
