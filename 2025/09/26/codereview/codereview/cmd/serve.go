package cmd

import (
	"embed"
	"encoding/json"
	"fmt"
	"io/fs"
	"net/http"
	"os/exec"
	"runtime"

	"strings"

	"github.com/spf13/cobra"
	"github.com/codereview/cli/internal/database"
	"github.com/codereview/cli/internal/models"
)

//go:embed all:web/dist
var webFiles embed.FS

func newServeCommand() *cobra.Command {
	var port int
	var host string
	var open bool
	var dev bool

	cmd := &cobra.Command{
		Use:   "serve",
		Short: "Start the web server",
		Long:  "Start the web server to serve the React frontend and API",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runServe(port, host, open, dev)
		},
	}

	cmd.Flags().IntVar(&port, "port", 8080, "Port to listen on")
	cmd.Flags().StringVar(&host, "host", "localhost", "Host to bind to")
	cmd.Flags().BoolVar(&open, "open", false, "Open browser automatically")
	cmd.Flags().BoolVar(&dev, "dev", false, "Development mode (serve from filesystem)")

	return cmd
}

func runServe(port int, host string, openBrowser bool, dev bool) error {
	// Initialize database
	db, err := database.New(".codereview/reviews.db")
	if err != nil {
		return fmt.Errorf("failed to initialize database: %v", err)
	}
	defer db.Close()

	// Setup routes
	mux := http.NewServeMux()

	// API routes
	mux.HandleFunc("/api/reviews", handleReviews(db))
	mux.HandleFunc("/api/reviews/", handleReviewDetail(db))
	mux.HandleFunc("/api/annotations", handleAnnotations(db))

	// Static file serving
	if dev {
		// In development, serve from filesystem
		fmt.Println("📝 Development mode: serving from filesystem")
		mux.Handle("/", http.FileServer(http.Dir("../codereview-frontend/dist/")))
	} else {
		// In production, serve from embedded files
		webFS, err := fs.Sub(webFiles, "web/dist")
		if err != nil {
			// Fallback to filesystem if embedded files not found
			fmt.Println("⚠️  Embedded files not found, serving from filesystem")
			mux.Handle("/", http.FileServer(http.Dir("cmd/web/dist/")))
		} else {
			fmt.Println("📦 Serving from embedded files")
			mux.Handle("/", http.FileServer(http.FS(webFS)))
		}
	}

	addr := fmt.Sprintf("%s:%d", host, port)
	url := fmt.Sprintf("http://%s", addr)
	
	fmt.Printf("🚀 Code Review server starting on %s\n", url)
	if dev {
		fmt.Println("📝 Running in development mode")
	}

	// Open browser if requested
	if openBrowser {
		go func() {
			if err := openURL(url); err != nil {
				fmt.Printf("Failed to open browser: %v\n", err)
			}
		}()
	}
	
	return http.ListenAndServe(addr, corsMiddleware(mux))
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

func handleReviews(db *database.DB) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		
		switch r.Method {
		case "GET":
			reviews, err := db.ListReviews("")
			if err != nil {
				http.Error(w, fmt.Sprintf("Failed to list reviews: %v", err), http.StatusInternalServerError)
				return
			}
			
			// Convert to JSON-friendly format
			var response []map[string]interface{}
			for _, review := range reviews {
				annotations, _ := db.GetAnnotationsForReview(review.ID)
				
				// Count annotation types
				issues := 0
				suggestions := 0
				critical := 0
				for _, ann := range annotations {
					if ann.Type == "issue" {
						issues++
					}
					if ann.Type == "suggestion" {
						suggestions++
					}
					if ann.Severity == "critical" {
						critical++
					}
				}
				
				reviewData := map[string]interface{}{
					"id":           review.ID,
					"title":        review.Title,
					"branch":       review.Branch,
					"commit":       review.Commit,
					"baseCommit":   review.BaseCommit,
					"reviewer":     review.Reviewer,
					"status":       review.Status,
					"filesChanged": review.FilesChanged,
					"created":      review.Created.Format("2006-01-02T15:04:05Z"),
					"annotations":  convertAnnotations(annotations),
					"stats": map[string]interface{}{
						"total":       len(annotations),
						"issues":      issues,
						"suggestions": suggestions,
						"critical":    critical,
					},
				}
				response = append(response, reviewData)
			}
			
			json.NewEncoder(w).Encode(map[string]interface{}{
				"reviews": response,
			})
			
		default:
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		}
	}
}

func handleReviewDetail(db *database.DB) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		
		// Extract review ID from path
		path := strings.TrimPrefix(r.URL.Path, "/api/reviews/")
		reviewID := strings.Split(path, "/")[0]
		
		if reviewID == "" {
			http.Error(w, "Review ID required", http.StatusBadRequest)
			return
		}
		
		switch r.Method {
		case "GET":
			review, err := db.GetReview(reviewID)
			if err != nil {
				http.Error(w, fmt.Sprintf("Review not found: %v", err), http.StatusNotFound)
				return
			}
			
			annotations, err := db.GetAnnotationsForReview(reviewID)
			if err != nil {
				http.Error(w, fmt.Sprintf("Failed to get annotations: %v", err), http.StatusInternalServerError)
				return
			}
			
			// Get file list - for now use mock data
			files := []string{
				"app.js",
				"package.json", 
				"src/services/UserService.js",
				"src/components/UserComponent.jsx",
				"src/utils/validation.js",
				"src/config/api.js",
			}
			
			reviewData := map[string]interface{}{
				"id":           review.ID,
				"title":        review.Title,
				"branch":       review.Branch,
				"commit":       review.Commit,
				"baseCommit":   review.BaseCommit,
				"reviewer":     review.Reviewer,
				"status":       review.Status,
				"filesChanged": review.FilesChanged,
				"created":      review.Created.Format("2006-01-02T15:04:05Z"),
				"files":        files,
				"annotations":  convertAnnotations(annotations),
			}
			
			json.NewEncoder(w).Encode(reviewData)
			
		default:
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		}
	}
}

func handleAnnotations(db *database.DB) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		
		switch r.Method {
		case "GET":
			reviewID := r.URL.Query().Get("review")
			if reviewID == "" {
				http.Error(w, "Review ID required", http.StatusBadRequest)
				return
			}
			
			annotations, err := db.GetAnnotationsForReview(reviewID)
			if err != nil {
				http.Error(w, fmt.Sprintf("Failed to get annotations: %v", err), http.StatusInternalServerError)
				return
			}
			
			json.NewEncoder(w).Encode(map[string]interface{}{
				"annotations": convertAnnotations(annotations),
			})
			
		default:
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		}
	}
}

func convertAnnotations(annotations []*models.Annotation) []map[string]interface{} {
	var result []map[string]interface{}
	for _, ann := range annotations {
		data := map[string]interface{}{
			"id":       ann.ID,
			"file":     ann.File,
			"type":     ann.Type,
			"severity": ann.Severity,
			"message":  ann.Message,
			"status":   ann.Status,
			"created":  ann.Created.Format("2006-01-02T15:04:05Z"),
		}
		
		if ann.Line != nil {
			data["line"] = *ann.Line
		}
		
		if ann.Suggestion != "" {
			data["suggestion"] = ann.Suggestion
		}
		
		result = append(result, data)
	}
	return result
}

func openURL(url string) error {
	var cmd string
	var args []string

	switch runtime.GOOS {
	case "windows":
		cmd = "cmd"
		args = []string{"/c", "start"}
	case "darwin":
		cmd = "open"
	default: // "linux", "freebsd", "openbsd", "netbsd"
		cmd = "xdg-open"
	}
	args = append(args, url)
	return exec.Command(cmd, args...).Start()
}
