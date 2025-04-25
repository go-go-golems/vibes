package server

import (
	"embed"
	"io/fs"
	"net/http"
	"path/filepath"
)

//go:embed static
var staticFiles embed.FS

// AddStaticRoutes adds routes for serving static files
func (s *Server) AddStaticRoutes() {
	// Serve static files
	staticFS, err := fs.Sub(staticFiles, "static")
	if err != nil {
		panic("Failed to load static files")
	}

	// Serve static files
	s.router.PathPrefix("/static/").Handler(http.StripPrefix("/static/", http.FileServer(http.FS(staticFS))))

	// Serve index.html for the root path
	s.router.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path == "/" {
			// Redirect to the web app
			http.Redirect(w, r, "/static/index.html", http.StatusTemporaryRedirect)
			return
		}

		// Try to serve static files if the path is not the root
		http.StripPrefix("/", http.FileServer(http.FS(staticFS))).ServeHTTP(w, r)
	})

	// Serve index.html for any other path that doesn't match a route or static file
	s.router.NotFoundHandler = http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// Check if the path corresponds to a file under the documents base path
		requestPath := r.URL.Path
		if requestPath == "" || requestPath == "/" {
			http.Redirect(w, r, "/static/index.html", http.StatusTemporaryRedirect)
			return
		}

		// Check if the path corresponds to an API endpoint
		if filepath.HasPrefix(requestPath, "/api/") {
			http.NotFound(w, r)
			return
		}

		// For any other path, redirect to the index.html page (SPA behavior)
		http.Redirect(w, r, "/static/index.html", http.StatusTemporaryRedirect)
	})
}