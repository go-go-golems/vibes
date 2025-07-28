package main

import (
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"
	"path/filepath"

	"bubbletea-todo-app/internal/websocket"
)

func main() {
	// Define command-line flags
	var port string
	flag.StringVar(&port, "port", "", "Port to listen on (default: 8080)")
	flag.StringVar(&port, "p", "", "Port to listen on (shorthand)")
	flag.Parse()

	// Check environment variable if flag not provided
	if port == "" {
		port = os.Getenv("PORT")
	}

	// Default to 8080 if neither flag nor env var is set
	if port == "" {
		port = "8080"
	}

	// Serve static files
	staticDir := filepath.Join("web", "static")
	http.Handle("/static/", http.StripPrefix("/static/", http.FileServer(http.Dir(staticDir))))

	// Enhanced WebSocket endpoint
	http.HandleFunc("/ws", websocket.HandleEnhancedWebSocket)

	// Main page - serve the full-screen terminal interface
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/" {
			http.NotFound(w, r)
			return
		}
		http.ServeFile(w, r, filepath.Join("web", "static", "index-fullscreen.html"))
	})

	// Enhanced interface endpoint
	http.HandleFunc("/enhanced", func(w http.ResponseWriter, r *http.Request) {
		http.ServeFile(w, r, filepath.Join("web", "static", "index-enhanced.html"))
	})

	// Health check endpoint
	http.HandleFunc("/health", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusOK)
		w.Write([]byte(`{"status":"ok","service":"bubbletea-todo-enhanced"}`))
	})

	fmt.Printf("🫧 Starting enhanced Bubbletea Todo web server\n")
	fmt.Printf("🌐 Full-screen terminal: http://0.0.0.0:%s\n", port)
	fmt.Printf("🎨 Enhanced interface:   http://0.0.0.0:%s/enhanced\n", port)
	fmt.Printf("💚 Health check:        http://0.0.0.0:%s/health\n", port)
	fmt.Println("✨ Features: ANSI colors, xterm.js, full-screen terminal")
	fmt.Printf("🚀 Ready to serve!\n")
	fmt.Printf("\n💡 Configuration options:\n")
	fmt.Printf("   --port/-p <port>  Set custom port\n")
	fmt.Printf("   PORT=<port>       Set port via environment variable\n")
	fmt.Printf("\n📖 Usage examples:\n")
	fmt.Printf("   ./web-app-enhanced --port 3000\n")
	fmt.Printf("   ./web-app-enhanced -p 9090\n")
	fmt.Printf("   PORT=5000 ./web-app-enhanced\n")
	
	log.Fatal(http.ListenAndServe("0.0.0.0:"+port, nil))
}

