package main

import (
	"fmt"
	"log"
	"net/http"
	"path/filepath"

	"bubbletea-todo-app/internal/websocket"
)

func main() {
	// Serve static files
	staticDir := filepath.Join("web", "static")
	http.Handle("/static/", http.StripPrefix("/static/", http.FileServer(http.Dir(staticDir))))

	// WebSocket endpoint
	http.HandleFunc("/ws", websocket.HandleWebSocket)

	// Main page
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/" {
			http.NotFound(w, r)
			return
		}
		http.ServeFile(w, r, filepath.Join("web", "static", "index.html"))
	})

	port := "8080"
	fmt.Printf("Starting web server on http://0.0.0.0:%s\n", port)
	fmt.Println("Open your browser and navigate to the URL above")
	
	log.Fatal(http.ListenAndServe("0.0.0.0:"+port, nil))
}

