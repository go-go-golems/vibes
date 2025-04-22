package sse

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"sync"
	"time"

	"github.com/blackboard-system/internal/blackboard"
)

// Client represents a connected SSE client
type Client struct {
	id           string
	messageChan  chan []byte
	disconnected chan bool
}

// Hub maintains the set of active SSE clients
type Hub struct {
	bb         *blackboard.Blackboard
	clients    map[string]*Client
	register   chan *Client
	unregister chan *Client
	mu         sync.Mutex
}

// NewHub creates a new SSE hub
func NewHub(bb *blackboard.Blackboard) *Hub {
	hub := &Hub{
		bb:         bb,
		clients:    make(map[string]*Client),
		register:   make(chan *Client),
		unregister: make(chan *Client),
	}

	// Register for blackboard events
	bb.RegisterEventHandler(func(event blackboard.Event) {
		data, err := json.Marshal(event)
		if err != nil {
			log.Printf("Error marshaling event: %v", err)
			return
		}
		hub.broadcast(data)
	})

	return hub
}

// Run starts the hub
func (h *Hub) Run() {
	for {
		select {
		case client := <-h.register:
			h.mu.Lock()
			h.clients[client.id] = client
			h.mu.Unlock()
			log.Printf("Client %s connected, total clients: %d", client.id, len(h.clients))
		case client := <-h.unregister:
			h.mu.Lock()
			if _, ok := h.clients[client.id]; ok {
				delete(h.clients, client.id)
				close(client.messageChan)
			}
			h.mu.Unlock()
			log.Printf("Client %s disconnected, total clients: %d", client.id, len(h.clients))
		}
	}
}

// broadcast sends a message to all connected clients
func (h *Hub) broadcast(message []byte) {
	h.mu.Lock()
	defer h.mu.Unlock()

	for _, client := range h.clients {
		select {
		case client.messageChan <- message:
		default:
			// Client's message channel is full, disconnect
			close(client.messageChan)
			delete(h.clients, client.id)
		}
	}
}

// ServeSSE handles SSE requests from clients
func (h *Hub) ServeSSE(w http.ResponseWriter, r *http.Request) {
	// Set headers for SSE
	w.Header().Set("Content-Type", "text/event-stream")
	w.Header().Set("Cache-Control", "no-cache")
	w.Header().Set("Connection", "keep-alive")
	w.Header().Set("Access-Control-Allow-Origin", "*")

	// Create a new client
	clientID := fmt.Sprintf("%d", time.Now().UnixNano())
	client := &Client{
		id:           clientID,
		messageChan:  make(chan []byte, 256),
		disconnected: make(chan bool),
	}

	// Register the client
	h.register <- client

	// Notify when the client disconnects
	notify := w.(http.CloseNotifier).CloseNotify()
	go func() {
		<-notify
		client.disconnected <- true
		h.unregister <- client
	}()

	// Send initial state
	state := h.bb.GetState()
	data, err := json.Marshal(map[string]interface{}{
		"type":      "initial_state",
		"data":      state,
		"timestamp": time.Now(),
	})
	if err != nil {
		log.Printf("Error marshaling initial state: %v", err)
	} else {
		fmt.Fprintf(w, "data: %s\n\n", data)
		if f, ok := w.(http.Flusher); ok {
			f.Flush()
		}
	}

	// Stream events to the client
	for {
		select {
		case <-client.disconnected:
			return
		case msg := <-client.messageChan:
			fmt.Fprintf(w, "data: %s\n\n", msg)
			if f, ok := w.(http.Flusher); ok {
				f.Flush()
			}
		case <-time.After(30 * time.Second):
			// Send a keep-alive comment
			fmt.Fprintf(w, ": keep-alive\n\n")
			if f, ok := w.(http.Flusher); ok {
				f.Flush()
			}
		}
	}
}
