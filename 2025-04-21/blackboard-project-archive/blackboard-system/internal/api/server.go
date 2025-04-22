package api

import (
	"net/http"

	"github.com/blackboard-system/internal/blackboard"
	"github.com/blackboard-system/internal/sse"
	"github.com/blackboard-system/internal/websocket"
	"github.com/gorilla/mux"
	"github.com/rs/cors"
)

// Server represents the API server
type Server struct {
	router *mux.Router
	handler *Handler
	wsHub   *websocket.Hub
	sseHub  *sse.Hub
}

// NewServer creates a new API server
func NewServer(bb *blackboard.Blackboard) *Server {
	handler := NewHandler(bb)
	router := mux.NewRouter()
	
	// Create WebSocket and SSE hubs
	wsHub := websocket.NewHub(bb)
	sseHub := sse.NewHub(bb)
	
	// Start the hubs
	go wsHub.Run()
	go sseHub.Run()
	
	server := &Server{
		router: router,
		handler: handler,
		wsHub:   wsHub,
		sseHub:  sseHub,
	}
	
	server.setupRoutes()
	return server
}

// setupRoutes sets up the API routes
func (s *Server) setupRoutes() {
	// Blackboard endpoints
	s.router.HandleFunc("/api/blackboard", s.handler.GetBlackboardState).Methods("GET")
	s.router.HandleFunc("/api/blackboard/levels/{level}", s.handler.GetHypothesesByLevel).Methods("GET")
	
	// Hypothesis endpoints
	s.router.HandleFunc("/api/hypothesis/{id}", s.handler.GetHypothesis).Methods("GET")
	s.router.HandleFunc("/api/hypothesis", s.handler.CreateHypothesis).Methods("POST")
	s.router.HandleFunc("/api/hypothesis/{id}", s.handler.UpdateHypothesis).Methods("PUT")
	s.router.HandleFunc("/api/hypothesis/{id}/lock", s.handler.LockHypothesis).Methods("PUT")
	s.router.HandleFunc("/api/hypothesis/{id}/unlock", s.handler.UnlockHypothesis).Methods("PUT")
	
	// Knowledge source endpoints
	s.router.HandleFunc("/api/knowledge-sources", s.handler.GetKnowledgeSources).Methods("GET")
	s.router.HandleFunc("/api/knowledge-source/{id}", s.handler.GetKnowledgeSource).Methods("GET")
	s.router.HandleFunc("/api/knowledge-source/{id}", s.handler.UpdateKnowledgeSource).Methods("PUT")
	s.router.HandleFunc("/api/knowledge-source/{id}/bid", s.handler.SubmitBid).Methods("POST")
	
	// Activity log endpoints
	s.router.HandleFunc("/api/activity-log", s.handler.GetActivityLog).Methods("GET")
	s.router.HandleFunc("/api/activity-log", s.handler.AddActivityLog).Methods("POST")
	
	// Focus control endpoints
	s.router.HandleFunc("/api/focus-control", s.handler.GetFocusControl).Methods("GET")
	s.router.HandleFunc("/api/focus-control", s.handler.UpdateFocusControl).Methods("PUT")
	s.router.HandleFunc("/api/focus-priorities", s.handler.GetFocusPriorities).Methods("GET")
	
	// Bid queue endpoints
	s.router.HandleFunc("/api/bid-queue", s.handler.GetBidQueue).Methods("GET")
	s.router.HandleFunc("/api/bid-queue/{id}", s.handler.DeleteBid).Methods("DELETE")
	
	// Cycle endpoint
	s.router.HandleFunc("/api/cycle", s.handler.GetCycle).Methods("GET")
	
	// WebSocket endpoint
	s.router.HandleFunc("/ws", func(w http.ResponseWriter, r *http.Request) {
		s.wsHub.ServeWs(w, r)
	})
	
	// SSE endpoint
	s.router.HandleFunc("/events", func(w http.ResponseWriter, r *http.Request) {
		s.sseHub.ServeSSE(w, r)
	})
}

// Handler returns the HTTP handler for the API
func (s *Server) Handler() http.Handler {
	// Set up CORS
	c := cors.New(cors.Options{
		AllowedOrigins:   []string{"*"},
		AllowedMethods:   []string{"GET", "POST", "PUT", "DELETE", "OPTIONS"},
		AllowedHeaders:   []string{"Accept", "Content-Type", "Content-Length", "Accept-Encoding", "Authorization"},
		AllowCredentials: true,
	})
	
	return c.Handler(s.router)
}
