package main

import (
	"encoding/json"
	"net/http"
	"os"
	"time"

	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/google/uuid"
	"github.com/gorilla/mux"
	orderEvents "github.com/scrapybara/kafka-watermill-project/idl/go"
	customlogger "github.com/scrapybara/kafka-watermill-project/pkg/logger"
)

// API holds dependencies for the API handlers
type API struct {
	publisher message.Publisher
	logger    *customlogger.StructuredLogger
}

// NewAPI creates a new API instance
func NewAPI(publisher message.Publisher, logger *customlogger.StructuredLogger) *API {
	return &API{
		publisher: publisher,
		logger:    logger,
	}
}

// StartServer starts the HTTP server
func (api *API) StartServer() {
	r := mux.NewRouter()

	// Routes
	r.HandleFunc("/health", api.healthHandler).Methods("GET")
	r.HandleFunc("/orders", api.createOrderHandler).Methods("POST")
	r.HandleFunc("/orders/{id}", api.getOrderHandler).Methods("GET")

	// Get port from environment or use default
	port := os.Getenv("PORT")
	if port == "" {
		port = "8001"
	}

	// Start server
	server := &http.Server{
		Addr:         ":" + port,
		Handler:      r,
		ReadTimeout:  10 * time.Second,
		WriteTimeout: 10 * time.Second,
	}

	api.logger.Info("Starting HTTP server", map[string]interface{}{
		"port": port,
	})

	if err := server.ListenAndServe(); err != nil {
		api.logger.Error("Server failed", map[string]interface{}{
			"error": err.Error(),
		})
	}
}

// healthHandler returns 200 OK if the service is healthy
func (api *API) healthHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(map[string]string{"status": "ok"})
}

// OrderRequest represents a request to create an order
type OrderRequest struct {
	UserID      string                  `json:"user_id"`
	Items       []orderEvents.OrderItem `json:"items"`
	TotalAmount float64                 `json:"total_amount"`
}

// OrderResponse represents the response from creating an order
type OrderResponse struct {
	OrderID     string                  `json:"order_id"`
	UserID      string                  `json:"user_id"`
	Items       []orderEvents.OrderItem `json:"items"`
	TotalAmount float64                 `json:"total_amount"`
	CreatedAt   string                  `json:"created_at"`
}

// createOrderHandler creates a new order and publishes an event
func (api *API) createOrderHandler(w http.ResponseWriter, r *http.Request) {
	// Generate correlation ID for request tracing
	correlationID := r.Header.Get("X-Correlation-ID")
	if correlationID == "" {
		correlationID = uuid.New().String()
	}

	// Create a logger with the correlation ID
	reqLogger := api.logger.WithCorrelationID(correlationID)

	reqLogger.Info("Processing create order request")

	var req OrderRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		reqLogger.Warn("Invalid request body", map[string]interface{}{
			"error": err.Error(),
		})
		http.Error(w, "Invalid request body", http.StatusBadRequest)
		return
	}

	// Validate request
	if req.UserID == "" || len(req.Items) == 0 {
		reqLogger.Warn("Missing required fields")
		http.Error(w, "UserID and Items are required", http.StatusBadRequest)
		return
	}

	// Calculate total amount if not provided
	if req.TotalAmount == 0 {
		for _, item := range req.Items {
			req.TotalAmount += float64(item.Quantity) * item.Price
		}
	}

	// Create order event
	orderID := uuid.New().String()
	timestamp := time.Now()

	orderLogger := reqLogger.WithOrderID(orderID)
	orderLogger.Info("Creating order", map[string]interface{}{
		"user_id":      req.UserID,
		"items_count":  len(req.Items),
		"total_amount": req.TotalAmount,
	})

	orderCreated := orderEvents.OrderCreated{
		OrderID:     orderID,
		UserID:      req.UserID,
		Items:       req.Items,
		TotalAmount: req.TotalAmount,
		Timestamp:   timestamp,
	}

	// Marshal event
	payload, err := orderEvents.ToJSON(orderCreated)
	if err != nil {
		orderLogger.Error("Error marshaling order created event", map[string]interface{}{
			"error": err.Error(),
		})
		http.Error(w, "Internal server error", http.StatusInternalServerError)
		return
	}

	// Create and publish message
	msg := message.NewMessage(uuid.New().String(), payload)
	// Add correlation ID to message metadata
	msg.Metadata.Set("correlation_id", correlationID)

	if err := api.publisher.Publish("order.created", msg); err != nil {
		orderLogger.Error("Error publishing order created event", map[string]interface{}{
			"error": err.Error(),
		})
		http.Error(w, "Failed to process order", http.StatusInternalServerError)
		return
	}

	orderLogger.Info("Order created successfully")

	// Create response
	resp := OrderResponse{
		OrderID:     orderID,
		UserID:      req.UserID,
		Items:       req.Items,
		TotalAmount: req.TotalAmount,
		CreatedAt:   timestamp.Format(time.RFC3339),
	}

	// Return response
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusCreated)
	json.NewEncoder(w).Encode(resp)
}

// getOrderHandler returns an order by ID
func (api *API) getOrderHandler(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	orderID := vars["id"]

	// Generate correlation ID for request tracing
	correlationID := r.Header.Get("X-Correlation-ID")
	if correlationID == "" {
		correlationID = uuid.New().String()
	}

	// Create a logger with the correlation ID and order ID
	reqLogger := api.logger.WithCorrelationID(correlationID).WithOrderID(orderID)

	reqLogger.Info("Getting order details")

	// In a real system, this would look up the order in a database
	// For demo purposes, we'll just return a mock response

	resp := OrderResponse{
		OrderID: orderID,
		UserID:  "user-123",
		Items: []orderEvents.OrderItem{
			{
				ProductID: "product-1",
				Name:      "Sample Product",
				Quantity:  2,
				Price:     19.99,
			},
		},
		TotalAmount: 39.98,
		CreatedAt:   time.Now().Format(time.RFC3339),
	}

	reqLogger.Info("Order details retrieved")

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(resp)
}
