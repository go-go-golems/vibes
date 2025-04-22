package main

import (
	"net/http"
	"time"

	"github.com/gin-contrib/cors"
	"github.com/gin-gonic/gin"
)

// FormData represents the form submission data
type FormData struct {
	FirstName   string `json:"firstName"`
	LastName    string `json:"lastName"`
	Email       string `json:"email"`
	City        string `json:"city"`
	ProjectIdea string `json:"projectIdea"`
}

// ChatMessage represents a message in the chat
type ChatMessage struct {
	Role    string `json:"role"` // "user" or "assistant"
	Content string `json:"content"`
}

// FormFieldUpdate represents a form field update request
type FormFieldUpdate struct {
	Field string `json:"field"`
	Value string `json:"value"`
}

// Response represents the API response
type Response struct {
	Success bool        `json:"success"`
	Message string      `json:"message"`
	Data    interface{} `json:"data,omitempty"`
}

// In-memory storage for form submissions
var submissions []FormData

// In-memory storage for chat history
var chatHistory = []ChatMessage{
	{Role: "assistant", Content: "Hello! I'm here to help you fill out the form. What's your name?"},
}

// Current form data being filled
var currentForm = FormData{}

func main() {
	r := gin.Default()

	// Configure CORS
	r.Use(cors.New(cors.Config{
		AllowOrigins:     []string{"http://localhost:3000", "http://localhost:3001", "http://localhost:3002"},
		AllowMethods:     []string{"GET", "POST", "PUT", "DELETE", "OPTIONS"},
		AllowHeaders:     []string{"Origin", "Content-Type", "Accept", "Authorization"},
		ExposeHeaders:    []string{"Content-Length"},
		AllowCredentials: true,
		MaxAge:           12 * time.Hour,
	}))

	// API routes
	api := r.Group("/api")
	{
		api.POST("/submit", submitForm)
		api.GET("/submissions", getSubmissions)
		api.GET("/health", healthCheck)
		api.POST("/chat", handleChat)
		api.POST("/set-field", setFormField)
		api.GET("/form", getCurrentForm)
	}

	// Start server
	r.Run(":8080")
}

// submitForm handles form submissions
func submitForm(c *gin.Context) {
	var formData FormData
	if err := c.ShouldBindJSON(&formData); err != nil {
		c.JSON(http.StatusBadRequest, Response{
			Success: false,
			Message: "Invalid form data",
		})
		return
	}

	// Validate form data
	if formData.FirstName == "" || formData.LastName == "" || formData.Email == "" {
		c.JSON(http.StatusBadRequest, Response{
			Success: false,
			Message: "First name, last name, and email are required",
		})
		return
	}

	// Store the submission
	submissions = append(submissions, formData)
	
	// Reset current form
	currentForm = FormData{}
	
	// Add to chat history
	chatHistory = append(chatHistory, ChatMessage{
		Role:    "assistant",
		Content: "Thank you for submitting the form!",
	})

	c.JSON(http.StatusOK, Response{
		Success: true,
		Message: "Form submitted successfully",
		Data:    formData,
	})
}

// getSubmissions returns all form submissions
func getSubmissions(c *gin.Context) {
	c.JSON(http.StatusOK, Response{
		Success: true,
		Message: "Submissions retrieved successfully",
		Data:    submissions,
	})
}

// handleChat processes chat messages and returns AI responses
func handleChat(c *gin.Context) {
	var message ChatMessage
	if err := c.ShouldBindJSON(&message); err != nil {
		c.JSON(http.StatusBadRequest, Response{
			Success: false,
			Message: "Invalid message format",
		})
		return
	}

	// Add user message to history
	chatHistory = append(chatHistory, message)

	// Mock AI response based on user input
	response := generateResponse(message.Content)
	
	// Add AI response to history
	aiMessage := ChatMessage{
		Role:    "assistant",
		Content: response,
	}
	chatHistory = append(chatHistory, aiMessage)

	c.JSON(http.StatusOK, Response{
		Success: true,
		Message: "Message processed",
		Data: map[string]interface{}{
			"reply":   aiMessage,
			"history": chatHistory,
		},
	})
}

// setFormField updates a field in the current form
func setFormField(c *gin.Context) {
	var update FormFieldUpdate
	if err := c.ShouldBindJSON(&update); err != nil {
		c.JSON(http.StatusBadRequest, Response{
			Success: false,
			Message: "Invalid field update format",
		})
		return
	}

	// Update the appropriate field
	switch update.Field {
	case "firstName":
		currentForm.FirstName = update.Value
	case "lastName":
		currentForm.LastName = update.Value
	case "email":
		currentForm.Email = update.Value
	case "city":
		currentForm.City = update.Value
	case "projectIdea":
		currentForm.ProjectIdea = update.Value
	default:
		c.JSON(http.StatusBadRequest, Response{
			Success: false,
			Message: "Invalid field name",
		})
		return
	}

	c.JSON(http.StatusOK, Response{
		Success: true,
		Message: "Field updated successfully",
		Data:    currentForm,
	})
}

// getCurrentForm returns the current form data
func getCurrentForm(c *gin.Context) {
	c.JSON(http.StatusOK, Response{
		Success: true,
		Message: "Current form data retrieved",
		Data:    currentForm,
	})
}

// healthCheck is a simple health check endpoint
func healthCheck(c *gin.Context) {
	c.JSON(http.StatusOK, Response{
		Success: true,
		Message: "API is running",
	})
}

// generateResponse creates a mock AI response based on user input
func generateResponse(input string) string {
	// Simple keyword-based response generation
	if contains(input, "name") {
		return "I'll help you with your name. What's your first and last name?"
	} else if contains(input, "email") {
		return "What email address would you like to use?"
	} else if contains(input, "city") {
		return "What city do you live in?"
	} else if contains(input, "project") || contains(input, "idea") {
		return "Do you have any project ideas you'd like to share?"
	} else if contains(input, "hello") || contains(input, "hi") {
		return "Hello! I'm here to help you fill out the form. What information would you like to provide first?"
	} else if contains(input, "thank") {
		return "You're welcome! Is there anything else you'd like to add to the form?"
	} else {
		return "I'm here to help you fill out the form. You can tell me your name, email, city, or project idea."
	}
}

// contains checks if a string contains a substring (case insensitive)
func contains(s, substr string) bool {
	s, substr = toLowerCase(s), toLowerCase(substr)
	return s == substr || len(s) >= len(substr) && s[:len(substr)] == substr || len(s) >= len(substr) && s[len(s)-len(substr):] == substr || len(s) >= len(substr) && s[len(s)/2-len(substr)/2:len(s)/2+len(substr)/2] == substr
}

// toLowerCase converts a string to lowercase
func toLowerCase(s string) string {
	result := ""
	for _, c := range s {
		if c >= 'A' && c <= 'Z' {
			result += string(c + 32)
		} else {
			result += string(c)
		}
	}
	return result
}
